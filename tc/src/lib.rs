use std::{
    borrow::Borrow,
    convert::Infallible,
    hash::{BuildHasherDefault, Hash, Hasher},
};

use aiahr_core::{
    ast::{Ast, Term, Term::*},
    id::{ItemId, ModuleId, VarId},
    memory::handle::{Handle, RefHandle},
};
use bumpalo::Bump;
use ena::unify::InPlaceUnificationTable;
use parking_lot::RwLock;
use rustc_hash::{FxHashMap, FxHasher};

mod ty;

use ty::{TypeKind::*, *};

/// Errors that may be produced during type checking
#[derive(Debug, PartialEq, Eq)]
pub enum TypeCheckError<'ty> {
    /// A variable we expected to exist with a type, did not
    VarNotDefined(VarId),
    ItemNotDefined((ModuleId, ItemId)),
    TypeMismatch(InferTy<'ty>, InferTy<'ty>),
    OccursCheckFailed(TcUnifierVar<'ty>),
}

impl<'ty> From<(Ty<'ty, TcUnifierVar<'ty>>, Ty<'ty, TcUnifierVar<'ty>>)> for TypeCheckError<'ty> {
    fn from((left, right): (Ty<'ty, TcUnifierVar<'ty>>, Ty<'ty, TcUnifierVar<'ty>>)) -> Self {
        TypeCheckError::TypeMismatch(left, right)
    }
}

/// A constraint produced during initial type checking.
/// It will be solved in the second half of type checking to produce a map from Unifier variables
/// to their types.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Constraint<Ty> {
    /// Two types must be equal for our term to type check
    Eq(Ty, Ty),
}

/// A context for type inference.
///
/// Type inference is done in two stages:
///     1. Constraint generation
///     2. Unification
/// During 1. we use a bidirectional type checker to walk our input AST and generate a set of
/// constraints (and unification variables) that have to be true for the ast to type check. This
/// stage also produces a mapping from variables in the AST to their types.
///
/// Once we have this set of constraints we solve them via unification. Unification maintains a
/// mapping from unification variables to their type. Each constraint is decomposed into a list of
/// (uni var, type) pairs that are saved in the mapping. At the end of this process we will either
/// encounter an error (e.g. If we try to unify int and a function type), or we will produce a
/// substitution from unification variables to types (possibly including type variables).
///
/// With this substitution constructed we walk the variable -> type mapping and substitute any
/// unification variables for their types. This process of unification variable removal is called
/// `zonking`.
struct InferCtx<'infer> {
    /// Store types for local variables.
    local_env: FxHashMap<VarId, InferTy<'infer>>,
    /// Mapping from unification variables to their types (if any).
    unifiers: InPlaceUnificationTable<TcUnifierVar<'infer>>,
    /// Errors that arise during type checking
    errors: Vec<TypeCheckError<'infer>>,
}

/// Check that a unification variable does not appear within the type the unification variable is
/// mapped to. This prevents unification from solving to a substitution with a cycle.
struct OccursCheck<'a, 'inf, I> {
    ctx: &'a I,
    var: TcUnifierVar<'inf>,
}
impl<'inf, I> FallibleTypeFold<'inf> for OccursCheck<'_, 'inf, I>
where
    I: MkTy<'inf, TcUnifierVar<'inf>>,
{
    type Error = TypeCheckError<'inf>;
    type TypeVar = TcUnifierVar<'inf>;
    type InTypeVar = TcUnifierVar<'inf>;

    fn ctx(&self) -> &dyn MkTy<'inf, TcUnifierVar<'inf>> {
        self.ctx
    }

    fn try_fold_var(&mut self, var: Self::TypeVar) -> Result<Ty<'inf, Self::TypeVar>, Self::Error> {
        if var == self.var {
            Err(TypeCheckError::OccursCheckFailed(var))
        } else {
            Ok(self.ctx.mk_ty(VarTy(var)))
        }
    }
}

/// Normalize a type for unification.
/// Walks a type and checks any variables it contains against current unifiers. Replacing
/// unification variables by their value when present.
struct Normalize<'a, 'inf, I> {
    ctx: &'a I,
    unifiers: &'a mut InPlaceUnificationTable<TcUnifierVar<'inf>>,
}
impl<'inf, I> FallibleTypeFold<'inf> for Normalize<'_, 'inf, I>
where
    I: MkTy<'inf, TcUnifierVar<'inf>>,
{
    type Error = Infallible;
    type TypeVar = TcUnifierVar<'inf>;
    type InTypeVar = TcUnifierVar<'inf>;

    fn ctx(&self) -> &dyn MkTy<'inf, TcUnifierVar<'inf>> {
        self.ctx
    }

    fn try_fold_var(
        &mut self,
        var: Self::InTypeVar,
    ) -> Result<Ty<'inf, Self::TypeVar>, Self::Error> {
        Ok(match self.unifiers.probe_value(var) {
            Some(ty) => ty,
            None => self.ctx().mk_ty(VarTy(var)),
        })
    }
}

impl<'infer> InferCtx<'infer> {
    /// This is the entrypoint to the bidirectional type checker. Since our language uses
    /// damnas-milner type inference we will always begin type checking with a call to infer.
    pub fn infer<I>(
        &mut self,
        infer_ctx: &I,
        term: &Term<'_, VarId>,
    ) -> (
        Vec<Constraint<InferTy<'infer>>>,
        FxHashMap<VarId, InferTy<'infer>>,
        InferTy<'infer>,
    )
    where
        I: MkTy<'infer, TcUnifierVar<'infer>>,
    {
        let mut constraints = vec![];
        let mut var_tys = FxHashMap::default();
        let ty = self._infer(infer_ctx, &mut constraints, &mut var_tys, term);
        (constraints, var_tys, ty)
    }

    /// Check a term against a given type.
    /// This method pairs with _infer to form a bidirectional type checker
    fn _check<I>(
        &mut self,
        infer_ctx: &I,
        constraints: &mut Vec<Constraint<InferTy<'infer>>>,
        var_tys: &mut FxHashMap<VarId, InferTy<'infer>>,
        term: &Term<'_, VarId>,
        expected_ty: InferTy<'infer>,
    ) -> ()
    where
        I: MkTy<'infer, TcUnifierVar<'infer>>,
    {
        use TypeKind::*;
        match (term, *expected_ty) {
            (Abstraction { arg, body }, FunTy(arg_ty, body_ty)) => {
                // Check an abstraction against a function type by checking the body checks against
                // the function return type with the function argument type in scope.
                self.local_env.insert(*arg, arg_ty);
                self._check(infer_ctx, constraints, var_tys, body, body_ty);
                self.local_env.remove(arg);
            }
            // Bucket case for when we need to check a rule against a type but no case applies
            (term, _) => {
                // Infer a type for our term and check that the expected type is equal to the
                // inferred type.
                let inferred_ty = self._infer(infer_ctx, constraints, var_tys, term);
                constraints.push(Constraint::Eq(expected_ty, inferred_ty));
            }
        }
    }

    /// Infer a type for a term
    /// This method pairs with check to form a bidirectional type checker
    fn _infer<I>(
        &mut self,
        infer_ctx: &I,
        constraints: &mut Vec<Constraint<InferTy<'infer>>>,
        var_tys: &mut FxHashMap<VarId, InferTy<'infer>>,
        term: &Term<'_, VarId>,
    ) -> InferTy<'infer>
    where
        I: MkTy<'infer, TcUnifierVar<'infer>>,
    {
        match term {
            // Abstraction inference  is done by creating two new unifiers <arg> and <body>
            // The abstraction body is checked against these fresh type variables
            // The resulting type of the inference is function type <arg> -> <body>
            Abstraction { arg, body } => {
                let arg_ty = infer_ctx.mk_ty(VarTy(self.unifiers.new_key(None)));
                let body_ty = infer_ctx.mk_ty(VarTy(self.unifiers.new_key(None)));

                var_tys.insert(*arg, arg_ty);
                self.local_env.insert(*arg, arg_ty);
                self._check(infer_ctx, constraints, var_tys, body, body_ty);
                self.local_env.remove(arg);

                infer_ctx.mk_ty(TypeKind::FunTy(arg_ty, body_ty))
            }
            // Application inference starts by inferring types for the func of the application.
            // We equate this inferred type to a function type, generating fresh unifiers if
            // needed.
            // We then check the arg of the application against the arg type of the function type
            // The resulting type of this application is the function result type.
            Application { func, arg } => {
                let fun_ty = self._infer(infer_ctx, constraints, var_tys, func);
                // Optimization: eagerly use FunTy if available. Otherwise dispatch fresh unifiers
                // for arg and ret type.
                let (arg_ty, ret_ty) = match *fun_ty {
                    FunTy(arg_ty, ret_ty) => (arg_ty, ret_ty),
                    _ => {
                        let arg_ty = infer_ctx.clone().mk_ty(VarTy(self.unifiers.new_key(None)));
                        let ret_ty = infer_ctx.clone().mk_ty(VarTy(self.unifiers.new_key(None)));
                        constraints.push(Constraint::Eq(
                            fun_ty,
                            infer_ctx.mk_ty(FunTy(arg_ty, ret_ty)),
                        ));
                        (arg_ty, ret_ty)
                    }
                };

                self._check(infer_ctx, constraints, var_tys, arg, arg_ty);

                ret_ty
            }
            // If the variable is in environemnt return it's type, otherwise return an error.
            Variable(var) => {
                if let Some(ty) = self.local_env.get(var).cloned() {
                    var_tys.insert(*var, ty);
                    ty
                } else {
                    self.errors.push(TypeCheckError::VarNotDefined(*var));
                    let err_ty = infer_ctx.mk_ty(ErrorTy);
                    var_tys.insert(*var, err_ty);
                    err_ty
                }
            }
            Label { .. } => todo!(),
            // TODOs
            Item(_) => todo!(),
            Unit => todo!(),
            Concat { .. } => todo!(),
        }
    }

    fn normalize_ty<I>(&mut self, ctx: &I, ty: InferTy<'infer>) -> InferTy<'infer>
    where
        I: MkTy<'infer, TcUnifierVar<'infer>>,
    {
        ty.try_fold_with(&mut Normalize {
            ctx,
            unifiers: &mut self.unifiers,
        })
        .unwrap()
    }

    /// Unify a variable and a type.
    /// This checks that the variable is not present in type, throwing an error if varaibles is
    /// present.
    /// If not we record that the unification variable is solved to given type.
    fn unify_var_ty<I>(
        &mut self,
        ctx: &I,
        var: TcUnifierVar<'infer>,
        ty: InferTy<'infer>,
    ) -> Result<(), TypeCheckError<'infer>>
    where
        I: MkTy<'infer, TcUnifierVar<'infer>>,
    {
        let ty_ = ty.try_fold_with(&mut OccursCheck { ctx, var })?;
        self.unifiers
            .unify_var_value(var, Some(ty_))
            .map_err(|(left, right)| TypeCheckError::TypeMismatch(left, right))
    }

    /// This is the main entry point of unificaiton and handles unifying two arbitrary types.
    /// Each type is substituted by the current substitution to remove as many unification
    /// variables as possible before unifying.
    fn unify_ty_ty<I>(
        &mut self,
        ctx: &I,
        left: InferTy<'infer>,
        right: InferTy<'infer>,
    ) -> Result<(), TypeCheckError<'infer>>
    where
        I: MkTy<'infer, TcUnifierVar<'infer>>,
    {
        // Apply current partial substitution before comparing
        let normal_left = self.normalize_ty(ctx, left);
        let normal_right = self.normalize_ty(ctx, right);

        match (*normal_left, *normal_right) {
            // If an error appears anywhere fail unification
            (ErrorTy, _) | (_, ErrorTy) => Err(TypeCheckError::TypeMismatch(left, right)),

            // Special case for when two keys meet
            // Instead of unifiying either variable as a value of the other, we need to record that
            // the two key's equivalence classes must be the same.
            (VarTy(left_var), VarTy(right_var)) => self
                .unifiers
                .unify_var_var(left_var, right_var)
                .map_err(|e| e.into()),

            // If a key meets a new ty record they must be equal
            (_, VarTy(var)) => self.unify_var_ty(ctx, var, left),
            (VarTy(var), _) => self.unify_var_ty(ctx, var, right),

            // Decompose compound types
            (FunTy(left_arg, left_ret), FunTy(right_arg, right_ret)) => {
                self.unify_ty_ty(ctx.clone(), left_arg, right_arg)?;
                self.unify_ty_ty(ctx, left_ret, right_ret)
            }
            (RowTy(_left_row), RowTy(_right_row)) => todo!(),
            // Discharge equal types
            (IntTy, IntTy) => Ok(()),

            // Type mismatch
            (IntTy, FunTy(_, _))
            | (FunTy(_, _), IntTy)
            | (IntTy, RowTy(_))
            | (RowTy(_), IntTy)
            | (RowTy(_), FunTy(_, _))
            | (FunTy(_, _), RowTy(_)) => Err(TypeCheckError::TypeMismatch(left, right)),
        }
    }

    /// Solve a list of constraints to a mapping from unifiers to types.
    /// If there is no solution to the list of constraints we return a relevant error.
    fn solve<I>(
        mut self,
        ctx: &I,
        constraints: Vec<Constraint<InferTy<'infer>>>,
    ) -> (
        InPlaceUnificationTable<TcUnifierVar<'infer>>,
        Vec<TypeCheckError<'infer>>,
    )
    where
        I: MkTy<'infer, TcUnifierVar<'infer>>,
    {
        for constr in constraints {
            match constr {
                Constraint::Eq(left, right) => self
                    .unify_ty_ty(ctx.clone(), left, right)
                    .map_err(|err| {
                        self.errors.push(err);
                        ()
                    })
                    .unwrap_or_default(),
            }
        }
        (self.unifiers, self.errors)
    }
}

/// Zonk a thing
/// This removes all unification variables.
/// If a unification variables is solved to a type, it is replaced by that type.
/// If a unification variable has no solution, we replace it by a fresh type variable and record it
/// as free.
pub struct Zonker<'a, 'ctx, 'infer> {
    ctx: &'a dyn MkTy<'ctx, TcVar>,
    unifiers: &'a mut InPlaceUnificationTable<TcUnifierVar<'infer>>,
    free_vars: Vec<TcUnifierVar<'infer>>,
}

impl<'ctx, 'infer> FallibleTypeFold<'ctx> for Zonker<'_, 'ctx, 'infer> {
    type Error = Infallible;
    type TypeVar = TcVar;
    type InTypeVar = TcUnifierVar<'infer>;

    fn ctx(&self) -> &dyn MkTy<'ctx, Self::TypeVar> {
        self.ctx
    }

    fn try_fold_var(
        &mut self,
        var: Self::InTypeVar,
    ) -> Result<Ty<'ctx, Self::TypeVar>, Self::Error> {
        match self.unifiers.probe_value(var) {
            Some(ty) => ty.try_fold_with(self),
            None => {
                // Our unification variable wasn't solved to a type.
                // Find the root unification variable and return a type varaible representing that
                // root.
                let root = self.unifiers.find(var);
                let var_indx = self
                    .free_vars
                    .iter()
                    .position(|uv| &root == uv)
                    // We have not seen this unification variable before, so create a new one.
                    .unwrap_or_else(|| {
                        let next_index = self.free_vars.len();
                        self.free_vars.push(var);
                        next_index
                    });
                Ok(self.ctx().mk_ty(VarTy(TcVar(var_indx))))
            }
        }
    }
}

fn type_check<'ty, 'infer, I, II>(
    ty_ctx: &I,
    infer_ctx: &II,
    ast: Ast<'_, VarId>,
) -> (
    FxHashMap<VarId, Ty<'ty, TcVar>>,
    Ty<'ty, TcVar>,
    Vec<TypeCheckError<'infer>>,
)
where
    I: MkTy<'ty, TcVar>,
    II: MkTy<'infer, TcUnifierVar<'infer>>,
{
    tc_term(ty_ctx, infer_ctx, ast.root())
}

fn tc_term<'ty, 'infer, I, II>(
    ty_ctx: &I,
    infer_ctx: &II,
    term: &'_ Term<'_, VarId>,
) -> (
    FxHashMap<VarId, Ty<'ty, TcVar>>,
    Ty<'ty, TcVar>,
    Vec<TypeCheckError<'infer>>,
)
where
    I: MkTy<'ty, TcVar>,
    II: MkTy<'infer, TcUnifierVar<'infer>>,
{
    let mut infer = InferCtx {
        local_env: FxHashMap::default(),
        errors: vec![],
        unifiers: InPlaceUnificationTable::default(),
    };

    // Infer types for all our variables and the root term.
    let (constraints, var_tys, ty) = infer.infer(infer_ctx, term);

    // Solve constraints into the unifiers mapping.
    let (mut unifiers, errors) = infer.solve(infer_ctx, constraints);

    // Zonk the variable -> type mapping and the root term type.
    let mut zoinkies = Zonker {
        ctx: ty_ctx,
        unifiers: &mut unifiers,
        free_vars: vec![],
    };
    let zonked_ty = ty.try_fold_with(&mut zoinkies).unwrap();
    let zonked_var_tys = var_tys
        .into_iter()
        .map(|(var, ty)| (var, ty.try_fold_with(&mut zoinkies).unwrap()))
        .collect::<FxHashMap<_, _>>();
    // TODO: Generalize root type as a scheme
    (zonked_var_tys, zonked_ty, errors)
}

const SHARD_BITS: usize = 5;
const SHARDS: usize = 1 << SHARD_BITS;

#[inline]
fn get_shard_index_by_hash(hash: u64) -> usize {
    let hash_len = std::mem::size_of::<usize>();
    let bits = (hash >> (hash_len * 8 - 7 - SHARD_BITS)) as usize;
    bits % SHARDS
}
fn make_hash<K: Hash + ?Sized>(val: &K) -> u64 {
    let mut state = FxHasher::default();
    val.hash(&mut state);
    state.finish()
}

struct Sharded<T> {
    shards: [parking_lot::RwLock<T>; SHARDS],
}

impl<T> Sharded<T> {
    fn new(mut mk_t: impl FnMut() -> T) -> Self {
        Self {
            shards: [(); SHARDS].map(|()| RwLock::new(mk_t())),
        }
    }

    fn get_shard_by_hash(&self, hash: u64) -> &RwLock<T> {
        &self.shards[get_shard_index_by_hash(hash)]
    }
}
impl<T: Default> Default for Sharded<T> {
    fn default() -> Self {
        Self::new(T::default)
    }
}

type ShardedHashMap<K, V> = Sharded<hashbrown::HashMap<K, V, BuildHasherDefault<FxHasher>>>;

impl<K: Eq + Hash + Copy> ShardedHashMap<K, ()> {
    fn _intern<Q>(&self, value: Q, make: impl FnOnce(Q) -> K) -> K
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        let hash = make_hash(&value);
        let mut shard = self.get_shard_by_hash(hash).write();
        let entry = shard.raw_entry_mut().from_key_hashed_nocheck(hash, &value);

        match entry {
            hashbrown::hash_map::RawEntryMut::Occupied(e) => *e.key(),
            hashbrown::hash_map::RawEntryMut::Vacant(e) => {
                let v = make(value);
                e.insert_hashed_nocheck(hash, v, ());
                v
            }
        }
    }

    fn _intern_ref<Q: ?Sized>(&self, value: &Q, make: impl FnOnce() -> K) -> K
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        let hash = make_hash(value);
        let mut shard = self.get_shard_by_hash(hash).write();
        let entry = shard.raw_entry_mut().from_key_hashed_nocheck(hash, value);

        match entry {
            hashbrown::hash_map::RawEntryMut::Occupied(e) => *e.key(),
            hashbrown::hash_map::RawEntryMut::Vacant(e) => {
                let v = make();
                e.insert_hashed_nocheck(hash, v, ());
                v
            }
        }
    }
}

// This looks frustratingly close to SyncInterner, except we hold a &'ctx Bump instead of an owned
// A. Which allows us to produce interned values with lifetime `'ctx` without TyCtx needing to be a
// `&'ctx TyCtx<...>`.
// TODO: Look into ways to represent this style of allocation with existing intern stuff.
//  1. We need to change
//      `intern<'a>(&'a self, T) -> RefHandle<'a, T>`
//  to
//      `intern(&self, T) -> RefHandler<'a, T>`
//
//  2. Similarly Arena needs to be decoupled from `&'a self`
struct TyCtx<'ctx, TV> {
    arena: &'ctx Bump,
    tys: ShardedHashMap<RefHandle<'ctx, TypeKind<'ctx, TV>>, ()>,
    labels: ShardedHashMap<RefHandle<'ctx, str>, ()>,
    row_labels: ShardedHashMap<RefHandle<'ctx, [RowLabel<'ctx>]>, ()>,
    row_values: ShardedHashMap<RefHandle<'ctx, [Ty<'ctx, TV>]>, ()>,
}

impl<'ctx, TV> TyCtx<'ctx, TV>
where
    TV: Eq + Copy + Hash,
{
    fn new(arena: &'ctx Bump) -> Self {
        Self {
            arena,
            tys: ShardedHashMap::default(),
            row_labels: ShardedHashMap::default(),
            row_values: ShardedHashMap::default(),
            labels: ShardedHashMap::default(),
        }
    }

    fn intern_ty(&self, kind: TypeKind<'ctx, TV>) -> RefHandle<'ctx, TypeKind<'ctx, TV>> {
        self.tys._intern(kind, |kind| {
            let kind_ref = self.arena.alloc(kind);
            Handle(kind_ref)
        })
    }

    fn intern_labels(&self, labels: &[RowLabel<'ctx>]) -> RefHandle<'ctx, [RowLabel<'ctx>]> {
        self.row_labels
            ._intern_ref(labels, || Handle(self.arena.alloc_slice_copy(labels)))
    }

    fn intern_values(&self, values: &[Ty<'ctx, TV>]) -> RefHandle<'ctx, [Ty<'ctx, TV>]> {
        self.row_values
            ._intern_ref(values, || Handle(self.arena.alloc_slice_copy(values)))
    }

    fn intern_label(&self, label: &str) -> RowLabel<'ctx> {
        self.labels
            ._intern_ref(label, || Handle(self.arena.alloc_str(label)))
    }
}

impl<'ctx, TV> MkTy<'ctx, TV> for TyCtx<'ctx, TV>
where
    TV: Eq + Copy + Hash,
{
    fn mk_ty(&self, kind: TypeKind<'ctx, TV>) -> Ty<'ctx, TV> {
        Ty(self.intern_ty(kind))
    }

    fn mk_label(&self, label: &str) -> RowLabel<'ctx> {
        self.intern_label(label)
    }

    fn mk_row(&self, labels: &[RowLabel<'ctx>], values: &[Ty<'ctx, TV>]) -> ClosedRow<'ctx, TV> {
        ClosedRow {
            labels: self.intern_labels(labels),
            values: self.intern_values(values),
        }
    }
}

#[cfg(test)]
mod tests {
    use aiahr_core::ast::Ast;
    use assert_matches::assert_matches;
    use bumpalo::Bump;

    use super::*;

    macro_rules! ty {
        ($kind:pat) => {
            Ty(Handle($kind))
        };
    }

    #[test]
    fn test_tc_abs() {
        let arena = Bump::new();
        let untyped_ast = Ast::new(
            FxHashMap::default(),
            arena.alloc(Abstraction {
                arg: VarId(0),
                body: arena.alloc(Abstraction {
                    arg: VarId(1),
                    body: arena.alloc(Variable(VarId(0))),
                }),
            }),
        );
        let infer_intern = TyCtx::new(&arena);
        let ty_intern = TyCtx::new(&arena);

        let (var_to_tys, ty, _) = type_check(&ty_intern, &infer_intern, untyped_ast);

        assert_matches!(var_to_tys.get(&VarId(0)), Some(ty!(VarTy(TcVar(0)))));
        assert_matches!(var_to_tys.get(&VarId(1)), Some(ty!(VarTy(TcVar(1)))));
        assert_matches!(
            ty,
            ty!(FunTy(
                ty!(VarTy(TcVar(0))),
                ty!(FunTy(ty!(VarTy(TcVar(1))), ty!(VarTy(TcVar(0))),)),
            ))
        );
    }

    #[test]
    fn test_tc_undefined_var_fails() {
        let arena = Bump::new();
        let untyped_ast = Ast::new(FxHashMap::default(), arena.alloc(Variable(VarId(0))));
        let infer_ctx = TyCtx::new(&arena);
        let ty_ctx = TyCtx::new(&arena);

        let (_, _, errors) = type_check(&infer_ctx, &ty_ctx, untyped_ast);

        assert!(errors.contains(&TypeCheckError::VarNotDefined(VarId(0))))
    }
}
