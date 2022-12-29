use std::{
    borrow::Borrow,
    hash::{BuildHasherDefault, Hasher}, convert::Infallible,
};

use aiahr_core::{
    ast::{Term, Term::*},
    id::{IdGen, ItemId, ModuleId, VarId},
    memory::handle::{Handle, RefHandle},
};
use bumpalo::Bump;
use ena::unify::InPlaceUnificationTable;
use parking_lot::RwLock;
use rustc_hash::{FxHashMap, FxHasher};

mod constraint;
mod subst;
mod ty;

use constraint::Constraint;
use ty::{TypeKind::*, *};

/// A type scheme. This is a type with all it's free type variables bound and it's required
/// constraints explicitly listed
struct Scheme<'ty, TV> {
    vars: IdGen<TV, ()>,
    constraints: Vec<Constraint<&'ty TypeKind<'ty, TV>>>,
    ty: &'ty TypeKind<'ty, TV>,
}

/// Errors that may be produced during type checking
#[derive(Debug, PartialEq, Eq)]
pub enum TypeCheckError<'ty> {
    /// A variable we expected to exist with a type, did not
    VarNotDefined(VarId),
    ItemNotDefined((ModuleId, ItemId)),
    // TODO: Consider whether we should expose the unifier var stuff in errors here or not?

    //UnsolvedTyVariable(UnifierTypeId),
    //UnresolvedTypes(TcUnifierVar, Vec<&'ty UnifierType<'ty>>),
    // TODO: Don't leak constraints in the public API here
    //UnsolvedConstraints(Vec<Constraint<&'ty UnifierType<'ty>>>),
    TypeMismatch(Ty<'ty, TcUnifierVar<'ty>>, Ty<'ty, TcUnifierVar<'ty>>),
    OccursCheckFailed(TcUnifierVar<'ty>),
}

impl<'ty> From<(Ty<'ty, TcUnifierVar<'ty>>, Ty<'ty, TcUnifierVar<'ty>>)> for TypeCheckError<'ty> {
    fn from((left, right): (Ty<'ty, TcUnifierVar<'ty>>, Ty<'ty, TcUnifierVar<'ty>>)) -> Self {
        TypeCheckError::TypeMismatch(left, right)
    }
}

/// # A bidirectional damnas-milner type checker.
///
/// Type checking a term happens in two main phases: Generating constraints, and solving constraints.
///
/// ## Constraint Generation
/// Constraint generation is done by two mutually recursive methods, infer and check.
///
/// * `infer` - generates a type based on the input term and will internally call check to verify
/// subterms.
/// * `check` - checks a term against an expected type, recording an error if types cannot match.
/// If no specific case applies, check will call infer and check the expected type is equal to the
/// inferred type.
///
/// ## Constraint Solving
/// Once constraint generation is completed we have a typed ast that contains unifiers. We move on
/// to solving the generated constraints. This is done by `ConstraintSolver` which runs a straight
/// forward loop to find active pairs and reduce them in the cosntraint graph.
///
/// The output of this solution will be a substitution from type variables (include unifier vars)
/// to their concrete types. Chains of type variables { a := b, b := c, c := int } are flattened to
/// { a := int, b := int, c : = int }.
///
/// We check for any unsolved unifier variables, any unifiers that are free are given fresh type
/// variables. This will generalize our results when we zonk away all our unifier variables.
///
/// ### Zonking (for lack of a better term)
/// The substitution produced by constraint solving is used for one final step in producing our type
/// checked term. We must ensure all unifier variables are removed from the typed term and
/// resulting type. They cannot leak outside the type checker.
///
/// This process is referred to as zonking, I won't defend that name. Nor will the people I've
/// deftly swiped it from. But I will use it to refer to the process that finds unifier variables
/// in our output and resolves them to the concrete types they are mapped to in our substitution.
/*struct TypeChecker<'ast, 'ty> {
    /// The context to allocate typed outputs of
    ctx: TyCtx<'ast, 'ty>,
    // convenience allocation so we don't have to realloc ErrorTy everytime we need it
    error_ty: &'ty UnifierType<'ty>,
    /// Store types for local variables.
    local_env: FxHashMap<VarId, &'ty UnifierType<'ty>>,
    /// Store type schemes for global variables.
    global_env: FxHashMap<(ModuleId, ItemId), Scheme<'ty, TcVar>>,
    /// List of unifiers and their current mapped types, if any.
    unifiers: UnifierSubst<'ty>, //Vec<Option<&'ty UnifierType<'ty>>>,
    /// List of errors produced during type checking
    errors: Vec<TypeCheckError<'ty>>,
    /// Constraints generated during intial stage of type checking,
    /// to be solved later.
    constraints: Vec<Constraint<&'ty UnifierType<'ty>>>,
    /// Supply of fresh type variables for when we generalize our term.
    type_var_supply: IdGen<TcVar, ()>,
}*/

struct InferCtx<'infer> {
    /// Store types for local variables.
    local_env: FxHashMap<VarId, InferTy<'infer>>,
    errors: Vec<TypeCheckError<'infer>>,
    unifiers: InPlaceUnificationTable<TcUnifierVar<'infer>>,
}

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

impl<'infer> InferCtx<'infer> {
    /// Check a term against a given type.
    /// This method pairs with infer to form a bidirectional type checker
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
            // TODOs
            Item(_) => todo!(),
            Unit => todo!(),
            Concat { .. } => todo!(),
            Label { .. } => todo!(),
        }
    }

    fn normalize_ty(&mut self, ty: InferTy<'infer>) -> Option<InferTy<'infer>> {
        match *ty {
            VarTy(var) => self.unifiers.probe_value(var),
            _ => None,
        }
    }

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

    fn unify_ty_ty<I>(
        &mut self,
        ctx: &I,
        left: InferTy<'infer>,
        right: InferTy<'infer>,
    ) -> Result<(), TypeCheckError<'infer>>
    where
        I: MkTy<'infer, TcUnifierVar<'infer>>,
    {
        let normal_left = self.normalize_ty(left).unwrap_or(left);
        let normal_right = self.normalize_ty(right).unwrap_or(right);

        match (*normal_left, *normal_right) {
            // If an error appears anywhere fail unification
            (ErrorTy, _) | (_, ErrorTy) => Err(TypeCheckError::TypeMismatch(left, right)),

            // Special case for when two keys meet
            (VarTy(left_var), VarTy(right_var)) => self
                .unifiers
                .unify_var_var(left_var, right_var)
                .map_err(|e| e.into()),

            // If a key meets a new ty try to set them
            (_, VarTy(var)) => self.unify_var_ty(ctx, var, left),
            (VarTy(var), _) => self.unify_var_ty(ctx, var, right),

            // Decompose compound types
            (FunTy(left_arg, left_ret), FunTy(right_arg, right_ret)) => {
                self.unify_ty_ty(ctx.clone(), left_arg, right_arg)?;
                self.unify_ty_ty(ctx, left_ret, right_ret)
            }
            // Discharge equal types
            (IntTy, IntTy) => Ok(()),

            // Type mismatch
            (IntTy, FunTy(_, _)) | (FunTy(_, _), IntTy) => {
                Err(TypeCheckError::TypeMismatch(left, right))
            }
        }
    }

    fn solve<I>(
        mut self,
        ctx: &I,
        constraints: Vec<Constraint<InferTy<'infer>>>,
    ) -> Result<InPlaceUnificationTable<TcUnifierVar<'infer>>, TypeCheckError<'infer>>
    where
        I: MkTy<'infer, TcUnifierVar<'infer>>,
    {
        // TODO: remove this clone
        for constr in constraints {
            match constr {
                Constraint::Eq(left, right) => self.unify_ty_ty(ctx.clone(), left, right)?,
            }
        }
        Ok(self.unifiers)
    }
}

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

    fn try_fold_var(&mut self, var: Self::InTypeVar) -> Result<Ty<'ctx, Self::TypeVar>, Self::Error> { 
        match self.unifiers.probe_value(var) {
            Some(ty) => {
                ty.try_fold_with(self)
            },
            None => {
                let root = self.unifiers.find(var);
                let var_indx = self.free_vars.iter()
                    .position(|uv| &root == uv)
                    .unwrap_or_else(|| {
                        let next_index = self.free_vars.len();
                        self.free_vars.push(var);
                        next_index
                    });
                Ok(self.ctx().mk_ty(VarTy(TcVar(var_indx))))
            },
        }
    }
}

pub fn tc_term<'ty, 'infer, I, II>(
    ty_ctx: &I,
    infer_ctx: &II,
    term: &'_ Term<'_, VarId>,
) -> (FxHashMap<VarId, Ty<'ty, TcVar>>, Ty<'ty, TcVar>)
where
    I: MkTy<'ty, TcVar>,
    II: MkTy<'infer, TcUnifierVar<'infer>>,
{
    let mut infer = InferCtx {
        local_env: FxHashMap::default(),
        errors: vec![],
        unifiers: InPlaceUnificationTable::default(),
    };
    let (constraints, var_tys, ty) = infer.infer(infer_ctx, term);
    let mut unifiers = infer
        .solve(infer_ctx, constraints)
        .expect("TODO: error reporting");
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
    (zonked_var_tys, zonked_ty)
}

const SHARD_BITS: usize = 5;
const SHARDS: usize = 1 << SHARD_BITS;

#[inline]
fn get_shard_index_by_hash(hash: u64) -> usize {
    let hash_len = std::mem::size_of::<usize>();
    let bits = (hash >> (hash_len * 8 - 7 - SHARD_BITS)) as usize;
    bits % SHARDS
}
fn make_hash<K: std::hash::Hash + ?Sized>(val: &K) -> u64 {
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

impl<K: Eq + std::hash::Hash + Copy> ShardedHashMap<K, ()> {
    fn _intern<'a, Q>(&'a self, value: Q, make: impl FnOnce(Q) -> K) -> K
    where
        K: Borrow<Q>,
        Q: std::hash::Hash + Eq,
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
}

struct TyCtx<'ctx, TV> {
    arena: &'ctx Bump,
    tys: ShardedHashMap<RefHandle<'ctx, TypeKind<'ctx, TV>>, ()>,
}

impl<'ctx, TV> TyCtx<'ctx, TV>
where
    TV: Eq + Copy + std::hash::Hash,
{
    fn intern(&self, kind: TypeKind<'ctx, TV>) -> RefHandle<'ctx, TypeKind<'ctx, TV>> {
        self.tys._intern(kind, |kind| {
            let kind_ref = self.arena.alloc(kind);
            Handle(kind_ref)
        })
    }
}

impl<'ctx, TV> MkTy<'ctx, TV> for TyCtx<'ctx, TV>
where
    TV: Eq + Copy + std::hash::Hash,
{
    fn mk_ty(&self, kind: TypeKind<'ctx, TV>) -> Ty<'ctx, TV> {
        Ty(self.intern(kind))
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
        let infer_intern = TyCtx {
            arena: &arena,
            tys: ShardedHashMap::default(),
        };
        let ty_intern = TyCtx {
            arena: &arena,
            tys: ShardedHashMap::default(),
        };

        let (var_to_tys, ty) = tc_term(&ty_intern, &infer_intern, untyped_ast.root());

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

    
    /*
    #[test]
    fn test_tc_undefined_var_fails() {
        let arena = Bump::new();
        let untyped_ast = Ast::new(
            FxHashMap::default(),
            arena.alloc(Variable(VarId(0)))
        );

        let (_, ty, errors) = tc_term(&arena, &arena, untyped_ast);

        assert_eq!(ty, &ErrorTy);
        assert!(errors.contains(&TypeCheckError::VarNotDefined(VarId(0))))
    }
    */

    /*
    #[test]
    fn test_tc_undefined_global_fails() {
        let arena = Bump::new();
        let item = (ModuleId(0), ItemId(0));
        let untyped_ast = Ast::new(
            FxHashMap::default(),
            arena.alloc(Item(item))
        );

        let (_, ty, errors) = tc_term(&arena, &arena, untyped_ast);

        assert_eq!(ty, &ErrorTy);
        assert!(errors.contains(&TypeCheckError::ItemNotDefined(item)))
    }

    #[test]
    fn test_tc_item() {
        let arena = Bump::new();
        let mut vars: IdGen<TcVar, ()> = IdGen::new();
        let arg = vars.push(());
        let ret = vars.push(());

        let scheme = Scheme {
            vars,
            constraints: vec![],
            ty: arena.alloc(FunTy(arena.alloc(VarTy(arg)), arena.alloc(VarTy(ret)))),
        };

        let item = (ModuleId::from_raw(0), ItemId::from_raw(0));

        let global_env = FxHashMap::from_iter([(item, scheme)]);

        let tc = TypeChecker::with_global_env(TyCtx { ast_arena: &arena, ty_arena: &arena }, global_env);
        let (typed_term, ty, errors) = tc_term_with(
            tc,
            Ast::new(
                FxHashMap::default(),
                arena.alloc(Abstraction {
                    arg: VarId(0),
                    body: arena.alloc(Application {
                        func: arena.alloc(Item(item)),
                        arg: arena.alloc(Variable(VarId(0))),
                    }),
                }),
            ),
        );

        assert_eq!(errors, vec![]);
        assert_eq!(
            typed_term.root(),
            &Abstraction {
                arg: TypedVarId {
                    var: VarId(0),
                    ty: &VarTy(arg)
                },
                body: &Application {
                    func: &Item(item),
                    arg: &Variable(TypedVarId {
                        var: VarId(0),
                        ty: &VarTy(arg)
                    })
                }
            }
        );
        assert_eq!(ty, &FunTy(&VarTy(arg), &VarTy(ret)));
    }*/
}
