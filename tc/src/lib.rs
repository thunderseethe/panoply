use aiahr_core::{
    ast::{Ast, Direction, Term, Term::*},
    id::{EffectId, EffectOpId, Id, ItemId, ModuleId, VarId},
    memory::handle::{Handle, RefHandle},
};
use bumpalo::Bump;
use ena::unify::{InPlaceUnificationTable, UnifyKey, UnifyValue};
use parking_lot::RwLock;
use rustc_hash::{FxHashMap, FxHashSet, FxHasher};
use std::{
    borrow::Borrow,
    cmp::Ordering,
    convert::Infallible,
    hash::{BuildHasherDefault, Hash, Hasher},
    ops::Deref,
};

mod ty;

use ty::{
    fold::{FallibleTypeFold, TypeFoldable},
    row::{ClosedRow, InferRow, PartialRow, Row, RowLabel, RowSet},
    TypeKind::*,
    *,
};

use crate::ty::row::CombineInto;

use self::ty::row::OrderedRowXorRow;

/// Errors that may be produced during type checking
#[derive(Debug, PartialEq, Eq)]
pub enum TypeCheckError<'ctx> {
    /// A variable we expected to exist with a type, did not
    VarNotDefined(VarId),
    ItemNotDefined((ModuleId, ItemId)),
    TypeMismatch(
        Candidate<
            'ctx,
            TcUnifierVar<'ctx>,
            (Row<'ctx, TcUnifierVar<'ctx>>, Row<'ctx, TcUnifierVar<'ctx>>),
        >,
        Candidate<
            'ctx,
            TcUnifierVar<'ctx>,
            (Row<'ctx, TcUnifierVar<'ctx>>, Row<'ctx, TcUnifierVar<'ctx>>),
        >,
    ),
    OccursCheckFailed(TcUnifierVar<'ctx>),
    UnifierToTcVar(UnifierToTcVarError),
    RowsNotDisjoint(
        ClosedRow<'ctx, TcUnifierVar<'ctx>>,
        ClosedRow<'ctx, TcUnifierVar<'ctx>>,
    ),
    RowsNotEqual(InferRow<'ctx>, InferRow<'ctx>),
}

impl<'ty> From<Infallible> for TypeCheckError<'ty> {
    fn from(_never: Infallible) -> Self {
        panic!("Function with Infallible parameter was called.")
    }
}
impl<'ty> From<UnifierToTcVarError> for TypeCheckError<'ty> {
    fn from(err: UnifierToTcVarError) -> Self {
        Self::UnifierToTcVar(err)
    }
}
impl<'ctx>
    From<(
        Candidate<'ctx, TcUnifierVar<'ctx>>,
        Candidate<'ctx, TcUnifierVar<'ctx>>,
    )> for TypeCheckError<'ctx>
{
    fn from(
        (left, right): (
            Candidate<'ctx, TcUnifierVar<'ctx>>,
            Candidate<'ctx, TcUnifierVar<'ctx>>,
        ),
    ) -> Self {
        TypeCheckError::TypeMismatch(left.into(), right.into())
    }
}
impl<'ctx>
    From<(
        CandidateView<'ctx, TcUnifierVar<'ctx>>,
        CandidateView<'ctx, TcUnifierVar<'ctx>>,
    )> for TypeCheckError<'ctx>
{
    fn from(
        (left, right): (
            CandidateView<'ctx, TcUnifierVar<'ctx>>,
            CandidateView<'ctx, TcUnifierVar<'ctx>>,
        ),
    ) -> Self {
        TypeCheckError::TypeMismatch(left, right)
    }
}
impl<'ctx> From<(InferTy<'ctx>, InferTy<'ctx>)> for TypeCheckError<'ctx> {
    fn from((left, right): (InferTy<'ctx>, InferTy<'ctx>)) -> Self {
        TypeCheckError::from((CandidateView::Ty(left), CandidateView::Ty(right)))
    }
}

/// A candidate solution for a unification variable.
/// This acts as the value in the unification table for a unification variable.
#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Candidate<'ctx, TV, OR = OrderedRowXorRow<'ctx, TV>> {
    /// Our unifier represents a type
    Ty(Ty<'ctx, TV>),
    /// Our unifier represents a row
    Row(RowSet<'ctx, TV, OR>),
}
pub type InferCandidate<'infer> = Candidate<'infer, TcUnifierVar<'infer>>;
pub type CandidateView<'ctx, TV> = Candidate<'ctx, TV, (Row<'ctx, TV>, Row<'ctx, TV>)>;
impl<'ctx, TV> Into<CandidateView<'ctx, TV>> for Candidate<'ctx, TV> {
    fn into(self) -> CandidateView<'ctx, TV> {
        match self {
            Candidate::Ty(ty) => Candidate::Ty(ty),
            Candidate::Row(row) => Candidate::Row(row.into()),
        }
    }
}
impl<'ctx> UnifyValue for Candidate<'ctx, TcUnifierVar<'ctx>> {
    type Error = (Self, Self);

    fn unify_values(left: &Self, right: &Self) -> Result<Self, Self::Error> {
        match (left, right) {
            (Candidate::Ty(left), Candidate::Ty(right)) => Ty::unify_values(left, right)
                .map(Candidate::Ty)
                .map_err(|(a, b)| (Candidate::Ty(a), Candidate::Ty(b))),
            (Candidate::Ty(ty), Candidate::Row(_)) | (Candidate::Row(_), Candidate::Ty(ty)) => {
                match ty.deref() {
                    // A solved row replaces a row set when unified
                    TypeKind::RowTy(_) => Ok(Candidate::Ty(*ty)),
                    _ => Err((left.clone(), right.clone())),
                }
            }
            (Candidate::Row(left), Candidate::Row(right)) => RowSet::unify_values(left, right)
                .map(Candidate::Row)
                .map_err(|_| unreachable!()),
        }
    }
}

impl<'ctx, TV: Clone> TypeFoldable<'ctx> for Candidate<'ctx, TV> {
    type TypeVar = TV;
    type Out<T: 'ctx> = Candidate<'ctx, T, (Row<'ctx, T>, Row<'ctx, T>)>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::TypeVar>, F::Error> {
        match self {
            Candidate::Ty(ty) => ty.try_fold_with(fold).map(Candidate::Ty),
            Candidate::Row(row) => row.try_fold_with(fold).map(Candidate::Row),
        }
    }
}
impl<'ctx, TV> From<Ty<'ctx, TV>> for Candidate<'ctx, TV> {
    fn from(ty: Ty<'ctx, TV>) -> Self {
        Candidate::Ty(ty)
    }
}
impl<'ctx, TV> From<RowSet<'ctx, TV>> for Candidate<'ctx, TV> {
    fn from(row: RowSet<'ctx, TV>) -> Self {
        Candidate::Row(row)
    }
}

/// A constraint produced during initial type checking.
/// It will be solved in the second half of type checking to produce a map from Unifier variables
/// to their types.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Constraint<'ctx, TV> {
    /// Two types must be equal for our term to type check
    Eq(Ty<'ctx, TV>, Ty<'ctx, TV>),
    /// Left and right row must combine to equal goal row
    RowCombine {
        left: Row<'ctx, TV>,
        right: Row<'ctx, TV>,
        goal: Row<'ctx, TV>,
    },
}
type InferConstraint<'infer> = Constraint<'infer, TcUnifierVar<'infer>>;
impl<'ctx, TV: Clone> TypeFoldable<'ctx> for Constraint<'_, TV> {
    type TypeVar = TV;
    type Out<T: 'ctx> = Constraint<'ctx, T>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::TypeVar>, F::Error> {
        Ok(match self {
            Constraint::Eq(left, right) => {
                Constraint::Eq(left.try_fold_with(fold)?, right.try_fold_with(fold)?)
            }
            Constraint::RowCombine { left, right, goal } => Constraint::RowCombine {
                left: left.try_fold_with(fold)?,
                right: right.try_fold_with(fold)?,
                goal: goal.try_fold_with(fold)?,
            },
        })
    }
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
            Some(Candidate::Ty(ty)) => ty,
            _ => self.ctx().mk_ty(VarTy(var)),
        })
    }

    fn try_fold_row_var(
        &mut self,
        var: Self::InTypeVar,
    ) -> Result<Row<'inf, Self::TypeVar>, Self::Error> {
        Ok(match self.unifiers.probe_value(var) {
            Some(Candidate::Ty(ty)) => ty
                .try_to_row()
                .expect("Unified a non row type with a row var"),
            _ => Row::Open(var),
        })
    }
}

/// Shorthand for a pattern that matches the given type
macro_rules! ty_pat {
    // The Unit type
    ({}) => {
        ProdTy(Row::Closed(ClosedRow {
            fields: Handle([]),
            values: Handle([]),
        }))
    };
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
struct InferCtx<'a, 'infer, I> {
    /// Store types for local variables.
    local_env: FxHashMap<VarId, InferTy<'infer>>,
    /// Mapping from unification variables to their types (if any).
    unifiers: InPlaceUnificationTable<TcUnifierVar<'infer>>,
    /// Errors that arise during type checking
    errors: Vec<TypeCheckError<'infer>>,
    /// Allocator for types created during inference
    ctx: &'a I,
}

impl<'infer, TV, I: MkTy<'infer, TV>> MkTy<'infer, TV> for InferCtx<'_, 'infer, I> {
    fn mk_ty(&self, kind: TypeKind<'infer, TV>) -> Ty<'infer, TV> {
        self.ctx.mk_ty(kind)
    }

    fn mk_label(&self, label: &str) -> RowLabel<'infer> {
        self.ctx.mk_label(label)
    }

    fn mk_row(
        &self,
        fields: &[RowLabel<'infer>],
        values: &[Ty<'infer, TV>],
    ) -> ClosedRow<'infer, TV> {
        self.ctx.mk_row(fields, values)
    }
}

struct TyChkRes<'infer, TV> {
    ty: Ty<'infer, TV>,
    eff: Row<'infer, TV>,
}
type InferResult<'infer> = TyChkRes<'infer, TcUnifierVar<'infer>>;

impl<'infer> InferResult<'infer> {
    fn new(ty: InferTy<'infer>, eff: InferRow<'infer>) -> Self {
        Self { ty, eff }
    }
    fn with_ty(self, ty: InferTy<'infer>) -> Self {
        Self { ty, ..self }
    }
    fn map_ty(self, f: impl FnOnce(InferTy<'infer>) -> InferTy<'infer>) -> Self {
        Self {
            ty: f(self.ty),
            ..self
        }
    }
}

impl<'ctx, TV: Clone> TypeFoldable<'ctx> for TyChkRes<'_, TV> {
    type TypeVar = TV;
    type Out<T: 'ctx> = TyChkRes<'ctx, T>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::TypeVar>, F::Error> {
        Ok(TyChkRes {
            ty: self.ty.try_fold_with(fold)?,
            eff: self.eff.try_fold_with(fold)?,
        })
    }
}

impl<'infer, I> InferCtx<'_, 'infer, I>
where
    I: MkTy<'infer, TcUnifierVar<'infer>>,
{
    /// This is the entrypoint to the bidirectional type checker. Since our language uses
    /// damnas-milner type inference we will always begin type checking with a call to infer.
    pub fn infer<'s, 'ty, E>(
        &mut self,
        eff_info: &E,
        term: &Term<'_, VarId>,
    ) -> (
        Vec<InferConstraint<'infer>>,
        FxHashMap<VarId, InferTy<'infer>>,
        InferResult<'infer>,
    )
    where
        E: EffectInfo<'s, 'ty>,
    {
        let mut constraints = vec![];
        let mut var_tys = FxHashMap::default();
        let res = self._infer(&mut constraints, &mut var_tys, eff_info, term);
        (constraints, var_tys, res)
    }

    /// Check a term against a given type.
    /// This method pairs with _infer to form a bidirectional type checker
    fn _check<'s, 'eff, E>(
        &mut self,
        constraints: &mut Vec<InferConstraint<'infer>>,
        var_tys: &mut FxHashMap<VarId, InferTy<'infer>>,
        eff_info: &E,
        term: &Term<'_, VarId>,
        expected: InferResult<'infer>,
    ) -> ()
    where
        E: EffectInfo<'s, 'eff>,
    {
        use TypeKind::*;
        match (term, *expected.ty) {
            (Abstraction { arg, body }, FunTy(arg_ty, body_ty)) => {
                // Check an abstraction against a function type by checking the body checks against
                // the function return type with the function argument type in scope.
                self.local_env.insert(*arg, arg_ty);
                self._check(
                    constraints,
                    var_tys,
                    eff_info,
                    body,
                    expected.with_ty(body_ty),
                );
                self.local_env.remove(arg);
            }
            (Label { .. }, ProdTy(row)) => {
                // A label can check against a product, if it checks against the product's internal
                // type
                self._check(
                    constraints,
                    var_tys,
                    eff_info,
                    term,
                    expected.with_ty(row.to_ty(self)),
                )
            }
            (Label { label, term }, RowTy(row)) => {
                // If our row is too small or too big, fail
                if row.fields.len() != 1 {
                    self.errors
                        .push((self.single_row_ty(label, self.mk_ty(ErrorTy)), expected.ty).into());
                    return ();
                }
                let field = self.mk_label(label);
                // If our singleton row is a different field name, fail
                if field != row.fields[0] {
                    self.errors.push(
                        (
                            self.single_row_ty(field.as_ref(), self.mk_ty(ErrorTy)),
                            expected.ty,
                        )
                            .into(),
                    )
                }

                // If this is a singleton row with the right label check it's value type matches
                // our term type
                self._check(
                    constraints,
                    var_tys,
                    eff_info,
                    term,
                    expected.with_ty(row.values[0]),
                )
            }
            (Unit, ty_pat!({})) => constraints.push(Constraint::Eq(
                expected.eff.to_ty(self),
                Row::Closed(self.empty_row()).to_ty(self),
            )),
            (Unlabel { label, term }, _) => {
                let expected_ty = self.single_row_ty(label, expected.ty);
                self._check(
                    constraints,
                    var_tys,
                    eff_info,
                    term,
                    expected.with_ty(expected_ty),
                )
            }
            (Concat { .. }, RowTy(row)) => {
                // Coerece a row type into a product and re-check.
                self._check(
                    constraints,
                    var_tys,
                    eff_info,
                    term,
                    expected.with_ty(self.mk_ty(ProdTy(Row::Closed(row)))),
                );
            }
            (Concat { left, right }, ProdTy(row)) => {
                let left_infer = self._infer(constraints, var_tys, eff_info, left);
                let left_row = self.equate_as_row(constraints, left_infer.ty);

                let right_infer = self._infer(constraints, var_tys, eff_info, right);
                let right_row = self.equate_as_row(constraints, right_infer.ty);

                // Check our expected effect is a combination of each components effects
                constraints.push(Constraint::RowCombine {
                    left: left_infer.eff,
                    right: right_infer.eff,
                    goal: expected.eff,
                });
                constraints.push(Constraint::RowCombine {
                    left: left_row,
                    right: right_row,
                    goal: row,
                });
            }
            (Project { .. }, RowTy(row)) => {
                // Coerce row into a product and re-check.
                self._check(
                    constraints,
                    var_tys,
                    eff_info,
                    term,
                    expected.with_ty(self.mk_ty(ProdTy(Row::Closed(row)))),
                );
            }
            (Project { direction, term }, ProdTy(row)) => {
                let term_infer = self._infer(constraints, var_tys, eff_info, term);
                let term_row = self.equate_as_row(constraints, term_infer.ty);
                let unbound_row = self.fresh_row();

                constraints.push(self.row_eq_constraint(expected.eff, term_infer.eff));
                constraints.push(match direction {
                    Direction::Left => Constraint::RowCombine {
                        left: row,
                        right: unbound_row,
                        goal: term_row,
                    },
                    Direction::Right => Constraint::RowCombine {
                        left: unbound_row,
                        right: row,
                        goal: term_row,
                    },
                });
            }
            // Bucket case for when we need to check a rule against a type but no case applies
            (term, _) => {
                // Infer a type for our term and check that the expected type is equal to the
                // inferred type.
                let inferred = self._infer(constraints, var_tys, eff_info, term);
                constraints.push(Constraint::Eq(expected.ty, inferred.ty));
                constraints.push(self.row_eq_constraint(expected.eff, inferred.eff));
            }
        }
    }

    /// Create a unique unbound row variable
    fn fresh_row(&mut self) -> InferRow<'infer> {
        let uv = self.unifiers.new_key(None);
        Row::Open(uv)
    }

    /// Create a unique unbound type variable
    fn fresh_var(&mut self) -> InferTy<'infer> {
        let uv = self.unifiers.new_key(None);
        self.mk_ty(VarTy(uv))
    }

    /// Infer a type for a term
    /// This method pairs with check to form a bidirectional type checker
    fn _infer<'s, 'eff, E>(
        &mut self,
        constraints: &mut Vec<InferConstraint<'infer>>,
        var_tys: &mut FxHashMap<VarId, InferTy<'infer>>,
        eff_info: &E,
        term: &Term<'_, VarId>,
    ) -> InferResult<'infer>
    where
        E: EffectInfo<'s, 'eff>,
    {
        match term {
            // Abstraction inference  is done by creating two new unifiers <arg> and <body>
            // The abstraction body is checked against these fresh type variables
            // The resulting type of the inference is function type <arg> -> <body>
            Abstraction { arg, body } => {
                let arg_ty = self.fresh_var();
                let body_ty = self.fresh_var();
                let eff = self.fresh_row();

                var_tys.insert(*arg, arg_ty);
                self.local_env.insert(*arg, arg_ty);
                self._check(
                    constraints,
                    var_tys,
                    eff_info,
                    body,
                    InferResult::new(body_ty, eff),
                );
                self.local_env.remove(arg);

                InferResult::new(self.mk_ty(TypeKind::FunTy(arg_ty, body_ty)), eff)
            }
            // Application inference starts by inferring types for the func of the application.
            // We equate this inferred type to a function type, generating fresh unifiers if
            // needed.
            // We then check the arg of the application against the arg type of the function type
            // The resulting type of this application is the function result type.
            Application { func, arg } => {
                let fun_infer = self._infer(constraints, var_tys, eff_info, func);
                // Optimization: eagerly use FunTy if available. Otherwise dispatch fresh unifiers
                // for arg and ret type.
                let (arg_ty, ret_ty) = match *fun_infer.ty {
                    FunTy(arg_ty, ret_ty) => (arg_ty, ret_ty),
                    _ => {
                        let arg_ty = self.fresh_var();
                        let ret_ty = self.fresh_var();
                        constraints.push(Constraint::Eq(
                            fun_infer.ty,
                            self.mk_ty(FunTy(arg_ty, ret_ty)),
                        ));
                        (arg_ty, ret_ty)
                    }
                };

                let arg_eff = self.fresh_row();
                self._check(
                    constraints,
                    var_tys,
                    eff_info,
                    arg,
                    InferResult::new(arg_ty, arg_eff),
                );

                let eff = self.fresh_row();
                constraints.push(Constraint::RowCombine {
                    left: fun_infer.eff,
                    right: arg_eff,
                    goal: eff,
                });
                InferResult::new(ret_ty, eff)
            }
            // If the variable is in environemnt return it's type, otherwise return an error.
            Variable(var) => {
                if let Some(ty) = self.local_env.get(var).cloned() {
                    var_tys.insert(*var, ty);
                    InferResult::new(ty, Row::Closed(self.empty_row()))
                } else {
                    self.errors.push(TypeCheckError::VarNotDefined(*var));
                    let err_ty = self.mk_ty(ErrorTy);
                    var_tys.insert(*var, err_ty);
                    InferResult::new(err_ty, Row::Closed(self.empty_row()))
                }
            }
            Label { label, term } => {
                let infer = self._infer(constraints, var_tys, eff_info, term);
                infer.map_ty(|ty| self.single_row_ty(label, ty))
            }
            Unlabel { label, term } => {
                let term_infer = self._infer(constraints, var_tys, eff_info, term);
                let field = self.mk_label(label);
                term_infer.map_ty(|ty| match *ty {
                    // If our output type is already a singleton row of without a label, use it
                    // directly. This avoids introducing needless unifiers
                    RowTy(row) if row.len() == 1 && row.fields[0] == field => row.values[0],
                    // Othewise introduce a unifier, and rely on unification for any needed error
                    // reporting
                    _ => {
                        let out_ty = self.fresh_var();
                        let row_ty = self.mk_ty(RowTy(self.mk_row(&[field], &[out_ty])));
                        constraints.push(Constraint::Eq(row_ty, ty));
                        out_ty
                    }
                })
            }
            Unit => {
                // Represent unit by an empty product type
                let unit = Row::Closed(self.empty_row());
                InferResult::new(self.mk_ty(ProdTy(unit)), unit)
            }
            Concat { left, right } => {
                let left_row = self.fresh_row();
                let left_eff = self.fresh_row();
                let right_row = self.fresh_row();
                let right_eff = self.fresh_row();
                let out_row = self.fresh_row();
                let out_eff = self.fresh_row();

                let left_ty = self.mk_ty(ProdTy(left_row));
                let right_ty = self.mk_ty(ProdTy(right_row));

                self._check(
                    constraints,
                    var_tys,
                    eff_info,
                    left,
                    InferResult::new(left_ty, left_eff),
                );
                self._check(
                    constraints,
                    var_tys,
                    eff_info,
                    right,
                    InferResult::new(right_ty, right_eff),
                );

                constraints.push(Constraint::RowCombine {
                    left: left_eff,
                    right: right_eff,
                    goal: out_eff,
                });
                constraints.push(Constraint::RowCombine {
                    left: left_row,
                    right: right_row,
                    goal: out_row,
                });

                InferResult::new(self.mk_ty(ProdTy(out_row)), out_eff)
            }
            Project { direction, term } => {
                let big_row = self.fresh_row();
                let small_row = self.fresh_row();
                // In a projection one of the row variables will be unbound
                let unbound_row = self.fresh_row();

                let eff = self.fresh_row();
                let term_ty = self.mk_ty(ProdTy(big_row));
                self._check(
                    constraints,
                    var_tys,
                    eff_info,
                    term,
                    InferResult::new(term_ty, eff),
                );

                constraints.push(match direction {
                    Direction::Left => Constraint::RowCombine {
                        left: small_row,
                        right: unbound_row,
                        goal: big_row,
                    },
                    Direction::Right => Constraint::RowCombine {
                        left: unbound_row,
                        right: small_row,
                        goal: big_row,
                    },
                });

                InferResult::new(self.mk_ty(ProdTy(small_row)), eff)
            }
            Operation(eff_op_id) => {
                let eff_id = eff_info.lookup_effect_by_member(*eff_op_id);
                let sig =
                    self.instantiate(constraints, eff_info.effect_member_sig(eff_id, *eff_op_id));

                InferResult::new(
                    sig.ty,
                    Row::Closed(
                        self.single_row(&eff_info.effect_name(eff_id), self.empty_row_ty()),
                    ),
                )
            }
            Term::Handle { eff, handler, body } => {
                let mut row = eff_info
                    .effect_members(*eff)
                    .iter()
                    .map(|mem| {
                        let sig =
                            self.instantiate(constraints, eff_info.effect_member_sig(*eff, *mem));
                        let name = eff_info.effect_member_name(*eff, *mem);
                        (self.mk_label(&name), sig.ty)
                    })
                    .collect::<Vec<_>>();

                let body_ty = self.fresh_var();
                let ret_ty = self.fresh_var();
                // Append the return to our expected handler type as it's not included in effect
                // definition.
                row.push((self.mk_label("return"), self.mk_ty(FunTy(body_ty, ret_ty))));

                let eff_handler = self.mk_ty(RowTy(self.construct_row(row)));
                self._check(
                    constraints,
                    var_tys,
                    eff_info,
                    handler,
                    InferResult::new(eff_handler, Row::Closed(self.empty_row())),
                );

                let body_eff = self.fresh_row();
                let out_eff = self.fresh_row();
                self._check(
                    constraints,
                    var_tys,
                    eff_info,
                    body,
                    InferResult::new(body_ty, body_eff),
                );

                let handled_eff =
                    Row::Closed(self.single_row(&eff_info.effect_name(*eff), self.empty_row_ty()));

                // Handle removes an effect so our out_eff should be whatever body_eff was minus
                // our handled effect
                constraints.push(Constraint::RowCombine {
                    left: out_eff,
                    right: handled_eff,
                    goal: body_eff,
                });

                InferResult::new(ret_ty, out_eff)
            }
            // TODOs
            Item(_) => todo!(),
        }
    }

    /// Apply the current partial substitution to a type, removing as many unifiers as possible
    /// before unification.
    fn normalize_ty(
        &mut self,
        ty: InferTy<'infer>,
    ) -> Either<
        InferTy<'infer>,
        (
            TcUnifierVar<'infer>,
            RowSet<'infer, TcUnifierVar<'infer>, (InferRow<'infer>, InferRow<'infer>)>,
        ),
    > {
        match *ty {
            VarTy(var) => self
                .unifiers
                .probe_value(var)
                .map(|val| {
                    let cand = val
                        .try_fold_with(&mut Normalize {
                            ctx: self.ctx,
                            unifiers: &mut self.unifiers,
                        })
                        .unwrap();
                    match cand {
                        Candidate::Ty(ty) => Left(ty),
                        Candidate::Row(row_set) => Right((var, row_set)),
                    }
                })
                .unwrap_or_else(|| Left(ty)),
            _ => Left(
                ty.try_fold_with(&mut Normalize {
                    ctx: self.ctx,
                    unifiers: &mut self.unifiers,
                })
                .unwrap(),
            ),
        }
    }

    fn normalize_row(
        &mut self,
        row: InferRow<'infer>,
    ) -> Either<
        ClosedRow<'infer, TcUnifierVar<'infer>>,
        (
            TcUnifierVar<'infer>,
            RowSet<'infer, TcUnifierVar<'infer>, (InferRow<'infer>, InferRow<'infer>)>,
        ),
    > {
        match row {
            Row::Open(var) => self
                .unifiers
                .probe_value(var)
                .map(|val| {
                    match val
                        .try_fold_with(&mut Normalize {
                            ctx: self.ctx,
                            unifiers: &mut self.unifiers,
                        })
                        .unwrap()
                    {
                        Candidate::Ty(ty) => Left(
                            ty.try_to_row()
                                .expect("Row unifier was mapped to a type")
                                .expect_closed(
                                    "Row variable should not be stored as unification value",
                                ),
                        ),
                        Candidate::Row(row) => Right((var, row)),
                    }
                })
                .unwrap_or_else(|| Right((var, RowSet::default()))),
            Row::Closed(row) => Left(
                row.try_fold_with(&mut Normalize {
                    ctx: self.ctx,
                    unifiers: &mut self.unifiers,
                })
                .unwrap(),
            ),
        }
    }

    /// Unify a variable and a type.
    /// This checks that the variable is not present in type, throwing an error if varaibles is
    /// present.
    /// If not we record that the unification variable is solved to given type.
    fn unify_var_ty(
        &mut self,
        var: TcUnifierVar<'infer>,
        ty: InferTy<'infer>,
    ) -> Result<(), TypeCheckError<'infer>> {
        let ty_ = ty.try_fold_with(&mut OccursCheck { ctx: self.ctx, var })?;
        self.unifiers
            .unify_var_value(var, Some(Candidate::Ty(ty_)))
            .map_err(|e| e.into())
    }

    fn unify_var_partialrow(
        &mut self,
        goal_var: TcUnifierVar<'infer>,
        row: PartialRow<'infer, TcUnifierVar<'infer>>,
    ) -> Result<(), TypeCheckError<'infer>> {
        self.unify_var_rowset(goal_var, RowSet::from(row))
    }

    fn unify_var_rowset(
        &mut self,
        goal_var: TcUnifierVar<'infer>,
        row: RowSet<'infer, TcUnifierVar<'infer>>,
    ) -> Result<(), TypeCheckError<'infer>> {
        let row_ = row.try_fold_with(&mut OccursCheck {
            ctx: self.ctx,
            var: goal_var,
        })?;
        // We know occurs check won't break our invariants so unwrap our (Row, Row) pairs into
        // their internal representation
        let row_ = RowSet {
            comps: row_.comps,
            goals: row_
                .goals
                .into_iter()
                .map(|rr| rr.try_into().unwrap())
                .collect(),
        };
        self.unifiers
            .unify_var_value(goal_var, Some(Candidate::Row(row_)))
            .map_err(|e| e.into())
    }

    fn unify_closedrow_closedrow(
        &mut self,
        left: ClosedRow<'infer, TcUnifierVar<'infer>>,
        right: ClosedRow<'infer, TcUnifierVar<'infer>>,
    ) -> Result<(), TypeCheckError<'infer>> {
        // If our row labels aren't equal the types cannot be equal
        if left.fields != right.fields {
            return Err(TypeCheckError::RowsNotEqual(
                Row::Closed(left),
                Row::Closed(right),
            ));
        }

        for (left_ty, right_ty) in left.values.iter().zip(right.values.iter()) {
            self.unify_ty_ty(*left_ty, *right_ty)?;
        }

        Ok(())
    }

    fn unify_ty_ty_normalized(
        &mut self,
        left: InferTy<'infer>,
        right: InferTy<'infer>,
    ) -> Result<(), TypeCheckError<'infer>> {
        match (*left, *right) {
            // If an error appears anywhere fail unification
            (ErrorTy, _) | (_, ErrorTy) => Err((left, right).into()),

            // Special case for when two keys meet
            // Instead of unifiying either variable as a value of the other, we need to record that
            // the two key's equivalence classes must be the same.
            (VarTy(left_var), VarTy(right_var)) => self
                .unifiers
                .unify_var_var(left_var, right_var)
                .map_err(|e| e.into()),

            // If a key meets a new ty record they must be equal
            (_, VarTy(var)) => self.unify_var_ty(var, left),
            (VarTy(var), _) => self.unify_var_ty(var, right),

            // Decompose compound types
            (FunTy(left_arg, left_ret), FunTy(right_arg, right_ret)) => {
                self.unify_ty_ty_normalized(left_arg, right_arg)?;
                self.unify_ty_ty_normalized(left_ret, right_ret)
            }
            (RowTy(left_row), RowTy(right_row)) => {
                self.unify_closedrow_closedrow(left_row, right_row)
            }
            // Coerce a product into a row
            (RowTy(_), ProdTy(right)) => self.unify_ty_ty_normalized(left, right.to_ty(self)),
            // Coerce a product into a row
            (ProdTy(left), RowTy(_)) => self.unify_ty_ty_normalized(left.to_ty(self), right),
            // Decompose product and unify both internal types
            (ProdTy(left), ProdTy(right)) => {
                self.unify_ty_ty_normalized(left.to_ty(self), right.to_ty(self))
            }
            // Discharge equal types
            (IntTy, IntTy) => Ok(()),

            // Type mismatch
            (IntTy, FunTy(_, _))
            | (FunTy(_, _), IntTy)
            | (IntTy, RowTy(_))
            | (RowTy(_), IntTy)
            | (RowTy(_), FunTy(_, _))
            | (FunTy(_, _), RowTy(_))
            | (IntTy, ProdTy(_))
            | (ProdTy(_), IntTy)
            | (FunTy(_, _), ProdTy(_))
            | (ProdTy(_), FunTy(_, _)) => Err((left, right).into()),
        }
    }

    /// This is the main entry point of unificaiton and handles unifying two arbitrary types.
    /// Each type is substituted by the current substitution to remove as many unification
    /// variables as possible before unifying.
    fn unify_ty_ty(
        &mut self,
        left: InferTy<'infer>,
        right: InferTy<'infer>,
    ) -> Result<(), TypeCheckError<'infer>> {
        // Apply current partial substitution before comparing
        let normal_left = self.normalize_ty(left);
        let normal_right = self.normalize_ty(right);

        match (normal_left, normal_right) {
            (Left(left_ty), Left(right_ty)) => self.unify_ty_ty_normalized(left_ty, right_ty),
            (Left(ty), Right((row_var, rows))) | (Right((row_var, rows)), Left(ty)) => {
                let row = self.dispatch_solved(row_var, rows)?;
                match row {
                    Left(row) => self.unify_ty_ty_normalized(ty, self.mk_ty(RowTy(row))),
                    Right((row_var, rows)) => {
                        match *ty {
                            VarTy(var) => self
                                .unifiers
                                .unify_var_var(var, row_var)
                                .map_err(|e| e.into()),
                            RowTy(row) => self.unify_closedrow_rowset(row, &rows),
                            // The rest of these cases are just type errors, things like (IntTy, Row) that
                            // could never succeed
                            _ => Err(TypeCheckError::from((
                                Candidate::Ty(ty),
                                Candidate::Row(rows),
                            ))),
                        }
                    }
                }
            }
            // TODO: Figure out what to do (if anything) here.
            (Right((left_var, _)), Right((right_var, _))) => self
                .unifiers
                .unify_var_var(left_var, right_var)
                .map_err(|e| e.into()),
        }
    }

    /// Solve a list of constraints to a mapping from unifiers to types.
    /// If there is no solution to the list of constraints we return a relevant error.
    fn solve(
        mut self,
        constraints: Vec<InferConstraint<'infer>>,
    ) -> (
        InPlaceUnificationTable<TcUnifierVar<'infer>>,
        Vec<TypeCheckError<'infer>>,
    ) {
        for constr in constraints {
            match constr {
                Constraint::Eq(left, right) => self
                    .unify_ty_ty(left, right)
                    .map_err(|err| {
                        self.errors.push(err);
                    })
                    .unwrap_or_default(),
                Constraint::RowCombine { left, right, goal } => self
                    .unify_row_combine(left, right, goal)
                    .map_err(|err| {
                        self.errors.push(err);
                    })
                    .unwrap_or_default(),
            }
        }
        (self.unifiers, self.errors)
    }

    fn unify_row_rowset(
        &mut self,
        row: InferRow<'infer>,
        row_var: TcUnifierVar<'infer>,
        row_set: &RowSet<'infer, TcUnifierVar<'infer>>,
    ) -> Result<(), TypeCheckError<'infer>> {
        match row {
            Row::Open(var) => self
                .unifiers
                .unify_var_var(var, row_var)
                .map_err(|e| e.into()),
            Row::Closed(closed_row) => {
                self.unify_closedrow_rowset(closed_row, row_set)?;
                self.unify_var_ty(row_var, self.mk_ty(RowTy(closed_row)))
            }
        }
    }

    fn unify_row_combine(
        &mut self,
        left: InferRow<'infer>,
        right: InferRow<'infer>,
        goal: InferRow<'infer>,
    ) -> Result<(), TypeCheckError<'infer>> {
        let goal = self
            .normalize_row(goal)
            .try_and_right(|(var, rows)| self.dispatch_solved(var, rows))?;
        let left = self
            .normalize_row(left)
            .try_and_right(|(var, rows)| self.dispatch_solved(var, rows))?;
        let right = self
            .normalize_row(right)
            .try_and_right(|(var, rows)| self.dispatch_solved(var, rows))?;

        fn find_unify_partial_row<'infer, I>(
            infer: &mut InferCtx<'_, 'infer, I>,
            a: InferRow<'infer>,
            a_row: ClosedRow<'infer, TcUnifierVar<'infer>>,
            b: InferRow<'infer>,
            (b_var, b_rows): (TcUnifierVar<'infer>, &RowSet<'infer, TcUnifierVar<'infer>>),
        ) -> Option<Result<(), TypeCheckError<'infer>>>
        where
            I: MkTy<'infer, TcUnifierVar<'infer>>,
        {
            match (a, b) {
                (a, Row::Open(b)) if b == b_var => {
                    Some(infer.unify_ty_ty(a.to_ty(infer), infer.mk_ty(RowTy(a_row))))
                }
                (Row::Open(a), b) if a == b_var => {
                    Some(infer.unify_ty_ty(b.to_ty(infer), infer.mk_ty(RowTy(a_row))))
                }
                (Row::Closed(a), b) if a.fields == a_row.fields => Some(
                    infer
                        .unify_closedrow_closedrow(a, a_row)
                        .and_then(|()| infer.unify_row_rowset(b, b_var, b_rows)),
                ),
                (a, Row::Closed(b)) if b.fields == a_row.fields => Some(
                    infer
                        .unify_closedrow_closedrow(b, a_row)
                        .and_then(|()| infer.unify_row_rowset(a, b_var, b_rows)),
                ),
                _ => None,
            }
        }

        println!(
            "unify_row_combine\n\tleft: {:?}\n\tright: {:?}\n\tgoal: {:?}",
            left, right, goal
        );

        match goal {
            // Try to propagate any solves we can make based
            Left(goal_row) => match (left, right) {
                (Left(left), Left(right)) => self.unify_row_combine_normalized(
                    Row::Closed(left),
                    Row::Closed(right),
                    Row::Closed(goal_row),
                ),
                (Left(left), Right((right_var, right_rows))) => {
                    let (fields, values) = goal_row.difference(left);
                    let right = self.mk_row(&fields, &values);
                    self.unify_closedrow_rowset(right, &right_rows)?;
                    self.unify_var_ty(right_var, self.mk_ty(RowTy(right)))
                }
                (Right((left_var, left_rows)), Left(right)) => {
                    let (fields, values) = goal_row.difference(right);
                    let left = self.mk_row(&fields, &values);
                    self.unify_closedrow_rowset(left, &left_rows)?;
                    self.unify_var_ty(left_var, self.mk_ty(RowTy(left)))
                }
                (Right((left_var, left_rows)), Right((right_var, right_rows))) => {
                    // Try to unify our constraint against an existing partial row
                    left_rows
                        .comps_iter()
                        .find_map(|combo| {
                            self.find_unify_closed_open_combineinto(
                                combo,
                                goal_row,
                                (right_var, &right_rows),
                            )
                        })
                        .unwrap_or_else(|| {
                            self.unify_var_combineinto(
                                left_var,
                                CombineInto {
                                    goal: Row::Closed(goal_row),
                                    other: Row::Open(right_var),
                                },
                            )
                        })
                        .and_then(|()| {
                            right_rows
                                .comps_iter()
                                .find_map(|combo| {
                                    self.find_unify_closed_open_combineinto(
                                        combo,
                                        goal_row,
                                        (left_var, &left_rows),
                                    )
                                })
                                .unwrap_or_else(|| {
                                    self.unify_var_combineinto(
                                        right_var,
                                        CombineInto {
                                            goal: Row::Closed(goal_row),
                                            other: Row::Open(left_var),
                                        },
                                    )
                                })
                        })
                }
            },
            Right((goal_var, goal_rows)) => match (left, right) {
                (Left(left), Left(right)) => {
                    let row = self.union_rows(left, right)?;
                    self.unify_closedrow_rowset(row, &goal_rows)?;
                    self.unify_var_ty(goal_var, self.mk_ty(RowTy(row)))
                }
                (Left(left_row), Right((right_var, right_rows))) => {
                    goal_rows
                        .goals_iter()
                        .find_map(|orxr| {
                            self.find_unify_closed_open_orxr(orxr, left_row, right_var)
                        })
                        .unwrap_or_else(|| {
                            self.unify_var_orderedrowxorrow(
                                goal_var,
                                OrderedRowXorRow::ClosedOpen(left_row, right_var),
                            )
                        })?;

                    right_rows
                        .comps_iter()
                        .find_map(|combo| {
                            find_unify_partial_row(
                                self,
                                combo.other,
                                left_row,
                                combo.goal,
                                (goal_var, &goal_rows),
                            )
                        })
                        .unwrap_or_else(|| {
                            self.unify_var_combineinto(
                                right_var,
                                CombineInto {
                                    goal: Row::Open(goal_var),
                                    other: Row::Closed(left_row),
                                },
                            )
                        })
                }
                (Right((left_var, left_rows)), Left(right_row)) => {
                    goal_rows
                        .goals_iter()
                        .find_map(|orxr| {
                            self.find_unify_closed_open_orxr(orxr, right_row, left_var)
                        })
                        .unwrap_or_else(|| {
                            self.unify_var_orderedrowxorrow(
                                goal_var,
                                OrderedRowXorRow::ClosedOpen(right_row, left_var),
                            )
                        })?;

                    left_rows
                        .comps_iter()
                        .find_map(|combo| {
                            find_unify_partial_row(
                                self,
                                combo.other,
                                right_row,
                                combo.goal,
                                (goal_var, &goal_rows),
                            )
                        })
                        .unwrap_or_else(|| {
                            self.unify_var_combineinto(
                                left_var,
                                CombineInto {
                                    other: Row::Closed(right_row),
                                    goal: Row::Open(goal_var),
                                },
                            )
                        })
                }
                (Right((left_var, left_rows)), Right((right_var, right_rows))) => {
                    let opt_left_unification =
                        left_rows
                            .comps_iter()
                            .find_map(|combo| match (combo.goal, combo.other) {
                                (goal, Row::Open(other)) if other == right_var => {
                                    Some(self.unify_row_rowset(goal, goal_var, &goal_rows))
                                }
                                (Row::Open(goal), other) if goal == goal_var => {
                                    Some(self.unify_row_rowset(other, right_var, &right_rows))
                                }
                                _ => None,
                            });

                    let left_unification = match opt_left_unification {
                        Some(res) => res,
                        None => self.unify_var_combineinto(
                            left_var,
                            CombineInto {
                                other: Row::Open(right_var),
                                goal: Row::Open(goal_var),
                            },
                        ),
                    };

                    let () = match left_unification {
                        Ok(_) => (),
                        Err(err) => return Err(err),
                    };

                    right_rows
                        .comps_iter()
                        .find_map(|combo| match (combo.goal, combo.other) {
                            (goal, Row::Open(other)) if other == left_var => {
                                Some(self.unify_row_rowset(goal, goal_var, &goal_rows))
                            }
                            (Row::Open(goal), other) if goal == goal_var => {
                                Some(self.unify_row_rowset(other, left_var, &left_rows))
                            }
                            _ => None,
                        })
                        .unwrap_or_else(|| {
                            self.unify_var_combineinto(
                                right_var,
                                CombineInto {
                                    other: Row::Open(left_var),
                                    goal: Row::Open(goal_var),
                                },
                            )
                        })?;

                    let opt_unification = goal_rows.goals_iter().find_map(|orxr| {
                        self.find_unify_open_open_orxr(
                            orxr,
                            (left_var, &left_rows),
                            (right_var, &right_rows),
                        )
                    });
                    match opt_unification {
                        Some(res) => res,
                        None => self.unify_var_orderedrowxorrow(
                            goal_var,
                            OrderedRowXorRow::with_open_open(left_var, right_var),
                        ),
                    }
                }
            },
        }
    }

    /// Make this type equal to a row and return that equivalent row.
    /// If it is already a row convert it directly, otherwise add a constraint that type must be a
    /// row.
    pub(crate) fn equate_as_row(
        &mut self,
        constraints: &mut Vec<InferConstraint<'infer>>,
        ty: InferTy<'infer>,
    ) -> InferRow<'infer> {
        match *ty {
            ProdTy(row) => row,
            RowTy(row) => Row::Closed(row),
            _ => {
                let unifier = self.unifiers.new_key(None);
                constraints.push(Constraint::Eq(self.mk_ty(VarTy(unifier.into())), ty));
                Row::Open(unifier.into())
            }
        }
    }

    fn unify_row_combine_normalized(
        &mut self,
        left: InferRow<'infer>,
        right: InferRow<'infer>,
        goal: InferRow<'infer>,
    ) -> Result<(), TypeCheckError<'infer>> {
        match goal {
            Row::Open(goal_var) => match (left, right) {
                // With only one open variable we can solve
                (Row::Closed(left), Row::Closed(right)) => {
                    let (fields, values) = left.disjoint_union(right)?;
                    let row = self.mk_ty(RowTy(self.mk_row(&fields, &values)));
                    self.unify_var_ty(goal_var, row)
                }
                (left @ Row::Open(left_var), right @ Row::Closed(_)) => {
                    self.unify_var_partialrow(left_var, PartialRow::OpenLeft { goal, right })?;
                    self.unify_var_partialrow(goal_var, PartialRow::OpenGoal { left, right })
                }
                (left @ Row::Closed(_), right @ Row::Open(right_var)) => {
                    self.unify_var_partialrow(right_var, PartialRow::OpenRight { goal, left })?;
                    self.unify_var_partialrow(goal_var, PartialRow::OpenGoal { left, right })
                }
                (left @ Row::Open(left_var), right @ Row::Open(right_var)) => {
                    self.unify_var_partialrow(left_var, PartialRow::OpenLeft { goal, right })?;
                    self.unify_var_partialrow(right_var, PartialRow::OpenRight { goal, left })?;
                    self.unify_var_partialrow(goal_var, PartialRow::OpenGoal { left, right })
                }
            },
            Row::Closed(goal_row) => match (left, right) {
                // Our rows are all closed, we just need to check they are equal and discharge them
                (Row::Closed(left_row), Row::Closed(right_row)) => {
                    let (fields, values) = left_row.disjoint_union(right_row)?;
                    let row = self.mk_row(&fields, &values);
                    self.unify_closedrow_closedrow(goal_row, row)
                }
                // With one open row we solve that row
                (Row::Open(left_var), Row::Closed(right_row)) => {
                    let (fields, values) = goal_row.difference(right_row);
                    let row = self.mk_ty(RowTy(self.mk_row(&fields, &values)));
                    self.unify_var_ty(left_var, row)
                }
                (Row::Closed(left_row), Row::Open(right_var)) => {
                    let (fields, values) = goal_row.difference(left_row);
                    let row = self.mk_ty(RowTy(self.mk_row(&fields, &values)));
                    self.unify_var_ty(right_var, row)
                }
                // We can't unify this case as is, we need more information.
                // And our goal is closed so we can't set it to pending
                (Row::Open(left_var), Row::Open(right_var)) => {
                    self.unify_var_partialrow(left_var, PartialRow::OpenLeft { goal, right })?;
                    self.unify_var_partialrow(right_var, PartialRow::OpenRight { goal, left })
                }
            },
        }
    }

    fn unify_closedrow_rowset(
        &mut self,
        row: ClosedRow<'infer, TcUnifierVar<'infer>>,
        row_set: &RowSet<'infer, TcUnifierVar<'infer>>,
    ) -> Result<(), TypeCheckError<'infer>> {
        for orxr in row_set.goals.iter() {
            match orxr {
                OrderedRowXorRow::ClosedOpen(subrow, var) => {
                    let (fields, values) = row.difference(*subrow);
                    let ty = self.mk_ty(RowTy(self.mk_row(&fields, &values)));
                    self.unify_var_ty(*var, ty)?;
                }
                OrderedRowXorRow::OpenOpen { .. } => { /* In this situation we don't know enough to unify anything so pass */
                }
            }
        }
        Ok(())
    }

    pub fn row_eq_constraint(
        &self,
        left: InferRow<'infer>,
        right: InferRow<'infer>,
    ) -> Constraint<'infer, TcUnifierVar<'infer>> {
        Constraint::Eq(left.to_ty(self), right.to_ty(self))
    }

    fn instantiate(
        &mut self,
        constraints: &mut Vec<Constraint<'infer, TcUnifierVar<'infer>>>,
        ty_scheme: TyScheme<'_, TcVar>,
    ) -> InferResult<'infer> {
        let mut inst = Instantiate {
            ctx: self.ctx,
            unifiers: ty_scheme
                .bound
                .into_iter()
                .map(|_| self.unifiers.new_key(None))
                .collect(),
        };
        constraints.extend(
            ty_scheme
                .constrs
                .into_iter()
                .map(|ev| match ev {
                    Evidence::Row { left, right, goal } => {
                        Constraint::RowCombine { left, right, goal }
                    }
                })
                .map(|constr| constr.try_fold_with(&mut inst).unwrap()),
        );
        InferResult::new(
            ty_scheme.ty.try_fold_with(&mut inst).unwrap(),
            ty_scheme.eff.try_fold_with(&mut inst).unwrap(),
        )
    }

    fn union_rows(
        &self,
        left: ClosedRow<'infer, TcUnifierVar<'infer>>,
        right: ClosedRow<'infer, TcUnifierVar<'infer>>,
    ) -> Result<ClosedRow<'infer, TcUnifierVar<'infer>>, TypeCheckError<'infer>> {
        left.disjoint_union(right)
            .map(|(fields, values)| self.mk_row(&fields, &values))
    }

    pub(crate) fn unify_var_combineinto(
        &mut self,
        var: TcUnifierVar<'infer>,
        combo: CombineInto<'infer, TcUnifierVar<'infer>>,
    ) -> Result<(), TypeCheckError<'infer>> {
        self.unifiers
            .unify_var_value(var, Some(Candidate::Row(RowSet::from(combo))))
            .map_err(|e| e.into())
    }

    pub(crate) fn unify_var_orderedrowxorrow(
        &mut self,
        var: TcUnifierVar<'infer>,
        goal: OrderedRowXorRow<'infer, TcUnifierVar<'infer>>,
    ) -> Result<(), TypeCheckError<'infer>> {
        self.unifiers
            .unify_var_value(var, Some(Candidate::Row(RowSet::from(goal))))
            .map_err(|e| e.into())
    }

    pub(crate) fn dispatch_solved(
        &mut self,
        var: TcUnifierVar<'infer>,
        rows: RowSet<'infer, TcUnifierVar<'infer>, (InferRow<'infer>, InferRow<'infer>)>,
    ) -> Result<
        Either<
            ClosedRow<'infer, TcUnifierVar<'infer>>,
            (TcUnifierVar<'infer>, RowSet<'infer, TcUnifierVar<'infer>>),
        >,
        TypeCheckError<'infer>,
    > {
        let mut orxrs = vec![];
        let mut solved = vec![];
        for rowrow in rows.goals_iter() {
            match OrderedRowXorRow::try_from(*rowrow) {
                Ok(orxr) => orxrs.push(orxr),
                Err((left, right)) => solved.push((left, right)),
            }
        }
        let row_set = RowSet {
            goals: orxrs,
            comps: rows.comps,
        };
        let mut s = solved.into_iter();
        s.next()
            .map(|(init_left, init_right)| {
                let init_row = self.union_rows(init_left, init_right)?;
                // Unify all solved rows together or fail
                for (left, right) in s {
                    let row = self.union_rows(left, right)?;
                    self.unify_closedrow_closedrow(init_row, row)?;
                }
                // Unify
                self.unify_closedrow_rowset(init_row, &row_set)?;
                self.unify_var_ty(var, self.mk_ty(RowTy(init_row)))?;
                Ok(Either::Left(init_row))
            })
            .unwrap_or_else(|| Ok(Either::Right((var, row_set))))
    }

    pub(crate) fn find_unify_open_open_orxr(
        &mut self,
        orxr: &OrderedRowXorRow<'infer, TcUnifierVar<'infer>>,
        (left_var, left_rows): (TcUnifierVar<'infer>, &RowSet<'infer, TcUnifierVar<'infer>>),
        (right_var, right_rows): (TcUnifierVar<'infer>, &RowSet<'infer, TcUnifierVar<'infer>>),
    ) -> Option<Result<(), TypeCheckError<'infer>>> {
        match orxr {
            OrderedRowXorRow::ClosedOpen(row, var) if left_var.eq(var) => Some(
                self.unify_closedrow_rowset(*row, right_rows)
                    .and_then(|()| self.unify_var_ty(right_var, self.mk_ty(RowTy(*row)))),
            ),
            OrderedRowXorRow::ClosedOpen(row, var) if right_var.eq(var) => Some(
                self.unify_closedrow_rowset(*row, left_rows)
                    .and_then(|()| self.unify_var_ty(left_var, self.mk_ty(RowTy(*row)))),
            ),
            OrderedRowXorRow::OpenOpen { min, max } if left_var.eq(min) => Some(
                self.unifiers
                    .unify_var_var(*max, right_var)
                    .map_err(|e| e.into()),
            ),
            OrderedRowXorRow::OpenOpen { min, max } if left_var.eq(max) => Some(
                self.unifiers
                    .unify_var_var(*min, right_var)
                    .map_err(|e| e.into()),
            ),
            OrderedRowXorRow::OpenOpen { min, max } if right_var.eq(min) => Some(
                self.unifiers
                    .unify_var_var(*max, left_var)
                    .map_err(|e| e.into()),
            ),
            OrderedRowXorRow::OpenOpen { min, max } if right_var.eq(max) => Some(
                self.unifiers
                    .unify_var_var(*min, left_var)
                    .map_err(|e| e.into()),
            ),
            _ => None,
        }
    }

    pub(crate) fn find_unify_closed_open_orxr(
        &mut self,
        orxr: &OrderedRowXorRow<'infer, TcUnifierVar<'infer>>,
        left_row: ClosedRow<'infer, TcUnifierVar<'infer>>,
        right_var: TcUnifierVar<'infer>,
    ) -> Option<Result<(), TypeCheckError<'infer>>> {
        match orxr {
            OrderedRowXorRow::ClosedOpen(row, tv) if left_row.fields == row.fields => Some(
                self.unify_closedrow_closedrow(left_row, *row)
                    .and_then(|()| {
                        self.unifiers
                            .unify_var_var(right_var, *tv)
                            .map_err(|e| e.into())
                    }),
            ),
            OrderedRowXorRow::ClosedOpen(row, tv) if right_var == *tv => Some(
                self.unify_closedrow_closedrow(left_row, *row)
                    .and_then(|()| {
                        self.unifiers
                            .unify_var_var(right_var, *tv)
                            .map_err(|e| e.into())
                    }),
            ),
            OrderedRowXorRow::OpenOpen { min, max } if right_var == *min => {
                Some(self.unify_var_ty(*max, self.mk_ty(RowTy(left_row))))
            }
            OrderedRowXorRow::OpenOpen { min, max } if right_var == *max => {
                Some(self.unify_var_ty(*min, self.mk_ty(RowTy(left_row))))
            }
            _ => None,
        }
    }

    pub(crate) fn find_unify_closed_open_combineinto(
        &mut self,
        combo: &CombineInto<'infer, TcUnifierVar<'infer>>,
        goal_row: ClosedRow<'infer, TcUnifierVar<'infer>>,
        (other_var, other_rows): (TcUnifierVar<'infer>, &RowSet<'infer, TcUnifierVar<'infer>>),
    ) -> Option<Result<(), TypeCheckError<'infer>>> {
        match (combo.goal, combo.other) {
            (Row::Closed(goal), other) if goal.fields == goal_row.fields => Some(
                self.unify_closedrow_closedrow(goal, goal_row)
                    .and_then(|()| self.unify_row_rowset(other, other_var, other_rows)),
            ),
            (goal, Row::Open(other)) if other_var == other => Some(match goal {
                Row::Open(var) => {
                    self.unify_ty_ty(self.mk_ty(VarTy(var)), self.mk_ty(RowTy(goal_row)))
                }
                Row::Closed(row) => self.unify_closedrow_closedrow(row, goal_row),
            }),
            _ => None,
        }
    }
}

// TODO: Bespoke use cases of this type once they've settled
#[derive(Debug)]
enum Either<A, B> {
    Left(A),
    Right(B),
}
impl<A, B> Either<A, B> {
    fn try_and_right<C, Err>(
        self,
        f: impl FnOnce(B) -> Result<Either<A, C>, Err>,
    ) -> Result<Either<A, C>, Err> {
        match self {
            Left(a) => Ok(Left(a)),
            Right(b) => f(b),
        }
    }
}
use Either::*;

/// Instantiate a type scheme for type checking.
/// This means replacing all it's TcVars with fresh unifiers and adding any constraints (post
/// substitution) to the list of constraints that must be true.
struct Instantiate<'a, 'infer, I> {
    ctx: &'a I,
    unifiers: Vec<TcUnifierVar<'infer>>,
}
impl<'infer, I> FallibleTypeFold<'infer> for Instantiate<'_, 'infer, I>
where
    I: MkTy<'infer, TcUnifierVar<'infer>>,
{
    type InTypeVar = TcVar;
    type TypeVar = TcUnifierVar<'infer>;
    type Error = TcVarToUnifierError;

    fn ctx(&self) -> &dyn MkTy<'infer, Self::TypeVar> {
        self.ctx
    }

    fn try_fold_var(
        &mut self,
        var: Self::InTypeVar,
    ) -> Result<Ty<'infer, Self::TypeVar>, Self::Error> {
        Ok(self.ctx().mk_ty(VarTy(self.unifiers[var.0])))
    }

    fn try_fold_row_var(
        &mut self,
        var: Self::InTypeVar,
    ) -> Result<Row<'infer, Self::TypeVar>, Self::Error> {
        Ok(Row::Open(self.unifiers[var.0]))
    }
}

/// Zonk anything that is TypeFoldable.
/// This removes all unification variables.
/// If a unification variables is solved to a type, it is replaced by that type.
/// If a unification variable has no solution, we replace it by a fresh type variable and record it
/// as free.
pub struct Zonker<'a, 'ctx, 'infer> {
    ctx: &'a dyn MkTy<'ctx, TcVar>,
    unifiers: &'a mut InPlaceUnificationTable<TcUnifierVar<'infer>>,
    free_vars: Vec<TcUnifierVar<'infer>>,
}

impl<'a, 'ctx, 'infer> Zonker<'a, 'ctx, 'infer> {
    fn add(&mut self, var: TcUnifierVar<'infer>) -> TcVar {
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
        TcVar::from_raw(var_indx)
    }
}

impl<'ctx, 'infer> FallibleTypeFold<'ctx> for Zonker<'_, 'ctx, 'infer> {
    type Error = UnifierToTcVarError;
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
            Some(Candidate::Ty(ty)) => ty.try_fold_with(self),
            _ => {
                // Our unification variable wasn't solved to a type.
                // Generalize it to a type variable.
                let ty_var = self.add(var);
                Ok(self.ctx().mk_ty(VarTy(ty_var)))
            }
        }
    }

    fn try_fold_row_var(
        &mut self,
        var: Self::InTypeVar,
    ) -> Result<Row<'ctx, Self::TypeVar>, Self::Error> {
        match self.unifiers.probe_value(var) {
            Some(Candidate::Ty(Ty(Handle(RowTy(row))))) => row.try_fold_with(self).map(Row::Closed),
            // TODO: Consider if we should handle PartialRow cases here
            _ => {
                let row_var = self.add(var);
                Ok(Row::Open(row_var))
            }
        }
    }
}

/// Information we need about effects during type checking
pub trait EffectInfo<'s, 'ctx> {
    /// Lookup the name of an effect from it's ID
    fn effect_name(&self, eff: EffectId) -> RefHandle<'s, str>;
    /// Lookup effect members from it's ID
    fn effect_members(&self, eff: EffectId) -> RefHandle<'ctx, [EffectOpId]>;

    /// Reverse index lookup, find an effect's ID from one of it's operation
    fn lookup_effect_by_member(&self, member: EffectOpId) -> EffectId;
    /// Lookup the type signature of an effect's member
    fn effect_member_sig(&self, eff: EffectId, member: EffectOpId) -> TyScheme<'ctx, TcVar>;
    /// Lookup the name of an effect's member
    fn effect_member_name(&self, eff: EffectId, member: EffectOpId) -> RefHandle<'s, str>;
}

pub fn type_check<'ty, 'infer, 's, 'eff, I, II, E>(
    ty_ctx: &I,
    infer_ctx: &II, // TODO: Consider removing this to ensure inference don't escape type checking.
    eff_info: &E,
    ast: Ast<'_, VarId>,
) -> (
    FxHashMap<VarId, Ty<'ty, TcVar>>,
    TyScheme<'ty, TcVar>,
    Vec<TypeCheckError<'infer>>,
)
where
    I: MkTy<'ty, TcVar>,
    II: MkTy<'infer, TcUnifierVar<'infer>>,
    E: EffectInfo<'s, 'eff>,
{
    tc_term(ty_ctx, infer_ctx, eff_info, ast.root())
}

fn tc_term<'ty, 'infer, 's, 'eff, I, II, E>(
    ty_ctx: &I,
    infer_ctx: &II,
    eff_info: &E,
    term: &'_ Term<'_, VarId>,
) -> (
    FxHashMap<VarId, Ty<'ty, TcVar>>,
    TyScheme<'ty, TcVar>,
    Vec<TypeCheckError<'infer>>,
)
where
    I: MkTy<'ty, TcVar>,
    II: MkTy<'infer, TcUnifierVar<'infer>>,
    E: EffectInfo<'s, 'eff>,
{
    let mut infer = InferCtx {
        local_env: FxHashMap::default(),
        unifiers: InPlaceUnificationTable::default(),
        errors: vec![],
        ctx: infer_ctx,
    };

    // Infer types for all our variables and the root term.
    let (constraints, var_tys, ty) = infer.infer(eff_info, term);

    // Solve constraints into the unifiers mapping.
    let (mut unifiers, errors) = infer.solve(constraints);

    // Zonk the variable -> type mapping and the root term type.
    let mut zonker = Zonker {
        ctx: ty_ctx,
        unifiers: &mut unifiers,
        free_vars: vec![],
    };
    let zonked_infer = ty.try_fold_with(&mut zonker).unwrap();
    let zonked_var_tys = var_tys
        .into_iter()
        .map(|(var, ty)| (var, ty.try_fold_with(&mut zonker).unwrap()))
        .collect::<FxHashMap<_, _>>();

    let ev = collect_evidence(&mut zonker);

    println!("{:#?}", zonker.unifiers);

    let scheme = TyScheme {
        bound: zonker
            .free_vars
            .into_iter()
            .enumerate()
            .map(|(i, _)| TcVar::from_raw(i))
            .collect(),
        constrs: ev.into_iter().collect(),
        eff: zonked_infer.eff,
        ty: zonked_infer.ty,
    };
    (zonked_var_tys, scheme, errors)
}

fn collect_evidence<'ctx, 'infer>(
    zonker: &mut Zonker<'_, 'ctx, 'infer>,
) -> Vec<Evidence<'ctx, TcVar>> {
    let mut evidence = FxHashSet::default();

    for unifier in (0..zonker.unifiers.len() as u32).map(TcUnifierVar::from_index) {
        let root = zonker.unifiers.find(unifier);
        // Only visit root unifiers
        if unifier != root {
            continue;
        }
        if let Some(Candidate::Row(row_set)) = zonker.unifiers.probe_value(root) {
            evidence.extend(
                row_set
                    .goals
                    .into_iter()
                    .map(|rows| match rows {
                        OrderedRowXorRow::ClosedOpen(row, tv) => Evidence::Row {
                            left: Row::Closed(row),
                            right: Row::Open(tv),
                            goal: Row::Open(root),
                        },
                        OrderedRowXorRow::OpenOpen { min, max } => Evidence::Row {
                            left: Row::Open(min),
                            right: Row::Open(max),
                            goal: Row::Open(root),
                        },
                    })
                    .map(|ev| ev.try_fold_with(zonker).unwrap()),
            );
            evidence.extend(
                row_set
                    .comps
                    .into_iter()
                    .map(|combo| match combo.other {
                        Row::Open(tv) => Evidence::Row {
                            left: Row::Open(std::cmp::min(tv, root)),
                            right: Row::Open(std::cmp::max(tv, root)),
                            goal: combo.goal,
                        },
                        Row::Closed(_) => Evidence::Row {
                            left: combo.other,
                            right: Row::Open(root),
                            goal: combo.goal,
                        },
                    })
                    .map(|ev| ev.try_fold_with(zonker).unwrap()),
            );
        }
    }

    evidence.into_iter().collect()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Evidence<'ctx, TV> {
    Row {
        left: Row<'ctx, TV>,
        right: Row<'ctx, TV>,
        goal: Row<'ctx, TV>,
    },
}

impl<'ctx, TV: Clone> TypeFoldable<'ctx> for Evidence<'_, TV> {
    type TypeVar = TV;
    type Out<T: 'ctx> = Evidence<'ctx, T>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::TypeVar>, F::Error> {
        match self {
            Evidence::Row { left, right, goal } => Ok(Evidence::Row {
                left: left.try_fold_with(fold)?,
                right: right.try_fold_with(fold)?,
                goal: goal.try_fold_with(fold)?,
            }),
        }
    }
}

/// A type scheme (also know as a polymorphic type).
/// Type schemes wrap a monomorphic type in any number of foralls binding the free variables within
/// the monomorphic type. They may also assert constraints on the bound type variables.
#[derive(Debug)]
pub struct TyScheme<'ctx, TV> {
    bound: Vec<TV>,
    constrs: Vec<Evidence<'ctx, TV>>,
    eff: Row<'ctx, TV>,
    ty: Ty<'ctx, TV>,
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
    row_fields: ShardedHashMap<RefHandle<'ctx, [RowLabel<'ctx>]>, ()>,
    row_values: ShardedHashMap<RefHandle<'ctx, [Ty<'ctx, TV>]>, ()>,
}

impl<'ctx, TV> TyCtx<'ctx, TV>
where
    TV: Eq + Copy + Hash,
{
    #[cfg(test)]
    fn new(arena: &'ctx Bump) -> Self {
        Self {
            arena,
            tys: ShardedHashMap::default(),
            row_fields: ShardedHashMap::default(),
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

    fn intern_fields(&self, fields: &[RowLabel<'ctx>]) -> RefHandle<'ctx, [RowLabel<'ctx>]> {
        self.row_fields
            ._intern_ref(fields, || Handle(self.arena.alloc_slice_copy(fields)))
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

    fn mk_row(&self, fields: &[RowLabel<'ctx>], values: &[Ty<'ctx, TV>]) -> ClosedRow<'ctx, TV> {
        debug_assert!(
            fields.len() == values.len(),
            "Expected row fields and valuse to be the same length"
        );
        debug_assert!(
            fields.iter().considered_sorted_by(|a, b| str::partial_cmp(a, b)),
            "Expected row fields to be sorted"
        );
        ClosedRow {
            fields: self.intern_fields(fields),
            values: self.intern_values(values),
        }
    }
}

// TODO: Replace this once `is_sorted` is stabilized
trait IteratorSorted: Iterator {
    // Is this iterator sorted based on compare function
    fn considered_sorted_by<F>(self, compare: F) -> bool
    where
        Self: Sized,
        F: FnMut(&Self::Item, &Self::Item) -> Option<Ordering>;

    // Is this iterator sorted based on it's PartialOrd instance
    fn considered_sorted(self) -> bool
    where
        Self: Sized,
        Self::Item: PartialOrd,
    {
        self.considered_sorted_by(PartialOrd::partial_cmp)
    }
}

impl<I: Iterator> IteratorSorted for I {
    fn considered_sorted_by<F>(mut self, mut compare: F) -> bool
    where
        Self: Sized,
        F: FnMut(&Self::Item, &Self::Item) -> Option<Ordering>,
    {
        let mut last = match self.next() {
            Some(e) => e,
            None => return true,
        };

        self.all(move |curr| {
            if let Some(Ordering::Greater) | None = compare(&last, &curr) {
                return false;
            }
            last = curr;
            true
        })
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use aiahr_core::ast::Ast;
    use assert_matches::assert_matches;
    use bumpalo::Bump;

    use super::*;

    macro_rules! ty {
        ({}) => {
            Ty(Handle(ProdTy(Row::Closed(ClosedRow {
                fields: Handle(&[]),
                values: Handle(&[]),
            }))))
        };
        ($kind:pat) => {
            Ty(Handle($kind))
        };
    }

    macro_rules! row {
        ([$($field:pat),*], [$($value:pat),*]) => {
            ClosedRow {
                fields: Handle([$(Handle($field)),*]),
                values: Handle([$($value),*]),
            }
        };
    }

    // Utility trait to remove a lot of the intermediate allocation when creating ASTs
    // Helps make tests a little more readable
    trait MkTerm<'a, Var> {
        fn mk_abs(&'a self, arg: Var, body: Term<'a, Var>) -> Term<'a, Var>;
        fn mk_app(&'a self, fun: Term<'a, Var>, arg: Term<'a, Var>) -> Term<'a, Var>;
        fn mk_label(&'a self, label: &str, term: Term<'a, Var>) -> Term<'a, Var>;
        fn mk_unlabel(&'a self, label: &str, term: Term<'a, Var>) -> Term<'a, Var>;
        fn mk_concat(&'a self, left: Term<'a, Var>, right: Term<'a, Var>) -> Term<'a, Var>;
        fn mk_project(&'a self, direction: Direction, term: Term<'a, Var>) -> Term<'a, Var>;

        fn mk_abss<II>(&'a self, args: II, body: Term<'a, Var>) -> Term<'a, Var>
        where
            II: IntoIterator,
            II::IntoIter: DoubleEndedIterator<Item = Var>,
        {
            args.into_iter()
                .rfold(body, |body, arg| self.mk_abs(arg, body))
        }
    }

    impl<'a, Var> MkTerm<'a, Var> for Bump {
        fn mk_abs(&'a self, arg: Var, body: Term<'a, Var>) -> Term<'a, Var> {
            Abstraction {
                arg,
                body: self.alloc(body),
            }
        }

        fn mk_app(&'a self, fun: Term<'a, Var>, arg: Term<'a, Var>) -> Term<'a, Var> {
            Application {
                func: self.alloc(fun),
                arg: self.alloc(arg),
            }
        }

        fn mk_label(&'a self, label: &str, term: Term<'a, Var>) -> Term<'a, Var> {
            Label {
                label: Handle(self.alloc_str(label)),
                term: self.alloc(term),
            }
        }

        fn mk_unlabel(&'a self, label: &str, term: Term<'a, Var>) -> Term<'a, Var> {
            Unlabel {
                label: Handle(self.alloc_str(label)),
                term: self.alloc(term),
            }
        }

        fn mk_concat(&'a self, left: Term<'a, Var>, right: Term<'a, Var>) -> Term<'a, Var> {
            Concat {
                left: self.alloc(left),
                right: self.alloc(right),
            }
        }

        fn mk_project(&'a self, direction: Direction, term: Term<'a, Var>) -> Term<'a, Var> {
            Project {
                direction,
                term: self.alloc(term),
            }
        }
    }

    struct DummyEff;
    impl EffectInfo<'static, 'static> for DummyEff {
        fn effect_name(&self, _eff: EffectId) -> RefHandle<'static, str> {
            Handle("")
        }

        fn effect_members(&self, _eff: EffectId) -> RefHandle<'static, [EffectOpId]> {
            Handle(&[])
        }

        fn lookup_effect_by_member(&self, _member: EffectOpId) -> EffectId {
            EffectId(0)
        }

        fn effect_member_sig(
            &self,
            _eff: EffectId,
            _member: EffectOpId,
        ) -> TyScheme<'static, TcVar> {
            TyScheme {
                bound: vec![],
                constrs: vec![],
                eff: Row::Open(TcVar(0)),
                ty: Ty(Handle(&VarTy(TcVar(1)))),
            }
        }

        fn effect_member_name(
            &self,
            _eff: EffectId,
            _member: EffectOpId,
        ) -> RefHandle<'static, str> {
            Handle("")
        }
    }

    #[test]
    fn test_tc_unlabel() {
        let arena = Bump::new();
        let x = VarId(0);
        let untyped_ast = Ast::new(
            FxHashMap::default(),
            arena.alloc(arena.mk_abs(
                x,
                arena.mk_unlabel("start", arena.mk_label("start", Variable(x))),
            )),
        );
        let infer_intern = TyCtx::new(&arena);
        let ty_intern = TyCtx::new(&arena);

        let (_, scheme, _) = type_check(&ty_intern, &infer_intern, &DummyEff, untyped_ast);

        assert_matches!(
            scheme.ty,
            ty!(FunTy(ty!(VarTy(TcVar(0))), ty!(VarTy(TcVar(0)))))
        );
    }

    #[test]
    fn test_tc_unlabel_fails_on_wrong_label() {
        let arena = Bump::new();
        let x = VarId(0);
        let untyped_ast = Ast::new(
            FxHashMap::default(),
            arena.alloc(arena.mk_abs(
                x,
                arena.mk_unlabel("start", arena.mk_label("end", Variable(x))),
            )),
        );
        let infer_intern = TyCtx::new(&arena);
        let ty_intern = TyCtx::new(&arena);

        let (_, _, errors) = type_check(&ty_intern, &infer_intern, &DummyEff, untyped_ast);

        assert_matches!(
            errors[0],
            TypeCheckError::TypeMismatch(
                Candidate::Ty(ty!(RowTy(row!(["end"], [ty!(ErrorTy)])))),
                Candidate::Ty(ty!(RowTy(row!(["start"], [ty!(VarTy(_))]))))
            )
        );
    }

    #[test]
    fn test_tc_label() {
        let arena = Bump::new();
        let x = VarId(0);
        let untyped_ast = Ast::new(
            FxHashMap::default(),
            arena.alloc(arena.mk_abs(x, arena.mk_label("start", Variable(x)))),
        );
        let infer_intern = TyCtx::new(&arena);
        let ty_intern = TyCtx::new(&arena);

        let (_, scheme, _) = type_check(&ty_intern, &infer_intern, &DummyEff, untyped_ast);

        assert_matches!(
            scheme.ty,
            ty!(FunTy(
                ty!(VarTy(TcVar(0))),
                ty!(RowTy(ClosedRow {
                    fields: Handle(&[Handle("start")]),
                    values: Handle(&[ty!(VarTy(TcVar(0)))])
                })),
            ))
        );
    }

    #[test]
    fn test_tc_abs() {
        let arena = Bump::new();
        let x = VarId(0);
        let y = VarId(1);
        let untyped_ast = Ast::new(
            FxHashMap::default(),
            arena.alloc(arena.mk_abs(x, arena.mk_abs(y, Variable(x)))),
        );
        let infer_intern = TyCtx::new(&arena);
        let ty_intern = TyCtx::new(&arena);

        let (var_to_tys, scheme, _) = type_check(&ty_intern, &infer_intern, &DummyEff, untyped_ast);

        assert_matches!(var_to_tys.get(&VarId(0)), Some(ty!(VarTy(TcVar(0)))));
        assert_matches!(var_to_tys.get(&VarId(1)), Some(ty!(VarTy(TcVar(1)))));
        assert_matches!(
            scheme.ty,
            ty!(FunTy(
                ty!(VarTy(TcVar(0))),
                ty!(FunTy(ty!(VarTy(TcVar(1))), ty!(VarTy(TcVar(0))),)),
            ))
        );
    }

    #[test]
    fn test_tc_product_literal() {
        let arena = Bump::new();
        let x = VarId(0);
        let untyped_ast = Ast::new(
            FxHashMap::default(),
            arena.alloc(arena.mk_abs(
                x,
                arena.mk_concat(
                    arena.mk_concat(
                        arena.mk_label("a", Variable(x)),
                        arena.mk_label("b", Variable(x)),
                    ),
                    arena.mk_concat(
                        arena.mk_label("c", Variable(x)),
                        arena.mk_label("d", Variable(x)),
                    ),
                ),
            )),
        );
        let infer_intern = TyCtx::new(&arena);
        let ty_intern = TyCtx::new(&arena);

        let (_var_to_tys, scheme, _) =
            type_check(&ty_intern, &infer_intern, &DummyEff, untyped_ast);

        assert_matches!(
            scheme.ty,
            ty!(FunTy(
                ty!(VarTy(TcVar(0))),
                ty!(ProdTy(Row::Closed(row!(
                    ["a", "b", "c", "d"],
                    [
                        ty!(VarTy(TcVar(0))),
                        ty!(VarTy(TcVar(0))),
                        ty!(VarTy(TcVar(0))),
                        ty!(VarTy(TcVar(0)))
                    ]
                ))))
            ))
        )
    }

    #[test]
    fn test_tc_product_wand() {
        let arena = Bump::new();
        let m = VarId(0);
        let n = VarId(1);
        let untyped_ast = Ast::new(
            FxHashMap::default(),
            arena.alloc(arena.mk_abss(
                [m, n],
                arena.mk_unlabel(
                    "x",
                    arena.mk_project(Direction::Right, arena.mk_concat(Variable(m), Variable(n))),
                ),
            )),
        );
        let infer_intern = TyCtx::new(&arena);
        let ty_intern = TyCtx::new(&arena);

        let (_var_to_tys, scheme, _) =
            type_check(&ty_intern, &infer_intern, &DummyEff, untyped_ast);

        // TODO: Fix this test.
        /*assert_matches!(
            scheme.constrs[0],
            Evidence::Row {
                left: Row::Open(_),
                right: Row::Closed(row!(["x"], [ty!(VarTy(TcVar(4)))])),
                goal: Row::Open(TcVar(2))
            }
        );
        assert_matches!(
            scheme.constrs[1],
            Evidence::Row {
                left: Row::Open(_),
                right: Row::Open(_),
                goal: Row::Open(TcVar(2))
            }
        );
        assert_matches!(
            scheme.ty,
            ty!(FunTy(
                ty!(ProdTy(Row::Open(_))),
                ty!(FunTy(ty!(ProdTy(Row::Open(_))), ty!(VarTy(TcVar(4)))))
            ))
        )*/
    }

    #[test]
    fn test_tc_applied_wand() {
        let arena = Bump::new();
        let m = VarId(0);
        let n = VarId(1);
        let untyped_ast = Ast::new(
            FxHashMap::default(),
            arena.alloc(arena.mk_app(
                arena.mk_abss(
                    [m, n],
                    arena.mk_unlabel(
                        "x",
                        arena.mk_project(
                            Direction::Right,
                            arena.mk_concat(Variable(m), Variable(n)),
                        ),
                    ),
                ),
                arena.mk_label("x", Unit),
            )),
        );
        let infer_intern = TyCtx::new(&arena);
        let ty_intern = TyCtx::new(&arena);

        let (_var_to_tys, scheme, _) =
            type_check(&ty_intern, &infer_intern, &DummyEff, untyped_ast);

        println!("TyScheme {{\n  bound: {:?},\n  constrs: {:?}\n  eff: {:?}\n  ty: {:?}\n}}", scheme.bound, scheme.constrs, scheme.eff, scheme.ty);

        // TODO: Fix this test
        /*assert_matches!(
            scheme.constrs[0],
            Evidence::Row {
                left: Row::Open(_),
                right: Row::Closed(row!(["x"], [ty!({})])),
                goal: Row::Open(_),
            }
        );
        assert_matches!(scheme.ty, ty!(FunTy(ty!(ProdTy(Row::Open(_))), ty!({}))))*/
    }

    #[test]
    fn test_tc_undefined_var_fails() {
        let arena = Bump::new();
        let untyped_ast = Ast::new(FxHashMap::default(), arena.alloc(Variable(VarId(0))));
        let infer_ctx = TyCtx::new(&arena);
        let ty_ctx = TyCtx::new(&arena);

        let (_, _, errors) = type_check(&infer_ctx, &ty_ctx, &DummyEff, untyped_ast);

        assert!(errors.contains(&TypeCheckError::VarNotDefined(VarId(0))))
    }
}
