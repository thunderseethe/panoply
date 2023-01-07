use std::{
    borrow::Borrow,
    convert::Infallible,
    hash::{BuildHasherDefault, Hash, Hasher},
};
use aiahr_core::{
    ast::{Ast, Direction, Term, Term::*},
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
pub enum TypeCheckError<'ctx> {
    /// A variable we expected to exist with a type, did not
    VarNotDefined(VarId),
    ItemNotDefined((ModuleId, ItemId)),
    TypeMismatch(
        UnifyVal<'ctx, TcUnifierVar<'ctx>>,
        UnifyVal<'ctx, TcUnifierVar<'ctx>>,
    ),
    OccursCheckFailed(TcUnifierVar<'ctx>),
    UnifierToTcVar(UnifierToTcVarError),
    RowsNotDisjoint(
        ClosedRow<'ctx, TcUnifierVar<'ctx>>,
        ClosedRow<'ctx, TcUnifierVar<'ctx>>,
    ),
    RowsNotEqual(InferRow<'ctx>, InferRow<'ctx>),
    Unification(
        UnifyVal<'ctx, TcUnifierVar<'ctx>>,
        UnifyVal<'ctx, TcUnifierVar<'ctx>>,
    ),
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
        UnifyVal<'ctx, TcUnifierVar<'ctx>>,
        UnifyVal<'ctx, TcUnifierVar<'ctx>>,
    )> for TypeCheckError<'ctx>
{
    fn from(
        (left, right): (
            UnifyVal<'ctx, TcUnifierVar<'ctx>>,
            UnifyVal<'ctx, TcUnifierVar<'ctx>>,
        ),
    ) -> Self {
        TypeCheckError::TypeMismatch(left, right)
    }
}
impl<'ctx> From<(InferTy<'ctx>, InferTy<'ctx>)> for TypeCheckError<'ctx> {
    fn from((left, right): (InferTy<'ctx>, InferTy<'ctx>)) -> Self {
        TypeCheckError::TypeMismatch(left.into(), right.into())
    }
}

/// A constraint produced during initial type checking.
/// It will be solved in the second half of type checking to produce a map from Unifier variables
/// to their types.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Constraint<'ctx, TV> {
    /// Two types must be equal for our term to type check
    Eq(Ty<'ctx, TV>, Ty<'ctx, TV>),
    RowCombine {
        left: Row<'ctx, TV>,
        right: Row<'ctx, TV>,
        goal: Row<'ctx, TV>,
    },
}
type InferConstraint<'infer> = Constraint<'infer, TcUnifierVar<'infer>>;

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
            Some(UnifyVal::Ty(ty)) => ty,
            _ => self.ctx().mk_ty(VarTy(var)),
        })
    }

    fn try_fold_row_var(
        &mut self,
        var: Self::InTypeVar,
    ) -> Result<Row<'inf, Self::TypeVar>, Self::Error> {
        Ok(match self.unifiers.probe_value(var) {
            Some(UnifyVal::Ty(ty)) => ty
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

impl<'infer> InferCtx<'infer> {
    /// This is the entrypoint to the bidirectional type checker. Since our language uses
    /// damnas-milner type inference we will always begin type checking with a call to infer.
    pub fn infer<I>(
        &mut self,
        infer_ctx: &I,
        term: &Term<'_, VarId>,
    ) -> (
        Vec<InferConstraint<'infer>>,
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
        constraints: &mut Vec<InferConstraint<'infer>>,
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
            (Label { .. }, ProdTy(row)) => {
                // A label can check against a product, if it checks against the product's internal
                // type
                self._check(infer_ctx, constraints, var_tys, term, row.to_ty(infer_ctx))
            }
            (Label { label, term }, RowTy(row)) => {
                // If our row is too small or too big, fail
                if row.fields.len() != 1 {
                    self.errors.push(
                        (
                            infer_ctx.single_row_ty(label, infer_ctx.mk_ty(ErrorTy)),
                            expected_ty,
                        )
                            .into(),
                    );
                    return ();
                }
                let field = infer_ctx.mk_label(label);
                // If our singleton row is a different field name, fail
                if field != row.fields[0] {
                    self.errors.push(
                        (
                            infer_ctx.single_row_ty(field.as_ref(), infer_ctx.mk_ty(ErrorTy)),
                            expected_ty,
                        )
                            .into(),
                    )
                }

                // If this is a singleton row with the right label check it's value type matches
                // our term type
                self._check(infer_ctx, constraints, var_tys, term, row.values[0])
            }
            (Unit, ty_pat!({})) => { /* Nothing to check here, this is trivially true */ }
            (Unlabel { label, term }, _) => {
                let expected_ty = infer_ctx.single_row_ty(label, expected_ty);
                self._check(infer_ctx, constraints, var_tys, term, expected_ty)
            }
            (Concat { .. }, RowTy(row)) => {
                // Coerece a row type into a product and re-check.
                self._check(
                    infer_ctx,
                    constraints,
                    var_tys,
                    term,
                    infer_ctx.mk_ty(ProdTy(Row::Closed(row))),
                );
            }
            (Concat { left, right }, ProdTy(row)) => {
                let left_ty = self._infer(infer_ctx, constraints, var_tys, left);
                let left_row = self.equate_as_row(infer_ctx, constraints, left_ty);

                let right_ty = self._infer(infer_ctx, constraints, var_tys, right);
                let right_row = self.equate_as_row(infer_ctx, constraints, right_ty);

                constraints.push(Constraint::RowCombine {
                    left: left_row,
                    right: right_row,
                    goal: row,
                });
            }
            (Project { .. }, RowTy(row)) => {
                // Coerce row into a product and re-check.
                self._check(
                    infer_ctx,
                    constraints,
                    var_tys,
                    term,
                    infer_ctx.mk_ty(ProdTy(Row::Closed(row))),
                );
            }
            (Project { direction, term }, ProdTy(row)) => {
                let term_ty = self._infer(infer_ctx, constraints, var_tys, term);
                let term_row = self.equate_as_row(infer_ctx, constraints, term_ty);
                let unbound_row = Row::Open(self.unifiers.new_key(None).into());

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
        constraints: &mut Vec<InferConstraint<'infer>>,
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
            Label { label, term } => {
                let ty = self._infer(infer_ctx, constraints, var_tys, term);
                infer_ctx.single_row_ty(label, ty)
            }
            Unlabel { label, term } => {
                let term_ty = self._infer(infer_ctx, constraints, var_tys, term);
                let field = infer_ctx.mk_label(label);
                match *term_ty {
                    // If our output type is already a singleton row of without a label, use it
                    // directly. This avoids introducing needless unifiers
                    RowTy(row) if row.len() == 1 && row.fields[0] == field => row.values[0],
                    // Othewise introduce a unifier, and rely on unification for any needed error
                    // reporting
                    _ => {
                        let out_ty = infer_ctx.mk_ty(VarTy(self.unifiers.new_key(None)));
                        let row_ty = infer_ctx.mk_ty(RowTy(infer_ctx.mk_row(&[field], &[out_ty])));
                        constraints.push(Constraint::Eq(row_ty, term_ty));
                        out_ty
                    }
                }
            }
            Unit => {
                // Represent unit by an empty product type
                let unit = infer_ctx.mk_row(&[], &[]);
                infer_ctx.mk_ty(ProdTy(Row::Closed(unit)))
            }
            Concat { left, right } => {
                let left_row = self.unifiers.new_key(None);
                let right_row = self.unifiers.new_key(None);
                let out_row = self.unifiers.new_key(None);

                let left_ty = infer_ctx.mk_ty(ProdTy(Row::Open(left_row.into())));
                let right_ty = infer_ctx.mk_ty(ProdTy(Row::Open(right_row.into())));

                self._check(infer_ctx, constraints, var_tys, left, left_ty);
                self._check(infer_ctx, constraints, var_tys, right, right_ty);

                constraints.push(Constraint::RowCombine {
                    left: Row::Open(left_row.into()),
                    right: Row::Open(right_row.into()),
                    goal: Row::Open(out_row.into()),
                });

                infer_ctx.mk_ty(ProdTy(Row::Open(out_row.into())))
            }
            Project { direction, term } => {
                let big_row: TcUnifierVar<'infer> = self.unifiers.new_key(None).into();
                let small_row: TcUnifierVar<'infer> = self.unifiers.new_key(None).into();
                // In a projection one of the row variables will be unbound
                let unbound_row: TcUnifierVar<'infer> = self.unifiers.new_key(None).into();

                let term_ty = infer_ctx.mk_ty(ProdTy(big_row.into()));
                self._check(infer_ctx, constraints, var_tys, term, term_ty);

                constraints.push(match direction {
                    Direction::Left => Constraint::RowCombine {
                        left: small_row.into(),
                        right: unbound_row.into(),
                        goal: big_row.into(),
                    },
                    Direction::Right => Constraint::RowCombine {
                        left: unbound_row.into(),
                        right: small_row.into(),
                        goal: big_row.into(),
                    },
                });

                infer_ctx.mk_ty(ProdTy(small_row.into()))
            }
            // TODOs
            Item(_) => todo!(),
        }
    }
 
    /// Apply the current partial substitution to a type, removing as many unifiers as possible
    /// before unification.
    fn normalize_ty<I>(&mut self, ctx: &I, ty: InferTy<'infer>) -> InferUnifyVal<'infer>
    where
        I: MkTy<'infer, TcUnifierVar<'infer>>,
    {
        match *ty {
            VarTy(var) => {
                self.unifiers.probe_value(var)
                    .map(|val| val.try_fold_with(&mut Normalize {ctx, unifiers: &mut self.unifiers}).unwrap())
                    .unwrap_or_else(|| ty.into())
            }
            _ => {
                ty.try_fold_with(&mut Normalize {ctx, unifiers: &mut self.unifiers}).unwrap().into()
            }
        }
    }

    fn normalize_row<I>(&mut self, ctx: &I, row: InferRow<'infer>) -> Result<InferRow<'infer>, PartialRow<'infer, TcUnifierVar<'infer>>>
    where
        I: MkTy<'infer, TcUnifierVar<'infer>>,
    {
        match row {
            Row::Open(var) => {
                self.unifiers.probe_value(var)
                    .map(|val| match val.try_fold_with(&mut Normalize {ctx, unifiers: &mut self.unifiers }).unwrap() {
                        UnifyVal::Ty(ty) => Ok(ty.try_to_row().expect("Row unifier was mapped to a type")),
                        UnifyVal::Row(row) => Err(row),
                    })
                    .unwrap_or_else(|| Ok(Row::Open(var)))
            },
            Row::Closed(row) => 
                Ok(Row::Closed(row.try_fold_with(&mut Normalize {ctx, unifiers: &mut self.unifiers}).unwrap())),
        }
        /*row.try_fold_with(&mut Normalize {
            ctx,
            unifiers: &mut self.unifiers,
        })
        .unwrap()*/
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
            .unify_var_value(var, Some(UnifyVal::Ty(ty_)))
            .map_err(|e| e.into())
    }

    fn unify_var_row<I>(
        &mut self,
        ctx: &I,
        goal_var: TcUnifierVar<'infer>,
        row: PartialRow<'infer, TcUnifierVar<'infer>>,
    ) -> Result<(), TypeCheckError<'infer>>
    where
        I: MkTy<'infer, TcUnifierVar<'infer>>,
    {
        let row_ = row.try_fold_with(&mut OccursCheck { ctx, var: goal_var })?;
        self.unifiers
            .unify_var_value(goal_var, Some(UnifyVal::Row(row_)))
            .map_err(|e| e.into())
    }

    fn unify_ty_ty_normalized<I>(
        &mut self,
        ctx: &I,
        left: InferTy<'infer>,
        right: InferTy<'infer>,
    ) -> Result<(), TypeCheckError<'infer>> 
    where
        I: MkTy<'infer, TcUnifierVar<'infer>>
    {
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
            (_, VarTy(var)) => self.unify_var_ty(ctx, var, left),
            (VarTy(var), _) => self.unify_var_ty(ctx, var, right),

            // Decompose compound types
            (FunTy(left_arg, left_ret), FunTy(right_arg, right_ret)) => {
                self.unify_ty_ty(ctx.clone(), left_arg, right_arg)?;
                self.unify_ty_ty(ctx, left_ret, right_ret)
            }
            (RowTy(left_row), RowTy(right_row)) => {
                // If our row labels aren't equal the types cannot be equal
                if left_row.fields != right_row.fields {
                    return Err((left, right).into());
                }

                for (left_ty, right_ty) in left_row.values.iter().zip(right_row.values.iter()) {
                    self.unify_ty_ty(ctx.clone(), *left_ty, *right_ty)?;
                }
                Ok(())
            }
            // Coerce a product into a row
            (RowTy(_), ProdTy(right)) => {
                self.unify_ty_ty(ctx, left, right.to_ty(ctx.clone()))
            }
            // Coerce a product into a row
            (ProdTy(left), RowTy(_)) => {
                self.unify_ty_ty(ctx, left.to_ty(ctx.clone()), right)
            }
            // Decompose product and unify both internal types
            (ProdTy(left), ProdTy(right)) => {
                self.unify_ty_ty(ctx, left.to_ty(ctx.clone()), right.to_ty(ctx.clone()))
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

        match (normal_left, normal_right) {
            (UnifyVal::Ty(left_ty), UnifyVal::Ty(right_ty)) => self.unify_ty_ty_normalized(ctx, left_ty, right_ty),
            (UnifyVal::Ty(ty), UnifyVal::Row(row)) | (UnifyVal::Row(row), UnifyVal::Ty(ty)) => {
                match (*ty, row) {
                    (VarTy(var), row) => self.unify_var_row(ctx, var, row),
                    (RowTy(row), PartialRow::OpenGoal { left, right }) => self.unify_row_row_towards(ctx, left, right, Row::Closed(row)),
                    (RowTy(row), PartialRow::OpenLeft { goal, right }) => self.unify_row_row_towards(ctx, Row::Closed(row), right, goal),
                    (RowTy(row), PartialRow::OpenRight { goal, left }) => self.unify_row_row_towards(ctx, left, Row::Closed(row), goal),
                    // The rest of these cases are just type errors, things like (IntTy, Row) that
                    // could never succeed
                    (_ , _) => Err(TypeCheckError::TypeMismatch(UnifyVal::Ty(ty), UnifyVal::Row(row))),
                }
            },
            // TODO: Figure out what to do (if anything) here.
            (UnifyVal::Row(left_row), UnifyVal::Row(right_row)) => panic!("Should this case be allowed? {:?} {:?}", left_row, right_row),
        }
    }

    /// Solve a list of constraints to a mapping from unifiers to types.
    /// If there is no solution to the list of constraints we return a relevant error.
    fn solve<I>(
        mut self,
        ctx: &I,
        constraints: Vec<InferConstraint<'infer>>,
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
                Constraint::RowCombine { left, right, goal } => self
                    .unify_row_row_towards(ctx.clone(), left, right, goal)
                    .map_err(|err| {
                        self.errors.push(err);
                        ()
                    })
                    .unwrap_or_default(),
            }
        }
        (self.unifiers, self.errors)
    }

    fn unify_row_row_towards<I>(
        &mut self,
        ctx: &I,
        left: InferRow<'infer>,
        right: InferRow<'infer>,
        goal: InferRow<'infer>,
    ) -> Result<(), TypeCheckError<'infer>>
    where
        I: MkTy<'infer, TcUnifierVar<'infer>>,
    {
        let goal = self.normalize_row(ctx, goal);
        let left = self.normalize_row(ctx, left);
        let right = self.normalize_row(ctx, right);

        match goal {
            Ok(goal) => match (left, right) {
                (Ok(left), Ok(right)) => self.unify_row_row_towards_normalized(ctx, left, right, goal),
                (Ok(left), Err(PartialRow::OpenRight { goal: right_goal, left: right_left })) => {
                    self.unify_row_row(ctx, left, right_left)?;
                    self.unify_row_row(ctx, goal, right_goal)
                },
                (Ok(left), Err(right)) => panic!("Does this case make sense? goal: {:?} left: {:?} right: {:?}", goal, left, right),
                (Err(PartialRow::OpenLeft { goal: left_goal, right: left_right }), Ok(right)) => {
                    self.unify_row_row(ctx, right, left_right)?;
                    self.unify_row_row(ctx, goal, left_goal)
                },
                (Err(left), Ok(right)) => panic!("Does this case make sense? goal: {:?} left: {:?} right: {:?}", goal, left, right),
                (Err(left), Err(right)) => panic!("Does this case make sense? goal: {:?} left: {:?} right: {:?}", goal, left, right),
            },
            Err(goal) => panic!("Does this case make sense? goal: {:?} left: {:?} right: {:?}", goal, left, right),

        }

    }

    /// Make this type equal to a row and return that equivalent row.
    /// If it is already a row convert it directly, otherwise add a constraint that type must be a
    /// row.
    pub(crate) fn equate_as_row<I>(
        &mut self,
        ctx: &I,
        constraints: &mut Vec<InferConstraint<'infer>>,
        ty: InferTy<'infer>,
    ) -> InferRow<'infer>
    where
        I: MkTy<'infer, TcUnifierVar<'infer>>,
    {
        match *ty {
            ProdTy(row) => row,
            RowTy(row) => Row::Closed(row),
            _ => {
                let unifier = self.unifiers.new_key(None);
                constraints.push(Constraint::Eq(ctx.mk_ty(VarTy(unifier.into())), ty));
                Row::Open(unifier.into())
            }
        }
    }

    fn unify_row_row_towards_normalized<I>(&mut self, ctx: &I, left: InferRow<'infer>, right: InferRow<'infer>, goal: InferRow<'infer>) -> Result<(), TypeCheckError<'infer>> 
    where
        I: MkTy<'infer, TcUnifierVar<'infer>>
    { 
        match goal {
            Row::Open(goal_var) => match (left, right) {
                // With only one open variable we can solve
                (Row::Closed(left), Row::Closed(right)) => {
                    let (fields, values) = left.disjoint_union(right)?;
                    let row = ctx.mk_ty(RowTy(ctx.mk_row(&fields, &values)));
                    self.unify_var_ty(ctx, goal_var, row)
                }
                (left, right) => {
                    self.unify_var_row(ctx, goal_var, PartialRow::OpenGoal { left, right })
                }
            },
            Row::Closed(goal_row) => match (left, right) {
                // Our rows are all closed, we just need to check they are equal and discharge them
                (Row::Closed(left_row), Row::Closed(right_row)) => {
                    let (fields, values) = left_row.disjoint_union(right_row)?;
                    let row = ctx.mk_ty(RowTy(ctx.mk_row(&fields, &values)));
                    self.unify_ty_ty(ctx, ctx.mk_ty(RowTy(goal_row)), row)
                }
                // With one open row we solve that row
                (Row::Open(left_var), Row::Closed(right_row)) => {
                    let (fields, values) = goal_row.difference(right_row);
                    let row = ctx.mk_ty(RowTy(ctx.mk_row(&fields, &values)));
                    self.unify_var_ty(ctx, left_var, row)
                }
                (Row::Closed(left_row), Row::Open(right_var)) => {
                    let (fields, values) = goal_row.difference(left_row);
                    let row = ctx.mk_ty(RowTy(ctx.mk_row(&fields, &values)));
                    self.unify_var_ty(ctx, right_var, row)
                }
                // We can't unify this case as is, we need more information.
                // And our goal is closed so we can't set it to pending
                (Row::Open(left_var), Row::Open(right_var)) => {
                    self.unify_var_row(ctx, left_var, PartialRow::OpenLeft { goal, right })?;
                    self.unify_var_row(ctx, right_var, PartialRow::OpenRight { goal, left })
                },
            },
        }
    }

    fn unify_row_row<I>(&mut self, ctx: &I, left: InferRow<'infer>, right: InferRow<'infer>) -> Result<(), TypeCheckError<'infer>> 
    where
        I: MkTy<'infer, TcUnifierVar<'infer>>,
    {
        match (left, right) {
            (Row::Open(left_var), Row::Open(right_var)) => self.unifiers.unify_var_var(left_var, right_var).map_err(|e| e.into()),
            (Row::Open(var), Row::Closed(row)) | (Row::Closed(row), Row::Open(var)) => self.unify_var_ty(ctx, var, ctx.mk_ty(RowTy(row))),
            // If both rows are closed unify them as types to ensure they are equal
            (Row::Closed(left), Row::Closed(right)) => self.unify_ty_ty_normalized(ctx, ctx.mk_ty(RowTy(left)), ctx.mk_ty(RowTy(right))),
        }
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
            Some(UnifyVal::Ty(ty)) => ty.try_fold_with(self),
            _ => {
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

pub fn type_check<'ty, 'infer, I, II>(
    ty_ctx: &I,
    infer_ctx: &II, // TODO: Consider removing this to ensure inference don't escape type checking.
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
    let mut zonker = Zonker {
        ctx: ty_ctx,
        unifiers: &mut unifiers,
        free_vars: vec![],
    };
    let zonked_ty = ty.try_fold_with(&mut zonker).unwrap();
    let zonked_var_tys = var_tys
        .into_iter()
        .map(|(var, ty)| (var, ty.try_fold_with(&mut zonker).unwrap()))
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
    row_fields: ShardedHashMap<RefHandle<'ctx, [RowLabel<'ctx>]>, ()>,
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
        ClosedRow {
            fields: self.intern_fields(fields),
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
        fn mk_label(&'a self, label: &str, term: Term<'a, Var>) -> Term<'a, Var>;
        fn mk_unlabel(&'a self, label: &str, term: Term<'a, Var>) -> Term<'a, Var>;
        fn mk_concat(&'a self, left: Term<'a, Var>, right: Term<'a, Var>) -> Term<'a, Var>;
    }

    impl<'a, Var> MkTerm<'a, Var> for Bump {
        fn mk_abs(&'a self, arg: Var, body: Term<'a, Var>) -> Term<'a, Var> {
            Abstraction {
                arg,
                body: self.alloc(body),
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
            Concat { left: self.alloc(left), right: self.alloc(right) }
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

        let (_, ty, _) = type_check(&ty_intern, &infer_intern, untyped_ast);

        assert_matches!(ty, ty!(FunTy(ty!(VarTy(TcVar(0))), ty!(VarTy(TcVar(0))))));
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

        let (_, _, errors) = type_check(&ty_intern, &infer_intern, untyped_ast);

        assert_matches!(
            errors[0],
            TypeCheckError::TypeMismatch(
                UnifyVal::Ty(ty!(RowTy(row!(["end"], [ty!(ErrorTy)])))),
                UnifyVal::Ty(ty!(RowTy(row!(["start"], [ty!(VarTy(_))]))))
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

        let (_, ty, _) = type_check(&ty_intern, &infer_intern, untyped_ast);

        assert_matches!(
            ty,
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
    fn test_tc_product_literal() {
        let arena = Bump::new();
        let x = VarId(0);
        let untyped_ast = Ast::new(
            FxHashMap::default(),
            arena.alloc(arena.mk_abs(VarId(0), arena.mk_concat(
                arena.mk_concat(arena.mk_label("a", Variable(x)), arena.mk_label("b", Variable(x))),
                arena.mk_concat(arena.mk_label("c", Variable(x)), arena.mk_label("d", Variable(x)))
            )))
        );
        let infer_intern = TyCtx::new(&arena);
        let ty_intern = TyCtx::new(&arena);

        let (_var_to_tys, ty, _) = type_check(&ty_intern, &infer_intern, untyped_ast);

        assert_matches!(ty,
                        ty!(FunTy(ty!(VarTy(TcVar(0))), ty!(ProdTy(Row::Closed(row!(
                            ["a", "b", "c", "d"],
                            [ty!(VarTy(TcVar(0))),ty!(VarTy(TcVar(0))),ty!(VarTy(TcVar(0))),ty!(VarTy(TcVar(0)))]
                        )))))))
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
