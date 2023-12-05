use aiahr_ast::{Ast, Direction, Term, Term::*};
use aiahr_core::{
    diagnostic::tc::TypeCheckDiagnostic, id::VarId, ident::Ident, memory::handle, modules::Module,
    span::Span,
};
use aiahr_ty::{
    infer::{
        InArena, InferTy, ScopedInferRow, ScopedRowK, SimpleInferRow, SimpleRowK, TcUnifierVar,
        TypeK,
    },
    row::*,
    TypeKind::*,
    *,
};
use ena::unify::InPlaceUnificationTable;
use la_arena::Idx;
use rustc_hash::{FxHashMap, FxHashSet};
use salsa::DebugWithDb;
use std::{collections::BTreeSet, convert::Infallible, ops::Deref};

use crate::{
    diagnostic::{into_diag, TypeCheckError},
    folds::{instantiate::Instantiate, normalize::Normalize, occurs_check::OccursCheck},
    type_scheme_of,
    unsolved_row::{ClosedGoal, OpenGoal, Operatives, UnsolvedRowEquation},
    Db, EffectInfo, Evidence, TyScheme,
};

mod unification;
use unification::Unify;

mod row_solver;
use row_solver::{
    RowCombination, RowEquationSolver, RowTheory, ScopedRowCombination, SimpleRowCombination,
};

use self::row_solver::IntoTypeCheckerError;

/// Constraints that the type of a product `handler` that handles effect `eff`
struct HandlesConstraint<'infer> {
    /// Product type that should be a handler
    handler: SimpleInferRow<'infer>,
    /// Effect that is handled by `handler`
    eff: ScopedInferRow<'infer>,
    /// The return type of the `handle` construct
    ret: InferTy<'infer>,
}

/// List of constraints produced during initial type checking.
/// These will be solved in the second half of type checking to produce a map from Unifier variables
/// to their types.
#[derive(Default)]
pub struct Constraints<'infer> {
    /// Each pair of types that should unify with each other
    tys: Vec<(InferTy<'infer>, InferTy<'infer>, Span)>,
    /// Sets of data row combinations predicates that must hold
    data_rows: Vec<(SimpleRowCombination<'infer>, Span)>,
    /// Sets of effect row combinations predicates that must hold
    effect_rows: Vec<(ScopedRowCombination<'infer>, Span)>,
    /// Sets of effect row equalities that must hold
    effect_eq: Vec<(ScopedInferRow<'infer>, ScopedInferRow<'infer>, Span)>,
    /// Set of handle constraints to connect handler product terms to their effect rows
    handles: Vec<(HandlesConstraint<'infer>, Span)>,
}

impl<'infer> Constraints<'infer> {
    fn add_ty_eq(&mut self, left: InferTy<'infer>, right: InferTy<'infer>, span: Span) {
        self.tys.push((left, right, span))
    }

    fn add_data_row_combine(
        &mut self,
        left: SimpleInferRow<'infer>,
        right: SimpleInferRow<'infer>,
        goal: SimpleInferRow<'infer>,
        span: Span,
    ) {
        self.data_rows
            .push((RowCombination { left, right, goal }, span))
    }

    fn add_effect_row_combine(
        &mut self,
        left: ScopedInferRow<'infer>,
        right: ScopedInferRow<'infer>,
        goal: ScopedInferRow<'infer>,
        span: Span,
    ) {
        self.effect_rows
            .push((RowCombination { left, right, goal }, span))
    }

    fn add_effect_row_eq(
        &mut self,
        left: ScopedInferRow<'infer>,
        right: ScopedInferRow<'infer>,
        span: Span,
    ) {
        self.effect_eq.push((left, right, span));
    }

    fn add_handles(
        &mut self,
        handler: SimpleInferRow<'infer>,
        eff: ScopedInferRow<'infer>,
        ret: InferTy<'infer>,
        span: Span,
    ) {
        self.handles
            .push((HandlesConstraint { handler, eff, ret }, span))
    }
}

/// Type states for infer context
pub(crate) struct Generation;
pub(crate) struct Solution;

pub(crate) trait InferState {
    type Storage<'infer>;
}

#[derive(Default)]
pub(crate) struct GenerationStorage<'infer> {
    pub(crate) var_tys: FxHashMap<VarId, InferTy<'infer>>,
    pub(crate) term_tys: FxHashMap<Idx<Term<VarId>>, InferResult<'infer>>,
    /// We use `Idx<Term<VarId>>` here (instead of `TermName`) because we need a wrapper for each
    /// individual use site of an item, and we may have multiple call sites for one item name.
    pub(crate) item_wrappers: FxHashMap<Idx<Term<VarId>>, Wrapper<InArena<'infer>>>,
    pub(crate) required_ev: FxHashSet<Evidence<InArena<'infer>>>,
}
impl InferState for Generation {
    type Storage<'infer> = GenerationStorage<'infer>;
}

#[derive(Default)]
pub(crate) struct SolutionStorage<'infer> {
    pub(crate) data_eqns: BTreeSet<UnsolvedRowEquation<InArena<'infer>, Simple>>,
    pub(crate) eff_eqns: BTreeSet<UnsolvedRowEquation<InArena<'infer>, Scoped>>,
}
impl InferState for Solution {
    type Storage<'infer> = SolutionStorage<'infer>;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TyChkRes<A: TypeAlloc> {
    pub ty: Ty<A>,
    pub eff: ScopedRow<A>,
}
impl<A: TypeAlloc> Copy for TyChkRes<A>
where
    A: Clone,
    Ty<A>: Copy,
    ScopedRow<A>: Copy,
{
}
impl<Db> DebugWithDb<Db> for TyChkRes<InDb>
where
    Db: ?Sized + aiahr_ty::Db,
{
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &Db,
        _include_all_fields: bool,
    ) -> std::fmt::Result {
        f.debug_struct("TyChkRes")
            .field("ty", &self.ty.debug(db))
            .field("eff", &self.eff.debug(db))
            .finish()
    }
}

impl<'ctx, A: TypeAlloc + Clone + 'ctx> TypeFoldable<'ctx> for TyChkRes<A> {
    type Alloc = A;
    type Out<B: TypeAlloc> = TyChkRes<B>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, In = Self::Alloc>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::Out>, F::Error> {
        Ok(TyChkRes {
            ty: self.ty.try_fold_with(fold)?,
            eff: self.eff.try_fold_with(fold)?,
        })
    }
}
type InferResult<'infer> = TyChkRes<InArena<'infer>>;

impl<'infer> InferResult<'infer> {
    fn new(ty: InferTy<'infer>, eff: ScopedInferRow<'infer>) -> Self {
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

/// A context for type inference.
///
/// Type inference is done in two stages:
///     1. Constraint generation
///     2. Unification
/// During 1. we use a bidirectional type checker to walk our input AST and generate a set of
/// constraints (and unification variables) that have to be true for the AST to type check. This
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
pub(crate) struct InferCtx<'a, 'infer, I, State: InferState = Generation> {
    /// Store types for local variables.
    local_env: FxHashMap<VarId, InferTy<'infer>>,
    /// Mapping from unification variables to their types (if any).
    ty_unifiers: InPlaceUnificationTable<TcUnifierVar<'infer>>,
    data_row_unifiers: InPlaceUnificationTable<TcUnifierVar<'infer, SimpleRowK>>,
    eff_row_unifiers: InPlaceUnificationTable<TcUnifierVar<'infer, ScopedRowK>>,
    /// Constraints that have to be true for this inference context.
    constraints: Constraints<'infer>,
    /// Errors that arise during type checking
    errors: Vec<TypeCheckDiagnostic>,
    /// Allocator for types created during inference
    ctx: &'a I,
    db: &'a dyn Db,
    /// Id of the module we're currently performing type checking within.
    module: Module,
    ast: &'a Ast<VarId>,
    state: State::Storage<'infer>,
    _marker: std::marker::PhantomData<State>,
}

impl<'infer, A: TypeAlloc, I: MkTy<A>, S: InferState> MkTy<A> for InferCtx<'_, 'infer, I, S> {
    fn mk_ty(&self, kind: TypeKind<A>) -> Ty<A> {
        self.ctx.mk_ty(kind)
    }

    fn mk_label(&self, label: &str) -> RowLabel {
        self.ctx.mk_label(label)
    }

    fn mk_row<R: NewRow<A>>(&self, fields: &[RowLabel], values: &[Ty<A>]) -> R {
        self.ctx.mk_row(fields, values)
    }

    fn mk_row_vec<R: NewRow<A>>(&self, fields: Vec<RowLabel>, values: Vec<Ty<A>>) -> R {
        self.ctx.mk_row_vec(fields, values)
    }

    fn mk_row_iter<R: NewRow<A>>(
        &self,
        fields: impl Iterator<Item = RowLabel>,
        values: impl Iterator<Item = Ty<A>>,
    ) -> R {
        self.ctx.mk_row_iter(fields, values)
    }
}

impl<'infer, A: TypeAlloc, I: AccessTy<'infer, A>, S: InferState> AccessTy<'infer, A>
    for InferCtx<'_, 'infer, I, S>
{
    fn kind(&self, ty: &Ty<A>) -> &'infer TypeKind<A> {
        self.ctx.kind(ty)
    }

    fn row_fields(&self, row: &<A as TypeAlloc>::RowFields) -> &'infer [RowLabel] {
        self.ctx.row_fields(row)
    }

    fn row_values(&self, row: &<A as TypeAlloc>::RowValues) -> &'infer [Ty<A>] {
        self.ctx.row_values(row)
    }
}

impl<'a, 'infer, I> InferCtx<'a, 'infer, I>
where
    I: MkTy<InArena<'infer>> + AccessTy<'infer, InArena<'infer>>,
{
    pub(crate) fn new(
        db: &'a dyn crate::Db,
        ctx: &'a I,
        module: Module,
        ast: &'a Ast<VarId>,
    ) -> Self {
        Self {
            local_env: FxHashMap::default(),
            ty_unifiers: InPlaceUnificationTable::default(),
            data_row_unifiers: InPlaceUnificationTable::default(),
            eff_row_unifiers: InPlaceUnificationTable::default(),
            errors: vec![],
            constraints: Constraints::default(),
            ctx,
            db,
            module,
            ast,
            state: GenerationStorage {
                var_tys: FxHashMap::default(),
                term_tys: FxHashMap::default(),
                required_ev: FxHashSet::default(),
                item_wrappers: FxHashMap::default(),
            },
            _marker: std::marker::PhantomData,
        }
    }

    pub fn entry_point_expected(&self) -> InferResult<'infer> {
        let unit = self.mk_ty(ProdTy(Row::Closed(self.empty_row())));
        InferResult::new(unit, Row::Closed(self.empty_row()))
    }

    /// This is the entrypoint to the bidirectional type checker. Since our language uses
    /// damnas-milner type inference we will always begin type checking with a call to infer.
    pub(crate) fn infer(
        mut self,
        term: Idx<Term<VarId>>,
    ) -> (
        InferCtx<'a, 'infer, I, Solution>,
        <Generation as InferState>::Storage<'infer>,
        InferResult<'infer>,
    ) {
        let res = self._infer(term);
        let (var_tys, infer_ctx) = InferCtx::with_generation(self);
        (infer_ctx, var_tys, res)
    }

    pub(crate) fn check(
        mut self,
        term: Idx<Term<VarId>>,
        expected: InferResult<'infer>,
    ) -> (
        InferCtx<'a, 'infer, I, Solution>,
        <Generation as InferState>::Storage<'infer>,
    ) {
        self._check(term, expected);
        let (var_tys, infer_ctx) = InferCtx::with_generation(self);
        (infer_ctx, var_tys)
    }

    fn add_effect_row_combine(
        &mut self,
        left: ScopedInferRow<'infer>,
        right: ScopedInferRow<'infer>,
        goal: ScopedInferRow<'infer>,
        span: Span,
    ) {
        self.constraints
            .add_effect_row_combine(left, right, goal, span);
        self.state
            .required_ev
            .insert(Evidence::EffRow { left, right, goal });
    }

    fn add_data_row_combine(
        &mut self,
        left: SimpleInferRow<'infer>,
        right: SimpleInferRow<'infer>,
        goal: SimpleInferRow<'infer>,
        span: Span,
    ) {
        self.constraints
            .add_data_row_combine(left, right, goal, span);
        self.state
            .required_ev
            .insert(Evidence::DataRow { left, right, goal });
    }

    /// Check a term against a given type.
    /// This method pairs with _infer to form a bidirectional type checker
    fn _check(&mut self, term: Idx<Term<VarId>>, expected: InferResult<'infer>) {
        use TypeKind::*;
        self.state.term_tys.insert(term, expected);
        let current_span = || {
            *self
                .ast
                .span_of(term)
                .expect("ICE: Term should have Span in tc after desugaring")
        };
        match (self.ast.view(term), *expected.ty) {
            (Abstraction { arg, body }, FunTy(arg_ty, body_ty)) => {
                // Check an abstraction against a function type by checking the body checks against
                // the function return type with the function argument type in scope.
                self.local_env.insert(*arg, arg_ty);
                self._check(*body, InferResult::new(body_ty, expected.eff));
                self.local_env.remove(arg);
            }
            (Label { .. }, ProdTy(Row::Closed(row))) => {
                // A label can check against a product, if it checks against the product's internal
                // type
                self._check(term, expected.with_ty(self.mk_ty(RowTy(row))))
            }
            (Label { .. }, SumTy(Row::Closed(row))) => {
                // A label can check against a sum, if it checks against the sum's internal
                // type
                self._check(term, expected.with_ty(self.mk_ty(RowTy(row))))
            }
            (Label { label, term }, RowTy(row)) => {
                // If our row is too small or too big, fail
                if row.len(self) != 1 {
                    self.errors.push(into_diag(
                        self.db,
                        (self.single_row_ty(*label, self.mk_ty(ErrorTy)), expected.ty).into(),
                        current_span(),
                    ));
                    return;
                }
                // If our singleton row is a different field name, fail
                if Some(label) != row.fields(self).first() {
                    self.errors.push(into_diag(
                        self.db,
                        (self.single_row_ty(*label, self.mk_ty(ErrorTy)), expected.ty).into(),
                        current_span(),
                    ))
                }

                // If this is a singleton row with the right label check it's value type matches
                // our term type
                self._check(
                    *term,
                    expected.with_ty(
                        *row.values(self)
                            .first()
                            .expect("ICE: singleton row with no value"),
                    ),
                )
            }
            (Unit, ProdTy(Row::Closed(closed))) if closed.is_empty(self) => {}
            (Int(_), IntTy) => {}
            (Unlabel { label, term }, _) => {
                let expected_ty = self.single_row_ty(*label, expected.ty);
                self._check(*term, expected.with_ty(expected_ty))
            }
            (Concat { .. }, RowTy(row)) => {
                // Coerece a row type into a product and re-check.
                self._check(term, expected.with_ty(self.mk_ty(ProdTy(Row::Closed(row)))));
            }
            (Concat { left, right }, ProdTy(row)) => {
                let left_infer = self._infer(*left);
                let left_row = self.equate_as_prod_row(left_infer.ty, current_span);

                let right_infer = self._infer(*right);
                let right_row = self.equate_as_prod_row(right_infer.ty, current_span);

                let span = current_span();
                // Check our expected effect is a combination of each components effects
                self.constraints
                    .add_effect_row_eq(expected.eff, left_infer.eff, current_span());
                self.constraints
                    .add_effect_row_eq(expected.eff, right_infer.eff, current_span());
                self.add_data_row_combine(left_row, right_row, row, span);
            }
            (Branch { left, right }, FunTy(Ty(handle::Handle(SumTy(arg_row))), ret)) => {
                let span = current_span();

                let left_row = self.fresh_simple_row();
                let right_row = self.fresh_simple_row();
                self.add_data_row_combine(left_row, right_row, *arg_row, span);

                self._check(
                    *left,
                    TyChkRes::new(
                        self.mk_ty(FunTy(self.mk_ty(SumTy(left_row)), ret)),
                        expected.eff,
                    ),
                );
                self._check(
                    *right,
                    TyChkRes::new(
                        self.mk_ty(FunTy(self.mk_ty(SumTy(right_row)), ret)),
                        expected.eff,
                    ),
                );
            }
            (Project { .. }, RowTy(row)) => {
                // Coerce row into a product and re-check.
                self._check(term, expected.with_ty(self.mk_ty(ProdTy(Row::Closed(row)))));
            }
            (Project { direction, term }, ProdTy(row)) => {
                let term_infer = self._infer(*term);
                let term_row = self.equate_as_prod_row(term_infer.ty, current_span);
                let unbound_row = self.fresh_simple_row();

                self.constraints
                    .add_effect_row_eq(expected.eff, term_infer.eff, current_span());
                match direction {
                    Direction::Left => {
                        self.add_data_row_combine(row, unbound_row, term_row, current_span())
                    }
                    Direction::Right => {
                        self.add_data_row_combine(unbound_row, row, term_row, current_span())
                    }
                };
            }
            (Inject { .. }, RowTy(row)) => {
                // Coerce a row into a sum and re-check.
                self._check(term, expected.with_ty(self.mk_ty(SumTy(Row::Closed(row)))));
            }
            (Inject { direction, term }, SumTy(row)) => {
                let term_infer = self._infer(*term);
                let term_row = self.equate_as_prod_row(term_infer.ty, current_span);
                let unbound_row = self.fresh_simple_row();

                self.constraints
                    .add_effect_row_eq(expected.eff, term_infer.eff, current_span());
                match direction {
                    Direction::Left => {
                        self.add_data_row_combine(term_row, unbound_row, row, current_span())
                    }
                    Direction::Right => {
                        self.add_data_row_combine(unbound_row, term_row, row, current_span())
                    }
                };
            }
            (Item(term_name), _) => {
                let scheme =
                    type_scheme_of(self.db.as_tc_db(), *term_name).ty_scheme(self.db.as_tc_db());
                let span = current_span();
                let (wrapper, ty_chk) = self.instantiate(scheme, span);
                self.state.item_wrappers.insert(term, wrapper);
                self.constraints
                    .add_effect_row_eq(ty_chk.eff, expected.eff, span);
                self.constraints.add_ty_eq(ty_chk.ty, expected.ty, span);
            }
            // Bucket case for when we need to check a rule against a type but no case applies
            (_, _) => {
                // Infer a type for our term and check that the expected type is equal to the
                // inferred type.
                let inferred = self._infer(term);
                self.constraints
                    .add_ty_eq(expected.ty, inferred.ty, current_span());
                self.constraints
                    .add_effect_row_eq(expected.eff, inferred.eff, current_span());
            }
        }
    }

    /// Create a unique unbound simple row variable
    fn fresh_simple_row(&mut self) -> SimpleInferRow<'infer> {
        let uv = self.data_row_unifiers.new_key(None);
        Row::Open(uv)
    }

    /// Create a unique unbound scoped row variable
    fn fresh_scoped_row(&mut self) -> ScopedInferRow<'infer> {
        let uv = self.eff_row_unifiers.new_key(None);
        Row::Open(uv)
    }

    /// Create a unique unbound type variable
    fn fresh_var(&mut self) -> InferTy<'infer> {
        let uv = self.ty_unifiers.new_key(None);
        self.mk_ty(VarTy(uv))
    }

    /// Infer a type for a term
    /// This method pairs with check to form a bidirectional type checker
    fn _infer(&mut self, term: Idx<Term<VarId>>) -> InferResult<'infer> {
        let current_span = || {
            *self.ast.span_of(term).unwrap_or_else(|| {
                panic!(
                    "ICE: Term should have span in tc after desugaring. {:?}",
                    self.ast.view(term)
                )
            })
        };
        let res = match self.ast.view(term) {
            // Abstraction inference  is done by creating two new unifiers <arg> and <body>
            // The abstraction body is checked against these fresh type variables
            // The resulting type of the inference is function type <arg> -> <body>
            Abstraction { arg, body } => {
                let arg_ty = self.fresh_var();

                self.state.var_tys.insert(*arg, arg_ty);
                self.local_env.insert(*arg, arg_ty);
                let body_out = self._infer(*body);
                self.local_env.remove(arg);

                InferResult::new(
                    self.mk_ty(TypeKind::FunTy(arg_ty, body_out.ty)),
                    body_out.eff,
                )
            }
            // Application inference starts by inferring types for the func of the application.
            // We equate this inferred type to a function type, generating fresh unifiers if
            // needed.
            // We then check the arg of the application against the arg type of the function type
            // The resulting type of this application is the function result type.
            Application { func, arg } => {
                let fun_infer = self._infer(*func);
                // Optimization: eagerly use FunTy if available. Otherwise dispatch fresh unifiers
                // for arg and ret type.
                let (arg_ty, ret_ty) = self.equate_as_fn_ty(fun_infer.ty, current_span);

                self._check(*arg, InferResult::new(arg_ty, fun_infer.eff));

                InferResult::new(ret_ty, fun_infer.eff)
            }
            // If the variable is in environemnt return it's type, otherwise return an error.
            Variable(var) => {
                if let Some(ty) = self.local_env.get(var).cloned() {
                    self.state.var_tys.insert(*var, ty);
                    InferResult::new(ty, self.fresh_scoped_row())
                } else {
                    self.errors.push(into_diag(
                        self.db,
                        TypeCheckError::VarNotDefined(*var),
                        current_span(),
                    ));
                    let err_ty = self.mk_ty(ErrorTy);
                    self.state.var_tys.insert(*var, err_ty);
                    InferResult::new(err_ty, self.fresh_scoped_row())
                }
            }
            Label { label, term } => {
                let infer = self._infer(*term);
                infer.map_ty(|ty| self.single_row_ty(*label, ty))
            }
            Unlabel { label, term } => {
                let term_infer = self._infer(*term);
                let field = *label;
                term_infer.map_ty(|ty| match *ty {
                    // If our output type is already a singleton row of without a label, use it
                    // directly. This avoids introducing needless unifiers
                    RowTy(row)
                        if row.len(self.ctx) == 1 && row.fields(self).first() == Some(&field) =>
                    {
                        *row.values(self)
                            .first()
                            .expect("ICE: singleton row has field but no value")
                    }
                    // Othewise introduce a unifier, and rely on unification for any needed error
                    // reporting
                    _ => {
                        let out_ty = self.fresh_var();
                        let row_ty = self.single_row_ty(field, out_ty);
                        self.constraints.add_ty_eq(row_ty, ty, current_span());
                        out_ty
                    }
                })
            }
            Unit => InferResult::new(
                self.mk_ty(ProdTy(Row::Closed(self.empty_row()))),
                self.fresh_scoped_row(),
            ),
            Int(_) => InferResult::new(self.mk_ty(IntTy), self.fresh_scoped_row()),
            Concat { left, right } => {
                let left_row = self.fresh_simple_row();
                let right_row = self.fresh_simple_row();
                let out_row = self.fresh_simple_row();
                let eff = self.fresh_scoped_row();

                let left_ty = self.mk_ty(ProdTy(left_row));
                let right_ty = self.mk_ty(ProdTy(right_row));

                self._check(*left, InferResult::new(left_ty, eff));
                self._check(*right, InferResult::new(right_ty, eff));

                let span = current_span();
                self.add_data_row_combine(left_row, right_row, out_row, span);

                InferResult::new(self.mk_ty(ProdTy(out_row)), eff)
            }
            Branch { left, right } => {
                let left_row = self.fresh_simple_row();
                let right_row = self.fresh_simple_row();
                let out_row = self.fresh_simple_row();

                let out_eff = self.fresh_scoped_row();

                let ret_ty = self.fresh_var();

                let left_ty = self.mk_ty(FunTy(self.mk_ty(SumTy(left_row)), ret_ty));
                let right_ty = self.mk_ty(FunTy(self.mk_ty(SumTy(right_row)), ret_ty));

                self._check(*left, InferResult::new(left_ty, out_eff));
                self._check(*right, InferResult::new(right_ty, out_eff));

                let span = current_span();
                self.add_data_row_combine(left_row, right_row, out_row, span);

                InferResult::new(
                    self.mk_ty(FunTy(self.mk_ty(SumTy(out_row)), ret_ty)),
                    out_eff,
                )
            }
            Project { direction, term } => {
                let big_row = self.fresh_simple_row();
                let small_row = self.fresh_simple_row();
                // In a projection one of the row variables will be unbound
                let unbound_row = self.fresh_simple_row();

                let eff = self.fresh_scoped_row();
                let term_ty = self.mk_ty(ProdTy(big_row));
                self._check(*term, InferResult::new(term_ty, eff));

                match direction {
                    Direction::Left => {
                        self.add_data_row_combine(small_row, unbound_row, big_row, current_span())
                    }
                    Direction::Right => {
                        self.add_data_row_combine(unbound_row, small_row, big_row, current_span())
                    }
                };

                InferResult::new(self.mk_ty(ProdTy(small_row)), eff)
            }
            Inject { direction, term } => {
                let big_row = self.fresh_simple_row();
                let small_row = self.fresh_simple_row();

                let unbound_row = self.fresh_simple_row();
                let eff = self.fresh_scoped_row();
                let term_ty = self.mk_ty(SumTy(small_row));
                self._check(*term, InferResult::new(term_ty, eff));

                match direction {
                    Direction::Left => {
                        self.add_data_row_combine(small_row, unbound_row, big_row, current_span())
                    }
                    Direction::Right => {
                        self.add_data_row_combine(unbound_row, small_row, big_row, current_span())
                    }
                };

                InferResult::new(self.mk_ty(SumTy(big_row)), eff)
            }
            Operation(op_name) => {
                let (wrapper, sig) =
                    self.instantiate(self.db.effect_member_sig(*op_name), current_span());

                self.state.item_wrappers.insert(term, wrapper);

                let unused = self.fresh_scoped_row();
                let goal = self.fresh_scoped_row();

                let ret_ty = self.fresh_var();
                let eff = Row::Closed(
                    self.single_row(
                        op_name
                            .effect(self.db.as_core_db())
                            .name(self.db.as_core_db()),
                        ret_ty,
                    ),
                );

                // Mark our op function with our effect type
                // This is our version of koka's `open` rule.
                let mut ty = sig.ty;
                if let FunTy(arg, ret) = self.kind(&ty) {
                    ty = self.mk_ty(FunTy(*arg, *ret))
                }

                self.add_effect_row_combine(unused, eff, goal, current_span());

                InferResult::new(ty, goal)
            }
            Term::Handle { handler, body } => {
                let ret_ty = self.fresh_var();
                let out_eff = self.fresh_scoped_row();

                let TyChkRes {
                    ty: body_ty,
                    eff: body_eff,
                } = self._infer(*body);

                // Ensure there is a return clause in the handler
                let ret_row = self.mk_row(
                    &[self.mk_label("return")],
                    &[self.mk_ty(FunTy(body_ty, ret_ty))],
                );
                let eff_sig_row = self.fresh_simple_row();
                let handler_row = self.fresh_simple_row();

                // Our handler should be an effect signature plus a return clause
                self.add_data_row_combine(
                    Row::Closed(ret_row),
                    eff_sig_row,
                    handler_row,
                    current_span(),
                );

                self._check(
                    *handler,
                    InferResult::new(self.mk_ty(ProdTy(handler_row)), out_eff),
                );

                let handled_eff = self.fresh_scoped_row();
                // Mark handler with the effect that it handles
                self.state.term_tys.insert(
                    *handler,
                    InferResult::new(self.mk_ty(ProdTy(handler_row)), handled_eff),
                );
                // We just want to match the signature against our effects, we don't need to
                // include the return clause.
                self.constraints
                    .add_handles(eff_sig_row, handled_eff, ret_ty, current_span());

                // Handle removes an effect so our out_eff should be whatever body_eff was minus
                // our handled effect
                self.add_effect_row_combine(out_eff, handled_eff, body_eff, current_span());

                InferResult::new(ret_ty, out_eff)
            }
            Annotated { ty, term } => {
                let mut inst = Instantiate::new(self.db, self.ctx); // TODO: We should possibly extract an effect from this
                let infer_ty = ty.try_fold_with(&mut inst).unwrap();
                let res = InferResult::new(infer_ty, self.fresh_scoped_row());
                self._check(*term, res);
                res
            }
            Item(term_name) => {
                let scheme =
                    type_scheme_of(self.db.as_tc_db(), *term_name).ty_scheme(self.db.as_tc_db());
                let (wrapper, res) = self.instantiate(scheme, current_span());
                self.state.item_wrappers.insert(term, wrapper);
                res
            }
        };
        self.state.term_tys.insert(term, res);
        res
    }

    fn instantiate(
        &mut self,
        ty_scheme: TyScheme,
        span: Span,
    ) -> (Wrapper<InArena<'infer>>, InferResult<'infer>) {
        let ty_unifiers = ty_scheme
            .bound_ty
            .into_iter()
            .map(|key| (key, self.ty_unifiers.new_key(None)))
            .collect();
        let datarow_unifiers = ty_scheme
            .bound_data_row
            .into_iter()
            .map(|key| (key, self.data_row_unifiers.new_key(None)))
            .collect();
        let effrow_unifiers = ty_scheme
            .bound_eff_row
            .into_iter()
            .map(|key| (key, self.eff_row_unifiers.new_key(None)))
            .collect();
        let mut inst = Instantiate {
            db: self.db,
            ctx: self.ctx,
            ty_unifiers,
            datarow_unifiers,
            effrow_unifiers,
        };
        let constrs = ty_scheme
            .constrs
            .into_iter()
            .map(|ev| {
                let ev = ev.try_fold_with(&mut inst).unwrap();
                match &ev {
                    Evidence::DataRow { left, right, goal } => {
                        self.add_data_row_combine(*left, *right, *goal, span)
                    }
                    Evidence::EffRow { left, right, goal } => {
                        self.add_effect_row_combine(*left, *right, *goal, span)
                    }
                }
                ev
            })
            .collect::<Vec<_>>();
        let res = InferResult::new(
            ty_scheme.ty.try_fold_with(&mut inst).unwrap(),
            ty_scheme.eff.try_fold_with(&mut inst).unwrap(),
        );

        (
            Wrapper {
                tys: inst
                    .ty_unifiers
                    .into_iter()
                    .map(|(_, var)| self.mk_ty(VarTy(var)))
                    .collect::<Vec<_>>(),
                data_rows: inst
                    .datarow_unifiers
                    .into_iter()
                    .map(|(_, var)| var)
                    .map(Row::Open)
                    .collect::<Vec<_>>(),
                eff_rows: inst
                    .effrow_unifiers
                    .into_iter()
                    .map(|(_, var)| var)
                    .map(Row::Open)
                    .collect::<Vec<_>>(),
                constrs,
            },
            res,
        )
    }

    pub(crate) fn equate_as_fn_ty(
        &mut self,
        ty: InferTy<'infer>,
        span: impl FnOnce() -> Span,
    ) -> (InferTy<'infer>, InferTy<'infer>) {
        ty.try_as_fn_ty().unwrap_or_else(|ty| {
            let arg = self.fresh_var();
            let ret = self.fresh_var();
            self.constraints
                .add_ty_eq(ty, self.mk_ty(FunTy(arg, ret)), span());
            (arg, ret)
        })
    }

    /// Make this type equal to a row and return that equivalent row.
    /// If it is already a row convert it directly, otherwise add a constraint that type must be a
    /// row.
    pub(crate) fn equate_as_prod_row(
        &mut self,
        ty: InferTy<'infer>,
        span: impl FnOnce() -> Span,
    ) -> SimpleInferRow<'infer> {
        match ty.deref() {
            TypeKind::ProdTy(Row::Closed(row)) | TypeKind::RowTy(row) => Row::Closed(*row),
            TypeKind::ProdTy(Row::Open(var)) => Row::Open(*var),
            _ => {
                let unifier = self.data_row_unifiers.new_key(None);
                self.constraints
                    .add_ty_eq(self.mk_ty(ProdTy(Row::Open(unifier))), ty, span());
                Row::Open(unifier)
            }
        }
    }
}

impl<'a, 'infer, I> InferCtx<'a, 'infer, I, Solution>
where
    I: MkTy<InArena<'infer>> + AccessTy<'infer, InArena<'infer>>,
{
    /// Convert our previous InferCtx in Generation state into Solution state.
    fn with_generation(
        prior: InferCtx<'a, 'infer, I, Generation>,
    ) -> (<Generation as InferState>::Storage<'infer>, Self) {
        (
            prior.state,
            Self {
                local_env: prior.local_env,
                ty_unifiers: prior.ty_unifiers,
                data_row_unifiers: prior.data_row_unifiers,
                eff_row_unifiers: prior.eff_row_unifiers,
                errors: prior.errors,
                constraints: prior.constraints,
                ctx: prior.ctx,
                db: prior.db,
                module: prior.module,
                ast: prior.ast,
                state: SolutionStorage::default(),
                _marker: std::marker::PhantomData,
            },
        )
    }

    /// Solve a list of constraints to a mapping from unifiers to types.
    /// If there is no solution to the list of constraints we return a relevant error.
    pub(crate) fn solve(
        mut self,
    ) -> (
        InPlaceUnificationTable<TcUnifierVar<'infer, TypeK>>,
        InPlaceUnificationTable<TcUnifierVar<'infer, SimpleRowK>>,
        InPlaceUnificationTable<TcUnifierVar<'infer, ScopedRowK>>,
        <Solution as InferState>::Storage<'infer>,
        Vec<TypeCheckDiagnostic>,
    ) {
        let constraints = std::mem::take(&mut self.constraints);
        for (left, right, span) in constraints.tys {
            self.unify(left, right)
                .map_err(|err| self.errors.push(into_diag(self.db, err, span)))
                .unwrap_or_default();
        }
        for (RowCombination { left, right, goal }, span) in constraints.data_rows {
            self.unify_row_combine(left, right, goal)
                .map_err(|err| self.errors.push(into_diag(self.db, err, span)))
                .unwrap_or_default();
        }
        for (left, right, span) in constraints.effect_eq {
            self.unify(left, right)
                .map_err(|err| self.errors.push(into_diag(self.db, err, span)))
                .unwrap_or_default();
        }
        for (RowCombination { left, right, goal }, span) in constraints.effect_rows {
            self.unify_row_combine(left, right, goal)
                .map_err(|err| self.errors.push(into_diag(self.db, err, span)))
                .unwrap_or_default();
        }

        // Order here is important.
        // We want to handle these constraints only after all our row constraints are solved.
        // That way if a handler doesn't have a concrete row type yet, we know it's because it
        // won't have one ever and we can fail with a type error.
        for (HandlesConstraint { handler, eff, ret }, span) in constraints.handles {
            self.lookup_effect_and_unify(handler, eff, ret)
                .map_err(|err| self.errors.push(into_diag(self.db, err, span)))
                .unwrap_or_default();
        }

        (
            self.ty_unifiers,
            self.data_row_unifiers,
            self.eff_row_unifiers,
            self.state,
            self.errors,
        )
    }

    /// Apply the current partial substitution to a type, removing as many unifiers as possible
    /// before unification.
    fn normalize_ty(&mut self, ty: InferTy<'infer>) -> InferTy<'infer> {
        match *ty {
            VarTy(var) => self
                .ty_unifiers
                .probe_value(var)
                .map(|val| {
                    val.try_fold_with(&mut Normalize {
                        ctx: self.ctx,
                        ty_unifiers: &mut self.ty_unifiers,
                        datarow_unifiers: &mut self.data_row_unifiers,
                        effrow_unifiers: &mut self.eff_row_unifiers,
                    })
                    .unwrap()
                })
                .unwrap_or_else(|| ty),
            _ => ty
                .try_fold_with(&mut Normalize {
                    ctx: self.ctx,
                    ty_unifiers: &mut self.ty_unifiers,
                    datarow_unifiers: &mut self.data_row_unifiers,
                    effrow_unifiers: &mut self.eff_row_unifiers,
                })
                .unwrap(),
        }
    }

    fn dispatch_solved<Sema: RowTheory>(
        &mut self,
        var: Sema::Open<InArena<'infer>>,
        row: Sema::Closed<InArena<'infer>>,
    ) -> Result<(), TypeCheckError<'infer>>
    where
        Self: RowEquationSolver<'infer, Sema>,
        Sema: Ord + Clone,
        Sema::Closed<InArena<'infer>>: Copy + std::fmt::Debug,
    {
        let mut combos: Vec<RowCombination<Row<Sema, InArena<'infer>>>> = vec![];
        self.with_equations(|eqns| {
            eqns.into_iter()
                .filter_map(|eqn| match eqn {
                    UnsolvedRowEquation::ClosedGoal(cand) if cand.left == var => {
                        combos.push(RowCombination {
                            left: Row::Closed(row),
                            right: Row::Open(cand.right),
                            goal: Row::Closed(cand.goal),
                        });
                        None
                    }
                    UnsolvedRowEquation::ClosedGoal(cand) if cand.right == var => {
                        combos.push(RowCombination {
                            left: Row::Open(cand.left),
                            right: Row::Closed(row),
                            goal: Row::Closed(cand.goal),
                        });
                        None
                    }
                    UnsolvedRowEquation::OpenGoal(cand) if cand.goal == var => match cand.ops {
                        Operatives::OpenOpen { left, right } => {
                            Some(UnsolvedRowEquation::ClosedGoal(ClosedGoal {
                                goal: row,
                                left,
                                right,
                            }))
                        }
                        Operatives::OpenClosed { left, right } => {
                            combos.push(RowCombination {
                                left: Row::Open(left),
                                right: Row::Closed(right),
                                goal: Row::Closed(row),
                            });
                            None
                        }
                        Operatives::ClosedOpen { left, right } => {
                            combos.push(RowCombination {
                                left: Row::Closed(left),
                                right: Row::Open(right),
                                goal: Row::Closed(row),
                            });
                            None
                        }
                    },
                    UnsolvedRowEquation::OpenGoal(OpenGoal {
                        goal,
                        ops: Operatives::OpenOpen { left, right },
                    }) if left == var => {
                        combos.push(RowCombination {
                            left: Row::Closed(row),
                            right: Row::Open(right),
                            goal: Row::Open(goal),
                        });
                        None
                    }
                    UnsolvedRowEquation::OpenGoal(OpenGoal {
                        goal,
                        ops: Operatives::OpenOpen { left, right },
                    }) if right == var => {
                        combos.push(RowCombination {
                            left: Row::Open(left),
                            right: Row::Closed(row),
                            goal: Row::Open(goal),
                        });
                        None
                    }
                    UnsolvedRowEquation::OpenGoal(OpenGoal {
                        goal,
                        ops: Operatives::ClosedOpen { left, right },
                    }) if right == var => {
                        combos.push(RowCombination {
                            left: Row::Closed(left),
                            right: Row::Closed(row),
                            goal: Row::Open(goal),
                        });
                        None
                    }
                    UnsolvedRowEquation::OpenGoal(OpenGoal {
                        goal,
                        ops: Operatives::OpenClosed { left, right },
                    }) if left == var => {
                        combos.push(RowCombination {
                            left: Row::Closed(row),
                            right: Row::Closed(right),
                            goal: Row::Open(goal),
                        });
                        None
                    }
                    eqn => Some(eqn),
                })
                .collect()
        });

        for RowCombination { left, right, goal } in combos {
            self.unify_row_combine(left, right, goal)?;
        }
        Ok(())
    }

    fn unify_row_combine<Sema>(
        &mut self,
        unnorm_left: Row<Sema, InArena<'infer>>,
        unnorm_right: Row<Sema, InArena<'infer>>,
        unnorm_goal: Row<Sema, InArena<'infer>>,
    ) -> Result<(), TypeCheckError<'infer>>
    where
        Self: RowEquationSolver<'infer, Sema>,
        Sema: RowTheory + Ord + Clone,
        Sema::Closed<InArena<'infer>>: std::fmt::Debug,
    {
        let left = self.normalize_row(unnorm_left);
        let right = self.normalize_row(unnorm_right);
        let goal = self.normalize_row(unnorm_goal);

        match (left, right, goal) {
            (Row::Closed(left), Row::Closed(right), goal) => {
                let (fields, values) =
                    Sema::combine(&left, &right).map_err(|err| err.into_tychk_err(self.ctx))?;
                let row: Sema::Closed<InArena<'infer>> = self.mk_row(&fields, &values);
                self.unify(goal, row)?;
            }
            (Row::Closed(left), Row::Open(right), Row::Closed(goal)) => {
                let (fields, values) = Sema::diff_left(&goal, &left);
                let goal_right: Sema::Closed<InArena<'infer>> = self.mk_row(&fields, &values);
                let (fields, values) = Sema::diff_right(&goal, &goal_right);
                self.unify(right, goal_right)?;

                // We still have to unify this so our left row types and goal types are unified
                let goal_left: Sema::Closed<InArena<'infer>> = self.mk_row(&fields, &values);
                self.unify(left, goal_left)?;
            }
            (Row::Open(left), Row::Closed(right), Row::Closed(goal)) => {
                let (fields, values) = Sema::diff_right(&goal, &right);
                let goal_left: Sema::Closed<InArena<'infer>> = self.mk_row(&fields, &values);
                let (fields, values) = Sema::diff_left(&goal, &goal_left);
                self.unify(left, goal_left)?;

                // We still have to unify this so our right row types and goal types are unified
                let goal_right: Sema::Closed<InArena<'infer>> = self.mk_row(&fields, &values);
                self.unify(right, goal_right)?;
            }
            (left, right, goal) => {
                let unifiable_equation = self.equations().iter().find_map(|eqn| {
                    Sema::match_eqn(
                        eqn,
                        RowCombination {
                            left: left.clone(),
                            right: right.clone(),
                            goal: goal.clone(),
                        },
                    )
                });
                match unifiable_equation {
                    Some(row_combo) => {
                        self.unify(left, row_combo.left)?;
                        self.unify(right, row_combo.right)?;
                        self.unify(goal, row_combo.goal)?;
                    }
                    None => {
                        self.equations_mut().insert(match (left, right, goal) {
                            (Row::Open(left), Row::Open(right), Row::Open(goal)) => {
                                UnsolvedRowEquation::OpenGoal(OpenGoal {
                                    goal,
                                    ops: Operatives::OpenOpen { left, right },
                                })
                            }
                            (Row::Open(left), Row::Closed(right), Row::Open(goal)) => {
                                UnsolvedRowEquation::OpenGoal(OpenGoal {
                                    goal,
                                    ops: Operatives::OpenClosed { left, right },
                                })
                            }
                            (Row::Closed(left), Row::Open(right), Row::Open(goal)) => {
                                UnsolvedRowEquation::OpenGoal(OpenGoal {
                                    goal,
                                    ops: Operatives::ClosedOpen { left, right },
                                })
                            }
                            (Row::Open(left), Row::Open(right), Row::Closed(goal)) => {
                                UnsolvedRowEquation::ClosedGoal(ClosedGoal { goal, left, right })
                            }
                            (Row::Open(_), Row::Closed(_), Row::Closed(_))
                            | (Row::Closed(_), Row::Closed(_), Row::Open(_))
                            | (Row::Closed(_), Row::Open(_), Row::Closed(_))
                            | (Row::Closed(_), Row::Closed(_), Row::Closed(_)) => unreachable!(),
                        });
                    }
                }
            }
        }
        Ok(())
    }

    fn lookup_effect_and_unify(
        &mut self,
        handler: SimpleInferRow<'infer>,
        eff: ScopedInferRow<'infer>,
        ret: InferTy<'infer>,
    ) -> Result<(), TypeCheckError<'infer>> {
        let normal_handler = self.normalize_row(handler);
        let normal_eff = self.normalize_row(eff);
        let normal_ret = self.normalize_ty(ret);

        let transform_to_cps_handler_ty = |ctx: &mut Self, ty: InferTy<'infer>| {
            // Transform our ty into the type a handler should have
            // This means it should take a resume parameter that is a function returning `ret` and return `ret` itself.
            match *ty {
                FunTy(a, b) => {
                    let kont_ty = ctx.mk_ty(FunTy(b, normal_ret));
                    Ok(ctx.mk_ty(FunTy(a, ctx.mk_ty(FunTy(kont_ty, normal_ret)))))
                }
                _ => {
                    // TODO: report a better error here.
                    // We should specialize the error so it's clear it was an
                    // effect signature with an invalid type that caused it
                    Err((ty, ctx.mk_ty(FunTy(ctx.mk_ty(ErrorTy), ctx.mk_ty(ErrorTy)))))
                }
            }
        };

        let unify_sig_handler =
            |ctx: &mut Self,
             members_sig: Vec<(Ident, TyScheme)>,
             sig: SimpleClosedRow<InArena<'infer>>| {
                let sig_unify = members_sig.into_iter().zip(sig.iter());
                for ((member_name, scheme), (field_name, ty)) in sig_unify {
                    // Sanity check that our handler fields and effect members line up the way we
                    // expect them to.
                    debug_assert_eq!(&member_name, field_name);

                    let mut inst = Instantiate {
                        db: ctx.db,
                        ctx: ctx.ctx,
                        ty_unifiers: scheme
                            .bound_ty
                            .into_iter()
                            .map(|key| (key, ctx.ty_unifiers.new_key(None)))
                            .collect(),
                        datarow_unifiers: scheme
                            .bound_data_row
                            .into_iter()
                            .map(|key| (key, ctx.data_row_unifiers.new_key(None)))
                            .collect(),
                        effrow_unifiers: scheme
                            .bound_eff_row
                            .into_iter()
                            .map(|key| (key, ctx.eff_row_unifiers.new_key(None)))
                            .collect(),
                    };

                    // Transform our scheme ty into the type a handler should have
                    // This means it should take a resume parameter that is a function returning `ret` and return `ret` itself.
                    let member_ty = transform_to_cps_handler_ty(
                        ctx,
                        scheme.ty.try_fold_with(&mut inst).unwrap(),
                    )?;

                    // Unify our instantiated and transformed member type agaisnt the handler field
                    // type.
                    ctx.unify(member_ty, *ty)?;

                    // We want to check constrs after we unify our scheme type so that we've alread
                    // unified as many fresh variables into handler field variables as possible.
                    for constrs in scheme.constrs {
                        match constrs.try_fold_with(&mut inst).unwrap() {
                            Evidence::DataRow { left, right, goal } => {
                                ctx.unify_row_combine(left, right, goal)?
                            }
                            Evidence::EffRow { left, right, goal } => {
                                ctx.unify_row_combine(left, right, goal)?
                            }
                        }
                    }
                    // TODO: We should check scheme.eff here as well, at least to confirm they are
                    // all the same
                }
                Ok(())
            };

        match (normal_handler, normal_eff) {
            (Row::Closed(handler), Row::Open(eff_var)) => {
                let eff_name = self
                    .db
                    .lookup_effect_by_member_names(self.module, handler.fields(self))
                    .ok_or(TypeCheckError::UndefinedEffectSignature(handler))?;
                let mut members_sig = self
                    .db
                    .effect_members(eff_name)
                    .iter()
                    .map(|eff_op_id| {
                        (
                            self.db.effect_member_name(*eff_op_id),
                            self.db.effect_member_sig(*eff_op_id),
                        )
                    })
                    .collect::<Vec<_>>();

                // Sort our members so they are in the same order as sig.fields
                // TODO: Should we have an invariant that effect_members returns members "in
                // order"?
                members_sig.sort_by(|(a, _), (b, _)| a.cmp(b));

                unify_sig_handler(self, members_sig, handler)?;

                // We succesfully unified the handler against it's expected signature.
                // That means we can unify our eff_var against our effect
                let name = self.db.effect_name(eff_name);
                let eff_row: ScopedClosedRow<InArena<'infer>> = self.mk_row(&[name], &[normal_ret]);
                self.unify(eff_var, eff_row)
            }
            (Row::Closed(handler), Row::Closed(eff)) => {
                debug_assert!(eff.len(self) == 1);
                let eff_ident = *eff.fields(self).first().unwrap();
                let eff_ret_ty = *eff.values(self).first().unwrap();
                self.unify(normal_ret, eff_ret_ty)?;

                let eff_name = self
                    .db
                    .lookup_effect_by_name(self.module, eff_ident)
                    .ok_or(TypeCheckError::UndefinedEffect(eff_ident))?;
                let mut members_sig = self
                    .db
                    .effect_members(eff_name)
                    .iter()
                    .map(|eff_op_id| {
                        (
                            self.db.effect_member_name(*eff_op_id),
                            self.db.effect_member_sig(*eff_op_id),
                        )
                    })
                    .collect::<Vec<_>>();

                // Sort our members so they are in the same order as sig.fields
                // TODO: Should we have an invariant that effect_members returns members "in
                // order"?
                members_sig.sort_by_key(|(id, _)| *id);

                unify_sig_handler(self, members_sig, handler)
            }
            (Row::Open(handler_var), Row::Closed(eff)) => {
                debug_assert!(eff.len(self) == 1);
                let eff_ident = *eff.fields(self).first().unwrap();
                let eff_ret_ty = *eff.values(self).first().unwrap();
                self.unify(normal_ret, eff_ret_ty)?;
                let eff_name = self
                    .db
                    .lookup_effect_by_name(self.module, eff_ident)
                    .ok_or(TypeCheckError::UndefinedEffect(eff_ident))?;
                let members_sig = self
                    .db
                    .effect_members(eff_name)
                    .iter()
                    .map(|eff_op_id| {
                        let scheme = self.db.effect_member_sig(*eff_op_id);
                        let mut inst = Instantiate {
                            db: self.db,
                            ctx: self.ctx,
                            ty_unifiers: scheme
                                .bound_ty
                                .into_iter()
                                .map(|key| (key, self.ty_unifiers.new_key(None)))
                                .collect(),
                            datarow_unifiers: Default::default(),
                            effrow_unifiers: Default::default(),
                        };

                        for constr in scheme.constrs {
                            match constr.try_fold_with(&mut inst).unwrap() {
                                Evidence::DataRow { left, right, goal } => {
                                    self.unify_row_combine(left, right, goal)?
                                }
                                Evidence::EffRow { left, right, goal } => {
                                    self.unify_row_combine(left, right, goal)?
                                }
                            }
                        }

                        let member_ty = transform_to_cps_handler_ty(
                            self,
                            scheme.ty.try_fold_with(&mut inst).unwrap(),
                        )?;

                        Ok((self.db.effect_member_name(*eff_op_id), member_ty))
                    })
                    .collect::<Result<Vec<_>, TypeCheckError<'infer>>>()?;

                let member_row = self.construct_simple_row(members_sig);
                self.unify(handler_var, member_row)
            }
            // We didn't learn enough info to solve our handle term, this is an error
            (Row::Open(handler_var), Row::Open(eff_var)) => Err(TypeCheckError::UnsolvedHandle {
                handler: handler_var,
                eff: eff_var,
            }),
        }
    }
}
