use aiahr_ast::{Ast, Direction, Term, Term::*};
use aiahr_core::{
    diagnostic::tc::TypeCheckDiagnostic, id::VarId, ident::Ident, modules::Module, span::Span,
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
use rustc_hash::FxHashMap;
use salsa::DebugWithDb;
use std::{collections::BTreeSet, ops::Deref};

use crate::{
    diagnostic::{into_diag, TypeCheckError},
    folds::{instantiate::Instantiate, normalize::Normalize, occurs_check::OccursCheck},
    type_scheme_of,
    unsolved_row::{
        ClosedGoal, OpenGoal, Operatives, OrderedRowXorRow, ScopedOpenGoal, ScopedRowEquation,
        UnsolvedRowEquation,
    },
    Db, EffectInfo, Evidence, TyScheme,
};

struct RowCombination<Row> {
    left: Row,
    right: Row,
    goal: Row,
}
type SimpleRowCombination<'infer> = RowCombination<SimpleRow<InArena<'infer>>>;
type ScopedRowCombination<'infer> = RowCombination<ScopedRow<InArena<'infer>>>;

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
    /// Sets of data row equalities that must hold
    data_eq: Vec<(SimpleInferRow<'infer>, SimpleInferRow<'infer>, Span)>,
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

    fn add_data_row_eq(
        &mut self,
        left: SimpleInferRow<'infer>,
        right: SimpleInferRow<'infer>,
        span: Span,
    ) {
        self.data_eq.push((left, right, span))
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
}
impl InferState for Generation {
    type Storage<'infer> = GenerationStorage<'infer>;
}

#[derive(Default)]
pub(crate) struct SolutionStorage<'infer> {
    pub(crate) data_eqns: BTreeSet<UnsolvedRowEquation<InArena<'infer>>>,
    pub(crate) eff_eqns: BTreeSet<ScopedRowEquation<InArena<'infer>>>,
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
            },
            _marker: std::marker::PhantomData,
        }
    }
    /// This is the entrypoint to the bidirectional type checker. Since our language uses
    /// damnas-milner type inference we will always begin type checking with a call to infer.
    pub(crate) fn infer<E>(
        mut self,
        eff_info: &E,
        term: Idx<Term<VarId>>,
    ) -> (
        InferCtx<'a, 'infer, I, Solution>,
        <Generation as InferState>::Storage<'infer>,
        InferResult<'infer>,
    )
    where
        E: ?Sized + EffectInfo,
    {
        let res = self._infer(eff_info, term);
        let (var_tys, infer_ctx) = InferCtx::with_generation(self);
        (infer_ctx, var_tys, res)
    }

    /// Check a term against a given type.
    /// This method pairs with _infer to form a bidirectional type checker
    fn _check<E>(
        &mut self,
        //var_tys: &mut FxHashMap<VarId, InferTy<'infer>>,
        eff_info: &E,
        term: Idx<Term<VarId>>,
        expected: InferResult<'infer>,
    ) where
        E: ?Sized + EffectInfo,
    {
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
                self._check(eff_info, *body, expected.with_ty(body_ty));
                self.local_env.remove(arg);
            }
            (Label { .. }, ProdTy(Row::Closed(row))) => {
                // A label can check against a product, if it checks against the product's internal
                // type
                self._check(eff_info, term, expected.with_ty(self.mk_ty(RowTy(row))))
            }
            (Label { .. }, SumTy(Row::Closed(row))) => {
                // A label can check against a sum, if it checks against the sum's internal
                // type
                self._check(eff_info, term, expected.with_ty(self.mk_ty(RowTy(row))))
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
                    eff_info,
                    *term,
                    expected.with_ty(
                        *row.values(self)
                            .first()
                            .expect("ICE: singleton row with no value"),
                    ),
                )
            }
            (Unit, ProdTy(Row::Closed(closed))) if closed.is_empty(self) => {
                self.constraints.add_effect_row_eq(
                    expected.eff,
                    Row::Closed(self.empty_row()),
                    current_span(),
                );
            }
            (Int(_), IntTy) => self.constraints.add_effect_row_eq(
                expected.eff,
                Row::Closed(self.empty_row()),
                current_span(),
            ),
            (Unlabel { label, term }, _) => {
                let expected_ty = self.single_row_ty(*label, expected.ty);
                self._check(eff_info, *term, expected.with_ty(expected_ty))
            }
            (Concat { .. }, RowTy(row)) => {
                // Coerece a row type into a product and re-check.
                self._check(
                    eff_info,
                    term,
                    expected.with_ty(self.mk_ty(ProdTy(Row::Closed(row)))),
                );
            }
            (Concat { left, right }, ProdTy(row)) => {
                let left_infer = self._infer(eff_info, *left);
                let left_row = self.equate_as_prod_row(left_infer.ty, current_span);

                let right_infer = self._infer(eff_info, *right);
                let right_row = self.equate_as_prod_row(right_infer.ty, current_span);

                let span = current_span();
                // Check our expected effect is a combination of each components effects
                self.constraints.add_effect_row_combine(
                    left_infer.eff,
                    right_infer.eff,
                    expected.eff,
                    span,
                );
                self.constraints
                    .add_data_row_combine(left_row, right_row, row, span);
            }
            (Branch { left, right }, FunTy(arg, ret)) => {
                let arg_row = self.equate_as_sum_row(arg, current_span);

                let left_infer = self._infer(eff_info, *left);
                let (left_arg, left_ret) = self.equate_as_fn_ty(left_infer.ty, current_span);
                let left_row = self.equate_as_sum_row(left_arg, current_span);

                let right_infer = self._infer(eff_info, *right);
                let (right_arg, right_ret) = self.equate_as_fn_ty(right_infer.ty, current_span);
                let right_row = self.equate_as_sum_row(right_arg, current_span);

                let span = current_span();
                self.constraints.add_effect_row_combine(
                    left_infer.eff,
                    right_infer.eff,
                    expected.eff,
                    span,
                );
                self.constraints
                    .add_data_row_combine(left_row, right_row, arg_row, span);
                // All branch return types must be equal
                self.constraints.add_ty_eq(left_ret, ret, current_span());
                self.constraints.add_ty_eq(right_ret, ret, current_span());
            }
            (Project { .. }, RowTy(row)) => {
                // Coerce row into a product and re-check.
                self._check(
                    eff_info,
                    term,
                    expected.with_ty(self.mk_ty(ProdTy(Row::Closed(row)))),
                );
            }
            (Project { direction, term }, ProdTy(row)) => {
                let term_infer = self._infer(eff_info, *term);
                let term_row = self.equate_as_prod_row(term_infer.ty, current_span);
                let unbound_row = self.fresh_simple_row();

                self.constraints
                    .add_effect_row_eq(expected.eff, term_infer.eff, current_span());
                match direction {
                    Direction::Left => self.constraints.add_data_row_combine(
                        row,
                        unbound_row,
                        term_row,
                        current_span(),
                    ),
                    Direction::Right => self.constraints.add_data_row_combine(
                        unbound_row,
                        row,
                        term_row,
                        current_span(),
                    ),
                };
            }
            (Inject { .. }, RowTy(row)) => {
                // Coerce a row into a sum and re-check.
                self._check(
                    eff_info,
                    term,
                    expected.with_ty(self.mk_ty(SumTy(Row::Closed(row)))),
                );
            }
            (Inject { direction, term }, SumTy(row)) => {
                let term_infer = self._infer(eff_info, *term);
                let term_row = self.equate_as_prod_row(term_infer.ty, current_span);
                let unbound_row = self.fresh_simple_row();

                self.constraints
                    .add_effect_row_eq(expected.eff, term_infer.eff, current_span());
                match direction {
                    Direction::Left => self.constraints.add_data_row_combine(
                        term_row,
                        unbound_row,
                        row,
                        current_span(),
                    ),
                    Direction::Right => self.constraints.add_data_row_combine(
                        unbound_row,
                        term_row,
                        row,
                        current_span(),
                    ),
                };
            }
            (Item(term_name), _) => {
                let scheme =
                    type_scheme_of(self.db.as_tc_db(), *term_name).ty_scheme(self.db.as_tc_db());
                let span = current_span();
                let ty_chk = self.instantiate(scheme, span);
                self.constraints
                    .add_effect_row_eq(ty_chk.eff, expected.eff, span);
                self.constraints.add_ty_eq(ty_chk.ty, expected.ty, span);
            }
            // Bucket case for when we need to check a rule against a type but no case applies
            (_, _) => {
                // Infer a type for our term and check that the expected type is equal to the
                // inferred type.
                let inferred = self._infer(eff_info, term);
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
    fn _infer<E>(
        &mut self,
        //var_tys: &mut FxHashMap<VarId, InferTy<'infer>>,
        eff_info: &E,
        term: Idx<Term<VarId>>,
    ) -> InferResult<'infer>
    where
        E: ?Sized + EffectInfo,
    {
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
                let body_ty = self.fresh_var();
                let eff = self.fresh_scoped_row();

                self.state.var_tys.insert(*arg, arg_ty);
                self.local_env.insert(*arg, arg_ty);
                self._check(eff_info, *body, InferResult::new(body_ty, eff));
                self.local_env.remove(arg);

                InferResult::new(self.mk_ty(TypeKind::FunTy(arg_ty, body_ty)), eff)
            }
            // Application inference starts by inferring types for the func of the application.
            // We equate this inferred type to a function type, generating fresh unifiers if
            // needed.
            // We then check the arg of the application against the arg type of the function type
            // The resulting type of this application is the function result type.
            Application { func, arg } => {
                let fun_infer = self._infer(eff_info, *func);
                // Optimization: eagerly use FunTy if available. Otherwise dispatch fresh unifiers
                // for arg and ret type.
                let (arg_ty, ret_ty) = self.equate_as_fn_ty(fun_infer.ty, current_span);

                let arg_eff = self.fresh_scoped_row();
                self._check(eff_info, *arg, InferResult::new(arg_ty, arg_eff));

                let eff = self.fresh_scoped_row();
                self.constraints.add_effect_row_combine(
                    fun_infer.eff,
                    arg_eff,
                    eff,
                    current_span(),
                );
                InferResult::new(ret_ty, eff)
            }
            // If the variable is in environemnt return it's type, otherwise return an error.
            Variable(var) => {
                if let Some(ty) = self.local_env.get(var).cloned() {
                    self.state.var_tys.insert(*var, ty);
                    InferResult::new(ty, Row::Closed(self.empty_row()))
                } else {
                    self.errors.push(into_diag(
                        self.db,
                        TypeCheckError::VarNotDefined(*var),
                        current_span(),
                    ));
                    let err_ty = self.mk_ty(ErrorTy);
                    self.state.var_tys.insert(*var, err_ty);
                    InferResult::new(err_ty, Row::Closed(self.empty_row()))
                }
            }
            Label { label, term } => {
                let infer = self._infer(eff_info, *term);
                infer.map_ty(|ty| self.single_row_ty(*label, ty))
            }
            Unlabel { label, term } => {
                let term_infer = self._infer(eff_info, *term);
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
            Unit => {
                // Represent unit by an empty product type
                let unit = Row::Closed(self.empty_row());
                InferResult::new(self.mk_ty(ProdTy(unit)), Row::Closed(self.empty_row()))
            }
            Concat { left, right } => {
                let left_row = self.fresh_simple_row();
                let left_eff = self.fresh_scoped_row();
                let right_row = self.fresh_simple_row();
                let right_eff = self.fresh_scoped_row();
                let out_row = self.fresh_simple_row();
                let out_eff = self.fresh_scoped_row();

                let left_ty = self.mk_ty(ProdTy(left_row));
                let right_ty = self.mk_ty(ProdTy(right_row));

                self._check(eff_info, *left, InferResult::new(left_ty, left_eff));
                self._check(eff_info, *right, InferResult::new(right_ty, right_eff));

                let span = current_span();
                self.constraints
                    .add_effect_row_combine(left_eff, right_eff, out_eff, span);
                self.constraints
                    .add_data_row_combine(left_row, right_row, out_row, span);

                InferResult::new(self.mk_ty(ProdTy(out_row)), out_eff)
            }
            Branch { left, right } => {
                let left_row = self.fresh_simple_row();
                let left_eff = self.fresh_scoped_row();
                let right_row = self.fresh_simple_row();
                let right_eff = self.fresh_scoped_row();
                let out_row = self.fresh_simple_row();
                let out_eff = self.fresh_scoped_row();

                let ret_ty = self.fresh_var();

                let left_ty = self.mk_ty(FunTy(self.mk_ty(SumTy(left_row)), ret_ty));
                let right_ty = self.mk_ty(FunTy(self.mk_ty(SumTy(right_row)), ret_ty));

                self._check(eff_info, *left, InferResult::new(left_ty, left_eff));
                self._check(eff_info, *right, InferResult::new(right_ty, right_eff));

                let span = current_span();
                self.constraints
                    .add_effect_row_combine(left_eff, right_eff, out_eff, span);
                self.constraints
                    .add_data_row_combine(left_row, right_row, out_row, span);

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
                self._check(eff_info, *term, InferResult::new(term_ty, eff));

                match direction {
                    Direction::Left => self.constraints.add_data_row_combine(
                        small_row,
                        unbound_row,
                        big_row,
                        current_span(),
                    ),
                    Direction::Right => self.constraints.add_data_row_combine(
                        unbound_row,
                        small_row,
                        big_row,
                        current_span(),
                    ),
                };

                InferResult::new(self.mk_ty(ProdTy(small_row)), eff)
            }
            Inject { direction, term } => {
                let big_row = self.fresh_simple_row();
                let small_row = self.fresh_simple_row();

                let unbound_row = self.fresh_simple_row();
                let eff = self.fresh_scoped_row();
                let term_ty = self.mk_ty(SumTy(small_row));
                self._check(eff_info, *term, InferResult::new(term_ty, eff));

                match direction {
                    Direction::Left => self.constraints.add_data_row_combine(
                        small_row,
                        unbound_row,
                        big_row,
                        current_span(),
                    ),
                    Direction::Right => self.constraints.add_data_row_combine(
                        unbound_row,
                        small_row,
                        big_row,
                        current_span(),
                    ),
                };

                InferResult::new(self.mk_ty(SumTy(big_row)), eff)
            }
            Operation(op_name) => {
                let sig = self.instantiate(eff_info.effect_member_sig(*op_name), current_span());

                InferResult::new(
                    sig.ty,
                    Row::Closed(self.single_row(
                        eff_info.effect_name(op_name.effect(self.db.as_core_db())),
                        self.mk_ty(ProdTy(Row::Closed(self.empty_row()))),
                    )),
                )
            }
            Term::Handle { handler, body } => {
                let ret_ty = self.fresh_var();

                let TyChkRes {
                    ty: body_ty,
                    eff: body_eff,
                } = self._infer(eff_info, *body);

                // Ensure there is a return clause in the handler
                let ret_row = self.mk_row(
                    &[self.mk_label("return")],
                    &[self.mk_ty(FunTy(body_ty, ret_ty))],
                );
                let eff_sig_row = self.fresh_simple_row();
                let handler_row = self.fresh_simple_row();

                // Our handler should be an effect signature plus a return clause
                self.constraints.add_data_row_combine(
                    Row::Closed(ret_row),
                    eff_sig_row,
                    handler_row,
                    current_span(),
                );

                let handler_eff = self.fresh_scoped_row();
                self._check(
                    eff_info,
                    *handler,
                    InferResult::new(self.mk_ty(ProdTy(handler_row)), handler_eff),
                );

                let handled_eff = self.fresh_scoped_row();
                // Mark handler with the effect that it handles
                self.state.term_tys.insert(
                    *handler,
                    InferResult::new(self.mk_ty(ProdTy(handler_row)), handled_eff),
                );

                let out_eff = self.fresh_scoped_row();

                // We just want to match the signature against our effects, we don't need to
                // include the return clause.
                self.constraints
                    .add_handles(eff_sig_row, handled_eff, ret_ty, current_span());

                // Handle removes an effect so our out_eff should be whatever body_eff was minus
                // our handled effect
                self.constraints.add_effect_row_combine(
                    out_eff,
                    handled_eff,
                    body_eff,
                    current_span(),
                );

                InferResult::new(ret_ty, out_eff)
            }
            Int(_) => InferResult::new(self.mk_ty(IntTy), Row::Closed(self.empty_row())),
            Annotated { ty, term } => {
                let eff = self.fresh_scoped_row();
                let mut inst = Instantiate::new(self.db, self.ctx); // TODO: We should possibly extract an effect from this
                let infer_ty = ty.try_fold_with(&mut inst).unwrap();
                let res = InferResult::new(infer_ty, eff);
                self._check(eff_info, *term, res);
                res
            }
            Item(term_name) => {
                let scheme =
                    type_scheme_of(self.db.as_tc_db(), *term_name).ty_scheme(self.db.as_tc_db());
                self.instantiate(scheme, current_span())
            }
        };
        self.state.term_tys.insert(term, res);
        res
    }

    fn instantiate(&mut self, ty_scheme: TyScheme, span: Span) -> InferResult<'infer> {
        let mut inst = Instantiate {
            db: self.db,
            ctx: self.ctx,
            ty_unifiers: ty_scheme
                .bound
                .into_iter()
                .map(|_| self.ty_unifiers.new_key(None))
                .collect(),
            datarow_unifiers: vec![],
            effrow_unifiers: vec![],
        };
        for ev in ty_scheme.constrs {
            match ev.try_fold_with(&mut inst).unwrap() {
                Evidence::Row { left, right, goal } => self
                    .constraints
                    .add_data_row_combine(left, right, goal, span),
            }
        }
        InferResult::new(
            ty_scheme.ty.try_fold_with(&mut inst).unwrap(),
            ty_scheme.eff.try_fold_with(&mut inst).unwrap(),
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
    pub(crate) fn equate_as_sum_row(
        &mut self,
        ty: InferTy<'infer>,
        span: impl FnOnce() -> Span,
    ) -> SimpleInferRow<'infer> {
        match ty.deref() {
            TypeKind::SumTy(Row::Closed(row)) | TypeKind::RowTy(row) => Row::Closed(*row),
            TypeKind::SumTy(Row::Open(var)) => Row::Open(*var),
            _ => {
                let unifier = self.data_row_unifiers.new_key(None);
                self.constraints
                    .add_ty_eq(self.mk_ty(SumTy(Row::Open(unifier))), ty, span());
                Row::Open(unifier)
            }
        }
    }

    fn row_eq_constraint(
        &mut self,
        left: SimpleInferRow<'infer>,
        right: SimpleInferRow<'infer>,
        span: Span,
    ) {
        self.constraints.add_data_row_eq(left, right, span)
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
    pub(crate) fn solve<E>(
        mut self,
        eff_info: &E,
    ) -> (
        InPlaceUnificationTable<TcUnifierVar<'infer, TypeK>>,
        InPlaceUnificationTable<TcUnifierVar<'infer, SimpleRowK>>,
        InPlaceUnificationTable<TcUnifierVar<'infer, ScopedRowK>>,
        <Solution as InferState>::Storage<'infer>,
        Vec<TypeCheckDiagnostic>,
    )
    where
        E: ?Sized + EffectInfo,
    {
        let constraints = std::mem::take(&mut self.constraints);
        for (left, right, span) in constraints.tys {
            self.unify_ty_ty(left, right)
                .map_err(|err| self.errors.push(into_diag(self.db, err, span)))
                .unwrap_or_default();
        }
        for (RowCombination { left, right, goal }, span) in constraints.data_rows {
            self.unify_data_row_combine(left, right, goal)
                .map_err(|err| self.errors.push(into_diag(self.db, err, span)))
                .unwrap_or_default();
        }
        for (HandlesConstraint { handler, eff, ret }, span) in constraints.handles {
            self.lookup_effect_and_unify(eff_info, handler, eff, ret)
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

    fn normalize_data_row(&mut self, row: SimpleInferRow<'infer>) -> SimpleInferRow<'infer> {
        match row {
            Row::Open(var) => self
                .data_row_unifiers
                .probe_value(var)
                .map(|row| {
                    row.try_fold_with(&mut Normalize {
                        ctx: self.ctx,
                        ty_unifiers: &mut self.ty_unifiers,
                        datarow_unifiers: &mut self.data_row_unifiers,
                        effrow_unifiers: &mut self.eff_row_unifiers,
                    })
                    .map(Row::Closed)
                    .unwrap()
                })
                .unwrap_or_else(|| Row::Open(var)),
            Row::Closed(row) => Row::Closed(
                row.try_fold_with(&mut Normalize {
                    ctx: self.ctx,
                    ty_unifiers: &mut self.ty_unifiers,
                    datarow_unifiers: &mut self.data_row_unifiers,
                    effrow_unifiers: &mut self.eff_row_unifiers,
                })
                .unwrap(),
            ),
        }
    }

    fn normalize_eff_row(&mut self, row: ScopedInferRow<'infer>) -> ScopedInferRow<'infer> {
        match row {
            Row::Open(var) => self
                .eff_row_unifiers
                .probe_value(var)
                .map(|row| {
                    row.try_fold_with(&mut Normalize {
                        ctx: self.ctx,
                        ty_unifiers: &mut self.ty_unifiers,
                        datarow_unifiers: &mut self.data_row_unifiers,
                        effrow_unifiers: &mut self.eff_row_unifiers,
                    })
                    .map(Row::Closed)
                    .unwrap()
                })
                .unwrap_or_else(|| Row::Open(var)),
            Row::Closed(row) => Row::Closed(
                row.try_fold_with(&mut Normalize {
                    ctx: self.ctx,
                    ty_unifiers: &mut self.ty_unifiers,
                    datarow_unifiers: &mut self.data_row_unifiers,
                    effrow_unifiers: &mut self.eff_row_unifiers,
                })
                .unwrap(),
            ),
        }
    }

    fn dispatch_solved_eff_eqs(
        &mut self,
        var: ScopedRowVarOf<InArena<'infer>>,
        row: ScopedClosedRow<InArena<'infer>>,
    ) -> Result<(), TypeCheckError<'infer>> {
        let mut combos: Vec<RowCombination<ScopedInferRow<'infer>>> = vec![];
        self.state.eff_eqns = std::mem::take(&mut self.state.eff_eqns)
            .into_iter()
            .filter_map(|eq| match eq {
                ScopedRowEquation::ClosedGoal(closed) if closed.min == var => {
                    combos.push(RowCombination {
                        left: Row::Closed(row),
                        right: Row::Open(closed.max),
                        goal: Row::Closed(closed.goal),
                    });
                    None
                }
                ScopedRowEquation::ClosedGoal(closed) if closed.max == var => {
                    combos.push(RowCombination {
                        left: Row::Open(closed.min),
                        right: Row::Closed(row),
                        goal: Row::Closed(closed.goal),
                    });
                    None
                }
                // Inert return the equation as is
                ScopedRowEquation::ClosedGoal(_) => Some(eq),
                ScopedRowEquation::OpenGoal(open) if open.goal == var => match open.ops {
                    Operatives::OpenOpen { left, right } => {
                        Some(ScopedRowEquation::ClosedGoal(ClosedGoal {
                            goal: row,
                            min: left,
                            max: right,
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
                ScopedRowEquation::OpenGoal(ScopedOpenGoal {
                    goal,
                    ops: Operatives::OpenOpen { left, right },
                }) if left == var => Some(ScopedRowEquation::OpenGoal(ScopedOpenGoal {
                    goal,
                    ops: Operatives::ClosedOpen { left: row, right },
                })),
                ScopedRowEquation::OpenGoal(ScopedOpenGoal {
                    goal,
                    ops: Operatives::OpenOpen { left, right },
                }) if right == var => Some(ScopedRowEquation::OpenGoal(ScopedOpenGoal {
                    goal,
                    ops: Operatives::OpenClosed { left, right: row },
                })),
                ScopedRowEquation::OpenGoal(ScopedOpenGoal {
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
                ScopedRowEquation::OpenGoal(ScopedOpenGoal {
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
                ScopedRowEquation::OpenGoal(_) => Some(eq),
            })
            .collect();
        for combo in combos {
            self.unify_eff_row_combine(combo.left, combo.right, combo.goal)?;
        }
        Ok(())
    }

    /// Dispatch any equations that become solved when we learn a variable is equal to a row.
    /// Solved equations are removed from the set and any equalities they produce are unified.
    fn dispatch_solved_data_eqs(
        &mut self,
        var: SimpleRowVarOf<InArena<'infer>>,
        row: SimpleClosedRow<InArena<'infer>>,
    ) -> Result<(), TypeCheckError<'infer>> {
        // We want any unifications we do as a byproduct of this to see the updated state so we
        // save them here until we've finished our intial removals from state
        let mut combos = vec![];
        self.state.data_eqns = self
            .state
            .data_eqns
            .iter()
            .filter_map(|eq| match eq {
                UnsolvedRowEquation::ClosedGoal(cand) if cand.min == var => {
                    combos.push(RowCombination {
                        left: Row::Closed(row),
                        right: Row::Open(cand.max),
                        goal: Row::Closed(cand.goal),
                    });
                    None
                }
                UnsolvedRowEquation::ClosedGoal(cand) if cand.max == var => {
                    combos.push(RowCombination {
                        left: Row::Closed(row),
                        right: Row::Open(cand.min),
                        goal: Row::Closed(cand.goal),
                    });
                    None
                }
                UnsolvedRowEquation::OpenGoal(cand) if cand.goal == var => {
                    match cand.orxr {
                        OrderedRowXorRow::OpenOpen { min, max } => {
                            Some(UnsolvedRowEquation::ClosedGoal(ClosedGoal {
                                goal: row,
                                min,
                                max,
                            }))
                        }
                        // We can solve for open
                        OrderedRowXorRow::ClosedOpen(closed, open) => {
                            combos.push(RowCombination {
                                left: Row::Closed(closed),
                                right: Row::Open(open),
                                goal: Row::Closed(row),
                            });
                            None
                        }
                    }
                }
                UnsolvedRowEquation::OpenGoal(OpenGoal {
                    goal,
                    orxr: OrderedRowXorRow::ClosedOpen(closed, open),
                }) if *open == var => {
                    combos.push(RowCombination {
                        left: Row::Closed(*closed),
                        right: Row::Closed(row),
                        goal: Row::Open(*goal),
                    });
                    None
                }
                UnsolvedRowEquation::OpenGoal(OpenGoal {
                    goal,
                    orxr: OrderedRowXorRow::OpenOpen { min, max },
                }) if *min == var => Some(UnsolvedRowEquation::OpenGoal(OpenGoal {
                    goal: *goal,
                    orxr: OrderedRowXorRow::ClosedOpen(row, *max),
                })),
                UnsolvedRowEquation::OpenGoal(OpenGoal {
                    goal,
                    orxr: OrderedRowXorRow::OpenOpen { min, max },
                }) if *max == var => Some(UnsolvedRowEquation::OpenGoal(OpenGoal {
                    goal: *goal,
                    orxr: OrderedRowXorRow::ClosedOpen(row, *min),
                })),
                eq => Some(*eq),
            })
            .collect::<BTreeSet<_>>();

        // Now we can perform our unifications against our new state
        for RowCombination { left, right, goal } in combos {
            self.unify_data_row_combine(left, right, goal)?;
        }
        Ok(())
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
        let ty_ = ty
            .try_fold_with(&mut OccursCheck { ctx: self.ctx, var })
            .map_err(|var| TypeCheckError::TypeOccursCheckFailed(var, ty))?;
        self.ty_unifiers
            .unify_var_value(var, Some(ty_))
            .map_err(|e| e.into())
    }

    fn unify_datarow_datarow(
        &mut self,
        left: SimpleClosedRow<InArena<'infer>>,
        right: SimpleClosedRow<InArena<'infer>>,
    ) -> Result<(), TypeCheckError<'infer>> {
        // If our row labels aren't equal the types cannot be equal
        if !left.is_unifiable(right) {
            return Err(TypeCheckError::DataRowsNotEqual(
                Row::Closed(left),
                Row::Closed(right),
            ));
        }

        for (left_ty, right_ty) in left.values(self).iter().zip(right.values(self).iter()) {
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
                .ty_unifiers
                .unify_var_var(left_var, right_var)
                .map_err(|e| e.into()),

            // If a key meets a new ty record they must be equal
            (_, VarTy(var)) => self.unify_var_ty(var, left),
            (VarTy(var), _) => self.unify_var_ty(var, right),

            // Coerce a product into a row
            (RowTy(left), ProdTy(right)) => self.unify_datarow_eq(Row::Closed(left), right),
            (ProdTy(left), RowTy(right)) => self.unify_datarow_eq(left, Row::Closed(right)),

            // Coerce a sum into a row
            (RowTy(left), SumTy(right)) => self.unify_datarow_eq(Row::Closed(left), right),
            (SumTy(left), RowTy(right)) => self.unify_datarow_eq(left, Row::Closed(right)),

            // Decompose compound types
            (FunTy(left_arg, left_ret), FunTy(right_arg, right_ret)) => {
                self.unify_ty_ty_normalized(left_arg, right_arg)?;
                self.unify_ty_ty_normalized(left_ret, right_ret)
            }
            (RowTy(left_row), RowTy(right_row)) => self.unify_datarow_datarow(left_row, right_row),
            (ProdTy(left), ProdTy(right)) => self.unify_datarow_eq(left, right),
            (SumTy(left), SumTy(right)) => self.unify_datarow_eq(left, right),
            // Discharge equal types
            (IntTy, IntTy) => Ok(()),

            // Type mismatch
            (IntTy, FunTy(_, _))
            | (IntTy, RowTy(_))
            | (IntTy, ProdTy(_))
            | (IntTy, SumTy(_))
            | (FunTy(_, _), IntTy)
            | (FunTy(_, _), RowTy(_))
            | (FunTy(_, _), ProdTy(_))
            | (FunTy(_, _), SumTy(_))
            | (RowTy(_), IntTy)
            | (RowTy(_), FunTy(_, _))
            | (ProdTy(_), IntTy)
            | (ProdTy(_), FunTy(_, _))
            | (ProdTy(_), SumTy(_))
            | (SumTy(_), IntTy)
            | (SumTy(_), FunTy(_, _))
            | (SumTy(_), ProdTy(_)) => Err((left, right).into()),
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

        self.unify_ty_ty_normalized(normal_left, normal_right)
    }

    fn unify_datarow_eq(
        &mut self,
        left: SimpleInferRow<'infer>,
        right: SimpleInferRow<'infer>,
    ) -> Result<(), TypeCheckError<'infer>> {
        match (left, right) {
            (Row::Open(l_var), Row::Open(r_var)) => {
                self.data_row_unifiers.unify_var_var(l_var, r_var)?
            }
            (Row::Open(var), Row::Closed(row)) | (Row::Closed(row), Row::Open(var)) => {
                self.unify_rowvar_datarow(var, row)?
            }
            (Row::Closed(left_row), Row::Closed(right_row)) => {
                if left_row.len(self) != right_row.len(self) {
                    return Err(TypeCheckError::DataRowsNotEqual(
                        Row::Closed(left_row),
                        Row::Closed(right_row),
                    ));
                }
                for ((left_field, left_value), (right_field, right_value)) in
                    left_row.iter().zip(right_row.iter())
                {
                    debug_assert_eq!(left_field, right_field);
                    self.unify_ty_ty(*left_value, *right_value)?;
                }
            }
        };
        Ok(())
    }

    fn unify_rowvar_datarow(
        &mut self,
        var: TcUnifierVar<'infer, SimpleRowK>,
        row: SimpleClosedRow<InArena<'infer>>,
    ) -> Result<(), TypeCheckError<'infer>> {
        let row_ = row
            .try_fold_with(&mut OccursCheck { ctx: self.ctx, var })
            .map_err(|var| TypeCheckError::DataRowOccursCheckFailed(var, row))?;
        self.dispatch_solved_data_eqs(var, row)?;
        self.data_row_unifiers
            .unify_var_value(var, Some(row_))
            .map_err(|err| TypeCheckError::DataRowsNotEqual(Row::Closed(err.0), Row::Closed(err.1)))
    }

    fn unify_rowvar_effrow(
        &mut self,
        var: TcUnifierVar<'infer, ScopedRowK>,
        row: ScopedClosedRow<InArena<'infer>>,
    ) -> Result<(), TypeCheckError<'infer>> {
        let row_ = row
            .try_fold_with(&mut OccursCheck { ctx: self.ctx, var })
            .map_err(|var| TypeCheckError::EffectRowOccursCheckFailed(var, row))?;
        self.dispatch_solved_eff_eqs(var, row)?;
        self.eff_row_unifiers
            .unify_var_value(var, Some(row_))
            .map_err(|err| {
                TypeCheckError::EffectRowsNotEqual(Row::Closed(err.0), Row::Closed(err.1))
            })
    }

    fn unify_eff_row_combine(
        &mut self,
        left: ScopedInferRow<'infer>,
        right: ScopedInferRow<'infer>,
        goal: ScopedInferRow<'infer>,
    ) -> Result<(), TypeCheckError<'infer>> {
        let goal = self.normalize_eff_row(goal);
        let left = self.normalize_eff_row(left);
        let right = self.normalize_eff_row(right);

        match goal {
            Row::Open(goal_var) => match (left, right) {
                (Row::Closed(left), Row::Closed(right)) => {
                    let (fields, values) = left.union(&right);
                    let row = self.mk_row(&fields, &values);
                    self.unify_rowvar_effrow(goal_var, row)?;
                }
                (Row::Open(left_var), Row::Closed(right_row)) => {
                    self.state
                        .eff_eqns
                        .iter()
                        .find_map(|eqn| match eqn {
                            ScopedRowEquation::OpenGoal(ScopedOpenGoal {
                                goal,
                                ops: Operatives::OpenClosed { left, right },
                            }) if (goal == &goal_var
                                && (left == &left_var || right.is_unifiable(right_row)))
                                || (left == &left_var && right.is_unifiable(right_row)) =>
                            {
                                Some((*left, *right, *goal))
                            }
                            _ => None,
                        })
                        .map(|(left, right, goal)| {
                            self.eff_row_unifiers.unify_var_var(goal, goal_var)?;
                            self.eff_row_unifiers.unify_var_var(left, left_var)?;
                            self.unify_effrow_effrow(right, right_row)
                        })
                        .unwrap_or_else(|| {
                            self.state.eff_eqns.insert(ScopedRowEquation::OpenGoal(
                                ScopedOpenGoal {
                                    goal: goal_var,
                                    ops: Operatives::OpenClosed {
                                        left: left_var,
                                        right: right_row,
                                    },
                                },
                            ));
                            Ok(())
                        })?;
                }
                (Row::Closed(left_row), Row::Open(right_var)) => {
                    self.state
                        .eff_eqns
                        .iter()
                        .find_map(|eqn| match eqn {
                            ScopedRowEquation::OpenGoal(ScopedOpenGoal {
                                goal,
                                ops: Operatives::ClosedOpen { left, right },
                            }) if (goal == &goal_var
                                && (right == &right_var || left.is_unifiable(left_row)))
                                || (right == &right_var && left.is_unifiable(left_row)) =>
                            {
                                Some((*left, *right, *goal))
                            }
                            _ => None,
                        })
                        .map(|(left, right, goal)| {
                            self.eff_row_unifiers.unify_var_var(goal, goal_var)?;
                            self.eff_row_unifiers.unify_var_var(right, right_var)?;
                            self.unify_effrow_effrow(left, left_row)
                        })
                        .unwrap_or_else(|| {
                            self.state.eff_eqns.insert(ScopedRowEquation::OpenGoal(
                                ScopedOpenGoal {
                                    goal: goal_var,
                                    ops: Operatives::ClosedOpen {
                                        left: left_row,
                                        right: right_var,
                                    },
                                },
                            ));
                            Ok(())
                        })?;
                }
                (Row::Open(left_var), Row::Open(right_var)) => {
                    self.state
                        .eff_eqns
                        .iter()
                        .find_map(|eqn| match eqn {
                            ScopedRowEquation::OpenGoal(ScopedOpenGoal {
                                goal,
                                ops: Operatives::OpenOpen { left, right },
                            }) if (goal == &goal_var
                                && (left == &left_var || right == &right_var))
                                || (left == &left_var && right == &right_var) =>
                            {
                                Some((*left, *right, *goal))
                            }
                            _ => None,
                        })
                        .map(|(left, right, goal)| {
                            self.eff_row_unifiers.unify_var_var(left, left_var)?;
                            self.eff_row_unifiers.unify_var_var(right, right_var)?;
                            self.eff_row_unifiers.unify_var_var(goal, goal_var)
                        })
                        .unwrap_or_else(|| {
                            self.state.eff_eqns.insert(ScopedRowEquation::OpenGoal(
                                ScopedOpenGoal {
                                    goal: goal_var,
                                    ops: Operatives::OpenOpen {
                                        left: left_var,
                                        right: right_var,
                                    },
                                },
                            ));
                            Ok(())
                        })?;
                }
            },
            Row::Closed(goal) => match (left, right) {
                (Row::Open(left), Row::Open(right)) => {
                    self.state
                        .eff_eqns
                        .iter()
                        .find_map(|eqn| match eqn {
                            ScopedRowEquation::ClosedGoal(closed)
                                if (closed.goal.is_unifiable(goal)
                                    && (closed.min == left || closed.max == right))
                                    || (closed.min == left && closed.max == right) =>
                            {
                                Some((closed.min, closed.max, closed.goal))
                            }
                            _ => None,
                        })
                        .map(|(left_var, right_var, goal_row)| {
                            self.eff_row_unifiers.unify_var_var(left_var, left)?;
                            self.eff_row_unifiers.unify_var_var(right_var, right)?;
                            self.unify_effrow_effrow(goal_row, goal)
                        })
                        .unwrap_or_else(|| {
                            self.state
                                .eff_eqns
                                .insert(ScopedRowEquation::ClosedGoal(ClosedGoal {
                                    goal,
                                    min: left,
                                    max: right,
                                }));
                            Ok(())
                        })?;
                }
                (Row::Open(_), Row::Closed(_)) => todo!("solve"),
                (Row::Closed(_), Row::Open(_)) => todo!("solve"),
                (Row::Closed(left), Row::Closed(right)) => {
                    let (fields, values) = left.union(&right);
                    let row = self.mk_row(&fields, &values);
                    self.unify_effrow_effrow(goal, row)?;
                }
            },
        };
        Ok(())
    }

    fn unify_effrow_effrow(
        &mut self,
        left: ScopedClosedRow<InArena<'infer>>,
        right: ScopedClosedRow<InArena<'infer>>,
    ) -> Result<(), TypeCheckError<'infer>> {
        if !left.is_unifiable(right) {
            return Err(TypeCheckError::EffectRowsNotEqual(
                Row::Closed(left),
                Row::Closed(right),
            ));
        }

        for (left_ty, right_ty) in left.values(self).iter().zip(right.values(self).iter()) {
            self.unify_ty_ty(*left_ty, *right_ty)?;
        }

        Ok(())
    }

    fn unify_data_row_combine(
        &mut self,
        left: SimpleInferRow<'infer>,
        right: SimpleInferRow<'infer>,
        goal: SimpleInferRow<'infer>,
    ) -> Result<(), TypeCheckError<'infer>> {
        let goal = self.normalize_data_row(goal);
        let left = self.normalize_data_row(left);
        let right = self.normalize_data_row(right);

        match goal {
            Row::Open(goal_var) => match (left, right) {
                // With only one open variable we can solve
                (Row::Closed(left), Row::Closed(right)) => {
                    let (fields, values) = left.disjoint_union(&right).map_err(|err| {
                        TypeCheckError::RowsNotDisjoint(err.left, err.right, err.label)
                    })?;
                    let row = self.mk_row(&fields, &values);
                    self.unify_rowvar_datarow(goal_var, row)
                }
                (Row::Open(left_var), Row::Closed(right_row)) => self
                    .state
                    .data_eqns
                    .iter()
                    .find_map(|eq| match eq {
                        UnsolvedRowEquation::OpenGoal(OpenGoal {
                            goal,
                            orxr: OrderedRowXorRow::ClosedOpen(closed, open),
                        }) if goal_var.eq(goal)
                            && (right_row.is_unifiable(*closed) || left_var.eq(open)) =>
                        {
                            Some((*goal, *closed, *open))
                        }
                        UnsolvedRowEquation::OpenGoal(OpenGoal {
                            goal,
                            orxr: OrderedRowXorRow::ClosedOpen(closed, open),
                        }) if right_row.is_unifiable(*closed) && left_var.eq(open) => {
                            Some((*goal, *closed, *open))
                        }
                        _ => None,
                    })
                    .map(|(goal, closed, open)| {
                        self.data_row_unifiers.unify_var_var(goal, goal_var)?;
                        self.data_row_unifiers.unify_var_var(open, left_var)?;
                        self.unify_datarow_datarow(closed, right_row)
                    })
                    .unwrap_or_else(|| {
                        self.state
                            .data_eqns
                            .insert(UnsolvedRowEquation::OpenGoal(OpenGoal {
                                goal: goal_var,
                                orxr: OrderedRowXorRow::ClosedOpen(right_row, left_var),
                            }));
                        Ok(())
                    }),
                (Row::Closed(left_row), Row::Open(right_var)) => self
                    .state
                    .data_eqns
                    .iter()
                    .find_map(|eq| match eq {
                        UnsolvedRowEquation::OpenGoal(OpenGoal {
                            goal,
                            orxr: OrderedRowXorRow::ClosedOpen(closed, open),
                        }) if *goal == goal_var
                            && (*open == right_var || closed.is_unifiable(left_row)) =>
                        {
                            Some((*closed, *open))
                        }
                        _ => None,
                    })
                    .map(|(closed, open)| {
                        self.data_row_unifiers.unify_var_var(open, right_var)?;
                        self.unify_datarow_datarow(closed, left_row)
                    })
                    .unwrap_or_else(|| {
                        self.state
                            .data_eqns
                            .insert(UnsolvedRowEquation::OpenGoal(OpenGoal {
                                goal: goal_var,
                                orxr: OrderedRowXorRow::ClosedOpen(left_row, right_var),
                            }));
                        Ok(())
                    }),
                (Row::Open(left_var), Row::Open(right_var)) => {
                    let (min_var, max_var) = if left_var <= right_var {
                        (left_var, right_var)
                    } else {
                        (right_var, left_var)
                    };
                    self.state
                        .data_eqns
                        .iter()
                        .find_map(|eq| match eq {
                            UnsolvedRowEquation::OpenGoal(OpenGoal {
                                goal,
                                orxr: OrderedRowXorRow::OpenOpen { min, max },
                            }) if (*goal == goal_var && (*min == min_var || *max == right_var))
                                || (*min == min_var && *max == max_var) =>
                            {
                                Some((min, max, goal))
                            }
                            _ => None,
                        })
                        .map(|(left, right, goal)| {
                            self.data_row_unifiers.unify_var_var(*left, left_var)?;
                            self.data_row_unifiers.unify_var_var(*right, right_var)?;
                            self.data_row_unifiers.unify_var_var(*goal, goal_var)?;
                            Ok(())
                        })
                        .unwrap_or_else(|| {
                            self.state
                                .data_eqns
                                .insert(UnsolvedRowEquation::OpenGoal(OpenGoal {
                                    goal: goal_var,
                                    orxr: OrderedRowXorRow::with_open_open(left_var, right_var),
                                }));
                            Ok(())
                        })
                }
            },
            Row::Closed(goal_row) => match (left, right) {
                // Our rows are all closed, we just need to check they are equal and discharge them
                (Row::Closed(left_row), Row::Closed(right_row)) => {
                    let (fields, values) = left_row.disjoint_union(&right_row).map_err(|err| {
                        TypeCheckError::RowsNotDisjoint(err.left, err.right, err.label)
                    })?;
                    let row = self.mk_row(&fields, &values);
                    self.unify_datarow_datarow(goal_row, row)
                }
                // With one open row we solve that row
                (Row::Open(left_var), Row::Closed(right_row)) => {
                    let (fields, values) = goal_row.difference(right_row);
                    let row = self.mk_row(&fields, &values);
                    let (fields, values) = goal_row.difference(row);
                    let right_goal_row = self.mk_row(&fields, &values);
                    self.unify_datarow_datarow(right_row, right_goal_row)?;
                    self.unify_rowvar_datarow(left_var, row)
                }
                (Row::Closed(left_row), Row::Open(right_var)) => {
                    let (fields, values) = goal_row.difference(left_row);
                    let row = self.mk_row(&fields, &values);
                    let (fields, values) = goal_row.difference(row);
                    let left_goal_row = self.mk_row(&fields, &values);
                    self.unify_datarow_datarow(left_row, left_goal_row).unwrap();
                    self.unify_rowvar_datarow(right_var, row)
                }
                // We can't unify this case as is, we need more information.
                // And our goal is closed so we can't set it to pending
                (Row::Open(left_var), Row::Open(right_var)) => {
                    let (min_var, max_var) = if left_var <= right_var {
                        (left_var, right_var)
                    } else {
                        (right_var, left_var)
                    };
                    let is_unifiable = |cand: &ClosedGoal<InArena<'infer>, Simple>| {
                        // If any two components are equal we should unify
                        (cand.goal.is_unifiable(goal_row)
                            && (cand.min == min_var || cand.max == max_var))
                            || (cand.min == min_var && cand.max == max_var)
                    };
                    self.state
                        .data_eqns
                        .iter()
                        .find_map(|eq| match eq {
                            UnsolvedRowEquation::ClosedGoal(cand) if is_unifiable(cand) => {
                                Some((cand.min, cand.max, Row::<InArena<'_>>::Closed(cand.goal)))
                            }
                            UnsolvedRowEquation::OpenGoal(OpenGoal {
                                goal,
                                orxr: OrderedRowXorRow::OpenOpen { min, max },
                            }) if min_var.eq(min) && max_var.eq(max) => {
                                Some((*min, *max, Row::<InArena<'_>>::Open(*goal)))
                            }
                            _ => None,
                        })
                        .map(|(min, max, goal)| {
                            self.data_row_unifiers.unify_var_var(min_var, min)?;
                            self.data_row_unifiers.unify_var_var(max_var, max)?;
                            match goal {
                                Row::Open(var) => self.unify_rowvar_datarow(var, goal_row),
                                Row::Closed(row) => self.unify_datarow_datarow(row, goal_row),
                            }
                        })
                        .unwrap_or_else(|| {
                            self.state.data_eqns.insert(UnsolvedRowEquation::ClosedGoal(
                                ClosedGoal {
                                    goal: goal_row,
                                    min: left_var,
                                    max: right_var,
                                },
                            ));
                            Ok(())
                        })
                }
            },
        }
    }

    fn lookup_effect_and_unify<E>(
        &mut self,
        eff_info: &E,
        handler: SimpleInferRow<'infer>,
        eff: ScopedInferRow<'infer>,
        ret: InferTy<'infer>,
    ) -> Result<(), TypeCheckError<'infer>>
    where
        E: ?Sized + EffectInfo,
    {
        let normal_handler = self.normalize_data_row(handler);
        let normal_eff = self.normalize_eff_row(eff);
        let normal_ret = self.normalize_ty(ret);

        println!(
            "Lookup effect:\n\tHandler: {:?}\n\tEffect: {:?}\n\tRet: {:?}",
            normal_handler, normal_eff, normal_ret
        );
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
                            .bound
                            .into_iter()
                            .map(|_| ctx.ty_unifiers.new_key(None))
                            .collect(),
                        datarow_unifiers: vec![],
                        effrow_unifiers: vec![],
                    };

                    // Transform our scheme ty into the type a handler should have
                    // This means it should take a resume parameter that is a function returning `ret` and return `ret` itself.
                    let member_ty = transform_to_cps_handler_ty(
                        ctx,
                        scheme.ty.try_fold_with(&mut inst).unwrap(),
                    )?;

                    // Unify our instantiated and transformed member type agaisnt the handler field
                    // type.
                    println!("unify_ty_ty\n\t{:?}\n\t{:?}", member_ty, ty);
                    ctx.unify_ty_ty(member_ty, *ty)?;

                    // We want to check constrs after we unify our scheme type so that we've alread
                    // unified as many fresh variables into handler field variables as possible.
                    for constrs in scheme.constrs {
                        match constrs.try_fold_with(&mut inst).unwrap() {
                            Evidence::Row { left, right, goal } => {
                                ctx.unify_data_row_combine(left, right, goal)?
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
                let eff_name = eff_info
                    .lookup_effect_by_member_names(self.module, handler.fields(self))
                    .ok_or(TypeCheckError::UndefinedEffectSignature(handler))?;
                let mut members_sig = eff_info
                    .effect_members(eff_name)
                    .iter()
                    .map(|eff_op_id| {
                        (
                            eff_info.effect_member_name(*eff_op_id),
                            eff_info.effect_member_sig(*eff_op_id),
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
                let name = eff_info.effect_name(eff_name);
                let unit_ty = self.mk_ty(ProdTy(Row::Closed(self.mk_row(&[], &[]))));
                let eff_row = self.mk_row(&[name], &[unit_ty]);
                self.unify_rowvar_effrow(eff_var, eff_row)
            }
            (Row::Closed(handler), Row::Closed(eff)) => {
                debug_assert!(eff.len(self) == 1);
                let eff_ident = *eff.fields(self).first().unwrap();
                let eff_name = eff_info
                    .lookup_effect_by_name(self.module, eff_ident)
                    .ok_or(TypeCheckError::UndefinedEffect(eff_ident))?;
                let mut members_sig = eff_info
                    .effect_members(eff_name)
                    .iter()
                    .map(|eff_op_id| {
                        (
                            eff_info.effect_member_name(*eff_op_id),
                            eff_info.effect_member_sig(*eff_op_id),
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
                let eff_name = eff_info
                    .lookup_effect_by_name(self.module, eff_ident)
                    .ok_or(TypeCheckError::UndefinedEffect(eff_ident))?;
                let members_sig = eff_info
                    .effect_members(eff_name)
                    .iter()
                    .map(|eff_op_id| {
                        let scheme = eff_info.effect_member_sig(*eff_op_id);
                        let mut inst = Instantiate {
                            db: self.db,
                            ctx: self.ctx,
                            ty_unifiers: scheme
                                .bound
                                .into_iter()
                                .map(|_| self.ty_unifiers.new_key(None))
                                .collect(),
                            datarow_unifiers: vec![],
                            effrow_unifiers: vec![],
                        };

                        for constr in scheme.constrs {
                            match constr.try_fold_with(&mut inst).unwrap() {
                                Evidence::Row { left, right, goal } => {
                                    self.unify_data_row_combine(left, right, goal)?
                                }
                            }
                        }

                        let member_ty = transform_to_cps_handler_ty(
                            self,
                            scheme.ty.try_fold_with(&mut inst).unwrap(),
                        )?;

                        Ok((eff_info.effect_member_name(*eff_op_id), member_ty))
                    })
                    .collect::<Result<Vec<_>, TypeCheckError<'infer>>>()?;

                let member_row = self.construct_simple_row(members_sig);
                self.unify_rowvar_datarow(handler_var, member_row)
            }
            // We didn't learn enough info to solve our handle term, this is an error
            (Row::Open(handler_var), Row::Open(eff_var)) => Err(TypeCheckError::UnsolvedHandle {
                handler: handler_var,
                eff: eff_var,
            }),
        }
    }
}
