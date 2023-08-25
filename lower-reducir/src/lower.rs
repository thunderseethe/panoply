use aiahr_ast::{Ast, Direction, Term};
use aiahr_core::{
    id::{ReducIrTyVarId, ReducIrVarId, TermName, TyVarId, VarId},
    modules::Module,
    pretty::PrettyErrorWithDb,
};
use aiahr_reducir::{
    ty::{
        Kind, MkReducIrTy, ReducIrRow, ReducIrTy, ReducIrTyApp, ReducIrTyKind, ReducIrTyKind::*,
        ReducIrVarTy, RowReducIrKind, UnwrapMonTy,
    },
    DelimCont, DelimReducIr, ReducIr, ReducIrKind,
    ReducIrKind::*,
    ReducIrLocal, ReducIrTermName, ReducIrVar, TypeCheck, P,
};
use aiahr_tc::{EffectInfo, TyChkRes};
use aiahr_ty::{
    row::{Row, RowOps, RowSema, Scoped, ScopedClosedRow, Simple, SimpleClosedRow},
    AccessTy, Evidence, InDb, MkTy, RowFields, Ty, TyScheme, TypeKind, Wrapper,
};
use la_arena::Idx;
use rustc_hash::FxHashMap;

use crate::{
    evidence::{EvidenceMap, PartialEv},
    id_converter::IdConverter,
    lower_row_ev, ReducIrEffectInfo, ReducIrRowEv,
};
/// Unwrap a type into it a product and return the product's row.
///
/// Because we are lowering from a type checked AST we would've failed with a type error already if
/// this operation would fail.
fn expect_prod_ty<'a, A: ?Sized + AccessTy<'a, InDb>>(db: &A, ty: Ty) -> Row<Simple> {
    ty.try_as_prod_row(db).unwrap_or_else(|_| unreachable!())
}

/// Unwrap a type into a sum and return the sum's row.
///
/// Because we are lowering from a type checked AST we would've failed with a type error already if
/// this operation would fail.
fn expect_sum_ty<'a>(db: &(impl ?Sized + AccessTy<'a, InDb>), ty: Ty) -> Row<Simple> {
    ty.try_as_sum_row(db).unwrap_or_else(|_| unreachable!())
}

/// Unwrap a type as a branch type, returning the row of the branch.
/// A branch type is a `FunTy(SumTy(row), VarTy(_))` and is used as the argument to branch
/// statements. We return the `row` from that type
///
/// Because we are lowering from a type checked AST we would've failed with a type error already if
/// this operation would fail.
fn expect_branch_ty<'a>(db: &(impl ?Sized + AccessTy<'a, InDb>), ty: Ty) -> Row<Simple> {
    ty.try_as_fn_ty(db)
        .and_then(|(arg, _)| arg.try_as_sum_row(db))
        .unwrap_or_else(|_| unreachable!())
}

pub trait ItemWrappers {
    fn lookup_wrapper(&self, term_name: TermName, term: Idx<Term<VarId>>) -> &Wrapper;
}

pub trait ItemSchemes {
    fn lookup_scheme(&self, term: TermName) -> TyScheme;
}
pub trait VarTys {
    fn lookup_var(&self, term: TermName, var_id: VarId) -> Ty;
}
pub trait TermTys {
    fn lookup_term(&self, term_name: TermName, term: Idx<Term<VarId>>) -> TyChkRes<InDb>;
}

// TODO: Wip name
pub(crate) struct Evidenceless;
pub(crate) struct Evidentfull;

pub(crate) struct LowerCtx<'a, 'b, State = Evidenceless> {
    db: &'a dyn crate::Db,
    current: ReducIrTermName,
    var_conv: &'b mut IdConverter<VarId, ReducIrVarId>,
    ty_ctx: LowerTyCtx<'a, 'b>,
    ev_map: EvidenceMap,
    evv_var_id: ReducIrVarId,
    _marker: std::marker::PhantomData<State>,
}

pub(crate) struct LowerTySchemeCtx<'a, 'b> {
    db: &'a dyn crate::Db,
    tyvar_conv: &'b mut IdConverter<TyVarId, ReducIrTyVarId>,
}
impl<'a, 'b> LowerTySchemeCtx<'a, 'b> {
    pub(crate) fn new(
        db: &'a dyn crate::Db,
        tyvar_conv: &'b mut IdConverter<TyVarId, ReducIrTyVarId>,
    ) -> Self {
        Self { db, tyvar_conv }
    }

    fn create_ty_ctx(self, tyvar_env: FxHashMap<TyVarId, i32>) -> LowerTyCtx<'a, 'b> {
        LowerTyCtx {
            db: self.db,
            tyvar_conv: self.tyvar_conv,
            tyvar_env,
        }
    }

    pub(crate) fn lower_scheme(
        self,
        module: Module,
        scheme: &TyScheme,
    ) -> (ReducIrTy, LowerTyCtx<'a, 'b>) {
        // We bind foralls in Type -> SimpleRow -> ScopedRow order
        // So we need to build our tyvar_env in reverse ordering so indices are correct
        let foralls = scheme
            .bound_eff_row
            .iter()
            .rev()
            .map(|tyvar| (*tyvar, Kind::ScopedRow))
            .chain(
                scheme
                    .bound_data_row
                    .iter()
                    .rev()
                    .map(|tyvar| (*tyvar, Kind::SimpleRow)),
            )
            .chain(
                scheme
                    .bound_ty
                    .iter()
                    .rev()
                    .map(|tyvar| (*tyvar, Kind::Type)),
            )
            .collect::<Vec<_>>();
        let tyvar_env = foralls
            .iter()
            .enumerate()
            .map(|(i, (tyvar, _))| (*tyvar, i as i32))
            .collect::<FxHashMap<_, _>>();

        let mut ty_ctx: LowerTyCtx<'a, 'b> = self.create_ty_ctx(tyvar_env);

        let ir_ty = ty_ctx.lower_ty(scheme.ty);

        // Add parameter to type for each constraint
        let constrs_ty = ty_ctx.db.mk_fun_ty(
            scheme
                .constrs
                .iter()
                .map(|constr| ty_ctx.row_evidence_ir_ty(module, constr)),
            ir_ty,
        );

        let ir_ty: ReducIrTy = foralls.into_iter().rfold(constrs_ty, |ir_ty, (_, kind)| {
            ty_ctx.db.mk_reducir_ty(ForallTy(kind, ir_ty))
        });
        // Add each type variable around type
        (ir_ty, ty_ctx)
    }
}

/// A single row is represented by two types in ReducIr
/// The row's product type, and it's coproduct type.
pub(crate) struct RowEvIrTy {
    pub(crate) prod: ReducIrTy,
    pub(crate) coprod: ReducIrTy,
}

// This exists as a standalone struct so we can construct it and lower types without having to
// construct a full LowerCtx
pub(crate) struct LowerTyCtx<'a, 'b> {
    db: &'a dyn crate::Db,
    tyvar_conv: &'b mut IdConverter<TyVarId, ReducIrTyVarId>,
    /// Maps type variables to debruijn index
    tyvar_env: FxHashMap<TyVarId, i32>,
}
impl<'a, 'b> LowerTyCtx<'a, 'b> {
    pub(crate) fn new(
        db: &'a dyn crate::Db,
        tyvar_conv: &'b mut IdConverter<TyVarId, ReducIrTyVarId>,
        tyvar_env: FxHashMap<TyVarId, i32>,
    ) -> Self {
        Self {
            db,
            tyvar_conv,
            tyvar_env,
        }
    }

    fn row_ir_tys<Sema: RowReducrIrEvidence>(&mut self, row: &Row<Sema>) -> RowEvIrTy
    where
        Self: RowVarConvert<Sema>,
        Sema::Open<InDb>: Copy,
    {
        match row {
            Row::Open(row_var) => {
                let var = self.lookup_row_var(row_var);
                RowEvIrTy {
                    prod: self.db.mk_reducir_ty(ProdVarTy(var)),
                    coprod: self.db.mk_reducir_ty(CoprodVarTy(var)),
                }
            }
            Row::Closed(row) => {
                let elems = row
                    .values(&self.db)
                    .iter()
                    .map(|ty| self.lower_ty(*ty))
                    .collect::<Vec<_>>();
                // Unwrap singleton rows
                if elems.len() == 1 {
                    RowEvIrTy {
                        prod: elems[0],
                        coprod: elems[0],
                    }
                } else {
                    RowEvIrTy {
                        prod: self.db.mk_prod_ty(elems.as_slice()),
                        coprod: self.db.mk_coprod_ty(elems.as_slice()),
                    }
                }
            }
        }
    }

    fn row_evidence_ir_ty(&mut self, module: Module, ev: &Evidence) -> ReducIrTy {
        let (left, right, goal) = match ev {
            Evidence::DataRow { left, right, goal } => (
                self.row_ir_tys(left),
                self.row_ir_tys(right),
                self.row_ir_tys(goal),
            ),
            Evidence::EffRow { left, right, goal } => (
                self.eff_row_into_evv_ty(module, *left),
                self.eff_row_into_evv_ty(module, *right),
                self.eff_row_into_evv_ty(module, *goal),
            ),
        };

        let branch_var_ty = self.db.mk_reducir_ty(VarTy(0));

        self.db.mk_prod_ty(&[
            self.db.mk_fun_ty([left.prod, right.prod], goal.prod),
            self.db.mk_reducir_ty(ForallTy(
                Kind::Type,
                // We introduce an internal forall here so we have to manually shift our types
                self.db.mk_fun_ty(
                    [
                        self.db
                            .mk_fun_ty([left.coprod.shift(self.db.as_ir_db(), 1)], branch_var_ty),
                        self.db
                            .mk_fun_ty([right.coprod.shift(self.db.as_ir_db(), 1)], branch_var_ty),
                        goal.coprod.shift(self.db.as_ir_db(), 1),
                    ],
                    branch_var_ty,
                ),
            )),
            self.db.mk_prod_ty(&[
                self.db.mk_fun_ty([goal.prod], left.prod),
                self.db.mk_fun_ty([left.coprod], goal.coprod),
            ]),
            self.db.mk_prod_ty(&[
                self.db.mk_fun_ty([goal.prod], right.prod),
                self.db.mk_fun_ty([right.coprod], goal.coprod),
            ]),
        ])
    }

    /// Lowers an effect row into it's corresponding reducir product type.
    /// The effect row stores a unit type for all effects, the reducir product type replaces these
    /// with the effect's handler type.
    fn eff_row_into_evv_ty(&mut self, module: Module, eff: Row<Scoped>) -> RowEvIrTy {
        match eff {
            Row::Open(v) => RowEvIrTy {
                prod: self.db.mk_reducir_ty(ProdVarTy(self.tyvar_env[&v])),
                coprod: self.db.mk_reducir_ty(CoprodVarTy(self.tyvar_env[&v])),
            },
            Row::Closed(row) => {
                let row_len = row.len(&self.db);
                let mut iter = row
                    .fields(&self.db)
                    .iter()
                    .zip(row.values(&self.db).iter())
                    .map(|(eff_id, ret_ty)| {
                        let eff = self
                            .db
                            .lookup_effect_by_name(module, *eff_id)
                            .expect("Effect Ident had no associated effect in lowering");
                        let ret_ty = self.lower_ty(*ret_ty);
                        self.db
                            .effect_handler_ir_ty(eff)
                            .reduce_forall(self.db.as_ir_db(), ret_ty)
                    });
                if row_len == 1 {
                    let elem = iter.next().unwrap();
                    RowEvIrTy {
                        prod: elem,
                        coprod: elem,
                    }
                } else {
                    let elems = iter.collect::<Vec<_>>();
                    RowEvIrTy {
                        prod: self.db.mk_prod_ty(elems.as_slice()),
                        coprod: self.db.mk_coprod_ty(elems.as_slice()),
                    }
                }
            }
        }
    }

    fn lower_row<Sema: RowReducIrKind>(&mut self, row: Row<Sema>) -> ReducIrRow
    where
        Self: RowVarConvert<Sema>,
    {
        match row {
            Row::Open(var) => ReducIrRow::Open(self.lookup_row_var(&var)),
            Row::Closed(row) => ReducIrRow::Closed(
                row.values(&self.db)
                    .iter()
                    .map(|ty| self.lower_ty(*ty))
                    .collect::<Vec<_>>(),
            ),
        }
    }

    fn lower_ty(&mut self, ty: Ty) -> ReducIrTy {
        match self.db.kind(&ty) {
            TypeKind::RowTy(_) => panic!("This should not be allowed"),
            TypeKind::ErrorTy => unreachable!(),
            TypeKind::IntTy => self.db.mk_reducir_ty(ReducIrTyKind::IntTy),
            TypeKind::VarTy(var) => self
                .db
                .mk_reducir_ty(ReducIrTyKind::VarTy(self.tyvar_env[var])),
            TypeKind::FunTy(arg, ret) => {
                let mut args = vec![self.lower_ty(*arg)];
                // gather any nested funcs into one
                let mut ret = ret;
                while let TypeKind::FunTy(arg, next) = self.db.kind(ret) {
                    args.push(self.lower_ty(*arg));
                    ret = next;
                }
                self.db.mk_fun_ty(args, self.lower_ty(*ret))
            }
            TypeKind::SumTy(Row::Open(row_var)) => self
                .db
                .mk_reducir_ty(ReducIrTyKind::CoprodVarTy(self.tyvar_env[row_var])),
            TypeKind::ProdTy(Row::Open(row_var)) => self
                .db
                .mk_reducir_ty(ReducIrTyKind::ProdVarTy(self.tyvar_env[row_var])),
            TypeKind::ProdTy(Row::Closed(row)) => {
                let elems = row
                    .values(&self.db)
                    .iter()
                    .map(|ty| self.lower_ty(*ty))
                    .collect::<Vec<_>>();
                self.db.mk_prod_ty(elems.as_slice())
            }
            TypeKind::SumTy(Row::Closed(row)) => {
                let elems = row
                    .values(&self.db)
                    .iter()
                    .map(|ty| self.lower_ty(*ty))
                    .collect::<Vec<_>>();
                self.db.mk_coprod_ty(elems.as_slice())
            }
        }
    }
}
pub(crate) trait RowVarConvert<Sema: RowReducIrKind> {
    fn convert_row_var(&mut self, row_var: Sema::Open<InDb>) -> ReducIrTyVarId;
    fn lookup_row_var(&mut self, row_var: &Sema::Open<InDb>) -> i32;
}
impl RowVarConvert<Simple> for LowerTyCtx<'_, '_> {
    fn convert_row_var(&mut self, row_var: <Simple as RowSema>::Open<InDb>) -> ReducIrTyVarId {
        self.tyvar_conv.convert(row_var)
    }

    fn lookup_row_var(&mut self, row_var: &TyVarId) -> i32 {
        self.tyvar_env[row_var]
    }
}
impl RowVarConvert<Scoped> for LowerTyCtx<'_, '_> {
    fn convert_row_var(&mut self, row_var: <Scoped as RowSema>::Open<InDb>) -> ReducIrTyVarId {
        self.tyvar_conv.convert(row_var)
    }

    fn lookup_row_var(&mut self, row_var: &TyVarId) -> i32 {
        self.tyvar_env[row_var]
    }
}

impl<'a, 'b, S> MkReducIrTy for LowerCtx<'a, 'b, S> {
    fn mk_reducir_ty(&self, kind: ReducIrTyKind) -> ReducIrTy {
        self.db.mk_reducir_ty(kind)
    }

    fn mk_prod_ty(&self, elems: &[ReducIrTy]) -> ReducIrTy {
        self.db.mk_prod_ty(elems)
    }

    fn mk_coprod_ty(&self, elems: &[ReducIrTy]) -> ReducIrTy {
        self.db.mk_coprod_ty(elems)
    }

    fn mk_fun_ty(
        &self,
        args: impl IntoIterator<Item = impl aiahr_reducir::ty::IntoReducIrTy>,
        ret: impl aiahr_reducir::ty::IntoReducIrTy,
    ) -> ReducIrTy {
        self.db.mk_fun_ty(args, ret)
    }
}

impl<'a, S> LowerCtx<'a, '_, S> {
    fn lookup_term(&self, term: Idx<Term<VarId>>) -> TyChkRes<InDb> {
        match self.current {
            ReducIrTermName::Term(name) => self.db.lookup_term(name, term),
            ReducIrTermName::Gen(_) => panic!("ICE: Called lookup term on a generated term"),
        }
    }

    fn lookup_var(&self, var_id: VarId) -> Ty {
        match self.current {
            ReducIrTermName::Term(name) => self.db.lookup_var(name, var_id),
            ReducIrTermName::Gen(_) => panic!("ICE: Called lookup var on a generated term"),
        }
    }

    fn lookup_wrapper(&self, term: Idx<Term<VarId>>) -> &'a Wrapper {
        match self.current {
            ReducIrTermName::Term(name) => self.db.lookup_wrapper(name, term),
            ReducIrTermName::Gen(_) => panic!("ICE: Called lookup wrapper on a generated term"),
        }
    }

    // TODO: Clean this up
    pub(crate) fn tyvar_conv(&mut self) -> &mut IdConverter<TyVarId, ReducIrTyVarId> {
        self.ty_ctx.tyvar_conv
    }

    pub(crate) fn evv_var(&mut self, ast: &Ast<VarId>) -> ReducIrVar {
        let term_infer = self.lookup_term(ast.root());
        ReducIrVar {
            var: ReducIrLocal {
                top_level: self.current,
                id: self.evv_var_id,
            },
            ty: match self.ty_ctx.lower_row(term_infer.eff) {
                ReducIrRow::Open(i) => self.db.mk_reducir_ty(ProdVarTy(i)),
                ReducIrRow::Closed(tys) => self.mk_reducir_ty(ProductTy(tys)),
            },
        }
    }
}

#[derive(Clone, Copy)]
pub(crate) enum RowIndx {
    Left(usize, Ty<InDb>),
    Right(usize, Ty<InDb>),
}

pub(crate) trait RowReducrIrEvidence: RowReducIrKind {
    fn merge<Db: ?Sized + crate::Db>(
        db: &Db,
        left: Self::Closed<InDb>,
        right: Self::Closed<InDb>,
    ) -> Box<[RowIndx]>;

    fn diff_left<Db: ?Sized + crate::Db>(
        db: &Db,
        goal: Self::Closed<InDb>,
        left: Self::Closed<InDb>,
    ) -> Box<[(usize, Ty<InDb>)]>;

    fn diff_right<Db: ?Sized + crate::Db>(
        db: &Db,
        goal: Self::Closed<InDb>,
        right: Self::Closed<InDb>,
    ) -> Box<[(usize, Ty<InDb>)]>;
}

impl RowReducrIrEvidence for Simple {
    fn merge<Db: ?Sized + crate::Db>(
        db: &Db,
        left: Self::Closed<InDb>,
        right: Self::Closed<InDb>,
    ) -> Box<[RowIndx]> {
        let left_fields = left.fields(&db);
        let right_fields = right.fields(&db);

        let left_indxs = left
            .values(&db)
            .iter()
            .enumerate()
            .map(|(i, ty)| RowIndx::Left(i, *ty))
            .collect::<Vec<_>>();
        let right_indxs = right
            .values(&db)
            .iter()
            .enumerate()
            .map(|(i, ty)| RowIndx::Right(i, *ty))
            .collect::<Vec<_>>();

        SimpleClosedRow::<InDb>::merge_rowlikes(
            (left_fields, left_indxs.as_slice()),
            (right_fields, right_indxs.as_slice()),
        )
        .unwrap_or_else(|_| unreachable!("ICE: Type checked simple rows should be disjoint"))
        .1
    }

    fn diff_left<Db: ?Sized + crate::Db>(
        db: &Db,
        goal: Self::Closed<InDb>,
        left: Self::Closed<InDb>,
    ) -> Box<[(usize, Ty<InDb>)]> {
        let goal_fields = goal.fields(&db);
        let goal_indxs = goal
            .values(&db)
            .iter()
            .copied()
            .enumerate()
            .collect::<Vec<_>>();
        SimpleClosedRow::<InDb>::difference_rowlikes((goal_fields, &goal_indxs), left.fields(&db)).1
    }

    fn diff_right<Db: ?Sized + crate::Db>(
        db: &Db,
        goal: Self::Closed<InDb>,
        right: Self::Closed<InDb>,
    ) -> Box<[(usize, Ty<InDb>)]> {
        let goal_fields = goal.fields(&db);
        let goal_indxs = goal
            .values(&db)
            .iter()
            .copied()
            .enumerate()
            .collect::<Vec<_>>();
        SimpleClosedRow::<InDb>::difference_rowlikes((goal_fields, &goal_indxs), right.fields(&db))
            .1
    }
}

impl RowReducrIrEvidence for Scoped {
    fn merge<Db: ?Sized + crate::Db>(
        db: &Db,
        left: Self::Closed<InDb>,
        right: Self::Closed<InDb>,
    ) -> Box<[RowIndx]> {
        let left_fields = left.fields(&db);
        let right_fields = right.fields(&db);

        let left_indxs = left
            .values(&db)
            .iter()
            .enumerate()
            .map(|(i, ty)| RowIndx::Left(i, *ty))
            .collect::<Vec<_>>();
        let right_indxs = right
            .values(&db)
            .iter()
            .enumerate()
            .map(|(i, ty)| RowIndx::Right(i, *ty))
            .collect::<Vec<_>>();

        ScopedClosedRow::<InDb>::merge_rowlikes(
            (left_fields, &left_indxs),
            (right_fields, &right_indxs),
        )
        .1
    }

    fn diff_left<Db: ?Sized + crate::Db>(
        db: &Db,
        goal: Self::Closed<InDb>,
        left: Self::Closed<InDb>,
    ) -> Box<[(usize, Ty<InDb>)]> {
        let goal_fields = goal.fields(&db);
        let goal_indxs = goal
            .values(&db)
            .iter()
            .copied()
            .enumerate()
            .collect::<Vec<_>>();

        ScopedClosedRow::<InDb>::diff_left_rowlikes((goal_fields, &goal_indxs), left.fields(&db)).1
    }

    fn diff_right<Db: ?Sized + crate::Db>(
        db: &Db,
        goal: Self::Closed<InDb>,
        right: Self::Closed<InDb>,
    ) -> Box<[(usize, Ty<InDb>)]> {
        let goal_fields = goal.fields(&db);
        let goal_indxs = goal
            .values(&db)
            .iter()
            .copied()
            .enumerate()
            .collect::<Vec<_>>();

        ScopedClosedRow::<InDb>::diff_right_rowlikes((goal_fields, &goal_indxs), right.fields(&db))
            .1
    }
}

impl<'a, 'b, S> LowerCtx<'a, 'b, S> {
    fn generate_local(&mut self) -> ReducIrLocal {
        ReducIrLocal {
            top_level: self.current,
            id: self.var_conv.generate(),
        }
    }

    pub(crate) fn row_evidence_ir<Sema: RowReducrIrEvidence>(
        &mut self,
        left: Sema::Closed<InDb>,
        right: Sema::Closed<InDb>,
        goal: Sema::Closed<InDb>,
    ) -> DelimReducIr
    where
        LowerTyCtx<'a, 'b>: RowVarConvert<Sema>,
        Sema::Closed<InDb>: Copy,
        Sema::Open<InDb>: Copy,
    {
        let left_ir = self.ty_ctx.row_ir_tys(&Row::<Sema>::Closed(left));
        let right_ir = self.ty_ctx.row_ir_tys(&Row::<Sema>::Closed(right));
        let goal_ir = self.ty_ctx.row_ir_tys(&Row::<Sema>::Closed(goal));

        let left_var_id = self.generate_local();
        let right_var_id = self.generate_local();
        let goal_var_id = self.generate_local();

        // Helpers to handle when we need to unwrap trivial single field structs
        let prj = |index, len, prod| {
            if len == 1 {
                prod
            } else {
                ReducIr::new(FieldProj(index, P::new(prod)))
            }
        };
        let inj = |index, len, ty, coprod| {
            if len == 1 {
                coprod
            } else {
                ReducIr::new(Tag(ty, index, P::new(coprod)))
            }
        };

        let indxs = Sema::merge(self.db, left, right);

        let left_len = left.len(&self.db);
        let right_len = right.len(&self.db);
        let goal_len = goal.len(&self.db);
        debug_assert_eq!(left_len + right_len, goal_len);

        let concat = {
            let left_prod_var = ReducIrVar {
                var: left_var_id,
                ty: left_ir.prod,
            };
            let right_prod_var = ReducIrVar {
                var: right_var_id,
                ty: right_ir.prod,
            };
            ReducIr::abss([left_prod_var, right_prod_var], {
                let mut elems = indxs.iter().map(|indx| match indx {
                    RowIndx::Left(i, _) => prj(*i, left_len, ReducIr::var(left_prod_var)),
                    RowIndx::Right(i, _) => prj(*i, right_len, ReducIr::var(right_prod_var)),
                });
                if goal_len == 1 {
                    elems.next().unwrap()
                } else {
                    ReducIr::new(Struct(elems.collect()))
                }
            })
        };

        let branch = {
            let branch_tyvar = ReducIrVarTy {
                var: self.ty_ctx.tyvar_conv.generate(),
                kind: Kind::Type,
            };
            let branch_var_ty = self.mk_reducir_ty(VarTy(0));
            let ir_db = self.db.as_ir_db();
            let left_coprod = left_ir.coprod.shift(self.db.as_ir_db(), 1);
            let right_coprod = right_ir.coprod.shift(self.db.as_ir_db(), 1);
            let left_branch_var = ReducIrVar {
                var: left_var_id,
                ty: self.mk_fun_ty([left_coprod], branch_var_ty),
            };
            let right_branch_var = ReducIrVar {
                var: right_var_id,
                ty: self.mk_fun_ty([right_coprod], branch_var_ty),
            };
            let goal_branch_var = ReducIrVar {
                var: goal_var_id,
                ty: goal_ir.coprod.shift(ir_db, 1),
            };
            ReducIr::new(TyAbs(
                branch_tyvar,
                P::new(ReducIr::abss(
                    [left_branch_var, right_branch_var, goal_branch_var],
                    {
                        let case_var_id = self.generate_local();
                        let mut elems = indxs.iter().map(|indx| {
                            let (i, ty, coprod_ty, length, branch_var) = match indx {
                                RowIndx::Left(i, ty) => {
                                    (i, ty, left_coprod, left_len, left_branch_var)
                                }
                                RowIndx::Right(i, ty) => {
                                    (i, ty, right_coprod, right_len, right_branch_var)
                                }
                            };

                            let case_var = ReducIrVar {
                                var: case_var_id,
                                ty: self.ty_ctx.lower_ty(*ty).shift(ir_db, 1),
                            };
                            ReducIr::abss(
                                [case_var],
                                ReducIr::app(
                                    ReducIr::var(branch_var),
                                    [inj(*i, length, coprod_ty, ReducIr::var(case_var))],
                                ),
                            )
                        });

                        if indxs.len() == 1 {
                            // Don't emit a case when we
                            ReducIr::app(elems.next().unwrap(), [ReducIr::var(goal_branch_var)])
                        } else {
                            ReducIr::case_on_var(branch_var_ty, goal_branch_var, elems)
                        }
                    },
                )),
            ))
        };

        let goal_prod_var = ReducIrVar {
            var: goal_var_id,
            ty: goal_ir.prod,
        };

        let left_indxs = Sema::diff_right(self.db, goal, right);

        let prj_l = {
            ReducIr::abss([goal_prod_var], {
                let mut elems = left_indxs
                    .iter()
                    .map(|(i, _)| prj(*i, goal_len, ReducIr::var(goal_prod_var)));
                if left_len == 1 {
                    elems.next().unwrap()
                } else {
                    ReducIr::new(Struct(elems.collect()))
                }
            })
        };
        let inj_l = {
            let left_coprod_var = ReducIrVar {
                var: left_var_id,
                ty: left_ir.coprod,
            };
            ReducIr::abss([left_coprod_var], {
                let case_var_id = self.generate_local();
                let mut elems = left_indxs.iter().map(|(i, ty)| {
                    let y = ReducIrVar {
                        var: case_var_id,
                        ty: self.ty_ctx.lower_ty(*ty),
                    };
                    ReducIr::abss([y], inj(*i, goal_len, goal_ir.coprod, ReducIr::var(y)))
                });
                if left_len == 1 {
                    ReducIr::app(elems.next().unwrap(), [ReducIr::var(left_coprod_var)])
                } else {
                    ReducIr::case_on_var(goal_ir.coprod, left_coprod_var, elems)
                }
            })
        };

        let right_indxs = Sema::diff_left(self.db, goal, left);
        let prj_r = {
            ReducIr::abss([goal_prod_var], {
                let mut elems = right_indxs
                    .iter()
                    .map(|(i, _)| prj(*i, goal_len, ReducIr::var(goal_prod_var)));
                if right_len == 1 {
                    elems.next().unwrap()
                } else {
                    ReducIr::new(Struct(elems.collect()))
                }
            })
        };
        let inj_r = {
            let right_coprod_var = ReducIrVar {
                var: right_var_id,
                ty: right_ir.coprod,
            };
            ReducIr::abss([right_coprod_var], {
                let case_var_id = self.generate_local();
                let mut elems = right_indxs.iter().map(|(i, ty)| {
                    let y = ReducIrVar {
                        var: case_var_id,
                        ty: self.ty_ctx.lower_ty(*ty),
                    };
                    ReducIr::abss([y], inj(*i, goal_len, goal_ir.coprod, ReducIr::var(y)))
                });
                if right_len == 1 {
                    ReducIr::app(elems.next().unwrap(), [ReducIr::var(right_coprod_var)])
                } else {
                    ReducIr::case_on_var(goal_ir.coprod, right_coprod_var, elems)
                }
            })
        };

        ReducIr::new(Struct(vec![
            concat,
            branch,
            ReducIr::new(Struct(vec![prj_l, inj_l])),
            ReducIr::new(Struct(vec![prj_r, inj_r])),
        ]))
    }

    pub(crate) fn lower_monadic_entry(&mut self, ir: &DelimReducIr) -> ReducIr {
        match ir.kind() {
            ReducIrKind::Abs(vars, _) => {
                match vars.iter().find(|var| var.var.id == self.evv_var_id) {
                    Some(evv_var) => self.lower_monadic(evv_var.ty, ir),
                    None => {
                        println!("{:?}", ir);
                        todo!()
                    }
                }
            }
            ReducIrKind::TyAbs(ty_var, ir) => ReducIr::new(ReducIrKind::TyAbs(
                *ty_var,
                P::new(self.lower_monadic_entry(ir)),
            )),
            ReducIrKind::TyApp(ir, ty_app) => ReducIr::new(ReducIrKind::TyApp(
                P::new(self.lower_monadic_entry(ir)),
                ty_app.clone(),
            )),
            kind => panic!("{:?}", kind),
        }
    }

    fn fresh_marker_item(&mut self) -> ReducIr {
        let ret_ty = self.mk_reducir_ty(VarTy(0));
        ReducIr::new(Item(
            ReducIrTermName::gen(self.db, "_mon_freshm", self.current.module(self.db)),
            self.mk_forall_ty(
                [Kind::Type, Kind::Type],
                self.mk_fun_ty(
                    [self.mk_fun_ty([self.mk_reducir_ty(VarTy(1))], ret_ty)],
                    ret_ty,
                ),
            ),
        ))
    }

    /// Prompt handles "installing" our prompt into the stack and running an action under an
    /// updated effect row
    fn prompt_item(&mut self) -> ReducIr {
        /*
          prompt : ∀𝜇 𝜇'. ∀𝛼 h. (𝜇 -> {Marker 𝜇 𝛼, h} -> Mon 𝜇' 𝛼) -> Marker 𝜇 𝛼 -> h -> Mon 𝜇' 𝛼 -> Mon 𝜇 𝛼
          prompt upd m h e = 𝜆w. case e (upd w {m, h}) of
            Pure x -> Pure x
            Yield m' f k ->
                case m == m' of
                    False -> Yield m' f (\x . prompt upd m h (k x))
                    True -> f (\x . prompt upd m h (k x)) w
        */
        let m = self.mk_reducir_ty(VarTy(2));
        let upd_m = self.mk_reducir_ty(VarTy(1));
        let a = self.mk_reducir_ty(VarTy(0));

        let mark = self.mk_reducir_ty(MarkerTy(a));

        let prompt_type = self.mk_forall_ty(
            [Kind::Type, Kind::Type, Kind::Type],
            self.mk_fun_ty(
                [
                    mark,
                    self.mk_fun_ty([m], upd_m),
                    self.mk_fun_ty([upd_m], self.mk_reducir_ty(ControlTy(upd_m, a))),
                ],
                self.mk_fun_ty([m], self.mk_reducir_ty(ControlTy(m, a))),
            ),
        );

        ReducIr::new(Item(
            ReducIrTermName::gen(self.db, "__mon_prompt", self.current.module(self.db)),
            prompt_type,
        ))
    }

    /// TODO: Return an item representing the bind implementation of our delimited continuation
    /// monad
    fn bind_item(&mut self) -> ReducIr {
        /*
        (e: Mon m a) |> (g : a -> Mon m b) : Mon m b =
           𝜆w. case e w of
               Pure x → g x w (monadic bind)
               Yield m f k → Yield m f (𝜆x. g x |> k)
        */
        let m = self.mk_reducir_ty(VarTy(2));
        let a = self.mk_reducir_ty(VarTy(1));
        let b = self.mk_reducir_ty(VarTy(0));
        let mon_m_b = self.mk_mon_ty(m, b);

        let bind_type = self.mk_forall_ty(
            [Kind::Type, Kind::Type, Kind::Type],
            self.mk_fun_ty(
                [self.mk_mon_ty(m, a), self.mk_fun_ty([a], mon_m_b)],
                mon_m_b,
            ),
        );
        ReducIr::new(Item(
            ReducIrTermName::gen(self.db, "__mon_bind", self.current.module(self.db)),
            bind_type,
        ))
    }

    fn bind(
        &mut self,
        ir: ReducIr,
        derive_out_ty: impl FnOnce(ReducIrTy) -> ReducIrTy,
        body: impl FnOnce(&mut Self, ReducIrVar) -> ReducIr,
    ) -> ReducIr {
        let ir_db = self.db.as_ir_db();
        let ty = ir.type_check(ir_db).map_err_pretty_with(ir_db).unwrap();
        let mon_ty = ty
            .try_unwrap_monadic(ir_db)
            .map_err_pretty_with(ir_db)
            .unwrap();
        let tmp = ReducIrVar {
            var: self.generate_local(),
            ty: mon_ty.a_ty,
        };
        let bind = self.bind_item();
        ReducIr::app(
            ReducIr::ty_app(
                bind,
                [
                    ReducIrTyApp::Ty(mon_ty.evv_ty),
                    ReducIrTyApp::Ty(mon_ty.a_ty),
                    ReducIrTyApp::Ty(derive_out_ty(mon_ty.a_ty)),
                ],
            ),
            [ir, ReducIr::abss([tmp], body(self, tmp))],
        )
    }

    /// Translate an IR term containing delimited control primitives into an IR term without those
    /// primitives that uses a delimited continuation monad to perform the primitives.
    fn lower_monadic(&mut self, evv_ty: ReducIrTy, ir: &ReducIr<DelimCont>) -> ReducIr {
        use ReducIrKind::*;
        let ir_db = self.db.as_ir_db();
        let evv_var_id = ReducIrLocal {
            top_level: self.current,
            id: self.evv_var_id,
        };
        let pure = |ir: ReducIr| {
            let ty = ir
                .type_check(ir_db)
                .expect("ICE: lower_monadic type check error");
            ReducIr::abss(
                [ReducIrVar {
                    var: evv_var_id,
                    ty: evv_ty,
                }],
                ReducIr::new(ReducIrKind::Tag(
                    ir_db.mk_reducir_ty(ControlTy(evv_ty, ty)),
                    0,
                    P::new(ir),
                )),
            )
        };
        match ir.kind() {
            Int(i) => pure(ReducIr::new(Int(*i))),
            Var(v) => ReducIr::var(*v),
            Abs(vars, body) => {
                let evv_ty = vars
                    .iter()
                    .find(|v| v.var.id == self.evv_var_id)
                    .map(|evv_var| evv_var.ty)
                    .unwrap_or(evv_ty);
                ReducIr::abss(vars.iter().copied(), self.lower_monadic(evv_ty, body))
            }
            App(func, args) => {
                let ir_db = self.db.as_ir_db();
                let bind = self.bind_item();
                let func_mon = self.lower_monadic(evv_ty, func);
                let func_ty = func_mon
                    .type_check(ir_db)
                    .map_err_pretty_with(ir_db)
                    .unwrap();

                let mut bind_args = vec![];
                let mon_args = args
                    .iter()
                    .map(|arg| {
                        let mon_arg = self.lower_monadic(evv_ty, arg);
                        let mon_arg_ty = mon_arg
                            .type_check(ir_db)
                            .map_err_pretty_with(ir_db)
                            .unwrap();
                        match mon_arg_ty.try_unwrap_monadic(ir_db) {
                            Ok(_) => {
                                let arg_ty =
                                    arg.type_check(ir_db).map_err_pretty_with(ir_db).unwrap();
                                let arg_var = ReducIrVar {
                                    var: self.generate_local(),
                                    ty: arg_ty,
                                };
                                bind_args.push((mon_arg, arg_var));
                                ReducIr::var(arg_var)
                            }
                            // We don't need to do anything for a non monadic arg
                            Err(_) => mon_arg,
                        }
                    })
                    .collect::<Vec<_>>();

                let (body, ret_ty) = match func_ty.try_fun_returns_monadic(ir_db) {
                    // Our function might take some number of argument and then return another
                    // function wrapped in our monad:
                    //    a -> b -> {evv} -> Ctl {evv} (c -> d -> e)
                    // To handle this case we split our args based on how many args appear before
                    // the monadic type. This application is then passed to bind to produce the
                    // underlying function which is applied to any remaining args and then wrapped
                    // up in our monad again.
                    Ok((arg_count, mon)) => {
                        let mut mon_args = mon_args;
                        let post_mon_args = mon_args.split_off(arg_count);
                        let pre_mon_args = mon_args;

                        let f = ReducIrVar {
                            var: self.generate_local(),
                            ty: mon.a_ty,
                        };

                        if post_mon_args.is_empty() {
                            (ReducIr::app(func_mon, pre_mon_args), mon.a_ty)
                        } else {
                            let applied_fun_ty =
                                mon.a_ty.drop_args(ir_db, post_mon_args.len()).unwrap();
                            let body = ReducIr::app(
                                ReducIr::ty_app(
                                    bind.clone(),
                                    [
                                        ReducIrTyApp::Ty(evv_ty),
                                        ReducIrTyApp::Ty(mon.a_ty),
                                        ReducIrTyApp::Ty(applied_fun_ty),
                                    ],
                                ),
                                [
                                    ReducIr::app(func_mon, pre_mon_args),
                                    ReducIr::abss([f], {
                                        let y = ReducIrVar {
                                            var: self.generate_local(),
                                            ty: applied_fun_ty,
                                        };
                                        ReducIr::local(
                                            y,
                                            ReducIr::app(ReducIr::var(f), post_mon_args),
                                            pure(ReducIr::var(y)),
                                        )
                                    }),
                                ],
                            );
                            (body, applied_fun_ty)
                        }
                    }
                    Err(ty) => {
                        // If we have no monadic args and our function isn't monadic return early
                        // with normal function application
                        if bind_args.is_empty() {
                            return ReducIr::app(func_mon, mon_args);
                        }

                        let applied_fun_ty = ty.drop_args(ir_db, mon_args.len()).unwrap();

                        // Otherwise lift our return value into our monad
                        let y = ReducIrVar {
                            var: self.generate_local(),
                            ty: applied_fun_ty,
                        };

                        let body = ReducIr::local(
                            y,
                            ReducIr::app(func_mon, mon_args),
                            pure(ReducIr::var(y)),
                        );

                        (body, applied_fun_ty)
                    }
                };

                bind_args.into_iter().rfold(body, |body, (arg, arg_var)| {
                    ReducIr::app(
                        ReducIr::ty_app(
                            bind.clone(),
                            [
                                ReducIrTyApp::Ty(evv_ty),
                                ReducIrTyApp::Ty(arg_var.ty),
                                ReducIrTyApp::Ty(ret_ty),
                            ],
                        ),
                        [arg, ReducIr::abss([arg_var], body)],
                    )
                })
            }
            TyAbs(tyvar, ir) => ReducIr::new(TyAbs(*tyvar, P::new(self.lower_monadic(evv_ty, ir)))),
            TyApp(ir, ty) => {
                ReducIr::new(TyApp(P::new(self.lower_monadic(evv_ty, ir)), ty.clone()))
            }
            Struct(elems) => {
                let mut binds = vec![];
                let mut is_mon = false;
                let vars = elems
                    .iter()
                    .map(|elem| match elem.kind() {
                        Var(v) => {
                            if v.ty.try_unwrap_monadic(ir_db).is_ok() {
                                is_mon = true;
                            }
                            *v
                        }
                        _ => {
                            let v = ReducIrVar {
                                var: self.generate_local(),
                                ty: elem.type_check(ir_db).map_err_pretty_with(ir_db).unwrap(),
                            };
                            binds.push((v, elem));
                            v
                        }
                    })
                    .collect::<Vec<_>>();
                // If all our elements are variables we just return the pure Struct
                if binds.is_empty() {
                    if is_mon {
                        return pure(ReducIr::new(Struct(
                            vars.into_iter().map(ReducIr::var).collect(),
                        )));
                    } else {
                        return ReducIr::new(Struct(vars.into_iter().map(ReducIr::var).collect()));
                    }
                }
                let anf = binds.into_iter().fold(
                    ReducIr::new(Struct(vars.into_iter().map(ReducIr::var).collect())),
                    |body, (var, defn)| ReducIr::local(var, defn.clone(), body),
                );
                self.lower_monadic(evv_ty, &anf)
            }
            FieldProj(indx, strukt) => {
                let strukt = self.lower_monadic(evv_ty, strukt);
                let ir_db = self.db.as_ir_db();
                let strukt_ty = strukt.type_check(ir_db).map_err_pretty_with(ir_db).unwrap();
                match strukt_ty.try_unwrap_monadic(ir_db) {
                    Ok(_) => self.bind(
                        strukt,
                        |ty| match ty.kind(ir_db) {
                            ProductTy(ref elems) => elems[*indx],
                            _ => unreachable!(),
                        },
                        |_, s| pure(ReducIr::field_proj(*indx, ReducIr::var(s))),
                    ),
                    // No bind required
                    Err(_) => ReducIr::field_proj(*indx, strukt),
                }
            }
            Tag(ty, tag, ir) => {
                let ir = self.lower_monadic(evv_ty, ir);
                self.bind(
                    ir,
                    |_| *ty,
                    |_, t| pure(ReducIr::new(Tag(*ty, *tag, P::new(ReducIr::var(t))))),
                )
            }
            Case(ty, disc, branches) => {
                let disc = self.lower_monadic(evv_ty, disc);
                self.bind(
                    disc,
                    |_| *ty,
                    |ctx, d| {
                        ReducIr::case_on_var(
                            ctx.mk_mon_ty(evv_ty, *ty),
                            d,
                            branches.iter().map(|b| ctx.lower_monadic(evv_ty, b)),
                        )
                    },
                )
            }
            // TODO: do we need to handle this specially? item should be lowered monadically so it
            // already returns a monad when we call it here
            Item(item, ty) => ReducIr::new(Item(*item, *ty)),
            X(DelimCont::NewPrompt(mark_var, ir)) => {
                let x = self.lower_monadic(evv_ty, ir);
                let ir = ReducIr::app(
                    ReducIr::ty_app(
                        self.fresh_marker_item(),
                        [
                            ReducIrTyApp::Ty(mark_var.ty),
                            ReducIrTyApp::Ty(
                                x.type_check(ir_db).map_err_pretty_with(ir_db).unwrap(),
                            ),
                        ],
                    ),
                    [ReducIr::abss([*mark_var], x)],
                );
                ir
            }
            X(DelimCont::Prompt(marker, upd_evv, body)) => {
                let ir_db = self.db.as_ir_db();
                let update_evv_fn_ty = upd_evv
                    .type_check(ir_db)
                    .map_err_pretty_with(ir_db)
                    .unwrap();
                // Invariant that this is a function from evv to upd_evv type.
                let (_, upd_evv_ty) = match update_evv_fn_ty.kind(ir_db) {
                    FunTy(args, ret) => (args[0], ret),
                    _ => unreachable!(),
                };
                let mon_body = self.lower_monadic(upd_evv_ty, body);
                let mon_marker = self.lower_monadic(evv_ty, marker);
                let mon_body_ty = mon_body
                    .type_check(ir_db)
                    .map_err_pretty_with(ir_db)
                    .unwrap();
                let UnwrapMonTy {
                    evv_ty: _,
                    a_ty: body_ty,
                } = match mon_body_ty.try_unwrap_monadic(ir_db) {
                    Ok(upd_mon) => upd_mon,
                    Err(_) => unreachable!(),
                };
                ReducIr::app(
                    ReducIr::ty_app(
                        self.prompt_item(),
                        [
                            ReducIrTyApp::Ty(evv_ty),
                            ReducIrTyApp::Ty(upd_evv_ty),
                            ReducIrTyApp::Ty(body_ty),
                        ],
                    ),
                    [mon_marker, upd_evv.assume_no_ext(), mon_body],
                )
            }
            X(DelimCont::Yield(ty, mark, f)) => {
                let w = ReducIrVar {
                    var: ReducIrLocal {
                        top_level: self.current,
                        id: self.evv_var_id,
                    },
                    ty: evv_ty,
                };
                let x = ReducIrVar {
                    var: ReducIrLocal {
                        top_level: self.current,
                        id: self.evv_var_id,
                    },
                    ty: *ty,
                };
                ReducIr::abss(
                    [w],
                    ReducIr::new(Tag(
                        self.mk_reducir_ty(ControlTy(evv_ty, *ty)),
                        1,
                        P::new(ReducIr::new(Struct(vec![
                            self.lower_monadic(evv_ty, mark),
                            self.lower_monadic(evv_ty, f),
                            ReducIr::abss([x], ReducIr::var(x)),
                        ]))),
                    )),
                )
            }
        }
    }
}

type LowerOutput<'a, 'b> = (
    LowerCtx<'a, 'b, Evidentfull>,
    Vec<(ReducIrVar, DelimReducIr)>,
    Vec<ReducIrVar>,
    Vec<ReducIrRowEv>,
);

impl<'a, 'b> LowerCtx<'a, 'b, Evidenceless> {
    pub(crate) fn new(
        db: &'a dyn crate::Db,
        var_conv: &'b mut IdConverter<VarId, ReducIrVarId>,
        ty_ctx: LowerTyCtx<'a, 'b>,
        current: ReducIrTermName,
    ) -> Self {
        let evv_id = var_conv.generate();
        Self {
            db,
            current,
            var_conv,
            ty_ctx,
            ev_map: EvidenceMap::default(),
            evv_var_id: evv_id,
            _marker: std::marker::PhantomData,
        }
    }

    pub(crate) fn collect_evidence_params<'ev>(
        mut self,
        ev_iter: impl Iterator<Item = &'ev Evidence>,
    ) -> LowerOutput<'a, 'b> {
        let ty_db = self.db.as_ty_db();
        let mut evs = ev_iter.collect::<Vec<_>>();

        evs.sort();
        let mut ev_items = vec![];
        let mut solved = vec![];
        let mut params = vec![];
        for ev in evs {
            let param = self.lower_evidence(ev);
            self.ev_map.insert(*ev, param);
            let (ir_item, ty_vals_iter) = match ev {
                Evidence::DataRow {
                    left: Row::Closed(left),
                    right: Row::Closed(right),
                    goal: Row::Closed(goal),
                } => {
                    let ir_row_ev = lower_row_ev(
                        self.db,
                        self.current.module(self.db),
                        left.raw_fields(),
                        right.raw_fields(),
                        goal.raw_fields(),
                    );
                    ev_items.push(ir_row_ev);
                    (
                        ir_row_ev.simple(self.db),
                        left.values(&ty_db)
                            .iter()
                            .chain(right.values(&ty_db))
                            .map(|ty| self.ty_ctx.lower_ty(*ty))
                            .collect::<Vec<_>>(),
                    )
                }
                Evidence::EffRow {
                    left: Row::Closed(left),
                    right: Row::Closed(right),
                    goal: Row::Closed(goal),
                } => {
                    let ir_row_ev = lower_row_ev(
                        self.db,
                        self.current.module(self.db),
                        left.raw_fields(),
                        right.raw_fields(),
                        goal.raw_fields(),
                    );
                    ev_items.push(ir_row_ev);
                    let left_row_iter = left.fields(&ty_db).iter().zip(left.values(&ty_db).iter());
                    let right_row_iter =
                        right.fields(&ty_db).iter().zip(right.values(&ty_db).iter());
                    let ty_vals = left_row_iter
                        .chain(right_row_iter)
                        .map(|(eff_label, eff_ret_ty)| {
                            let eff = self
                                .db
                                .lookup_effect_by_name(self.current.module(self.db), *eff_label)
                                .expect("Effect Ident had no associated effect in lowering");
                            let ret_ty = self.ty_ctx.lower_ty(*eff_ret_ty);
                            self.db
                                .effect_handler_ir_ty(eff)
                                .reduce_forall(self.db.as_ir_db(), ret_ty)
                        })
                        .collect::<Vec<_>>();
                    (ir_row_ev.scoped(self.db), ty_vals)
                }
                _ => {
                    params.push(param);
                    // Not a solved evidence so don't push an IR term
                    continue;
                }
            };
            let item = ir_item.item(self.db);
            let ir_ty = item
                .type_check(self.db.as_ir_db())
                .expect("ICE: Generated effect row evidence didn't type check");
            let ir = ReducIr::new(Item(ReducIrTermName::Gen(ir_item.name(self.db)), ir_ty));
            let ir = ty_vals_iter.into_iter().rfold(ir, |ir, ty| {
                ReducIr::new(TyApp(P::new(ir), ReducIrTyApp::Ty(ty)))
            });
            solved.push((param, ir));
        }

        (LowerCtx::with_evidenceless(self), solved, params, ev_items)
    }

    fn lower_evidence(&mut self, ev: &Evidence) -> ReducIrVar {
        let ev_term = self.generate_local();
        let row_ev_ty = self
            .ty_ctx
            .row_evidence_ir_ty(self.current.module(self.db), ev);
        ReducIrVar {
            var: ev_term,
            ty: row_ev_ty,
        }
    }
}

impl<'a, 'b> LowerCtx<'a, 'b, Evidentfull> {
    fn with_evidenceless(prior: LowerCtx<'a, 'b, Evidenceless>) -> Self {
        Self {
            db: prior.db,
            current: prior.current,
            var_conv: prior.var_conv,
            ty_ctx: prior.ty_ctx,
            ev_map: prior.ev_map,
            evv_var_id: prior.evv_var_id,
            _marker: std::marker::PhantomData,
        }
    }

    fn apply_wrapper(&mut self, wrapper: &Wrapper, ir: DelimReducIr) -> DelimReducIr {
        // This needs to be done in the reverse order that we add our Abs and TyAbs when we lower
        let ir = wrapper.tys.iter().rfold(ir, |body, ty| {
            ReducIr::new(ReducIrKind::TyApp(
                P::new(body),
                ReducIrTyApp::Ty(self.ty_ctx.lower_ty(*ty)),
            ))
        });
        let ir = wrapper.eff_rows.iter().rfold(ir, |body, row| {
            ReducIr::new(ReducIrKind::TyApp(
                P::new(body),
                ReducIrTyApp::EffRow(self.ty_ctx.lower_row(*row)),
            ))
        });
        let ir = wrapper.data_rows.iter().rfold(ir, |body, row| {
            ReducIr::new(ReducIrKind::TyApp(
                P::new(body),
                ReducIrTyApp::DataRow(self.ty_ctx.lower_row(*row)),
            ))
        });
        let ir = ReducIr::app(
            ir,
            wrapper
                .constrs
                .iter()
                .map(|ev| ReducIr::var(self.ev_map[ev])),
        );
        ir
    }

    pub(crate) fn lower_term(&mut self, ast: &Ast<VarId>, term: Idx<Term<VarId>>) -> DelimReducIr {
        use Term::*;
        match ast.view(term) {
            Unit => ReducIr::new(Struct(vec![])),
            Abstraction { arg, body } => {
                let arg_ty = self.lookup_var(*arg);
                let ty = self.ty_ctx.lower_ty(arg_ty);
                let mut vars = vec![ReducIrVar {
                    var: ReducIrLocal {
                        top_level: self.current,
                        id: self.var_conv.convert(*arg),
                    },
                    ty,
                }];
                let mut body = *body;
                while let Abstraction { arg, body: next } = ast.view(body) {
                    let arg_ty = self.lookup_var(*arg);
                    let ty = self.ty_ctx.lower_ty(arg_ty);
                    vars.push(ReducIrVar {
                        var: ReducIrLocal {
                            top_level: self.current,
                            id: self.var_conv.convert(*arg),
                        },
                        ty,
                    });
                    body = *next;
                }
                ReducIr::abss(vars, self.lower_term(ast, body))
            }
            Application { func, arg } => {
                let mut func = *func;
                let mut args = vec![self.lower_term(ast, *arg)];
                while let Application { func: next, arg } = ast.view(func) {
                    args.push(self.lower_term(ast, *arg));
                    func = *next;
                }
                ReducIr::app(self.lower_term(ast, func), args.into_iter().rev())
            }
            Variable(var) => {
                let ty = self.lookup_var(*var);
                ReducIr::new(Var(ReducIrVar {
                    var: ReducIrLocal {
                        top_level: self.current,
                        id: self.var_conv.convert(*var),
                    },
                    ty: self.ty_ctx.lower_ty(ty),
                }))
            }
            Term::Int(i) => ReducIr::new(ReducIrKind::Int(*i)),
            Item(term_name) => {
                let wrapper = self.lookup_wrapper(term);
                let scheme = self
                    .db
                    .type_scheme_of(*term_name)
                    .ty_scheme(self.db.as_tc_db());
                // Construct an ad hoc LowerTySchemeCtx to lower our scheme
                let ty_scheme_ctx = LowerTySchemeCtx::new(self.db, self.ty_ctx.tyvar_conv);
                let (ir_scheme, _) =
                    ty_scheme_ctx.lower_scheme(self.current.module(self.db), &scheme);
                let ir = ReducIr::new(ReducIrKind::Item(
                    ReducIrTermName::Term(*term_name),
                    ir_scheme,
                ));
                self.apply_wrapper(wrapper, ir)
            }
            // At this level Label/Unlabel are removed
            Label { term, .. } => self.lower_term(ast, *term),
            Unlabel { term, .. } => self.lower_term(ast, *term),
            // Row stuff
            Concat { left, right } => {
                let goal_row = expect_prod_ty(&self.db, self.lookup_term(term).ty);
                let left_row = expect_prod_ty(&self.db, self.lookup_term(*left).ty);
                let right_row = expect_prod_ty(&self.db, self.lookup_term(*right).ty);
                let ev = Evidence::DataRow {
                    left: left_row,
                    right: right_row,
                    goal: goal_row,
                };
                let param = self.ev_map[&ev];
                let concat = ReducIr::new(FieldProj(0, P::new(ReducIr::new(Var(param)))));

                ReducIr::app(
                    concat,
                    [self.lower_term(ast, *left), self.lower_term(ast, *right)],
                )
            }
            Branch { left, right } => {
                let left_row = expect_branch_ty(&self.db, self.lookup_term(*left).ty);
                let right_row = expect_branch_ty(&self.db, self.lookup_term(*right).ty);
                let goal_row = expect_branch_ty(&self.db, self.lookup_term(term).ty);

                let param = self.ev_map[&(Evidence::DataRow {
                    left: left_row,
                    right: right_row,
                    goal: goal_row,
                })];
                let branch = ReducIr::new(FieldProj(1, P::new(ReducIr::var(param))));

                ReducIr::app(
                    branch,
                    [self.lower_term(ast, *left), self.lower_term(ast, *right)],
                )
            }
            Project {
                direction,
                term: subterm,
            } => {
                let goal = expect_prod_ty(&self.db, self.lookup_term(*subterm).ty);
                let other = expect_prod_ty(&self.db, self.lookup_term(term).ty);

                let param = self.ev_map[&PartialEv::Data { goal, other }];
                let idx = match direction {
                    Direction::Left => 2,
                    Direction::Right => 3,
                };

                let prj = ReducIr::new(FieldProj(
                    0,
                    P::new(ReducIr::new(FieldProj(idx, P::new(ReducIr::var(param))))),
                ));
                ReducIr::app(prj, [self.lower_term(ast, *subterm)])
            }
            Inject {
                direction,
                term: subterm,
            } => {
                let goal = expect_sum_ty(&self.db, self.lookup_term(term).ty);
                let other = expect_sum_ty(&self.db, self.lookup_term(*subterm).ty);

                let param = self.ev_map[&PartialEv::Data { other, goal }];
                let idx = match direction {
                    Direction::Left => 2,
                    Direction::Right => 3,
                };

                let inj = ReducIr::new(FieldProj(
                    1,
                    P::new(ReducIr::new(FieldProj(idx, P::new(ReducIr::var(param))))),
                ));
                ReducIr::app(inj, [self.lower_term(ast, *subterm)])
            }
            // Effect stuff
            Operation(op) => {
                let term_infer = self.lookup_term(term);
                let (value_ty, op_ret) = term_infer
                    .ty
                    .try_as_fn_ty(&self.db)
                    .unwrap_or_else(|_| unreachable!());
                let value_var = ReducIrVar {
                    var: self.generate_local(),
                    ty: self.ty_ctx.lower_ty(value_ty),
                };
                let eff = op.effect(self.db.as_core_db());

                let eff_name = eff.name(self.db.as_core_db());
                let (eff_vals, eff_ev) = self
                    .ev_map
                    .match_right_eff_ev(
                        RowFields::new(self.db.as_ty_db(), vec![eff_name]),
                        term_infer.eff,
                    )
                    .expect("Evidence for operation to exist");

                // We know this is safe because we looked up a RowFields with one field
                let kont_ret_ty = self
                    .ty_ctx
                    .lower_ty(*eff_vals.values(self.db.as_ty_db()).first().unwrap());
                let handle_var = ReducIrVar {
                    var: self.generate_local(),
                    ty: self
                        .db
                        .effect_handler_ir_ty(eff)
                        .reduce_forall(self.db.as_ir_db(), kont_ret_ty),
                };
                let op_ret = self.ty_ctx.lower_ty(op_ret);
                let kont_var = ReducIrVar {
                    var: self.generate_local(),
                    ty: self.mk_fun_ty([op_ret], kont_ret_ty),
                };

                let prj = ReducIr::field_proj(0, ReducIr::field_proj(3, ReducIr::var(eff_ev)));
                let eff_var = ReducIrVar {
                    var: ReducIrLocal {
                        top_level: self.current,
                        id: self.evv_var_id,
                    },
                    ty: self
                        .ty_ctx
                        .eff_row_into_evv_ty(self.current.module(self.db), term_infer.eff)
                        .prod,
                };
                let eff_handler = ReducIr::app(prj, [ReducIr::var(eff_var)]);

                let handler_index = self.db.effect_handler_op_index(*op);
                let handler = ReducIr::field_proj(
                    handler_index,
                    ReducIr::field_proj(1, ReducIr::var(handle_var)),
                );
                ReducIr::abss(
                    [value_var],
                    ReducIr::local(
                        handle_var,
                        eff_handler,
                        ReducIr::ext(DelimCont::Yield(
                            op_ret,
                            P::new(ReducIr::field_proj(0, ReducIr::var(handle_var))),
                            P::new(ReducIr::abss(
                                [kont_var],
                                ReducIr::app(
                                    handler,
                                    [ReducIr::var(value_var), ReducIr::var(kont_var)],
                                ),
                            )),
                        )),
                    ),
                )
            }
            Handle { handler, body } => {
                let handler_infer = self.lookup_term(*handler);

                let handler_row = match expect_prod_ty(&self.db, handler_infer.ty) {
                    Row::Closed(row) => row,
                    Row::Open(_) => {
                        panic!("ICE: Handler should be solved to closed row during type checking")
                    }
                };
                let ret_label = self.db.ident_str("return");
                let ret_idx = handler_row
                    .fields(&self.db)
                    .binary_search(&ret_label)
                    .unwrap_or_else(|_| {
                        panic!("ICE: Type checked handler should contain 'return' field")
                    });
                let handler_ret_ty = handler_row.values(&self.db)[ret_idx];
                let handler_ret_row = self.db.single_row(ret_label, handler_ret_ty);
                let handler_ret_ev = self.ev_map[&PartialEv::Data {
                    other: Row::Closed(handler_ret_row),
                    goal: Row::Closed(handler_row),
                }];

                let handler_var = ReducIrVar {
                    var: self.generate_local(),
                    ty: self.ty_ctx.lower_ty(handler_infer.ty),
                };
                let handler_prj_ret = ReducIr::app(
                    ReducIr::field_proj(0, ReducIr::field_proj(3, ReducIr::var(handler_ret_ev))),
                    [ReducIr::var(handler_var)],
                );
                let handler_prj_sig = ReducIr::app(
                    ReducIr::field_proj(0, ReducIr::field_proj(2, ReducIr::var(handler_ret_ev))),
                    [ReducIr::var(handler_var)],
                );
                let handler_ir = self.lower_term(ast, *handler);

                let term_infer = self.lookup_term(term);

                let body_infer = self.lookup_term(*body);
                let eff_ev = self.ev_map[&Evidence::EffRow {
                    left: term_infer.eff,
                    right: handler_infer.eff,
                    goal: body_infer.eff,
                }];
                let return_ = ReducIr::app(handler_prj_ret, [self.lower_term(ast, *body)]);
                let prompt_var = ReducIrVar {
                    var: self.generate_local(),
                    ty: self
                        .mk_reducir_ty(MarkerTy(return_.type_check(self.db.as_ir_db()).unwrap())),
                };
                let update_evv = {
                    let outer_evv_var = ReducIrVar {
                        var: ReducIrLocal {
                            top_level: self.current,
                            id: self.evv_var_id,
                        },
                        ty: self
                            .ty_ctx
                            .eff_row_into_evv_ty(self.current.module(self.db), term_infer.eff)
                            .prod,
                    };
                    ReducIr::abss(
                        [outer_evv_var],
                        ReducIr::app(
                            ReducIr::new(FieldProj(0, P::new(ReducIr::var(eff_ev)))),
                            [
                                ReducIr::var(outer_evv_var),
                                ReducIr::new(Struct(vec![
                                    ReducIr::var(prompt_var),
                                    handler_prj_sig,
                                ])),
                            ],
                        ),
                    )
                };
                let install_prompt = ReducIr::ext(DelimCont::NewPrompt(
                    prompt_var,
                    P::new(
                        // Install prompt and wrap handler body in handler's return function
                        ReducIr::ext(DelimCont::Prompt(
                            P::new(ReducIr::var(prompt_var)),
                            P::new(update_evv),
                            P::new(return_),
                        )),
                    ),
                ));

                ReducIr::local(handler_var, handler_ir, install_prompt)
            }
            Annotated { term, .. } => {
                // We type checked so this is handled, we can just unwrap here.
                self.lower_term(ast, *term)
            }
        }
    }
}
