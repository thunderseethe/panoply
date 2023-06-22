use std::collections::BTreeSet;

use aiahr_ast::{Ast, Direction, Term};
use aiahr_core::id::{ReducIrTyVarId, ReducIrVarId, TermName, TyVarId, VarId};
use aiahr_reducir::{
    ty::{
        Kind, MkReducIrTy, ReducIrRow, ReducIrTy, ReducIrTyApp, ReducIrTyKind, ReducIrTyKind::*,
        ReducIrVarTy, RowReducIrKind,
    },
    ReducIr, ReducIrKind,
    ReducIrKind::*,
    ReducIrVar, P,
};
use aiahr_tc::{EffectInfo, TyChkRes};
use aiahr_ty::{
    row::{Row, RowOps, RowSema, Scoped, ScopedClosedRow, Simple, SimpleClosedRow},
    AccessTy, Evidence, InDb, MkTy, Ty, TyScheme, TypeKind, Wrapper,
};
use la_arena::Idx;

use crate::{
    evidence::{EvidenceMap, PartialEv},
    id_converter::IdConverter,
    lower_scoped_row_ev_item, lower_simple_row_ev_item, ReducIrEffectInfo,
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
    current: TermName,
    var_conv: &'b mut IdConverter<VarId, ReducIrVarId>,
    ty_ctx: LowerTyCtx<'a, 'b>,
    ev_map: EvidenceMap,
    evv_var: ReducIrVar,
    _marker: std::marker::PhantomData<State>,
}

// This exists as a standalone struct so we can construct it and lower types without having to
// construct a full LowerCtx
pub(crate) struct LowerTyCtx<'a, 'b> {
    db: &'a dyn crate::Db,
    tyvar_conv: &'b mut IdConverter<TyVarId, ReducIrTyVarId>,
}
impl<'a, 'b> LowerTyCtx<'a, 'b> {
    pub(crate) fn new(
        db: &'a dyn crate::Db,
        tyvar_conv: &'b mut IdConverter<TyVarId, ReducIrTyVarId>,
    ) -> Self {
        Self { db, tyvar_conv }
    }

    fn row_ir_tys<Sema: RowReducrIrEvidence>(&mut self, row: &Row<Sema>) -> (ReducIrTy, ReducIrTy)
    where
        Self: RowVarConvert<Sema>,
        Sema::Open<InDb>: Copy,
    {
        match row {
            Row::Open(row_var) => {
                let var = self.convert_row_var(*row_var);
                let var = ReducIrVarTy {
                    var,
                    kind: Sema::kind(),
                };
                (
                    self.db.mk_reducir_ty(ProdVarTy(var)),
                    self.db.mk_reducir_ty(CoprodVarTy(var)),
                )
            }
            Row::Closed(row) => {
                let elems = row
                    .values(&self.db)
                    .iter()
                    .map(|ty| self.lower_ty(*ty))
                    .collect::<Vec<_>>();
                // Unwrap singleton rows
                if elems.len() == 1 {
                    (elems[0], elems[0])
                } else {
                    (
                        self.db.mk_prod_ty(elems.as_slice()),
                        self.db.mk_coprod_ty(elems.as_slice()),
                    )
                }
            }
        }
    }

    fn row_evidence_ir_ty(&mut self, ev: &Evidence) -> ReducIrTy {
        let ((left_prod, left_coprod), (right_prod, right_coprod), (goal_prod, goal_coprod)) =
            match ev {
                Evidence::DataRow { left, right, goal } => (
                    self.row_ir_tys(left),
                    self.row_ir_tys(right),
                    self.row_ir_tys(goal),
                ),
                Evidence::EffRow { left, right, goal } => (
                    self.row_ir_tys(left),
                    self.row_ir_tys(right),
                    self.row_ir_tys(goal),
                ),
            };

        let branch_var = ReducIrVarTy {
            var: self.tyvar_conv.generate(),
            kind: Kind::Type,
        };
        let branch_var_ty = self.db.mk_reducir_ty(VarTy(branch_var));

        self.db.mk_prod_ty(&[
            self.db.mk_binary_fun_ty(left_prod, right_prod, goal_prod),
            self.db.mk_reducir_ty(ForallTy(
                branch_var,
                self.db.mk_binary_fun_ty(
                    FunTy(left_coprod, branch_var_ty),
                    FunTy(right_coprod, branch_var_ty),
                    FunTy(goal_coprod, branch_var_ty),
                ),
            )),
            self.db.mk_prod_ty(&[
                self.db.mk_reducir_ty(FunTy(goal_prod, left_prod)),
                self.db.mk_reducir_ty(FunTy(left_coprod, goal_coprod)),
            ]),
            self.db.mk_prod_ty(&[
                self.db.mk_reducir_ty(FunTy(goal_prod, right_prod)),
                self.db.mk_reducir_ty(FunTy(right_coprod, goal_coprod)),
            ]),
        ])
    }

    fn lower_row<Sema: RowReducIrKind>(&mut self, row: Row<Sema>) -> ReducIrRow
    where
        Self: RowVarConvert<Sema>,
    {
        match row {
            Row::Open(var) => ReducIrRow::Open(ReducIrVarTy {
                var: self.convert_row_var(var),
                kind: Sema::kind(),
            }),
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
            TypeKind::VarTy(var) => {
                let var = self.tyvar_conv.convert(*var);
                self.db.mk_reducir_ty(ReducIrTyKind::VarTy(ReducIrVarTy {
                    var,
                    kind: Kind::Type,
                }))
            }
            TypeKind::FunTy(arg, ret) => {
                let arg = self.lower_ty(*arg);
                let ret = self.lower_ty(*ret);
                self.db.mk_reducir_ty(ReducIrTyKind::FunTy(arg, ret))
            }
            TypeKind::SumTy(Row::Open(row_var)) => {
                let var = self.tyvar_conv.convert(*row_var);
                self.db
                    .mk_reducir_ty(ReducIrTyKind::CoprodVarTy(ReducIrVarTy {
                        var,
                        kind: Kind::SimpleRow,
                    }))
            }
            TypeKind::ProdTy(Row::Open(row_var)) => {
                let var = self.tyvar_conv.convert(*row_var);
                self.db
                    .mk_reducir_ty(ReducIrTyKind::ProdVarTy(ReducIrVarTy {
                        var,
                        kind: Kind::SimpleRow,
                    }))
            }
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

    pub(crate) fn lower_scheme(&mut self, scheme: &TyScheme) -> ReducIrTy {
        let ir_ty = self.lower_ty(scheme.ty);

        // Add parameter to type for each constraint
        let constrs_ty = scheme.constrs.iter().rfold(ir_ty, |ty, constr| {
            let arg = self.row_evidence_ir_ty(constr);
            self.db.mk_reducir_ty(ReducIrTyKind::FunTy(arg, ty))
        });
        // Add each type variable around type
        scheme.bound_ty.iter().rfold(constrs_ty, |ty, ty_var| {
            let ir_ty_var_id = self.tyvar_conv.convert(*ty_var);
            let var = ReducIrVarTy {
                var: ir_ty_var_id,
                kind: if scheme.bound_data_row.contains(ty_var) {
                    Kind::SimpleRow
                } else if scheme.bound_eff_row.contains(ty_var) {
                    Kind::ScopedRow
                } else {
                    Kind::Type
                },
            };
            self.db.mk_reducir_ty(ForallTy(var, ty))
        })
    }
}
pub(crate) trait RowVarConvert<Sema: RowReducIrKind> {
    fn convert_row_var(&mut self, row_var: Sema::Open<InDb>) -> ReducIrTyVarId;
}
impl RowVarConvert<Simple> for LowerTyCtx<'_, '_> {
    fn convert_row_var(&mut self, row_var: <Simple as RowSema>::Open<InDb>) -> ReducIrTyVarId {
        self.tyvar_conv.convert(row_var)
    }
}
impl RowVarConvert<Scoped> for LowerTyCtx<'_, '_> {
    fn convert_row_var(&mut self, row_var: <Scoped as RowSema>::Open<InDb>) -> ReducIrTyVarId {
        self.tyvar_conv.convert(row_var)
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
}

impl<'a, S> LowerCtx<'a, '_, S> {
    fn lookup_term(&self, term: Idx<Term<VarId>>) -> TyChkRes<InDb> {
        self.db.lookup_term(self.current, term)
    }

    fn lookup_var(&self, var_id: VarId) -> Ty {
        self.db.lookup_var(self.current, var_id)
    }

    fn lookup_wrapper(&self, term: Idx<Term<VarId>>) -> &'a Wrapper {
        self.db.lookup_wrapper(self.current, term)
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
    pub(crate) fn row_evidence_ir<Sema: RowReducrIrEvidence>(
        &mut self,
        left: Sema::Closed<InDb>,
        right: Sema::Closed<InDb>,
        goal: Sema::Closed<InDb>,
    ) -> ReducIr
    where
        LowerTyCtx<'a, 'b>: RowVarConvert<Sema>,
        Sema::Closed<InDb>: Copy,
        Sema::Open<InDb>: Copy,
    {
        let (left_prod, left_coprod) = self.ty_ctx.row_ir_tys(&Row::<Sema>::Closed(left));
        let (right_prod, right_coprod) = self.ty_ctx.row_ir_tys(&Row::<Sema>::Closed(right));
        let (goal_prod, goal_coprod) = self.ty_ctx.row_ir_tys(&Row::<Sema>::Closed(goal));

        let left_var_id = self.var_conv.generate();
        let right_var_id = self.var_conv.generate();
        let goal_var_id = self.var_conv.generate();

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
                ty: left_prod,
            };
            let right_prod_var = ReducIrVar {
                var: right_var_id,
                ty: right_prod,
            };
            P::new(ReducIr::abss([left_prod_var, right_prod_var], {
                let mut elems = indxs.iter().map(|indx| match indx {
                    RowIndx::Left(i, _) => prj(*i, left_len, ReducIr::var(left_prod_var)),
                    RowIndx::Right(i, _) => prj(*i, right_len, ReducIr::var(right_prod_var)),
                });
                if goal_len == 1 {
                    elems.next().unwrap()
                } else {
                    ReducIr::new(Struct(elems.map(P::new).collect()))
                }
            }))
        };

        let branch = {
            let branch_tyvar = ReducIrVarTy {
                var: self.ty_ctx.tyvar_conv.generate(),
                kind: Kind::Type,
            };
            let branch_var_ty = self.mk_reducir_ty(VarTy(branch_tyvar));
            let left_branch_var = ReducIrVar {
                var: left_var_id,
                ty: self.mk_reducir_ty(FunTy(left_coprod, branch_var_ty)),
            };
            let right_branch_var = ReducIrVar {
                var: right_var_id,
                ty: self.mk_reducir_ty(FunTy(right_coprod, branch_var_ty)),
            };
            let goal_branch_var = ReducIrVar {
                var: goal_var_id,
                ty: goal_coprod,
            };
            P::new(ReducIr::new(TyAbs(
                branch_tyvar,
                P::new(ReducIr::abss(
                    [left_branch_var, right_branch_var, goal_branch_var],
                    {
                        let case_var_id = self.var_conv.generate();
                        let case_ty = self.mk_reducir_ty(VarTy(branch_tyvar));
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
                                ty: self.ty_ctx.lower_ty(*ty),
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
                            elems.next().unwrap()
                        } else {
                            ReducIr::case_on_var(case_ty, goal_branch_var, elems)
                        }
                    },
                )),
            )))
        };

        let goal_prod_var = ReducIrVar {
            var: goal_var_id,
            ty: goal_prod,
        };

        let left_indxs = Sema::diff_right(self.db, goal, right);

        let prj_l = {
            P::new(ReducIr::abss([goal_prod_var], {
                let mut elems = left_indxs
                    .iter()
                    .map(|(i, _)| prj(*i, goal_len, ReducIr::var(goal_prod_var)));
                if left_len == 1 {
                    elems.next().unwrap()
                } else {
                    ReducIr::new(Struct(elems.map(P::new).collect()))
                }
            }))
        };
        let inj_l = {
            let left_coprod_var = ReducIrVar {
                var: left_var_id,
                ty: left_coprod,
            };
            P::new(ReducIr::abss([left_coprod_var], {
                let case_var_id = self.var_conv.generate();
                let mut elems = left_indxs.iter().map(|(i, ty)| {
                    let y = ReducIrVar {
                        var: case_var_id,
                        ty: self.ty_ctx.lower_ty(*ty),
                    };
                    ReducIr::abss([y], inj(*i, goal_len, goal_coprod, ReducIr::var(y)))
                });
                if left_len == 1 {
                    ReducIr::app(elems.next().unwrap(), [ReducIr::var(left_coprod_var)])
                } else {
                    ReducIr::case_on_var(goal_coprod, left_coprod_var, elems)
                }
            }))
        };

        let right_indxs = Sema::diff_left(self.db, goal, left);
        let prj_r = {
            P::new(ReducIr::abss([goal_prod_var], {
                let mut elems = right_indxs
                    .iter()
                    .map(|(i, _)| prj(*i, goal_len, ReducIr::var(goal_prod_var)));
                if right_len == 1 {
                    elems.next().unwrap()
                } else {
                    ReducIr::new(Struct(elems.map(P::new).collect()))
                }
            }))
        };
        let inj_r = {
            let right_coprod_var = ReducIrVar {
                var: right_var_id,
                ty: right_coprod,
            };
            P::new(ReducIr::abss([right_coprod_var], {
                let case_var_id = self.var_conv.generate();
                let mut elems = right_indxs.iter().map(|(i, ty)| {
                    let y = ReducIrVar {
                        var: case_var_id,
                        ty: self.ty_ctx.lower_ty(*ty),
                    };
                    ReducIr::abss([y], inj(*i, goal_len, goal_coprod, ReducIr::var(y)))
                });
                if right_len == 1 {
                    ReducIr::app(elems.next().unwrap(), [ReducIr::var(right_coprod_var)])
                } else {
                    ReducIr::case_on_var(goal_coprod, right_coprod_var, elems)
                }
            }))
        };

        ReducIr::new(Struct(vec![
            concat,
            branch,
            P::new(ReducIr::new(Struct(vec![prj_l, inj_l]))),
            P::new(ReducIr::new(Struct(vec![prj_r, inj_r]))),
        ]))
    }
}

type LowerOutput<'a, 'b> = (
    LowerCtx<'a, 'b, Evidentfull>,
    Vec<(ReducIrVar, ReducIr)>,
    Vec<ReducIrVar>,
);

impl<'a, 'b> LowerCtx<'a, 'b, Evidenceless> {
    pub(crate) fn new(
        db: &'a dyn crate::Db,
        var_conv: &'b mut IdConverter<VarId, ReducIrVarId>,
        tyvar_conv: &'b mut IdConverter<TyVarId, ReducIrTyVarId>,
        current: TermName,
    ) -> Self {
        let evv_id = var_conv.generate();
        Self {
            db,
            current,
            var_conv,
            ty_ctx: LowerTyCtx { db, tyvar_conv },
            ev_map: EvidenceMap::default(),
            evv_var: ReducIrVar {
                var: evv_id,
                // We start off with no effects by default.
                ty: db.mk_prod_ty(&[]),
            },
            _marker: std::marker::PhantomData,
        }
    }

    pub(crate) fn collect_evidence_params<'ev>(
        mut self,
        ev_iter: impl Iterator<Item = &'ev Evidence>,
    ) -> LowerOutput<'a, 'b> {
        let mut evs = ev_iter.collect::<Vec<_>>();

        evs.sort();
        let mut solved = vec![];
        let mut params = vec![];
        for ev in evs {
            let param = self.lower_evidence(ev);
            self.ev_map.insert(*ev, param);
            match ev {
                Evidence::DataRow {
                    left: Row::Closed(left),
                    right: Row::Closed(right),
                    goal: Row::Closed(goal),
                } => {
                    let ir_item = lower_simple_row_ev_item(
                        self.db,
                        self.current.module(self.db.as_core_db()),
                        *left,
                        *right,
                        *goal,
                    );
                    let item = ir_item.item(self.db);
                    let ir_ty = item
                        .type_check(self.db.as_ir_db())
                        .expect("ICE: Generated effect row evidence didn't type check");
                    let ty_db = self.db.as_ty_db();
                    let open_ty_vars = left
                        .values(&ty_db)
                        .iter()
                        .chain(right.values(&ty_db).iter())
                        .chain(goal.values(&ty_db).iter())
                        .flat_map(|ty| ty.ty_vars(ty_db))
                        .collect::<BTreeSet<_>>();
                    let ir = ReducIr::new(Item(ir_item.name(self.db), ir_ty));
                    let ir = open_ty_vars.into_iter().fold(ir, |body, ty_var| {
                        let var = ReducIrVarTy {
                            var: self.ty_ctx.tyvar_conv.convert(ty_var),
                            kind: Kind::Type,
                        };
                        ReducIr::new(TyApp(
                            P::new(body),
                            ReducIrTyApp::Ty(self.mk_reducir_ty(VarTy(var))),
                        ))
                    });
                    solved.push((param, ir));
                }
                Evidence::EffRow {
                    left: Row::Closed(left),
                    right: Row::Closed(right),
                    goal: Row::Closed(goal),
                } => {
                    let ir_item = lower_scoped_row_ev_item(
                        self.db,
                        self.current.module(self.db.as_core_db()),
                        *left,
                        *right,
                        *goal,
                    );
                    let ir_ty = ir_item
                        .item(self.db)
                        .type_check(self.db.as_ir_db())
                        .expect("ICE: Generated effect row evidence didn't type check");
                    let ty_db = self.db.as_ty_db();
                    let open_ty_vars = left
                        .values(&ty_db)
                        .iter()
                        .chain(right.values(&ty_db).iter())
                        .chain(goal.values(&ty_db).iter())
                        .flat_map(|ty| ty.ty_vars(ty_db))
                        .collect::<BTreeSet<_>>();
                    let ir = ReducIr::new(Item(ir_item.name(self.db), ir_ty));
                    let ir = open_ty_vars.into_iter().fold(ir, |body, ty_var| {
                        let var = ReducIrVarTy {
                            var: self.ty_ctx.tyvar_conv.convert(ty_var),
                            kind: Kind::Type,
                        };
                        ReducIr::new(TyApp(
                            P::new(body),
                            ReducIrTyApp::Ty(self.mk_reducir_ty(VarTy(var))),
                        ))
                    });
                    solved.push((param, ir));
                }
                _ => {
                    params.push(param);
                }
            }
        }

        (LowerCtx::with_evidenceless(self), solved, params)
    }

    fn lower_evidence(&mut self, ev: &Evidence) -> ReducIrVar {
        let ev_term = self.var_conv.generate();
        let row_ev_ty = self.ty_ctx.row_evidence_ir_ty(ev);
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
            evv_var: prior.evv_var,
            _marker: std::marker::PhantomData,
        }
    }

    pub(crate) fn lower_top_level(&mut self, ast: &Ast<VarId>, term: Idx<Term<VarId>>) -> ReducIr {
        let ir = self.lower_term(ast, term);
        ReducIr::abss([self.evv_var], ir)
    }

    fn apply_wrapper(&mut self, wrapper: &Wrapper, ir: ReducIr) -> ReducIr {
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

    fn lower_term(&mut self, ast: &Ast<VarId>, term: Idx<Term<VarId>>) -> ReducIr {
        use Term::*;
        match ast.view(term) {
            Unit => ReducIr::new(Struct(vec![])),
            Abstraction { arg, body } => {
                let term_ty = self.lookup_var(*arg);
                let ty = self.ty_ctx.lower_ty(term_ty);
                let var = ReducIrVar {
                    var: self.var_conv.convert(*arg),
                    ty,
                };
                ReducIr::new(Abs(var, P::new(self.lower_term(ast, *body))))
            }
            Application { func, arg } => ReducIr::new(App(
                P::new(self.lower_term(ast, *func)),
                P::new(self.lower_term(ast, *arg)),
            )),
            Variable(var) => {
                let ty = self.lookup_var(*var);
                ReducIr::new(Var(ReducIrVar {
                    var: self.var_conv.convert(*var),
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
                let ir = ReducIr::new(ReducIrKind::Item(
                    *term_name,
                    self.ty_ctx.lower_scheme(&scheme),
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
                    var: self.var_conv.generate(),
                    ty: self.ty_ctx.lower_ty(value_ty),
                };
                let eff = op.effect(self.db.as_core_db());
                let handle_var = ReducIrVar {
                    var: self.var_conv.generate(),
                    ty: self.db.effect_handler_ir_ty(eff),
                };
                let kont_ret_ty = match term_infer.eff {
                    Row::Open(_) => {
                        todo!("Is this case possible? If so how do we build our continuation type")
                    }
                    Row::Closed(eff_row) => {
                        let eff_name = eff.name(self.db.as_core_db());
                        let ret_ty_indx = eff_row
                            .fields(&self.db)
                            .iter()
                            .rev()
                            .position(|eff_id| *eff_id == eff_name)
                            .expect("ICE: Type checked effect should appear in effet row");
                        let ret_ty = eff_row.values(&self.db)[ret_ty_indx];
                        self.ty_ctx.lower_ty(ret_ty)
                    }
                };
                let op_ret = self.ty_ctx.lower_ty(op_ret);
                let kont_var = ReducIrVar {
                    var: self.var_conv.generate(),
                    ty: self.mk_reducir_ty(FunTy(op_ret, kont_ret_ty)),
                };

                let op_ty = self.db.effect_member_sig(*op);

                let eff_ev = self.ev_map[&PartialEv::ScopedRight {
                    right: op_ty.eff,
                    goal: term_infer.eff,
                }];
                let prj = ReducIr::field_proj(0, ReducIr::field_proj(3, ReducIr::var(eff_ev)));
                let eff_handler = ReducIr::app(prj, [ReducIr::var(self.evv_var)]);

                let handler_index = self.db.effect_handler_op_index(*op);
                let wrapper = self.lookup_wrapper(term);
                let handler = self.apply_wrapper(
                    wrapper,
                    ReducIr::field_proj(
                        handler_index,
                        ReducIr::field_proj(1, ReducIr::var(handle_var)),
                    ),
                );
                ReducIr::app(
                    ReducIr::abss(
                        [handle_var, value_var],
                        ReducIr::new(Yield(
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
                    [eff_handler],
                )
            }
            Handle { handler, body } => {
                let prompt_var = ReducIrVar {
                    var: self.var_conv.generate(),
                    // Figure out this type? Dynamically type it maybe
                    ty: self.mk_reducir_ty(IntTy),
                };
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
                let handler_ev = self.ev_map[&PartialEv::Data {
                    other: Row::Closed(handler_ret_row),
                    goal: Row::Closed(handler_row),
                }];
                let handler_var = ReducIrVar {
                    var: self.var_conv.generate(),
                    ty: self.ty_ctx.lower_ty(handler_infer.ty),
                };
                let handler_prj_ret = ReducIr::app(
                    ReducIr::field_proj(0, ReducIr::field_proj(3, ReducIr::var(handler_ev))),
                    [ReducIr::var(handler_var)],
                );
                let handler_ir = self.lower_term(ast, *handler);

                let term_infer = self.lookup_term(term);

                let body_infer = self.lookup_term(*body);
                let body_ty = self.ty_ctx.lower_ty(body_infer.ty);
                let eff_ev = self.ev_map[&Evidence::EffRow {
                    left: term_infer.eff,
                    right: handler_infer.eff,
                    goal: body_infer.eff,
                }];
                let updated_evv = ReducIr::app(
                    ReducIr::new(FieldProj(0, P::new(ReducIr::var(eff_ev)))),
                    [
                        ReducIr::var(self.evv_var),
                        ReducIr::new(Struct(vec![
                            P::new(ReducIr::var(prompt_var)),
                            P::new(ReducIr::var(handler_var)),
                        ])),
                    ],
                );
                let install_prompt = ReducIr::new(NewPrompt(
                    prompt_var,
                    P::new(ReducIr::local(
                        self.evv_var,
                        // Update evv to include the new handler
                        updated_evv,
                        // Install prompt and wrap handler body in handler's return function
                        ReducIr::new(Prompt(
                            P::new(ReducIr::var(prompt_var)),
                            P::new(ReducIr::app(
                                ReducIr::new(TyApp(
                                    P::new(handler_prj_ret),
                                    ReducIrTyApp::Ty(body_ty),
                                )),
                                [self.lower_term(ast, *body)],
                            )),
                        )),
                    )),
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
