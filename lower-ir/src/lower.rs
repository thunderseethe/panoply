use aiahr_ast::{Ast, Direction, Term};
use aiahr_core::{
    id::{IrTyVarId, IrVarId, TermName, TyVarId, VarId},
    modules::Module,
};
use aiahr_ir::{
    Ir, IrKind, IrKind::*, IrTy, IrTyKind, IrTyKind::*, IrVar, IrVarTy, Kind, MkIrTy, P,
};
use aiahr_tc::{EffectInfo, TyChkRes};
use aiahr_ty::{
    row::{Row, RowOps, Scoped, ScopedClosedRow, ScopedRow, Simple, SimpleClosedRow, SimpleRow},
    AccessTy, Evidence, InDb, MkTy, Ty, TyScheme, TypeKind,
};
use la_arena::Idx;

use crate::{
    evidence::{EvidenceMap, PartialEv},
    id_converter::IdConverter,
    IrEffectInfo,
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

pub trait ItemSchemes {
    fn lookup_scheme(&self, term: TermName) -> TyScheme;
}
pub trait VarTys {
    fn lookup_var(&self, term: TermName, var_id: VarId) -> Ty;
}
pub trait TermTys {
    fn lookup_term(&self, term_name: TermName, term: Idx<Term<VarId>>) -> TyChkRes<InDb>;
}

/// Selects an item based on it's module id and item id.
pub(crate) struct ItemSelector {
    pub(crate) module: Module,
    pub(crate) item: TermName,
}

// TODO: Wip name
pub(crate) struct Evidenceless;
pub(crate) struct Evidentfull;

pub(crate) struct LowerCtx<'a, 'b, State = Evidenceless> {
    db: &'a dyn crate::Db,
    current: ItemSelector,
    var_conv: &'b mut IdConverter<VarId, IrVarId>,
    tyvar_conv: &'b mut IdConverter<TyVarId, IrTyVarId>,
    ev_map: EvidenceMap,
    evv_var: IrVar,
    _marker: std::marker::PhantomData<State>,
}

impl<'a, 'b, S> MkIrTy for LowerCtx<'a, 'b, S> {
    fn mk_ir_ty(&self, kind: IrTyKind) -> IrTy {
        self.db.mk_ir_ty(kind)
    }

    fn mk_prod_ty(&self, elems: &[IrTy]) -> IrTy {
        self.db.mk_prod_ty(elems)
    }

    fn mk_coprod_ty(&self, elems: &[IrTy]) -> IrTy {
        self.db.mk_coprod_ty(elems)
    }
}

impl<'a, S> LowerCtx<'a, '_, S> {
    fn lookup_term(&self, term: Idx<Term<VarId>>) -> TyChkRes<InDb> {
        self.db.lookup_term(self.current.item, term)
    }

    fn lookup_var(&self, var_id: VarId) -> Ty {
        self.db.lookup_var(self.current.item, var_id)
    }
}

impl<'a, S> LowerCtx<'a, '_, S> {
    pub(crate) fn lower_scheme(&mut self, scheme: &TyScheme) -> IrTy {
        let ir_ty = self.lower_ty(scheme.ty);

        // Add parameter to type for each constraint
        let constrs_ty = scheme.constrs.iter().rfold(ir_ty, |ty, constr| {
            let arg = self.row_evidence_ir_ty(constr);
            self.mk_ir_ty(IrTyKind::FunTy(arg, ty))
        });
        // Add each type variable around type
        scheme.bound_ty.iter().rfold(constrs_ty, |ty, ty_var| {
            let ir_ty_var_id = self.tyvar_conv.convert(*ty_var);
            let var = IrVarTy {
                var: ir_ty_var_id,
                kind: if scheme.bound_data_row.contains(ty_var) {
                    Kind::SimpleRow
                } else if scheme.bound_eff_row.contains(ty_var) {
                    Kind::ScopedRow
                } else {
                    Kind::Type
                },
            };
            self.mk_ir_ty(ForallTy(var, ty))
        })
    }

    fn lower_ty(&mut self, ty: Ty) -> IrTy {
        match self.db.kind(&ty) {
            TypeKind::RowTy(_) => panic!("This should not be allowed"),
            TypeKind::ErrorTy => unreachable!(),
            TypeKind::IntTy => self.mk_ir_ty(IrTyKind::IntTy),
            TypeKind::VarTy(var) => {
                let var = self.tyvar_conv.convert(*var);
                self.mk_ir_ty(IrTyKind::VarTy(IrVarTy {
                    var,
                    kind: Kind::Type,
                }))
            }
            TypeKind::FunTy(arg, ret) => {
                let arg = self.lower_ty(*arg);
                let ret = self.lower_ty(*ret);
                self.mk_ir_ty(IrTyKind::FunTy(arg, ret))
            }
            TypeKind::SumTy(Row::Open(row_var)) | TypeKind::ProdTy(Row::Open(row_var)) => {
                let var = self.tyvar_conv.convert(*row_var);
                self.mk_ir_ty(IrTyKind::VarTy(IrVarTy {
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
                self.mk_prod_ty(elems.as_slice())
            }
            TypeKind::SumTy(Row::Closed(row)) => {
                let elems = row
                    .values(&self.db)
                    .iter()
                    .map(|ty| self.lower_ty(*ty))
                    .collect::<Vec<_>>();
                self.mk_coprod_ty(elems.as_slice())
            }
        }
    }

    #[allow(unused)]
    fn scoped_row_evidence_ir(
        &mut self,
        left: ScopedClosedRow,
        right: ScopedClosedRow,
        goal: ScopedClosedRow,
    ) -> Ir {
        let (left_prod, _left_coprod) = self.scoped_row_ir_tys(&ScopedRow::Closed(left));
        let (right_prod, _right_coprod) = self.scoped_row_ir_tys(&ScopedRow::Closed(right));
        let (goal_prod, _goal_coprod) = self.scoped_row_ir_tys(&ScopedRow::Closed(goal));

        let _branch_tyvar = IrVarTy {
            var: self.tyvar_conv.generate(),
            kind: Kind::Type,
        };
        let left_var_id = self.var_conv.generate();
        let right_var_id = self.var_conv.generate();
        let goal_var_id = self.var_conv.generate();

        let _left_prod_var = IrVar {
            var: left_var_id,
            ty: left_prod,
        };
        let _right_prod_var = IrVar {
            var: right_var_id,
            ty: right_prod,
        };
        let _goal_prod_var = IrVar {
            var: goal_var_id,
            ty: goal_prod,
        };

        todo!("Implement witness of scoped row evidence")
    }

    #[allow(unused)]
    fn row_evidence_ir(
        &mut self,
        left: SimpleClosedRow,
        right: SimpleClosedRow,
        goal: SimpleClosedRow,
    ) -> Ir {
        let (left_prod, left_coprod) = self.row_ir_tys(&SimpleRow::Closed(left));
        let (right_prod, right_coprod) = self.row_ir_tys(&SimpleRow::Closed(right));
        let (goal_prod, goal_coprod) = self.row_ir_tys(&SimpleRow::Closed(goal));

        let branch_tyvar = IrVarTy {
            var: self.tyvar_conv.generate(),
            kind: Kind::Type,
        };
        let left_var_id = self.var_conv.generate();
        let right_var_id = self.var_conv.generate();
        let goal_var_id = self.var_conv.generate();

        // Product combinators: Concat, PrjL, and PrjR
        let left_prod_var = IrVar {
            var: left_var_id,
            ty: left_prod,
        };
        let right_prod_var = IrVar {
            var: right_var_id,
            ty: right_prod,
        };
        let goal_prod_var = IrVar {
            var: goal_var_id,
            ty: goal_prod,
        };
        // Helper to handle when we need to unwrap trivial single field structs
        let prj = |index, len, prod| {
            if len == 1 {
                prod
            } else {
                Ir::new(FieldProj(index, P::new(prod)))
            }
        };
        let left_len = left.len(&self.db);
        let right_len = right.len(&self.db);
        let goal_len = goal.len(&self.db);
        let concat = P::new(Ir::abss(
            [left_prod_var, right_prod_var],
            Ir::new(match (left.is_empty(&self.db), right.is_empty(&self.db)) {
                (true, true) => Struct(vec![]),
                (true, false) => Var(right_prod_var),
                (false, true) => Var(left_prod_var),
                (false, false) => {
                    let left_elems =
                        (0..left_len).map(|i| prj(i, left_len, Ir::var(left_prod_var)));
                    let right_elems =
                        (0..right_len).map(|i| prj(i, right_len, Ir::var(right_prod_var)));
                    Struct(left_elems.chain(right_elems).map(P::new).collect())
                }
            }),
        ));
        let prj_l = P::new(Ir::abss(
            [goal_prod_var],
            if left_len == 1 {
                prj(0, goal_len, Ir::var(goal_prod_var))
            } else {
                Ir::new(Struct(
                    (0..left_len)
                        .map(|i| prj(i, goal_len, Ir::var(goal_prod_var)))
                        .map(P::new)
                        .collect(),
                ))
            },
        ));
        let prj_r = P::new(Ir::abss(
            [goal_prod_var],
            if right_len == 1 {
                prj(goal_len - 1, goal_len, Ir::var(goal_prod_var))
            } else {
                let range = (goal_len - right_len)..goal_len;
                Ir::new(Struct(
                    range
                        .map(|i| prj(i, goal_len, Ir::var(goal_prod_var)))
                        .map(P::new)
                        .collect(),
                ))
            },
        ));

        let left_branch_var = IrVar {
            var: left_var_id,
            ty: self.mk_ir_ty(FunTy(left_coprod, self.mk_ir_ty(VarTy(branch_tyvar)))),
        };
        let right_branch_var = IrVar {
            var: right_var_id,
            ty: self.mk_ir_ty(FunTy(right_coprod, self.mk_ir_ty(VarTy(branch_tyvar)))),
        };
        let goal_branch_var = IrVar {
            var: goal_var_id,
            ty: goal_coprod,
        };
        let inj = |i, j, e| {
            if j == 1 {
                e
            } else {
                Ir::new(Tag(i, P::new(e)))
            }
        };
        let branch = P::new(Ir::new(TyAbs(
            branch_tyvar,
            P::new(Ir::abss(
                [left_branch_var, right_branch_var, goal_branch_var],
                match (left.is_empty(&self.db), right.is_empty(&self.db)) {
                    // we're discriminating void, produce a case with no branches
                    (true, true) => Ir::case_on_var(goal_branch_var, vec![]),
                    (true, false) => Ir::app(Ir::var(left_branch_var), [Ir::var(goal_branch_var)]),
                    (false, true) => Ir::app(Ir::var(right_branch_var), [Ir::var(goal_branch_var)]),
                    (false, false) => {
                        debug_assert!(left_len + right_len == goal_len);

                        let case_var_id = self.var_conv.generate();
                        let elems = left
                            .values(&self.db)
                            .iter()
                            .chain(right.values(&self.db).iter())
                            .enumerate()
                            .map(|(i, ty)| {
                                let case_var = IrVar {
                                    var: case_var_id,
                                    ty: self.lower_ty(*ty),
                                };
                                let (branch_var, length) = if i < left_len {
                                    (left_branch_var, left_len)
                                } else {
                                    (right_branch_var, right_len)
                                };
                                Ir::abss(
                                    [case_var],
                                    Ir::app(
                                        Ir::var(branch_var),
                                        [inj(i, length, Ir::var(case_var))],
                                    ),
                                )
                            });

                        Ir::case_on_var(goal_branch_var, elems)
                    }
                },
            )),
        )));

        let left_coprod_var = IrVar {
            var: left_var_id,
            ty: left_coprod,
        };
        let right_coprod_var = IrVar {
            var: right_var_id,
            ty: right_coprod,
        };
        let inj_l = P::new(Ir::abss(
            [left_coprod_var],
            if left_len == 1 {
                inj(0, goal_len, Ir::var(left_coprod_var))
            } else {
                let case_var_id = self.var_conv.generate();
                Ir::case_on_var(
                    left_coprod_var,
                    left.values(&self.db).iter().enumerate().map(|(i, ty)| {
                        let y = IrVar {
                            var: case_var_id,
                            ty: self.lower_ty(*ty),
                        };
                        Ir::abss([y], inj(i, goal_len, Ir::var(y)))
                    }),
                )
            },
        ));
        let inj_r = P::new(Ir::abss(
            [right_coprod_var],
            if right_len == 1 {
                inj(goal_len - 1, goal_len, Ir::var(right_coprod_var))
            } else {
                let case_var_id = self.var_conv.generate();
                Ir::case_on_var(
                    right_coprod_var,
                    right.values(&self.db).iter().enumerate().map(|(i, ty)| {
                        let y = IrVar {
                            var: case_var_id,
                            ty: self.lower_ty(*ty),
                        };
                        Ir::abss([y], inj(goal_len - right_len + i, goal_len, Ir::var(y)))
                    }),
                )
            },
        ));

        Ir::new(Struct(vec![
            concat,
            branch,
            P::new(Ir::new(Struct(vec![prj_l, inj_l]))),
            P::new(Ir::new(Struct(vec![prj_r, inj_r]))),
        ]))
    }

    fn scoped_row_ir_tys(&mut self, row: &Row<Scoped>) -> (IrTy, IrTy) {
        match row {
            Row::Open(row_var) => {
                let var = self.tyvar_conv.convert(*row_var);
                let var = self.mk_ir_ty(VarTy(IrVarTy {
                    var,
                    kind: Kind::ScopedRow,
                }));
                (var, var)
            }
            Row::Closed(row) => {
                let elems = row
                    .values(&self.db)
                    .iter()
                    .map(|ty| self.lower_ty(*ty))
                    .collect::<Vec<_>>();
                (
                    self.mk_prod_ty(elems.as_slice()),
                    self.mk_coprod_ty(elems.as_slice()),
                )
            }
        }
    }

    fn row_ir_tys(&mut self, row: &Row<Simple>) -> (IrTy, IrTy) {
        match row {
            Row::Open(row_var) => {
                let var = self.tyvar_conv.convert(*row_var);
                let var = self.mk_ir_ty(VarTy(IrVarTy {
                    var,
                    kind: Kind::SimpleRow,
                }));
                (var, var)
            }
            Row::Closed(row) => {
                let elems = row
                    .values(&self.db)
                    .iter()
                    .map(|ty| self.lower_ty(*ty))
                    .collect::<Vec<_>>();
                (
                    self.mk_prod_ty(elems.as_slice()),
                    self.mk_coprod_ty(elems.as_slice()),
                )
            }
        }
    }

    fn row_evidence_ir_ty(&mut self, ev: &Evidence) -> IrTy {
        let ((left_prod, left_coprod), (right_prod, right_coprod), (goal_prod, goal_coprod)) =
            match ev {
                Evidence::DataRow { left, right, goal } => (
                    self.row_ir_tys(left),
                    self.row_ir_tys(right),
                    self.row_ir_tys(goal),
                ),
                Evidence::EffRow { left, right, goal } => (
                    self.scoped_row_ir_tys(left),
                    self.scoped_row_ir_tys(right),
                    self.scoped_row_ir_tys(goal),
                ),
            };

        let branch_var = IrVarTy {
            var: self.tyvar_conv.generate(),
            kind: Kind::Type,
        };
        let branch_var_ty = self.mk_ir_ty(VarTy(branch_var));

        self.mk_prod_ty(&[
            self.mk_binary_fun_ty(left_prod, right_prod, goal_prod),
            self.mk_ir_ty(ForallTy(
                branch_var,
                self.mk_binary_fun_ty(
                    FunTy(left_coprod, branch_var_ty),
                    FunTy(right_coprod, branch_var_ty),
                    FunTy(goal_coprod, branch_var_ty),
                ),
            )),
            self.mk_prod_ty(&[
                self.mk_ir_ty(FunTy(goal_prod, left_prod)),
                self.mk_ir_ty(FunTy(left_coprod, goal_coprod)),
            ]),
            self.mk_prod_ty(&[
                self.mk_ir_ty(FunTy(goal_prod, right_prod)),
                self.mk_ir_ty(FunTy(right_coprod, goal_coprod)),
            ]),
        ])
    }
}

type LowerOutput<'a, 'b> = (LowerCtx<'a, 'b, Evidentfull>, Vec<(IrVar, Ir)>, Vec<IrVar>);

impl<'a, 'b> LowerCtx<'a, 'b, Evidenceless> {
    pub(crate) fn new(
        db: &'a dyn crate::Db,
        var_conv: &'b mut IdConverter<VarId, IrVarId>,
        tyvar_conv: &'b mut IdConverter<TyVarId, IrTyVarId>,
        current: ItemSelector,
    ) -> Self {
        let evv_id = var_conv.generate();
        Self {
            db,
            current,
            var_conv,
            tyvar_conv,
            ev_map: EvidenceMap::default(),
            evv_var: IrVar {
                var: evv_id,
                ty: db.mk_ir_ty(EvidenceVectorTy),
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
        let params = evs
            .into_iter()
            .map(|ev| {
                let param = self.lower_evidence(ev);
                self.ev_map.insert(*ev, param);
                param
            })
            .collect::<Vec<_>>();

        (LowerCtx::with_evidenceless(self), vec![], params)
    }

    fn lower_evidence(&mut self, ev: &Evidence) -> IrVar {
        let ev_term = self.var_conv.generate();
        let row_ev_ty = self.row_evidence_ir_ty(ev);
        IrVar {
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
            tyvar_conv: prior.tyvar_conv,
            ev_map: prior.ev_map,
            evv_var: prior.evv_var,
            _marker: std::marker::PhantomData,
        }
    }

    pub(crate) fn lower_top_level(&mut self, ast: &Ast<VarId>, term: Idx<Term<VarId>>) -> Ir {
        let ir = self.lower_term(ast, term);
        Ir::abss([self.evv_var], ir)
    }

    fn lower_term(&mut self, ast: &Ast<VarId>, term: Idx<Term<VarId>>) -> Ir {
        use Term::*;
        match ast.view(term) {
            Unit => Ir::new(Struct(vec![])),
            Abstraction { arg, body } => {
                let term_ty = self.lookup_var(*arg);
                let ty = self.lower_ty(term_ty);
                let var = IrVar {
                    var: self.var_conv.convert(*arg),
                    ty,
                };
                Ir::new(Abs(var, P::new(self.lower_term(ast, *body))))
            }
            Application { func, arg } => Ir::new(App(
                P::new(self.lower_term(ast, *func)),
                P::new(self.lower_term(ast, *arg)),
            )),
            Variable(var) => {
                let ty = self.lookup_var(*var);
                Ir::new(Var(IrVar {
                    var: self.var_conv.convert(*var),
                    ty: self.lower_ty(ty),
                }))
            }
            Term::Int(i) => Ir::new(IrKind::Int(*i)),
            Item(_term_name) => todo!(),
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
                let concat = Ir::new(FieldProj(0, P::new(Ir::new(Var(param)))));

                Ir::app(
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
                let branch = Ir::new(FieldProj(1, P::new(Ir::var(param))));

                Ir::app(
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

                let prj = Ir::new(FieldProj(
                    0,
                    P::new(Ir::new(FieldProj(idx, P::new(Ir::var(param))))),
                ));
                Ir::app(prj, [self.lower_term(ast, *subterm)])
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

                let inj = Ir::new(FieldProj(
                    1,
                    P::new(Ir::new(FieldProj(idx, P::new(Ir::var(param))))),
                ));
                Ir::app(inj, [self.lower_term(ast, *subterm)])
            }
            // Effect stuff
            Operation(op) => {
                let term_infer = self.lookup_term(term);
                let (value_ty, _) = term_infer
                    .ty
                    .try_as_fn_ty(&self.db)
                    .unwrap_or_else(|_| unreachable!());
                let value_var = IrVar {
                    var: self.var_conv.generate(),
                    ty: self.lower_ty(value_ty),
                };
                let eff = op.effect(self.db.as_core_db());
                let handle_var = IrVar {
                    var: self.var_conv.generate(),
                    ty: self.db.effect_handler_ir_ty(eff),
                };
                let kont_var = IrVar {
                    var: self.var_conv.generate(),
                    // Figure out how to construct this return type
                    // I think we might be able infer it from top level term type and op sig
                    // where op sig is `op_arg -> op_ret`. We need to lean on the effect row here.
                    // We'll stash the return type of a particular handler in the effect row so
                    // that when we look up the effet row here the effect we're yielding too should
                    // hold the innermost return type and we can make use of it to know how to type
                    // our continuation.
                    ty: self.mk_ir_ty(IntTy),
                };

                let op_ty = self.db.effect_member_sig(*op);

                let eff_ev = self.ev_map[&PartialEv::ScopedRight {
                    right: op_ty.eff,
                    goal: term_infer.eff,
                }];
                let prj = Ir::new(FieldProj(
                    0,
                    P::new(Ir::new(FieldProj(3, P::new(Ir::var(eff_ev))))),
                ));
                let eff_handler = Ir::app(prj, [Ir::var(self.evv_var)]);

                let handler_index = self.db.effect_handler_op_index(*op);
                Ir::app(
                    Ir::abss(
                        [handle_var, value_var],
                        Ir::new(Yield(
                            P::new(Ir::new(FieldProj(0, P::new(Ir::var(handle_var))))),
                            P::new(Ir::abss(
                                [kont_var],
                                Ir::app(
                                    Ir::new(FieldProj(
                                        handler_index,
                                        P::new(Ir::new(FieldProj(1, P::new(Ir::var(handle_var))))),
                                    )),
                                    [Ir::var(value_var), Ir::var(kont_var)],
                                ),
                            )),
                        )),
                    ),
                    [eff_handler],
                )
            }
            Handle { handler, body } => {
                let prompt_var = IrVar {
                    var: self.var_conv.generate(),
                    // Figure out this type? Dynamically type it maybe
                    ty: self.mk_ir_ty(IntTy),
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
                        panic!("ICE: Type checked handler should contain 'return'")
                    });
                let handler_ret_ty = handler_row.values(&self.db)[ret_idx];
                let handler_ret_row = self.db.single_row(ret_label, handler_ret_ty);
                let handler_ev = self.ev_map[&PartialEv::Data {
                    other: Row::Closed(handler_ret_row),
                    goal: Row::Closed(handler_row),
                }];
                let handler_var = IrVar {
                    var: self.var_conv.generate(),
                    ty: self.lower_ty(handler_infer.ty),
                };
                let handler_prj_ret = Ir::app(
                    Ir::field_proj(0, Ir::field_proj(3, Ir::var(handler_ev))),
                    [Ir::var(handler_var)],
                );
                let handler_ir = self.lower_term(ast, *handler);

                let term_infer = self.lookup_term(term);
                let ret_ty = self.lower_ty(term_infer.ty);

                let body_infer = self.lookup_term(*body);
                let body_ty = self.lower_ty(body_infer.ty);
                let eff_ev = self.ev_map[&Evidence::EffRow {
                    left: term_infer.eff,
                    right: handler_infer.eff,
                    goal: body_infer.eff,
                }];
                let updated_evv = Ir::app(
                    Ir::new(FieldProj(0, P::new(Ir::var(eff_ev)))),
                    [
                        Ir::var(self.evv_var),
                        Ir::new(Struct(vec![
                            P::new(Ir::var(prompt_var)),
                            P::new(Ir::var(handler_var)),
                        ])),
                    ],
                );
                let install_prompt = Ir::new(NewPrompt(
                    prompt_var,
                    P::new(Ir::local(
                        self.evv_var,
                        // Update evv to include the new handler
                        updated_evv,
                        // Install prompt and wrap handler body in handler's return function
                        Ir::new(Prompt(
                            P::new(Ir::var(prompt_var)),
                            P::new(Ir::app(
                                Ir::new(TyApp(P::new(handler_prj_ret), body_ty)),
                                [self.lower_term(ast, *body)],
                            )),
                        )),
                    )),
                ));

                Ir::local(
                    handler_var,
                    Ir::new(TyApp(P::new(handler_ir), ret_ty)),
                    install_prompt,
                )
            }
            Annotated { term, .. } => {
                // We type checked so this is handled, we can just unwrap here.
                self.lower_term(ast, *term)
            }
        }
    }
}
