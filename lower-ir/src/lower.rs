use aiahr_ast::{Ast, Direction, RowTerm, RowTermView, Term};
use aiahr_core::{
    id::{IrTyVarId, IrVarId, TermName, TyVarId, VarId},
    modules::Module,
};
use aiahr_ir::{
    Ir, IrKind, IrKind::*, IrTy, IrTyKind, IrTyKind::*, IrVar, IrVarTy, Kind, MkIrTy, P,
};
use aiahr_tc::{EffectInfo, TyChkRes};
use aiahr_ty::{
    row::{ClosedRow, Row},
    AccessTy, Evidence, InDb, MkTy, Ty, TyScheme, TypeKind,
};
use la_arena::Idx;
use rustc_hash::FxHashSet;

use crate::{
    evidence::{EvidenceMap, PartialEv, SolvedRowEv},
    id_converter::IdConverter,
    IrEffectInfo,
};

/// Unwrap a type into it a product and return the product's row.
///
/// Because we are lowering from a type checked AST we would've failed with a type error already if
/// this operation would fail.
fn expect_prod_ty<'a, A: ?Sized + AccessTy<'a, InDb>>(db: &A, ty: Ty) -> Row {
    ty.try_as_prod_row(db).unwrap_or_else(|_| unreachable!())
}

/// Unwrap a type into a sum and return the sum's row.
///
/// Because we are lowering from a type checked AST we would've failed with a type error already if
/// this operation would fail.
fn expect_sum_ty<'a>(db: &(impl ?Sized + AccessTy<'a, InDb>), ty: Ty) -> Row {
    ty.try_as_sum_row(db).unwrap_or_else(|_| unreachable!())
}

/// Unwrap a type as a branch type, returning the row of the branch.
/// A branch type is a `FunTy(SumTy(row), VarTy(_))` and is used as the argument to branch
/// statements. We return the `row` from that type
///
/// Because we are lowering from a type checked AST we would've failed with a type error already if
/// this operation would fail.
fn expect_branch_ty<'a>(db: &(impl ?Sized + AccessTy<'a, InDb>), ty: Ty) -> Row {
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
        let mut row_kinds = FxHashSet::default();
        // Add parameter to type for each constraint
        let constrs_ty = scheme.constrs.iter().rfold(ir_ty, |ty, constr| {
            match constr {
                Evidence::Row { left, right, goal } => {
                    if let Row::Open(ty_var) = left {
                        row_kinds.insert(ty_var);
                    }
                    if let Row::Open(ty_var) = right {
                        row_kinds.insert(ty_var);
                    }
                    if let Row::Open(ty_var) = goal {
                        row_kinds.insert(ty_var);
                    }
                }
            };
            let arg = self.row_evidence_ir_ty(constr);
            self.mk_ir_ty(IrTyKind::FunTy(arg, ty))
        });
        // Add each type variable around type
        scheme.bound.iter().rfold(constrs_ty, |ty, ty_var| {
            let ir_ty_var_id = self.tyvar_conv.convert(*ty_var);
            let var = IrVarTy {
                var: ir_ty_var_id,
                kind: if row_kinds.contains(&ty_var) {
                    Kind::Row
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
                    kind: Kind::Row,
                }))
            }
            TypeKind::ProdTy(Row::Closed(row)) => {
                let elems = self
                    .db
                    .row_values(&row.values)
                    .iter()
                    .map(|ty| self.lower_ty(*ty))
                    .collect::<Vec<_>>();
                self.mk_prod_ty(elems.as_slice())
            }
            TypeKind::SumTy(Row::Closed(row)) => {
                let elems = self
                    .db
                    .row_values(&row.values)
                    .iter()
                    .map(|ty| self.lower_ty(*ty))
                    .collect::<Vec<_>>();
                self.mk_coprod_ty(elems.as_slice())
            }
        }
    }

    fn row_evidence_ir(&mut self, left: ClosedRow, right: ClosedRow, goal: ClosedRow) -> Ir {
        let (left_prod, left_coprod) = self.row_ir_tys(&Row::Closed(left));
        let (right_prod, right_coprod) = self.row_ir_tys(&Row::Closed(right));
        let (goal_prod, goal_coprod) = self.row_ir_tys(&Row::Closed(goal));

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
                        let elems = self
                            .db
                            .row_values(&left.values)
                            .iter()
                            .chain(self.db.row_values(&right.values).iter())
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
                    self.db
                        .row_values(&left.values)
                        .iter()
                        .enumerate()
                        .map(|(i, ty)| {
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
                    self.db
                        .row_values(&right.values)
                        .iter()
                        .enumerate()
                        .map(|(i, ty)| {
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

    fn row_ir_tys(&mut self, row: &Row) -> (IrTy, IrTy) {
        match row {
            Row::Open(row_var) => {
                let var = self.tyvar_conv.convert(*row_var);
                let var = self.mk_ir_ty(VarTy(IrVarTy {
                    var,
                    kind: Kind::Row,
                }));
                (var, var)
            }
            Row::Closed(row) => {
                let elems = self
                    .db
                    .row_values(&row.values)
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
        match ev {
            Evidence::Row { left, right, goal } => {
                let (left_prod, left_coprod) = self.row_ir_tys(left);
                let (right_prod, right_coprod) = self.row_ir_tys(right);
                let (goal_prod, goal_coprod) = self.row_ir_tys(goal);

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

    fn solved_row_ev(
        &self,
        term_rows: impl IntoIterator<Item = RowTermView<VarId>>,
    ) -> Vec<SolvedRowEv> {
        // This is used to fill in the unbound row for otherwise solved Project and Inject terms.
        // Since we type-checked successfully we know nothing refers to that variable and we can use
        // whatever row type for it.
        let unit_row: ClosedRow = self.db.mk_row(&[], &[]);

        let mut solved_ev = term_rows
            .into_iter()
            .filter_map(|row_view| match row_view.view {
                RowTerm::Concat { left, right } => {
                    let left_row = expect_prod_ty(&self.db, self.lookup_term(left).ty);
                    let right_row = expect_prod_ty(&self.db, self.lookup_term(right).ty);
                    let goal_row = expect_prod_ty(&self.db, self.lookup_term(row_view.parent).ty);

                    match (left_row, right_row, goal_row) {
                        (Row::Closed(left), Row::Closed(right), Row::Closed(goal)) => {
                            Some(SolvedRowEv::new(left, right, goal))
                        }
                        _ => None,
                    }
                }
                RowTerm::Branch { left, right } => {
                    let left_row = expect_branch_ty(&self.db, self.lookup_term(left).ty);
                    let right_row = expect_branch_ty(&self.db, self.lookup_term(right).ty);
                    let goal_row = expect_branch_ty(&self.db, self.lookup_term(row_view.parent).ty);

                    match (left_row, right_row, goal_row) {
                        (Row::Closed(left), Row::Closed(right), Row::Closed(goal)) => {
                            Some(SolvedRowEv::new(left, right, goal))
                        }
                        _ => None,
                    }
                }
                RowTerm::Project { direction, term } => {
                    let sub_row = expect_prod_ty(&self.db, self.lookup_term(term).ty);
                    let goal_row = expect_prod_ty(&self.db, self.lookup_term(row_view.parent).ty);

                    match (sub_row, goal_row) {
                        (Row::Closed(sub), Row::Closed(goal)) => Some(match direction {
                            Direction::Left => SolvedRowEv::new(sub, unit_row, goal),
                            Direction::Right => SolvedRowEv::new(unit_row, sub, goal),
                        }),
                        _ => None,
                    }
                }
                RowTerm::Inject { direction, term } => {
                    let sub_row = expect_sum_ty(&self.db, self.lookup_term(term).ty);
                    let goal_row = expect_sum_ty(&self.db, self.lookup_term(row_view.parent).ty);

                    match (sub_row, goal_row) {
                        (Row::Closed(sub), Row::Closed(goal)) => Some(match direction {
                            Direction::Left => SolvedRowEv::new(sub, unit_row, goal),
                            Direction::Right => SolvedRowEv::new(unit_row, sub, goal),
                        }),
                        _ => None,
                    }
                }
            })
            .collect::<Vec<_>>();
        solved_ev.dedup();
        solved_ev
    }

    pub(crate) fn collect_evidence_params<'ev>(
        mut self,
        term_evs: impl IntoIterator<Item = RowTermView<VarId>>,
        scheme_constrs: impl IntoIterator<Item = &'ev Evidence>,
    ) -> LowerOutput<'a, 'b> {
        let locals = self
            .solved_row_ev(term_evs)
            .into_iter()
            .map(|solved| {
                let ev = solved.into();
                // We lower solved evidence so that during term lowering we can lookup any
                // evidence and receive back a variable. The logic below handles whether that
                // variable points to a concrete term or a top-level parameter.
                let param = self.lower_evidence(&ev);
                let ir = self.row_evidence_ir(solved.left, solved.right, solved.goal);
                self.ev_map.insert(ev, param);
                (param, ir)
            })
            .collect::<Vec<_>>();
        let params = scheme_constrs
            .into_iter()
            .map(|ev| {
                let param = self.lower_evidence(ev);
                self.ev_map.insert(*ev, param);
                param
            })
            .collect::<Vec<_>>();
        (LowerCtx::with_evidenceless(self), locals, params)
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

    pub(crate) fn lower_term(&mut self, ast: &Ast<VarId>, term: Idx<Term<VarId>>) -> Ir {
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
                let ev = Evidence::Row {
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

                let param = self.ev_map[&(Evidence::Row {
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

                let param = self.ev_map[&PartialEv { goal, other }];
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

                let param = self.ev_map[&PartialEv { other, goal }];
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
                let (value_ty, _) = self
                    .lookup_term(term)
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

                let handler_index = self.db.effect_handler_op_index(*op);
                let eff_index = self.db.effect_vector_index(eff);
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
                    [Ir::new(VectorGet(self.evv_var, eff_index))],
                )
            }
            Handle { handler, body } => {
                let prompt_var = IrVar {
                    var: self.var_conv.generate(),
                    ty: self.mk_ir_ty(IntTy),
                };
                let handler_infer = self.lookup_term(*handler);
                let eff_name = match handler_infer.eff {
                    Row::Closed(eff_row) => {
                        debug_assert!(eff_row.len(&self.db) == 1);
                        self.db.row_fields(&eff_row.fields)[0]
                    }
                    Row::Open(_) => {
                        unreachable!("Handler effect expect to be closed row, found row variable")
                    }
                };
                let eff_name = self
                    .db
                    .lookup_effect_by_name(self.current.module, eff_name)
                    .expect("Invalid effect name should've been caught in type checking");
                let eff_index = self.db.effect_vector_index(eff_name);
                let handler_var = IrVar {
                    var: self.var_conv.generate(),
                    ty: self.lower_ty(handler_infer.ty),
                };
                let handler_ir = self.lower_term(ast, *handler);

                let ret_ty = self.lower_ty(self.lookup_term(term).ty);

                let body_ty = self.lower_ty(self.lookup_term(*body).ty);
                let ret_index = self.db.effect_handler_return_index(eff_name);
                let updated_evv = Ir::new(VectorSet(
                    self.evv_var,
                    eff_index,
                    P::new(Ir::new(Struct(vec![
                        P::new(Ir::var(prompt_var)),
                        P::new(Ir::var(handler_var)),
                    ]))),
                ));
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
                                Ir::new(TyApp(
                                    P::new(Ir::new(FieldProj(
                                        ret_index,
                                        P::new(Ir::var(handler_var)),
                                    ))),
                                    body_ty,
                                )),
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
