use std::ops::Index;

use aiahr_core::id::{EffectId, EffectOpId};
use aiahr_core::ident::Ident;
use aiahr_core::memory::intern::{Interner, InternerByRef, SyncInterner};
use aiahr_core::{
    ast::{Ast, Direction, RowTerm, RowTermView, Term},
    id::{Id, IdGen, IrTyVarId, IrVarId, ItemId, ModuleId, TyVarId, VarId},
    ir::{IrKind::*, IrTyKind::*, *},
    memory::handle::{Handle, RefHandle},
};
use aiahr_tc::{ClosedRow, EffectInfo, Evidence, Row, Ty, TyChkRes, TyScheme, TypeKind};
use bumpalo::Bump;
use rustc_hash::FxHashMap;

struct IrCtx<'ctx> {
    tys: SyncInterner<'ctx, IrTyKind<'ctx>, Bump>,
    tys_slices: SyncInterner<'ctx, [IrTy<'ctx>], Bump>,
}

impl<'ctx> IrCtx<'ctx> {
    #[allow(dead_code)]
    fn new(arena: &'ctx Bump) -> Self {
        Self {
            tys: SyncInterner::new(arena),
            tys_slices: SyncInterner::new(arena),
        }
    }
}

pub trait MkIrTy<'ctx> {
    fn mk_ir_ty(&self, kind: IrTyKind<'ctx>) -> IrTy<'ctx>;
    fn mk_prod_ty(&self, elems: &[IrTy<'ctx>]) -> IrTy<'ctx>;
    fn mk_coprod_ty(&self, elems: &[IrTy<'ctx>]) -> IrTy<'ctx>;

    fn mk_binary_fun_ty<F, S, R>(&self, fst_arg: F, snd_arg: S, ret: R) -> IrTy<'ctx>
    where
        F: IntoIrTy<'ctx>,
        S: IntoIrTy<'ctx>,
        R: IntoIrTy<'ctx>,
    {
        self.mk_ir_ty(FunTy(
            fst_arg.into_ir_ty(self),
            self.mk_ir_ty(FunTy(snd_arg.into_ir_ty(self), ret.into_ir_ty(self))),
        ))
    }
}

pub trait IntoIrTy<'ctx> {
    fn into_ir_ty<I: ?Sized + MkIrTy<'ctx>>(self, ctx: &I) -> IrTy<'ctx>;
}
impl<'ctx> IntoIrTy<'ctx> for IrTy<'ctx> {
    fn into_ir_ty<I: ?Sized + MkIrTy<'ctx>>(self, _ctx: &I) -> IrTy<'ctx> {
        self
    }
}
impl<'ctx> IntoIrTy<'ctx> for IrTyKind<'ctx> {
    fn into_ir_ty<I: ?Sized + MkIrTy<'ctx>>(self, ctx: &I) -> IrTy<'ctx> {
        ctx.mk_ir_ty(self)
    }
}

impl<'ctx> MkIrTy<'ctx> for IrCtx<'ctx> {
    fn mk_ir_ty(&self, kind: IrTyKind<'ctx>) -> IrTy<'ctx> {
        IrTy::new(self.tys.intern(kind))
    }

    fn mk_prod_ty(&self, elems: &[IrTy<'ctx>]) -> IrTy<'ctx> {
        IrTy::new(
            self.tys
                .intern(IrTyKind::ProductTy(self.tys_slices.intern_by_ref(elems))),
        )
    }

    fn mk_coprod_ty(&self, elems: &[IrTy<'ctx>]) -> IrTy<'ctx> {
        IrTy::new(
            self.tys
                .intern(IrTyKind::CoproductTy(self.tys_slices.intern_by_ref(elems))),
        )
    }
}

pub trait ItemSchemes<'ctx> {
    fn lookup_scheme(&self, module_id: ModuleId, item_id: ItemId) -> TyScheme<'ctx, TyVarId>;
}
pub trait VarTys<'ctx> {
    fn lookup_var(&self, var_id: VarId) -> Ty<'ctx, TyVarId>;
}
pub trait TermTys<'ctx> {
    fn lookup_term(&self, term: &'ctx Term<'ctx, VarId>) -> TyChkRes<'ctx, TyVarId>;
}

struct IdConverter<VarIn, VarOut> {
    cache: FxHashMap<VarIn, VarOut>,
    gen: IdGen<VarOut, ()>,
}
impl<VarIn, VarOut> IdConverter<VarIn, VarOut>
where
    VarIn: std::hash::Hash + Eq,
    VarOut: Id + Copy,
{
    fn new() -> Self {
        Self {
            cache: FxHashMap::default(),
            gen: IdGen::new(),
        }
    }

    fn convert(&mut self, var_id: VarIn) -> VarOut {
        *self
            .cache
            .entry(var_id)
            .or_insert_with(|| self.gen.push(()))
    }

    fn generate(&mut self) -> VarOut {
        self.gen.generate()
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug)]
struct PartialEv<'ctx> {
    other: Row<'ctx, TyVarId>,
    goal: Row<'ctx, TyVarId>,
}

#[derive(Default, Debug)]
struct EvidenceMap<'ctx> {
    /// Unique list of parameters we've generated so far
    params: Vec<IrVar<'ctx>>,
    // Find evidence when we only have partial information about it.
    // Like when we encounter a Project or Inject node.
    partial_map: FxHashMap<PartialEv<'ctx>, usize>,
    complete_map: FxHashMap<Evidence<'ctx, TyVarId>, usize>,
}
impl<'ctx> EvidenceMap<'ctx> {
    fn insert(&mut self, ev: Evidence<'ctx, TyVarId>, param: IrVar<'ctx>) {
        let idx = self
            .params
            .iter()
            .position(|p| p == &param)
            .unwrap_or_else(|| {
                let idx = self.params.len();
                self.params.push(param);
                idx
            });

        match &ev {
            Evidence::Row { left, right, goal } => {
                self.partial_map.insert(
                    PartialEv {
                        other: *left,
                        goal: *goal,
                    },
                    idx,
                );
                self.partial_map.insert(
                    PartialEv {
                        other: *right,
                        goal: *goal,
                    },
                    idx,
                );
            }
        }

        self.complete_map.insert(ev, idx);
    }
}
impl<'ctx> Index<&Evidence<'ctx, TyVarId>> for EvidenceMap<'ctx> {
    type Output = IrVar<'ctx>;

    fn index(&self, index: &Evidence<'ctx, TyVarId>) -> &Self::Output {
        &self.params[self.complete_map[index]]
    }
}
impl<'ctx> Index<&PartialEv<'ctx>> for EvidenceMap<'ctx> {
    type Output = IrVar<'ctx>;

    fn index(&self, index: &PartialEv<'ctx>) -> &Self::Output {
        &self.params[*self.partial_map.get(index).unwrap_or_else(|| {
            panic!(
                "Could not find partial ev: {:?} in\n{:#?}",
                index, self.partial_map
            )
        })]
    }
}

/// Unwrap a type into it a product and return the product's row.
///
/// Because we are lowering from a type checked AST we would've failed with a type error already if
/// this operation would fail.
fn expect_prod_ty<TV: Clone>(ty: Ty<'_, TV>) -> Row<'_, TV> {
    ty.try_as_prod_row().unwrap_or_else(|_| unreachable!())
}

/// Unwrap a type into a sum and return the sum's row.
///
/// Because we are lowering from a type checked AST we would've failed with a type error already if
/// this operation would fail.
fn expect_sum_ty<TV: Clone>(ty: Ty<'_, TV>) -> Row<'_, TV> {
    ty.try_as_sum_row().unwrap_or_else(|_| unreachable!())
}

/// Unwrap a type as a branch type, returning the row of the branch.
/// A branch type is a `FunTy(SumTy(row), VarTy(_))` and is used as the argument to branch
/// statements. We return the `row` from that type
///
/// Because we are lowering from a type checked AST we would've failed with a type error already if
/// this operation would fail.
fn expect_branch_ty<TV: Clone>(ty: Ty<'_, TV>) -> Row<'_, TV> {
    ty.try_as_fn_ty()
        .and_then(|(arg, _)| arg.try_as_sum_row())
        .unwrap_or_else(|_| unreachable!())
}

/// Row evidence where every row is closed.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
struct SolvedRowEv<'ctx> {
    goal: ClosedRow<'ctx, TyVarId>,
    left: ClosedRow<'ctx, TyVarId>,
    right: ClosedRow<'ctx, TyVarId>,
}
impl<'ctx> From<SolvedRowEv<'ctx>> for Evidence<'ctx, TyVarId> {
    fn from(val: SolvedRowEv<'ctx>) -> Self {
        Evidence::Row {
            left: Row::Closed(val.left),
            right: Row::Closed(val.right),
            goal: Row::Closed(val.goal),
        }
    }
}
impl<'ctx> SolvedRowEv<'ctx> {
    fn new(
        left: ClosedRow<'ctx, TyVarId>,
        right: ClosedRow<'ctx, TyVarId>,
        goal: ClosedRow<'ctx, TyVarId>,
    ) -> Self {
        Self { goal, left, right }
    }
}

// TODO: Wip name
struct Evidenceless;
struct Evidentfull;

struct LowerCtx<'a, 'ctx, Db, I, State = Evidenceless> {
    db: &'a Db,
    ctx: &'a I,
    var_conv: &'a mut IdConverter<VarId, IrVarId>,
    tyvar_conv: &'a mut IdConverter<TyVarId, IrTyVarId>,
    ev_map: EvidenceMap<'ctx>,
    evv_var: IrVar<'ctx>,
    _marker: std::marker::PhantomData<State>,
}

impl<'ctx, Db, I, S> LowerCtx<'_, 'ctx, Db, I, S>
where
    I: MkIrTy<'ctx>,
{
    fn lower_ty(&mut self, ty: Ty<'_, TyVarId>) -> IrTy<'ctx> {
        match *ty {
            TypeKind::RowTy(_) => panic!("This should not be allowed"),
            TypeKind::ErrorTy => unreachable!(),
            TypeKind::IntTy => self.ctx.mk_ir_ty(IrTyKind::IntTy),
            TypeKind::VarTy(var) => self.ctx.mk_ir_ty(IrTyKind::VarTy(IrVarTy {
                var: self.tyvar_conv.convert(var),
                kind: Kind::Type,
            })),
            TypeKind::FunTy(arg, ret) => self
                .ctx
                .mk_ir_ty(IrTyKind::FunTy(self.lower_ty(arg), self.lower_ty(ret))),
            TypeKind::SumTy(Row::Open(row_var)) | TypeKind::ProdTy(Row::Open(row_var)) => {
                self.ctx.mk_ir_ty(IrTyKind::VarTy(IrVarTy {
                    var: self.tyvar_conv.convert(row_var),
                    kind: Kind::Row,
                }))
            }
            TypeKind::ProdTy(Row::Closed(row)) => {
                let elems = row
                    .values
                    .iter()
                    .map(|ty| self.lower_ty(*ty))
                    .collect::<Vec<_>>();
                self.ctx.mk_prod_ty(elems.as_slice())
            }
            TypeKind::SumTy(Row::Closed(row)) => {
                let elems = row
                    .values
                    .iter()
                    .map(|ty| self.lower_ty(*ty))
                    .collect::<Vec<_>>();
                self.ctx.mk_coprod_ty(elems.as_slice())
            }
        }
    }

    fn row_evidence_ir(
        &mut self,
        left: ClosedRow<'ctx, TyVarId>,
        right: ClosedRow<'ctx, TyVarId>,
        goal: ClosedRow<'ctx, TyVarId>,
    ) -> Ir<'ctx> {
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
        let concat = P::new(Ir::abss(
            [left_prod_var, right_prod_var],
            Ir::new(match (left.fields.is_empty(), right.fields.is_empty()) {
                (true, true) => Struct(vec![]),
                (true, false) => Var(right_prod_var),
                (false, true) => Var(left_prod_var),
                (false, false) => {
                    let left_elems =
                        (0..left.len()).map(|i| prj(i, left.len(), Ir::var(left_prod_var)));
                    let right_elems =
                        (0..right.len()).map(|i| prj(i, right.len(), Ir::var(right_prod_var)));
                    Struct(left_elems.chain(right_elems).map(P::new).collect())
                }
            }),
        ));
        let prj_l = P::new(Ir::abss(
            [goal_prod_var],
            if left.len() == 1 {
                prj(0, goal.len(), Ir::var(goal_prod_var))
            } else {
                Ir::new(Struct(
                    (0..left.len())
                        .map(|i| prj(i, goal.len(), Ir::var(goal_prod_var)))
                        .map(P::new)
                        .collect(),
                ))
            },
        ));
        let prj_r = P::new(Ir::abss(
            [goal_prod_var],
            if right.len() == 1 {
                prj(goal.len() - 1, goal.len(), Ir::var(goal_prod_var))
            } else {
                let range = (goal.len() - right.len())..goal.len();
                Ir::new(Struct(
                    range
                        .map(|i| prj(i, goal.len(), Ir::var(goal_prod_var)))
                        .map(P::new)
                        .collect(),
                ))
            },
        ));

        let left_branch_var = IrVar {
            var: left_var_id,
            ty: self
                .ctx
                .mk_ir_ty(FunTy(left_coprod, self.ctx.mk_ir_ty(VarTy(branch_tyvar)))),
        };
        let right_branch_var = IrVar {
            var: right_var_id,
            ty: self
                .ctx
                .mk_ir_ty(FunTy(right_coprod, self.ctx.mk_ir_ty(VarTy(branch_tyvar)))),
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
                match (left.fields.is_empty(), right.fields.is_empty()) {
                    // we're discriminating void, produce a case with no branches
                    (true, true) => Ir::case_on_var(goal_branch_var, vec![]),
                    (true, false) => Ir::app(Ir::var(left_branch_var), [Ir::var(goal_branch_var)]),
                    (false, true) => Ir::app(Ir::var(right_branch_var), [Ir::var(goal_branch_var)]),
                    (false, false) => {
                        debug_assert!(left.len() + right.len() == goal.len());

                        let case_var_id = self.var_conv.generate();
                        let elems = left
                            .values
                            .iter()
                            .chain(right.values.iter())
                            .enumerate()
                            .map(|(i, ty)| {
                                let case_var = IrVar {
                                    var: case_var_id,
                                    ty: self.lower_ty(*ty),
                                };
                                let length = if i < left.len() {
                                    left.len()
                                } else {
                                    right.len()
                                };
                                Ir::abss(
                                    [case_var],
                                    Ir::app(Ir::var(case_var), [inj(i, length, Ir::var(case_var))]),
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
            if left.len() == 1 {
                inj(0, goal.len(), Ir::var(left_coprod_var))
            } else {
                let case_var_id = self.var_conv.generate();
                Ir::case_on_var(
                    left_coprod_var,
                    left.values.iter().enumerate().map(|(i, ty)| {
                        let y = IrVar {
                            var: case_var_id,
                            ty: self.lower_ty(*ty),
                        };
                        Ir::abss([y], inj(i, goal.len(), Ir::var(y)))
                    }),
                )
            },
        ));
        let inj_r = P::new(Ir::abss(
            [right_coprod_var],
            if right.len() == 1 {
                inj(goal.len() - 1, goal.len(), Ir::var(right_coprod_var))
            } else {
                let case_var_id = self.var_conv.generate();
                Ir::case_on_var(
                    right_coprod_var,
                    right.values.iter().enumerate().map(|(i, ty)| {
                        let y = IrVar {
                            var: case_var_id,
                            ty: self.lower_ty(*ty),
                        };
                        Ir::abss(
                            [y],
                            inj(goal.len() - right.len() + i, goal.len(), Ir::var(y)),
                        )
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

    fn row_ir_tys(&mut self, row: &Row<'ctx, TyVarId>) -> (IrTy<'ctx>, IrTy<'ctx>) {
        match row {
            Row::Open(row_var) => {
                let var = self.ctx.mk_ir_ty(VarTy(IrVarTy {
                    var: self.tyvar_conv.convert(*row_var),
                    kind: Kind::Row,
                }));
                (var, var)
            }
            Row::Closed(row) => {
                let elems = row
                    .values
                    .iter()
                    .map(|ty| self.lower_ty(*ty))
                    .collect::<Vec<_>>();
                (
                    self.ctx.mk_prod_ty(elems.as_slice()),
                    self.ctx.mk_coprod_ty(elems.as_slice()),
                )
            }
        }
    }

    fn row_evidence_ir_ty(&mut self, ev: &Evidence<'ctx, TyVarId>) -> IrTy<'ctx> {
        match ev {
            Evidence::Row { left, right, goal } => {
                let (left_prod, left_coprod) = self.row_ir_tys(left);
                let (right_prod, right_coprod) = self.row_ir_tys(right);
                let (goal_prod, goal_coprod) = self.row_ir_tys(goal);

                let branch_var = IrVarTy {
                    var: self.tyvar_conv.generate(),
                    kind: Kind::Type,
                };
                let branch_var_ty = self.ctx.mk_ir_ty(VarTy(branch_var));

                self.ctx.mk_prod_ty(&[
                    self.ctx.mk_binary_fun_ty(left_prod, right_prod, goal_prod),
                    self.ctx.mk_ir_ty(ForallTy(
                        branch_var,
                        self.ctx.mk_binary_fun_ty(
                            FunTy(left_coprod, branch_var_ty),
                            FunTy(right_coprod, branch_var_ty),
                            FunTy(goal_coprod, branch_var_ty),
                        ),
                    )),
                    self.ctx.mk_prod_ty(&[
                        self.ctx.mk_ir_ty(FunTy(goal_prod, left_prod)),
                        self.ctx.mk_ir_ty(FunTy(left_coprod, goal_coprod)),
                    ]),
                    self.ctx.mk_prod_ty(&[
                        self.ctx.mk_ir_ty(FunTy(goal_prod, right_prod)),
                        self.ctx.mk_ir_ty(FunTy(right_coprod, goal_coprod)),
                    ]),
                ])
            }
        }
    }
}

impl<'a, 'ctx, Db, I> LowerCtx<'a, 'ctx, Db, I, Evidenceless>
where
    Db: ItemSchemes<'ctx> + VarTys<'ctx> + TermTys<'ctx> + IrEffectInfo<'ctx>,
    I: MkIrTy<'ctx>,
{
    pub fn new(
        db: &'a Db,
        ctx: &'a I,
        var_conv: &'a mut IdConverter<VarId, IrVarId>,
        tyvar_conv: &'a mut IdConverter<TyVarId, IrTyVarId>,
    ) -> Self {
        let evv_id = var_conv.generate();
        Self {
            db,
            ctx,
            var_conv,
            tyvar_conv,
            ev_map: EvidenceMap::default(),
            evv_var: IrVar {
                var: evv_id,
                ty: ctx.mk_ir_ty(EvidenceVectorTy),
            },
            _marker: std::marker::PhantomData,
        }
    }

    fn solved_row_ev<'ev>(
        &self,
        term_rows: impl IntoIterator<Item = RowTermView<'ctx, VarId>>,
    ) -> Vec<SolvedRowEv<'ctx>>
    where
        'ctx: 'ev,
    {
        // This is used to fill in the unbound row for otherwise solved Project and Inject terms.
        // Since we type-checked successfully we know nothing refers to that variable and we can use
        // whatever row type for it.
        let unit_row: ClosedRow<'ctx, TyVarId> = ClosedRow {
            fields: Handle(&[]),
            values: Handle(&[]),
        };

        let mut solved_ev = term_rows
            .into_iter()
            .filter_map(|row_view| match row_view.view {
                RowTerm::Concat { left, right } => {
                    let left_row = expect_prod_ty(self.db.lookup_term(left).ty);
                    let right_row = expect_prod_ty(self.db.lookup_term(right).ty);
                    let goal_row = expect_prod_ty(self.db.lookup_term(row_view.parent).ty);

                    match (left_row, right_row, goal_row) {
                        (Row::Closed(left), Row::Closed(right), Row::Closed(goal)) => {
                            Some(SolvedRowEv::new(left, right, goal))
                        }
                        _ => None,
                    }
                }
                RowTerm::Branch { left, right } => {
                    let left_row = expect_branch_ty(self.db.lookup_term(left).ty);
                    let right_row = expect_branch_ty(self.db.lookup_term(right).ty);
                    let goal_row = expect_branch_ty(self.db.lookup_term(row_view.parent).ty);

                    match (left_row, right_row, goal_row) {
                        (Row::Closed(left), Row::Closed(right), Row::Closed(goal)) => {
                            Some(SolvedRowEv::new(left, right, goal))
                        }
                        _ => None,
                    }
                }
                RowTerm::Project { direction, term } => {
                    let sub_row = expect_prod_ty(self.db.lookup_term(term).ty);
                    let goal_row = expect_prod_ty(self.db.lookup_term(row_view.parent).ty);

                    match (sub_row, goal_row) {
                        (Row::Closed(sub), Row::Closed(goal)) => Some(match direction {
                            Direction::Left => SolvedRowEv::new(sub, unit_row, goal),
                            Direction::Right => SolvedRowEv::new(unit_row, sub, goal),
                        }),
                        _ => None,
                    }
                }
                RowTerm::Inject { direction, term } => {
                    let sub_row = expect_sum_ty(self.db.lookup_term(term).ty);
                    let goal_row = expect_sum_ty(self.db.lookup_term(row_view.parent).ty);

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

    fn collect_evidence_params<'ev>(
        mut self,
        term_evs: impl IntoIterator<Item = RowTermView<'ctx, VarId>>,
        scheme_constrs: impl IntoIterator<Item = &'ev Evidence<'ctx, TyVarId>>,
    ) -> (
        LowerCtx<'a, 'ctx, Db, I, Evidentfull>,
        Vec<(IrVar<'ctx>, Ir<'ctx>)>,
        Vec<IrVar<'ctx>>,
    )
    where
        'ctx: 'ev,
    {
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

    fn lower_evidence(&mut self, ev: &Evidence<'ctx, TyVarId>) -> IrVar<'ctx> {
        let ev_term = self.var_conv.generate();
        let row_ev_ty = self.row_evidence_ir_ty(ev);
        IrVar {
            var: ev_term,
            ty: row_ev_ty,
        }
    }
}

impl<'a, 'ctx, Db, I> LowerCtx<'a, 'ctx, Db, I, Evidentfull>
where
    Db: ItemSchemes<'ctx> + VarTys<'ctx> + TermTys<'ctx> + IrEffectInfo<'ctx>,
    I: MkIrTy<'ctx>,
{
    fn with_evidenceless(prior: LowerCtx<'a, 'ctx, Db, I, Evidenceless>) -> Self {
        Self {
            db: prior.db,
            ctx: prior.ctx,
            var_conv: prior.var_conv,
            tyvar_conv: prior.tyvar_conv,
            ev_map: prior.ev_map,
            evv_var: prior.evv_var,
            _marker: std::marker::PhantomData,
        }
    }

    fn lower_term(&mut self, term: &'ctx Term<'ctx, VarId>) -> Ir<'ctx> {
        use Term::*;
        match term {
            Unit => Ir::new(Struct(vec![])),
            Abstraction { arg, body } => {
                let term_ty = self.db.lookup_var(*arg);
                let ty = self.lower_ty(term_ty);
                let var = IrVar {
                    var: self.var_conv.convert(*arg),
                    ty,
                };
                Ir::new(Abs(var, P::new(self.lower_term(body))))
            }
            Application { func, arg } => Ir::new(App(
                P::new(self.lower_term(func)),
                P::new(self.lower_term(arg)),
            )),
            Variable(var) => {
                let ty = self.db.lookup_var(*var);
                Ir::new(Var(IrVar {
                    var: self.var_conv.convert(*var),
                    ty: self.lower_ty(ty),
                }))
            }
            Term::Int(i) => Ir::new(IrKind::Int(*i)),
            Item((_module_id, _item_id)) => todo!(),
            // At this level Label/Unlabel are removed
            Label { term, .. } => self.lower_term(term),
            Unlabel { term, .. } => self.lower_term(term),
            // Row stuff
            Concat { left, right } => {
                let goal_row = expect_prod_ty(self.db.lookup_term(term).ty);
                let left_row = expect_prod_ty(self.db.lookup_term(left).ty);
                let right_row = expect_prod_ty(self.db.lookup_term(right).ty);
                let ev = Evidence::Row {
                    left: left_row,
                    right: right_row,
                    goal: goal_row,
                };
                let param = self.ev_map[&ev];
                let concat = Ir::new(FieldProj(0, P::new(Ir::new(Var(param)))));

                Ir::app(concat, [self.lower_term(left), self.lower_term(right)])
            }
            Branch { left, right } => {
                let left_row = expect_branch_ty(self.db.lookup_term(left).ty);
                let right_row = expect_branch_ty(self.db.lookup_term(right).ty);
                let goal_row = expect_branch_ty(self.db.lookup_term(term).ty);

                let param = self.ev_map[&(Evidence::Row {
                    left: left_row,
                    right: right_row,
                    goal: goal_row,
                })];
                let branch = Ir::new(FieldProj(1, P::new(Ir::var(param))));

                Ir::app(branch, [self.lower_term(left), self.lower_term(right)])
            }
            Project {
                direction,
                term: subterm,
            } => {
                let goal = expect_prod_ty(self.db.lookup_term(subterm).ty);
                let other = expect_prod_ty(self.db.lookup_term(term).ty);

                let param = self.ev_map[&PartialEv { goal, other }];
                let idx = match direction {
                    Direction::Left => 2,
                    Direction::Right => 3,
                };

                let prj = Ir::new(FieldProj(
                    0,
                    P::new(Ir::new(FieldProj(idx, P::new(Ir::var(param))))),
                ));
                Ir::app(prj, [self.lower_term(subterm)])
            }
            Inject {
                direction,
                term: subterm,
            } => {
                let goal = expect_sum_ty(self.db.lookup_term(term).ty);
                let other = expect_sum_ty(self.db.lookup_term(subterm).ty);

                let param = self.ev_map[&PartialEv { other, goal }];
                let idx = match direction {
                    Direction::Left => 2,
                    Direction::Right => 3,
                };

                let inj = Ir::new(FieldProj(
                    1,
                    P::new(Ir::new(FieldProj(idx, P::new(Ir::var(param))))),
                ));
                Ir::app(inj, [self.lower_term(subterm)])
            }
            // Effect stuff
            Operation(op) => {
                let eff_id = self.db.lookup_effect_by_member(*op);
                let (value_ty, _) = self
                    .db
                    .lookup_term(term)
                    .ty
                    .try_as_fn_ty()
                    .unwrap_or_else(|_| unreachable!());
                let value_var = IrVar {
                    var: self.var_conv.generate(),
                    ty: self.lower_ty(value_ty),
                };
                let handle_var = IrVar {
                    var: self.var_conv.generate(),
                    ty: self.db.effect_handler_ir_ty(eff_id),
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
                    ty: self.ctx.mk_ir_ty(IntTy),
                };

                let handler_index = self.db.effect_member_op_index(eff_id, *op);
                let eff_index = self.db.effect_vector_index(eff_id);
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
                    ty: self.ctx.mk_ir_ty(IntTy),
                };
                let handler_infer = self.db.lookup_term(handler);
                let eff_name = match handler_infer.eff {
                    Row::Closed(eff_row) => {
                        debug_assert!(eff_row.len() == 1);
                        eff_row.fields[0]
                    }
                    Row::Open(_) => {
                        unreachable!("Handler effect expect to be closed row, found row variable")
                    }
                };
                let eff = self
                    .db
                    .lookup_effect_by_name(eff_name)
                    .expect("Invalid effect name should've been caught in type checking");
                let eff_index = self.db.effect_vector_index(eff);
                let handler_var = IrVar {
                    var: self.var_conv.generate(),
                    ty: self.lower_ty(handler_infer.ty),
                };
                let handler_ir = self.lower_term(handler);

                let ret_ty = self.lower_ty(self.db.lookup_term(term).ty);

                let body_ty = self.lower_ty(self.db.lookup_term(body).ty);
                let ret_index = self.db.effect_handler_return_index(eff);
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
                                [self.lower_term(body)],
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
        }
    }
}

/// Slightly lower level of information than required by EffectInfo.
/// However this is all calculatable off of the effect definition
pub trait IrEffectInfo<'ctx> {
    fn lookup_effect_by_member(&self, op_id: EffectOpId) -> EffectId;

    fn lookup_effect_by_name(&self, name: Ident) -> Option<EffectId>;

    fn effect_handler_return_index(&self, eff_id: EffectId) -> usize;
    fn effect_member_op_index(&self, eff_id: EffectId, op_id: EffectOpId) -> usize;
    fn effect_vector_index(&self, eff_id: EffectId) -> usize;

    fn effect_handler_ir_ty(&self, eff_id: EffectId) -> IrTy<'ctx>;
}

/// Lower an `Ast` into an `Ir`.
/// TODO: Real documentation.
pub fn lower<'ctx, Db, I>(
    db: &Db,
    ctx: &I,
    scheme: &TyScheme<'ctx, TyVarId>,
    ast: &Ast<'ctx, VarId>,
) -> Ir<'ctx>
where
    Db: ItemSchemes<'ctx> + VarTys<'ctx> + TermTys<'ctx> + IrEffectInfo<'ctx>,
    I: MkIrTy<'ctx>,
{
    let mut var_conv = IdConverter::new();
    let mut tyvar_conv = IdConverter::new();
    let (mut lower_ctx, ev_locals, ev_params) =
        LowerCtx::new(db, ctx, &mut var_conv, &mut tyvar_conv)
            .collect_evidence_params(ast.root().row_ev_terms(), scheme.constrs.iter());

    let body = lower_ctx.lower_term(ast.root());
    // Bind all unique solved row evidence to local variables at top of the term
    let body = ev_locals.into_iter().fold(body, |body, (ev_param, ev_ir)| {
        Ir::app(Ir::abss([ev_param], body), [ev_ir])
    });
    // Add unsolved row evidence as parameters of the term
    let body = ev_params
        .into_iter()
        .rfold(body, |body, arg| Ir::new(Abs(arg, P::new(body))));

    // Finally wrap our term in any type variables it needs to bind
    scheme.bound.iter().rfold(body, |acc, ty_var| {
        Ir::new(TyAbs(
            IrVarTy {
                var: tyvar_conv.convert(*ty_var),
                kind: Kind::Type,
            },
            P::new(acc),
        ))
    })
}

pub mod test_utils {
    use aiahr_tc::test_utils::DummyEff;
    use aiahr_tc::TyChkRes;

    use super::*;

    macro_rules! ir_ty {
        ($ty:expr) => {{
            IrTy(Handle(&$ty))
        }};
    }

    pub struct LowerDb<'a, 'ctx> {
        var_tys: FxHashMap<VarId, Ty<'ctx, TyVarId>>,
        term_tys: FxHashMap<&'ctx Term<'ctx, VarId>, TyChkRes<'ctx, TyVarId>>,
        eff_info: DummyEff<'a>,
    }

    impl<'a, 'ctx> LowerDb<'a, 'ctx> {
        pub fn new(
            db: &'a dyn aiahr_tc::Db,
            var_tys: FxHashMap<VarId, Ty<'ctx, TyVarId>>,
            term_tys: FxHashMap<&'ctx Term<'ctx, VarId>, TyChkRes<'ctx, TyVarId>>,
        ) -> Self {
            Self {
                var_tys,
                term_tys,
                eff_info: DummyEff(db),
            }
        }
    }
    impl<'ctx> VarTys<'ctx> for LowerDb<'_, 'ctx> {
        fn lookup_var(&self, var_id: VarId) -> Ty<'ctx, TyVarId> {
            self.var_tys[&var_id]
        }
    }
    impl<'ctx> TermTys<'ctx> for LowerDb<'_, 'ctx> {
        fn lookup_term(&self, term: &'ctx Term<'ctx, VarId>) -> TyChkRes<'ctx, TyVarId> {
            self.term_tys[term]
        }
    }
    impl<'ctx> ItemSchemes<'ctx> for LowerDb<'_, 'ctx> {
        fn lookup_scheme(&self, _module_id: ModuleId, _item_id: ItemId) -> TyScheme<'ctx, TyVarId> {
            todo!()
        }
    }
    impl<'ctx> EffectInfo<'ctx, 'ctx> for LowerDb<'_, 'ctx> {
        fn effect_name(&self, eff: aiahr_core::id::EffectId) -> Ident {
            self.eff_info.effect_name(eff)
        }

        fn effect_members(
            &self,
            eff: aiahr_core::id::EffectId,
        ) -> RefHandle<'static, [aiahr_core::id::EffectOpId]> {
            self.eff_info.effect_members(eff)
        }

        fn lookup_effect_by_member(
            &self,
            member: aiahr_core::id::EffectOpId,
        ) -> aiahr_core::id::EffectId {
            self.eff_info.lookup_effect_by_member(member)
        }

        fn effect_member_sig(
            &self,
            eff: aiahr_core::id::EffectId,
            member: aiahr_core::id::EffectOpId,
        ) -> TyScheme<'static, TyVarId> {
            self.eff_info.effect_member_sig(eff, member)
        }

        fn effect_member_name(
            &self,
            eff: aiahr_core::id::EffectId,
            member: aiahr_core::id::EffectOpId,
        ) -> Ident {
            self.eff_info.effect_member_name(eff, member)
        }

        fn lookup_effect_by_member_names<'a>(&self, members: &[Ident]) -> Option<EffectId> {
            self.eff_info.lookup_effect_by_member_names(members)
        }

        fn lookup_effect_by_name(&self, name: Ident) -> Option<EffectId> {
            self.eff_info.lookup_effect_by_name(name)
        }
    }

    impl<'ctx> IrEffectInfo<'ctx> for LowerDb<'_, 'ctx> {
        fn lookup_effect_by_member(&self, op_id: EffectOpId) -> EffectId {
            self.eff_info.lookup_effect_by_member(op_id)
        }

        fn effect_handler_return_index(&self, eff_id: EffectId) -> usize {
            match eff_id {
                DummyEff::STATE_ID => 2,
                DummyEff::READER_ID => 1,
                _ => unimplemented!(),
            }
        }

        fn effect_member_op_index(&self, _eff_id: EffectId, op_id: EffectOpId) -> usize {
            match op_id {
                DummyEff::GET_ID | DummyEff::ASK_ID => 0,
                DummyEff::PUT_ID => 1,
                _ => unimplemented!(),
            }
        }

        fn effect_vector_index(&self, eff_id: EffectId) -> usize {
            match eff_id {
                DummyEff::STATE_ID => 0,
                DummyEff::READER_ID => 1,
                _ => unimplemented!(),
            }
        }

        fn effect_handler_ir_ty(&self, eff_id: EffectId) -> IrTy<'ctx> {
            // Find a better solution for this
            // Guessing a big enough irvartyid that we won't hit it in tests is flaky.
            static R: IrVarTy = IrVarTy {
                var: IrTyVarId(1024),
                kind: Kind::Type,
            };
            static A: IrVarTy = IrVarTy {
                var: IrTyVarId(1025),
                kind: Kind::Type,
            };
            static RET_TY: IrTy<'static> = ir_ty!(VarTy(R));
            static UNIT_TY: IrTy<'static> = ir_ty!(ProductTy(Handle(&[])));
            static INT_TY: IrTy<'static> = ir_ty!(IntTy);
            static UNIT_KONT_TY: IrTy<'static> = ir_ty!(FunTy(UNIT_TY, RET_TY));
            static INT_KONT_TY: IrTy<'static> = ir_ty!(FunTy(INT_TY, RET_TY));

            static RETURN_TY: IrTy<'static> = ir_ty!(ForallTy(
                A,
                ir_ty!(FunTy(ir_ty!(VarTy(A)), ir_ty!(VarTy(R))))
            ));
            static STATE_HANDLER_TY: IrTy<'static> = ir_ty!(ForallTy(
                R,
                ir_ty!(ProductTy(Handle(&[
                    ir_ty!(FunTy(UNIT_TY, ir_ty!(FunTy(UNIT_KONT_TY, RET_TY)))),
                    ir_ty!(FunTy(INT_TY, ir_ty!(FunTy(INT_KONT_TY, RET_TY)))),
                    RETURN_TY
                ])))
            ));

            static READER_HANDLER_TY: IrTy<'static> = ir_ty!(ForallTy(
                R,
                ir_ty!(ProductTy(Handle(&[
                    ir_ty!(FunTy(UNIT_TY, ir_ty!(FunTy(UNIT_KONT_TY, RET_TY)))),
                    RETURN_TY
                ])))
            ));

            match eff_id {
                DummyEff::STATE_ID => STATE_HANDLER_TY,
                DummyEff::READER_ID => READER_HANDLER_TY,
                _ => unimplemented!(),
            }
        }

        fn lookup_effect_by_name(&self, name: Ident) -> Option<EffectId> {
            self.eff_info.lookup_effect_by_name(name)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Deref;

    use super::{test_utils::LowerDb, *};
    use aiahr_analysis::names::Names;
    use aiahr_analysis::resolve::resolve_term;
    use aiahr_analysis::top_level::BaseBuilder;
    use aiahr_core::memory::intern::{InternerByRef, SyncInterner};
    use aiahr_core::modules::ModuleTree;
    use aiahr_core::AsCoreDb;
    use aiahr_tc::test_utils::DummyEff;
    use aiahr_test::ast::*;
    use assert_matches::assert_matches;
    use bumpalo::Bump;
    use ir_matcher::ir_matcher;

    const MODNAME: &str = "test_module";

    #[derive(Default)]
    #[salsa::db(aiahr_core::Jar, aiahr_tc::Jar, aiahr_desugar::Jar)]
    struct TestDatabase {
        storage: salsa::Storage<Self>,
    }
    impl salsa::Database for TestDatabase {}
    impl AsCoreDb for TestDatabase {
        fn as_core_db<'a>(&'a self) -> &'a dyn aiahr_core::Db {
            <TestDatabase as salsa::DbWithJar<aiahr_core::Jar>>::as_jar_db(self)
        }
    }

    /// Compile an input string up to (but not including) the lower stage.
    fn compile_upto_lower<'a, 'ctx, S>(
        db: &'a TestDatabase,
        arena: &'ctx Bump,
        interner: &'ctx S,
        ty_ctx: &aiahr_tc::TyCtx<'ctx, TyVarId>,
        input: &str,
    ) -> (LowerDb<'a, 'ctx>, TyScheme<'ctx, TyVarId>, Ast<'ctx, VarId>)
    where
        S: InternerByRef<'ctx, str>,
    {
        let (m, modules) = {
            let mut modules = ModuleTree::new();
            let m = modules.add_package(interner.intern_by_ref(MODNAME));
            (m, modules)
        };

        let unresolved = aiahr_parser::parser::test_utils::parse_term(arena, interner, input);
        let mut errors: Vec<aiahr_core::diagnostic::nameres::NameResolutionError<'_>> = Vec::new();

        let mut module_names = FxHashMap::default();
        let base = BaseBuilder::new().build(arena, m, &modules, &mut module_names);
        let mut names = Names::new(&base);

        let resolved = resolve_term(arena, unresolved, &mut names, &mut errors)
            .expect("Name resolution to succeed");

        let mut vars = names
            .into_vars()
            .into_iter()
            .map(|span_of| db.ident(span_of.value.0))
            .collect();

        let ast = aiahr_desugar::desugar(db, arena, &mut vars, resolved).unwrap();

        let infer_ctx = aiahr_tc::TyCtx::new(db, arena);
        let (var_tys, term_tys, scheme, _) =
            aiahr_tc::type_check(db, ty_ctx, &infer_ctx, &DummyEff(db), &ast);
        (LowerDb::new(db, var_tys, term_tys), scheme, ast)
    }

    #[test]
    fn lower_id() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let interner = SyncInterner::new(&arena);
        let ty_ctx = aiahr_tc::TyCtx::new(&db, &arena);
        let (db, scheme, ast) = compile_upto_lower(&db, &arena, &interner, &ty_ctx, "|x| x");
        let ir_ctx = IrCtx::new(&arena);
        let ir = lower(&db, &ir_ctx, &scheme, &ast);

        ir_matcher!(ir, TyAbs([ty_var], Abs([var], Var(var))) => {
            assert_eq!(var.ty.0.0, &VarTy(*ty_var));
        })
    }

    #[test]
    fn lower_product_literal() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let interner = SyncInterner::new(&arena);
        let ty_ctx = aiahr_tc::TyCtx::new(&db, &arena);
        let (db, scheme, ast) =
            compile_upto_lower(&db, &arena, &interner, &ty_ctx, "|a| { x = a, y = a }");

        let ir_ctx = IrCtx::new(&arena);
        let ir = lower(&db, &ir_ctx, &scheme, &ast);
        ir_matcher!(ir,
            TyAbs([_ty_var],
                  App([
                      Abs([ev, a], App([FieldProj(0, Var(ev)), Var(a), Var(a)])),
                      Struct(ev_terms)])) => {
            let ir = &ev_terms[0];
            ir_matcher!(ir, Abs([m, n], Struct(splat)) => {
                assert_matches!(splat[0].deref().kind, Var(_m) => { assert_eq!(*m, _m); });
                assert_matches!(splat[1].deref().kind, Var(_n) => { assert_eq!(*n, _n); });
            });
        });
    }

    #[test]
    fn lower_wand() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let ty_ctx = aiahr_tc::TyCtx::new(&db, &arena);
        let infer_ctx = aiahr_tc::TyCtx::new(&db, &arena);
        let ir_ctx = IrCtx::new(&arena);
        let m = VarId(0);
        let n = VarId(1);
        let ast = AstBuilder::with_builder(&db, &arena, |builder| {
            builder.mk_abss(
                [m, n],
                builder.mk_unlabel(
                    "x",
                    builder.mk_project(
                        Direction::Left,
                        builder.mk_concat(Term::Variable(m), Term::Variable(n)),
                    ),
                ),
            )
        });
        let (var_tys, term_ress, scheme, _) = aiahr_tc::type_check(
            &db,
            &ty_ctx,
            &infer_ctx,
            &aiahr_tc::test_utils::DummyEff(&db),
            &ast,
        );

        let ir = lower(
            &LowerDb::new(&db, var_tys, term_ress),
            &ir_ctx,
            &scheme,
            &ast,
        );

        ir_matcher!(
            ir,
            TyAbs(
                [_a, _b, _c, _d, _e],
                Abs(
                    [w, x, y, z],
                    App([
                        FieldProj(0, FieldProj(2, Var(w))),
                        App([FieldProj(0, Var(x)), Var(y), Var(z)])
                    ])
                )
            )
        );
    }
}
