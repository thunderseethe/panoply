use std::ops::Index;

use aiahr_core::ast::{Ast, Direction, Term};
use aiahr_core::define_ids;
use aiahr_core::id::{Id, IdGen, ItemId, ModuleId, TyVarId, VarId};
use aiahr_core::memory::handle::{Handle, RefHandle};
use aiahr_tc::{EffectInfo, Evidence, Row, ShardedHashMap, Ty, TyScheme, TypeKind};
use bumpalo::Bump;
use rustc_hash::FxHashMap;

define_ids!(
/// Uniquely identifies variables in IR. Unique within a module.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub IrVarId;

/// Uniquely identifies
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub IrTyVarId;
);

/// An owned T that is frozen and exposes a reduced Box API.
struct P<T: ?Sized> {
    ptr: Box<T>,
}
impl<T> P<T> {
    fn new(value: T) -> Self {
        Self {
            ptr: Box::new(value),
        }
    }
}

/// The kind of a type variable
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
enum Kind {
    Type,
    Row,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct IrVarTy {
    var: IrTyVarId,
    kind: Kind,
}

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
pub enum IrTyKind<'ctx> {
    VarTy(IrVarTy),
    IntTy,
    FunTy(IrTy<'ctx>, IrTy<'ctx>),
    ForallTy(IrVarTy, IrTy<'ctx>),
    ProductTy(RefHandle<'ctx, [IrTy<'ctx>]>),
    CoproductTy(RefHandle<'ctx, [IrTy<'ctx>]>),
}
use IrTyKind::*;

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
pub struct IrTy<'ctx>(RefHandle<'ctx, IrTyKind<'ctx>>);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
struct IrVar<'ctx> {
    var: IrVarId,
    ty: IrTy<'ctx>,
}

struct IrCtx<'ctx> {
    arena: &'ctx Bump,
    tys: ShardedHashMap<RefHandle<'ctx, IrTyKind<'ctx>>, ()>,
    tys_slices: ShardedHashMap<RefHandle<'ctx, [IrTy<'ctx>]>, ()>,
}

impl<'ctx> IrCtx<'ctx> {
    fn new(arena: &'ctx Bump) -> Self {
        Self {
            arena,
            tys: ShardedHashMap::default(),
            tys_slices: ShardedHashMap::default(),
        }
    }

    fn intern_ir_ty(&self, kind: IrTyKind<'ctx>) -> RefHandle<'ctx, IrTyKind<'ctx>> {
        self.tys._intern(kind, |kind| {
            let kind_ref = self.arena.alloc(kind);
            Handle(kind_ref)
        })
    }

    fn intern_ir_ty_slice(&self, kinds: &[IrTy<'ctx>]) -> RefHandle<'ctx, [IrTy<'ctx>]> {
        self.tys_slices
            ._intern_ref(kinds, || Handle(self.arena.alloc_slice_copy(kinds)))
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
        IrTy(self.intern_ir_ty(kind))
    }

    fn mk_prod_ty(&self, elems: &[IrTy<'ctx>]) -> IrTy<'ctx> {
        IrTy(self.intern_ir_ty(IrTyKind::ProductTy(self.intern_ir_ty_slice(elems))))
    }

    fn mk_coprod_ty(&self, elems: &[IrTy<'ctx>]) -> IrTy<'ctx> {
        IrTy(self.intern_ir_ty(IrTyKind::CoproductTy(self.intern_ir_ty_slice(elems))))
    }
}

/// An Ir node
/// `Ir` is much more explicit than `Term` from `Ast`. It is based on System F with some modest
/// extensions. Each variable is annotated with it's type, and each type is annotated with it's kind.
/// Type constraints are represented as explicit parameters in `Ir`.
///
/// The row typing of `Ast` is boiled down to trivial products and coproducts at the `Ir` level.
/// Evidence parameters (which are just value parameters in `Ir`) are used to replicate the
/// behavior of rows seen in `Ast`.
///
/// Effect typing is also made explicit and transformed to a lower level reprsentation in `Ir`.
/// `Handler`s become `Prompt`s, and `Operation`s become `Yield`s. Prompt and yield together form
/// the primitives to express delimited control which is how we implement effects under the hood.
pub struct Ir<'ctx> {
    kind: IrKind<'ctx>,
}
impl<'ctx> Ir<'ctx> {
    fn new(kind: IrKind<'ctx>) -> Self {
        Self { kind }
    }

    fn var(var: IrVar<'ctx>) -> Self {
        Ir::new(Var(var))
    }

    fn app(head: Self, spine: impl IntoIterator<Item = Self>) -> Self {
        spine
            .into_iter()
            .fold(head, |func, arg| Ir::new(App(P::new(func), P::new(arg))))
    }
}

enum IrKind<'ctx> {
    Int(usize),
    Var(IrVar<'ctx>),
    // Value abstraction and application
    Abs(IrVar<'ctx>, P<Ir<'ctx>>),
    App(P<Ir<'ctx>>, P<Ir<'ctx>>),
    // Type abstraction and application
    TyAbs(TyVarId, P<Ir<'ctx>>),
    TyApp(P<Ir<'ctx>>, Ty<'ctx, TyVarId>),
    // Trivial products
    Struct(Vec<P<Ir<'ctx>>>),
    FieldProj(usize, P<Ir<'ctx>>),
    // Trivial coproducts
    Tag(usize, P<Ir<'ctx>>),
}
use IrKind::*;

pub trait ItemSchemes<'ctx> {
    fn lookup_scheme(&self, module_id: ModuleId, item_id: ItemId) -> TyScheme<'ctx, TyVarId>;
}
pub trait VarTys<'ctx> {
    fn lookup_var(&self, var_id: VarId) -> Ty<'ctx, TyVarId>;
}
pub trait TermTys<'ctx> {
    fn lookup_term(&self, term: &'ctx Term<'ctx, VarId>) -> Ty<'ctx, TyVarId>;
}

struct IdConverter<VarIn, VarOut> {
    cache: FxHashMap<VarIn, VarOut>,
    gen: IdGen<VarOut, ()>,
}
impl<'a, VarIn, VarOut> IdConverter<VarIn, VarOut>
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

// TODO: Wip name
struct Evidenceless;
struct Evidentfull;

struct LowerCtx<'a, 'ctx, Db, I, State = Evidenceless> {
    db: &'a Db,
    ctx: &'a I,
    var_conv: &'a mut IdConverter<VarId, IrVarId>,
    tyvar_conv: &'a mut IdConverter<TyVarId, IrTyVarId>,
    ev_map: EvidenceMap<'ctx>,
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
}

impl<'a, 'ctx, Db, I> LowerCtx<'a, 'ctx, Db, I, Evidenceless>
where
    Db: ItemSchemes<'ctx> + VarTys<'ctx> + TermTys<'ctx>,
    I: MkIrTy<'ctx>,
{
    fn new(
        db: &'a Db,
        ctx: &'a I,
        var_conv: &'a mut IdConverter<VarId, IrVarId>,
        tyvar_conv: &'a mut IdConverter<TyVarId, IrTyVarId>,
    ) -> Self {
        Self {
            db,
            ctx,
            var_conv,
            tyvar_conv,
            ev_map: EvidenceMap::default(),
            _marker: std::marker::PhantomData,
        }
    }

    fn collect_evidence_params<'ev>(
        mut self,
        evs: impl IntoIterator<Item = &'ev Evidence<'ctx, TyVarId>>,
    ) -> (LowerCtx<'a, 'ctx, Db, I, Evidentfull>, Vec<IrVar<'ctx>>)
    where
        'ctx: 'ev,
    {
        let params = evs
            .into_iter()
            .map(|ev| {
                let param = self.lower_evidence(ev);
                self.ev_map.insert(*ev, param);
                param
            })
            .collect::<Vec<_>>();
        (LowerCtx::with_evidenceless(self), params)
    }

    fn lower_evidence(&mut self, ev: &Evidence<'ctx, TyVarId>) -> IrVar<'ctx> {
        match ev {
            Evidence::Row { left, right, goal } => {
                let ev_term = self.var_conv.generate();
                let mut expand = |row: &Row<'ctx, TyVarId>| match row {
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
                };
                let (left_prod, left_coprod) = expand(left);
                let (right_prod, right_coprod) = expand(right);
                let (goal_prod, goal_coprod) = expand(goal);

                let branch_var = IrVarTy {
                    var: self.tyvar_conv.generate(),
                    kind: Kind::Type,
                };
                let branch_var_ty = self.ctx.mk_ir_ty(VarTy(branch_var));

                let row_ev_ty = self.ctx.mk_prod_ty(&[
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
                ]);
                IrVar {
                    var: ev_term,
                    ty: row_ev_ty,
                }
            }
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
struct PartialEv<'ctx> {
    other: Row<'ctx, TyVarId>,
    goal: Row<'ctx, TyVarId>,
}

#[derive(Default)]
struct EvidenceMap<'ctx> {
    params: Vec<IrVar<'ctx>>,
    /// Find evidence when we only have partial information about it.
    /// Like when we enounter a Project or Inject node.
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
        &self.params[self.partial_map[index]]
    }
}

impl<'a, 'ctx, Db, I> LowerCtx<'a, 'ctx, Db, I, Evidentfull>
where
    Db: ItemSchemes<'ctx> + VarTys<'ctx> + TermTys<'ctx>,
    I: MkIrTy<'ctx>,
{
    fn with_evidenceless(prior: LowerCtx<'a, 'ctx, Db, I, Evidenceless>) -> Self {
        Self {
            db: prior.db,
            ctx: prior.ctx,
            var_conv: prior.var_conv,
            tyvar_conv: prior.tyvar_conv,
            ev_map: prior.ev_map,
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
                P::new(self.lower_term(*func)),
                P::new(self.lower_term(*arg)),
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
                let goal_row = match *self.db.lookup_term(term) {
                    TypeKind::ProdTy(Row::Closed(row)) | TypeKind::RowTy(row) => Row::Closed(row),
                    TypeKind::ProdTy(Row::Open(var)) | TypeKind::VarTy(var) => Row::Open(var),
                    _ => unreachable!(),
                };
                let left_row = match *self.db.lookup_term(left) {
                    TypeKind::ProdTy(Row::Closed(row)) | TypeKind::RowTy(row) => Row::Closed(row),
                    TypeKind::ProdTy(Row::Open(var)) | TypeKind::VarTy(var) => Row::Open(var),
                    _ => unreachable!(),
                };
                let right_row = match *self.db.lookup_term(right) {
                    TypeKind::ProdTy(Row::Closed(row)) | TypeKind::RowTy(row) => Row::Closed(row),
                    TypeKind::ProdTy(Row::Open(var)) | TypeKind::VarTy(var) => Row::Open(var),
                    _ => unreachable!(),
                };
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
                //
                let left_row = self
                    .db
                    .lookup_term(left)
                    .try_as_fn_ty()
                    .and_then(|(arg, _)| arg.try_as_sum_row())
                    .unwrap_or_else(|_| unreachable!());
                let right_row = self
                    .db
                    .lookup_term(right)
                    .try_as_fn_ty()
                    .and_then(|(arg, _)| arg.try_as_sum_row())
                    .unwrap_or_else(|_| unreachable!());
                let goal_row = self
                    .db
                    .lookup_term(term)
                    .try_as_fn_ty()
                    .and_then(|(arg, _)| arg.try_as_sum_row())
                    .unwrap_or_else(|_| unreachable!());

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
                let goal = self
                    .db
                    .lookup_term(term)
                    .try_as_prod_row()
                    .unwrap_or_else(|_| unreachable!());
                let other = self
                    .db
                    .lookup_term(subterm)
                    .try_as_prod_row()
                    .unwrap_or_else(|_| unreachable!());

                let param = self.ev_map[&PartialEv { goal, other }];
                let idx = match direction {
                    Direction::Left => 2,
                    Direction::Right => 3,
                };

                let prj = Ir::new(FieldProj(
                    idx,
                    P::new(Ir::new(FieldProj(0, P::new(Ir::var(param))))),
                ));
                Ir::app(prj, [self.lower_term(subterm)])
            }
            Inject {
                direction,
                term: subterm,
            } => {
                let goal = self
                    .db
                    .lookup_term(term)
                    .try_as_sum_row()
                    .unwrap_or_else(|_| unreachable!());
                let other = self
                    .db
                    .lookup_term(subterm)
                    .try_as_sum_row()
                    .unwrap_or_else(|_| unreachable!());

                let param = self.ev_map[&PartialEv { other, goal }];
                let idx = match direction {
                    Direction::Left => 2,
                    Direction::Right => 3,
                };

                let inj = Ir::new(FieldProj(
                    idx,
                    P::new(Ir::new(FieldProj(1, P::new(Ir::var(param))))),
                ));
                Ir::app(inj, [self.lower_term(subterm)])
            }
            // Effect stuff
            Operation(eff_op_id) => todo!(),
            Handle { eff, handler, body } => todo!(),
        }
    }
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
    Db: ItemSchemes<'ctx> + VarTys<'ctx> + TermTys<'ctx> + EffectInfo<'ctx, 'ctx>,
    I: MkIrTy<'ctx>,
{
    let mut var_conv = IdConverter::new();
    let mut tyvar_conv = IdConverter::new();
    let lower_ctx: LowerCtx<'_, 'ctx, Db, I> =
        LowerCtx::new(&db, &ctx, &mut var_conv, &mut tyvar_conv);
    let (mut lower_ctx, ev_params) = lower_ctx.collect_evidence_params(scheme.constrs.iter());
    let body = lower_ctx.lower_term(ast.root());
    let body = ev_params
        .into_iter()
        .rfold(body, |body, arg| Ir::new(Abs(arg, P::new(body))));
    scheme
        .bound
        .iter()
        .rfold(body, |acc, ty_var| Ir::new(TyAbs(*ty_var, P::new(acc))))
}
