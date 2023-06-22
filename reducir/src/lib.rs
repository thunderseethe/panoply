use aiahr_core::id::{ReducIrVarId, TermName};
use pretty::{docs, DocAllocator, DocBuilder, Pretty};
use rustc_hash::FxHashSet;
use std::fmt;
use std::ops::Deref;

#[salsa::jar(db = Db)]
pub struct Jar(ty::ReducIrTy);
pub trait Db: salsa::DbWithJar<Jar> + aiahr_core::Db + aiahr_ty::Db {
    fn as_ir_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
    }
}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + aiahr_core::Db + aiahr_ty::Db {}

pub mod ty;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct ReducIrVar {
    pub var: ReducIrVarId,
    pub ty: ReducIrTy,
}

/// An owned T that is frozen and exposes a reduced Box API.
#[derive(Clone, PartialEq, Eq)]
pub struct P<T: ?Sized> {
    ptr: Box<T>,
}
impl<T> Deref for P<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.ptr
    }
}
impl<T: fmt::Debug> fmt::Debug for P<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.ptr.as_ref().fmt(f)
    }
}
impl<T> P<T> {
    pub fn new(value: T) -> Self {
        Self {
            ptr: Box::new(value),
        }
    }

    pub fn into_inner(self) -> T {
        *self.ptr
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ReducIrKind<IR = P<ReducIr>> {
    Int(usize),
    Var(ReducIrVar),
    Item(TermName, ReducIrTy),
    // Value abstraction and application
    Abs(ReducIrVar, IR),
    App(IR, IR),
    // Type abstraction and application
    TyAbs(ReducIrVarTy, IR),
    TyApp(IR, ReducIrTyApp),
    // Trivial products
    Struct(Vec<IR>),      // Intro
    FieldProj(usize, IR), // Elim
    // Trivial coproducts
    Tag(ReducIrTy, usize, IR), // Intro
    Case(IR, Vec<IR>),         // Elim
    // Delimited control
    // Generate a new prompt marker
    NewPrompt(ReducIrVar, IR),
    // Install a prompt for a marker
    Prompt(IR, IR),
    // Yield to a marker's prompt
    Yield(IR, IR),
}
use ReducIrKind::*;

use crate::ty::{ReducIrTyKind, ReducIrVarTy};

use self::ty::{Kind, MkReducIrTy, ReducIrTy, ReducIrTyApp};

/// A ReducIr node
/// `ReducIr` is much more explicit than `Term`. It is based on System F with some modest
/// extensions. Each variable is annotated with it's type, and each type is annotated with it's kind.
/// Type constraints are represented as explicit parameters in `ReducIr`.
///
/// The row typing of `Ast` is boiled down to trivial products and coproducts at the `ReducIr` level.
/// Evidence parameters (which are just value parameters in `ReducIr`) are used to replicate the
/// behavior of rows seen in `Ast`.
///
/// Effect typing is also made explicit and transformed to a lower level reprsentation in `ReducIr`.
/// `Handler`s become `Prompt`s, and `Operation`s become `Yield`s. Prompt and yield together form
/// the primitives to express delimited control which is how we implement effects under the hood.
#[derive(Clone, PartialEq, Eq)]
pub struct ReducIr {
    pub kind: ReducIrKind,
}
impl fmt::Debug for ReducIr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}
impl ReducIr {
    pub fn new(kind: ReducIrKind) -> Self {
        Self { kind }
    }

    pub fn kind(&self) -> &ReducIrKind {
        &self.kind
    }

    pub fn var(var: ReducIrVar) -> Self {
        ReducIr::new(Var(var))
    }

    pub fn app(head: Self, spine: impl IntoIterator<Item = Self>) -> Self {
        spine.into_iter().fold(head, |func, arg| {
            ReducIr::new(App(P::new(func), P::new(arg)))
        })
    }

    pub fn abss<I>(vars: I, body: ReducIr) -> Self
    where
        I: IntoIterator,
        I::IntoIter: DoubleEndedIterator<Item = ReducIrVar>,
    {
        vars.into_iter()
            .rfold(body, |body, var| ReducIr::new(Abs(var, P::new(body))))
    }

    pub fn case_on_var(var: ReducIrVar, cases: impl IntoIterator<Item = ReducIr>) -> Self {
        ReducIr::new(Case(
            P::new(ReducIr::var(var)),
            cases.into_iter().map(P::new).collect(),
        ))
    }

    pub fn local(var: ReducIrVar, value: ReducIr, body: ReducIr) -> Self {
        ReducIr::new(App(
            P::new(ReducIr::new(Abs(var, P::new(body)))),
            P::new(value),
        ))
    }

    pub fn field_proj(indx: usize, target: ReducIr) -> Self {
        ReducIr::new(FieldProj(indx, P::new(target)))
    }

    pub fn unbound_vars(&self) -> impl Iterator<Item = ReducIrVar> + '_ {
        self.unbound_vars_with_bound(FxHashSet::default())
    }

    pub fn unbound_vars_with_bound(
        &self,
        bound: FxHashSet<ReducIrVarId>,
    ) -> impl Iterator<Item = ReducIrVar> + '_ {
        UnboundVars {
            bound,
            stack: vec![self],
        }
    }

    pub fn type_check(&self, ctx: &dyn crate::Db) -> Result<ReducIrTy, ReducIrTyErr> {
        use ty::ReducIrTyKind::*;
        match &self.kind {
            Int(_) => Ok(ctx.mk_reducir_ty(IntTy)),
            Var(v) => Ok(v.ty),
            Abs(arg, body) => {
                let ret_ty = body.type_check(ctx).map_err(|err| match err {
                    ReducIrTyErr::TyMismatch(lhs, rhs) => ReducIrTyErr::TyMismatch(
                        ctx.mk_reducir_ty(FunTy(arg.ty, lhs)),
                        ctx.mk_reducir_ty(FunTy(arg.ty, rhs)),
                    ),
                    err => err,
                })?;
                Ok(ctx.mk_reducir_ty(FunTy(arg.ty, ret_ty)))
            }
            App(func, arg) => {
                let func_ty = func.type_check(ctx)?;
                let arg_ty = arg.type_check(ctx)?;
                match func_ty.kind(ctx.as_ir_db()) {
                    FunTy(fun_arg_ty, ret_ty) => {
                        if fun_arg_ty.ty_eq(ctx, &arg_ty) {
                            Ok(ret_ty)
                        } else {
                            Err(ReducIrTyErr::TyMismatch(fun_arg_ty, arg_ty))
                        }
                    }
                    _ => Err(ReducIrTyErr::ExpectedFunTy(func_ty)),
                }
            }
            TyAbs(ty_arg, body) => {
                let ret_ty = body.type_check(ctx).map_err(|err| match err {
                    ReducIrTyErr::TyMismatch(lhs, rhs) => ReducIrTyErr::TyMismatch(
                        ctx.mk_reducir_ty(ForallTy(*ty_arg, lhs)),
                        ctx.mk_reducir_ty(ForallTy(*ty_arg, rhs)),
                    ),
                    err => err,
                })?;
                Ok(ctx.mk_reducir_ty(ForallTy(*ty_arg, ret_ty)))
            }
            TyApp(forall, ty_app) => {
                let forall_ty = forall.type_check(ctx)?;
                match forall_ty.kind(ctx) {
                    ForallTy(ty_var, ret_ty) => match (ty_var.kind, ty_app) {
                        (Kind::Type, ReducIrTyApp::Ty(ty)) => Ok(ret_ty.subst_ty(ctx, ty_var, *ty)),
                        (Kind::SimpleRow, ReducIrTyApp::DataRow(row))
                        | (Kind::ScopedRow, ReducIrTyApp::EffRow(row)) => {
                            Ok(ret_ty.subst_row(ctx, ty_var, row))
                        }
                        (k, ReducIrTyApp::Ty(_)) => Err(ReducIrTyErr::KindMistmatch(k, Kind::Type)),
                        (k, ReducIrTyApp::DataRow(_)) => {
                            Err(ReducIrTyErr::KindMistmatch(k, Kind::SimpleRow))
                        }
                        (k, ReducIrTyApp::EffRow(_)) => {
                            Err(ReducIrTyErr::KindMistmatch(k, Kind::ScopedRow))
                        }
                    },
                    _ => Err(ReducIrTyErr::ExpectedForallTy(forall_ty)),
                }
            }
            Struct(elems) => {
                let elems = elems
                    .iter()
                    .map(|e| e.type_check(ctx))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(ctx.mk_prod_ty(&elems))
            }
            FieldProj(indx, strukt) => {
                let strukt_ty = strukt.type_check(ctx)?;
                match strukt_ty.kind(ctx) {
                    ProductTy(tys) => Ok(tys[*indx]),
                    _ => Err(ReducIrTyErr::ExpectedProdTy(strukt_ty)),
                }
            }
            Case(discr, branches) => {
                let coprod = discr.type_check(ctx)?;
                let tys = match coprod.kind(ctx) {
                    CoproductTy(tys) => tys,
                    _ => {
                        return Err(ReducIrTyErr::ExpectedCoprodTy(coprod));
                    }
                };
                branches
                    .iter()
                    .zip(tys.into_iter())
                    .map(|(branch, ty)| {
                        let branch_ty = branch.type_check(ctx)?;
                        match branch_ty.kind(ctx) {
                            FunTy(arg_ty, ret_ty) => {
                                if arg_ty.ty_eq(ctx, &ty) {
                                    Ok(ret_ty)
                                } else {
                                    Err(ReducIrTyErr::TyMismatch(arg_ty, ty))
                                }
                            }
                            _ => Err(ReducIrTyErr::ExpectedFunTy(branch_ty)),
                        }
                    })
                    .reduce(|a, b| {
                        let a = a?;
                        let b = b?;
                        if a.ty_eq(ctx, &b) {
                            Ok(a)
                        } else {
                            Err(ReducIrTyErr::TyMismatch(a, b))
                        }
                    })
                    .unwrap_or_else(|| Ok(ctx.mk_reducir_ty(ReducIrTyKind::NeverTy)))
            }
            NewPrompt(prompt, body) => {
                if let IntTy = prompt.ty.kind(ctx) {
                    body.type_check(ctx)
                } else {
                    Err(ReducIrTyErr::TyMismatch(
                        prompt.ty,
                        ctx.mk_reducir_ty(IntTy),
                    ))
                }
            }
            Prompt(marker, body) | Yield(marker, body) => {
                let marker_ty = marker.type_check(ctx)?;
                if let IntTy = marker_ty.kind(ctx) {
                    body.type_check(ctx)
                } else {
                    Err(ReducIrTyErr::TyMismatch(
                        marker_ty,
                        ctx.mk_reducir_ty(IntTy),
                    ))
                }
            }
            Tag(ty, _, _) => Ok(*ty),
            Item(_, ty) => Ok(*ty),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ReducIrTyErr {
    TyMismatch(ReducIrTy, ReducIrTy),
    KindMistmatch(Kind, Kind),
    ExpectedFunTy(ReducIrTy),
    ExpectedForallTy(ReducIrTy),
    ExpectedProdTy(ReducIrTy),
    ExpectedCoprodTy(ReducIrTy),
}
impl ReducIrTyErr {
    pub fn pretty<'a, D, DB>(&self, db: &DB, a: &'a D) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        DB: ?Sized + crate::Db,
    {
        match self {
            ReducIrTyErr::TyMismatch(lhs, rhs) => a.text("TyMistmatch").append(
                lhs.pretty(db, a)
                    .append(a.text(","))
                    .append(a.softline())
                    .append(rhs.pretty(db, a))
                    .parens(),
            ),
            ReducIrTyErr::KindMistmatch(lhs, rhs) => a.text("KindMistmatch").append(
                lhs.pretty(a)
                    .append(a.text(","))
                    .append(a.softline())
                    .append(rhs.pretty(a))
                    .parens(),
            ),
            ReducIrTyErr::ExpectedFunTy(ty) => a
                .text("Expected a function type, but got:")
                .append(a.softline())
                .append(ty.pretty(db, a)),
            ReducIrTyErr::ExpectedForallTy(ty) => a
                .text("Expected a forall type, but got:")
                .append(a.softline())
                .append(ty.pretty(db, a)),
            ReducIrTyErr::ExpectedProdTy(ty) => a
                .text("Expected a prod type, but got:")
                .append(a.softline())
                .append(ty.pretty(db, a)),
            ReducIrTyErr::ExpectedCoprodTy(ty) => a
                .text("Expected a coprod type, but got:")
                .append(a.softline())
                .append(ty.pretty(db, a)),
        }
    }
}

struct UnboundVars<'a> {
    bound: FxHashSet<ReducIrVarId>,
    stack: Vec<&'a ReducIr>,
}
impl Iterator for UnboundVars<'_> {
    type Item = ReducIrVar;

    fn next(&mut self) -> Option<Self::Item> {
        self.stack.pop().and_then(|ir| match ir.kind() {
            Int(_) => self.next(),
            Item(_, _) => self.next(),
            Var(v) => (!self.bound.contains(&v.var))
                .then_some(*v)
                .or_else(|| self.next()),
            Abs(v, body) | NewPrompt(v, body) => {
                self.bound.insert(v.var);
                self.stack.push(body.deref());
                self.next()
            }
            App(a, b) | Prompt(a, b) | Yield(a, b) => {
                self.stack.extend([a.deref(), b.deref()]);
                self.next()
            }
            TyAbs(_, a) | TyApp(a, _) | FieldProj(_, a) | Tag(_, _, a) => {
                self.stack.extend([a.deref()]);
                self.next()
            }
            Struct(irs) => {
                self.stack.extend(irs.iter().map(|ir| ir.deref()));
                self.next()
            }
            Case(discr, branches) => {
                self.stack.push(discr.deref());
                self.stack.extend(branches.iter().map(|ir| ir.deref()));
                self.next()
            }
        })
    }
}

impl<'a, A: 'a, D: ?Sized + DocAllocator<'a, A>> Pretty<'a, D, A> for &ReducIrVarTy {
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        allocator
            .as_string(self.var.0)
            .append(allocator.text(":"))
            .append(allocator.space())
            .append(allocator.text(match self.kind {
                Kind::Type => "Type",
                Kind::SimpleRow => "SimpleRow",
                Kind::ScopedRow => "ScopedRow",
            }))
            .parens()
    }
}

impl<'a, A: 'a, D: ?Sized + DocAllocator<'a, A>> Pretty<'a, D, A> for &ReducIrVar {
    fn pretty(self, arena: &'a D) -> DocBuilder<'a, D, A> {
        arena.text("V").append(arena.text(self.var.0.to_string()))
    }
}

impl ReducIr {
    pub fn pretty<'a, D, DB>(&self, db: &DB, allocator: &'a D) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        DocBuilder<'a, D>: Clone,
        DB: ?Sized + crate::Db,
        D::Doc: pretty::Pretty<'a, D> + Clone,
    {
        self.kind.pretty(db, allocator)
    }
}

impl ReducIrKind {
    fn pretty<'a, D, DB>(&self, db: &DB, arena: &'a D) -> pretty::DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        DocBuilder<'a, D>: Clone,
        DB: ?Sized + crate::Db,
        D::Doc: pretty::Pretty<'a, D> + Clone,
    {
        fn gather_abs<'a>(vars: &mut Vec<ReducIrVar>, kind: &'a ReducIrKind) -> &'a ReducIrKind {
            match kind {
                Abs(arg, body) => {
                    vars.push(*arg);
                    gather_abs(vars, &body.deref().kind)
                }
                _ => kind,
            }
        }
        fn gather_ty_abs<'a>(
            vars: &mut Vec<ReducIrVarTy>,
            kind: &'a ReducIrKind,
        ) -> &'a ReducIrKind {
            match kind {
                TyAbs(arg, body) => {
                    vars.push(*arg);
                    gather_ty_abs(vars, &body.deref().kind)
                }
                _ => kind,
            }
        }
        fn gather_app<'a>(
            vars: &mut Vec<&'a ReducIrKind>,
            kind: &'a ReducIrKind,
        ) -> &'a ReducIrKind {
            match kind {
                App(func, arg) => {
                    vars.push(&arg.deref().kind);
                    gather_app(vars, &func.deref().kind)
                }
                ir => ir,
            }
        }
        match self {
            Int(i) => i.to_string().pretty(arena),
            Var(v) => v.pretty(arena),
            Abs(arg, body) => {
                let mut vars = vec![*arg];
                let body = gather_abs(&mut vars, &body.deref().kind);
                let param_single = arena.space().append(
                    arena
                        .intersperse(
                            vars.iter().map(|v| v.pretty(arena)),
                            arena.text(",").append(arena.space()),
                        )
                        .brackets(),
                );
                let param_multi = arena
                    .hardline()
                    .append(
                        arena
                            .intersperse(
                                vars.iter().map(|v| v.pretty(arena)),
                                arena.hardline().append(","),
                            )
                            .brackets(),
                    )
                    .nest(2);

                docs![
                    arena,
                    "fun",
                    param_multi.flat_alt(param_single).group(),
                    arena.line().append(body.pretty(db, arena)).nest(2).group(),
                ]
                .parens()
            }
            App(func, arg) => {
                match &func.deref().kind {
                    // If we see App(Abs(_, _), _) print this as a let binding
                    Abs(var, body) => {
                        let bind = var
                            .pretty(arena)
                            .append(arena.space())
                            .append(arg.deref().pretty(db, arena))
                            .parens();
                        docs![
                            arena,
                            "let",
                            arena.line().append(bind).nest(2).group(),
                            arena
                                .line()
                                .append(body.deref().pretty(db, arena))
                                .nest(2)
                                .group()
                        ]
                        .parens()
                    }
                    // Print application as normal
                    func => {
                        let mut args = vec![&arg.deref().kind];
                        let func = gather_app(&mut args, func);
                        func.pretty(db, arena)
                            .append(
                                arena
                                    .line()
                                    .append(
                                        arena
                                            .intersperse(
                                                args.into_iter()
                                                    .rev()
                                                    .map(|arg| arg.pretty(db, arena)),
                                                arena.line(),
                                            )
                                            .align(),
                                    )
                                    .nest(2)
                                    .group(),
                            )
                            .parens()
                    }
                }
            }
            TyAbs(tyvar, body) => {
                let mut tyvars = vec![*tyvar];
                let body = gather_ty_abs(&mut tyvars, &body.deref().kind);
                let params = arena
                    .softline()
                    .append(
                        arena
                            .intersperse(
                                tyvars.iter().map(|tv| tv.pretty_with_kind(arena)),
                                arena.space(),
                            )
                            .align()
                            .brackets(),
                    )
                    .nest(2);
                let body_doc = arena.softline().append(body.pretty(db, arena)).nest(2);
                docs![arena, "forall", params.group(), body_doc.group()].parens()
            }
            Struct(elems) => arena
                .intersperse(
                    elems.iter().map(|elem| elem.deref().pretty(db, arena)),
                    arena.text(",").append(arena.space()),
                )
                .enclose(arena.softline_(), arena.softline_())
                .braces()
                .group(),
            FieldProj(idx, term) => term
                .deref()
                .pretty(db, arena)
                .append(arena.as_string(idx).brackets()),
            Tag(_, tag, term) => docs![
                arena,
                arena.as_string(tag),
                arena.text(":"),
                arena.space(),
                term.pretty(db, arena).nest(2)
            ]
            .angles(),
            Case(discr, branches) => docs![
                arena,
                "case",
                arena.space(),
                discr.pretty(db, arena).nest(2).group(),
                arena.space(),
                arena.intersperse(
                    branches.iter().map(|b| b.deref().pretty(db, arena)),
                    arena.space()
                )
            ]
            .parens(),
            TyApp(body, ty) => body
                .pretty(db, arena)
                .append(arena.space())
                .append(arena.as_string("@"))
                .append(arena.space())
                .append(ty.pretty(db, arena))
                .parens(),
            NewPrompt(p_var, body) => docs![
                arena,
                "new_prompt",
                arena.space(),
                p_var.pretty(arena).brackets(),
                arena.space(),
                body.deref().kind.pretty(db, arena)
            ]
            .parens(),
            Prompt(marker, body) => arena
                .as_string("prompt")
                .append(
                    arena
                        .softline()
                        .append(marker.deref().pretty(db, arena))
                        .nest(2),
                )
                .append(
                    arena
                        .softline()
                        .append(body.deref().pretty(db, arena))
                        .nest(2),
                )
                .parens(),
            Yield(marker, body) => arena
                .as_string("yield")
                .append(
                    arena
                        .softline()
                        .append(marker.deref().kind.pretty(db, arena))
                        .nest(2),
                )
                .append(
                    arena
                        .softline()
                        .append(body.deref().kind.pretty(db, arena))
                        .nest(2),
                )
                .parens(),
            Item(name, _) => arena.text(name.name(db.as_core_db()).text(db.as_core_db()).clone()),
        }
    }
}
