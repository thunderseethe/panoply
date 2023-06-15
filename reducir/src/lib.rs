use aiahr_core::id::{ReducIrTyVarId, ReducIrVarId, TermName};
use aiahr_ty::row::{ScopedRow, SimpleRow};
use aiahr_ty::PrettyType;
use pretty::{DocAllocator, *};
use rustc_hash::FxHashSet;
use std::fmt;
use std::ops::Deref;

#[salsa::jar(db = Db)]
pub struct Jar(ReducIrTy);
pub trait Db: salsa::DbWithJar<Jar> + aiahr_core::Db + aiahr_ty::Db {
    fn as_ir_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
    }
}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + aiahr_core::Db + aiahr_ty::Db {}

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub enum ReducIrTyKind {
    IntTy,
    VarTy(ReducIrVarTy),
    FunTy(ReducIrTy, ReducIrTy),
    ForallTy(ReducIrVarTy, ReducIrTy),
    ProductTy(Vec<ReducIrTy>),
    CoproductTy(Vec<ReducIrTy>),
}

impl ReducIrTyKind {
    fn pretty<'a, D, DB, A>(self, db: &DB, a: &'a D) -> DocBuilder<'a, D, A>
    where
        A: 'a,
        D: DocAllocator<'a, A>,
        DB: ?Sized + crate::Db,
    {
        match self {
            ReducIrTyKind::IntTy => a.text("Int"),
            ReducIrTyKind::VarTy(ty_var) => ty_var.pretty(a),
            ReducIrTyKind::FunTy(arg, ret) => {
                let mut arg_doc = arg.pretty(db, a);
                if let ReducIrTyKind::FunTy(_, _) = arg.kind(db.as_ir_db()) {
                    arg_doc = arg_doc.parens();
                }
                arg_doc
                    .append(a.softline())
                    .append(a.text("->"))
                    .append(a.softline())
                    .append(ret.pretty(db, a))
            }
            ReducIrTyKind::ForallTy(ty_var, ty) => a
                .text("forall")
                .append(a.space())
                .append(ty_var.pretty(a))
                .append(a.space())
                .append(a.text("."))
                .append(a.softline())
                .append(ty.pretty(db, a)),
            ReducIrTyKind::ProductTy(tys) => a
                .intersperse(tys.into_iter().map(|ty| ty.pretty(db, a)), ",")
                .braces(),
            ReducIrTyKind::CoproductTy(tys) => a
                .intersperse(tys.into_iter().map(|ty| ty.pretty(db, a)), ",")
                .angles(),
        }
    }
}

#[salsa::interned]
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct ReducIrTy {
    pub kind: ReducIrTyKind,
}

// We allow Rows in type applications because they might show up in constraints.
// But we want to ensure they don't appear in our ReducIr types outside of that so we make a specific type
// for it
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum ReducIrTyApp {
    Ty(ReducIrTy),
    DataRow(SimpleRow),
    EffRow(ScopedRow),
}
impl ReducIrTyApp {
    fn pretty<'a, D, DB, A>(&self, db: &DB, a: &'a D) -> DocBuilder<'a, D, A>
    where
        A: 'a,
        D: DocAllocator<'a, A>,
        DB: ?Sized + crate::Db,
        D::Doc: pretty::Pretty<'a, D, A> + Clone,
    {
        match self {
            ReducIrTyApp::Ty(ty) => ty.pretty(db, a),
            ReducIrTyApp::DataRow(simp) => simp.pretty(a, db, &db),
            ReducIrTyApp::EffRow(scope) => scope.pretty(a, db, &db),
        }
    }
}

impl ReducIrTy {
    fn pretty<'a, D, DB, A>(self, db: &DB, a: &'a D) -> DocBuilder<'a, D, A>
    where
        A: 'a,
        D: DocAllocator<'a, A>,
        DB: ?Sized + crate::Db,
    {
        self.kind(db.as_ir_db()).pretty(db, a)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct ReducIrVar {
    pub var: ReducIrVarId,
    pub ty: ReducIrTy,
}

pub trait MkReducIrTy {
    fn mk_reducir_ty(&self, kind: ReducIrTyKind) -> ReducIrTy;
    fn mk_prod_ty(&self, elems: &[ReducIrTy]) -> ReducIrTy;
    fn mk_coprod_ty(&self, elems: &[ReducIrTy]) -> ReducIrTy;

    fn mk_binary_fun_ty(
        &self,
        fst_arg: impl IntoReducIrTy,
        snd_arg: impl IntoReducIrTy,
        ret: impl IntoReducIrTy,
    ) -> ReducIrTy {
        use ReducIrTyKind::*;
        self.mk_reducir_ty(FunTy(
            fst_arg.into_reducir_ty(self),
            self.mk_reducir_ty(FunTy(
                snd_arg.into_reducir_ty(self),
                ret.into_reducir_ty(self),
            )),
        ))
    }
}
pub trait IntoReducIrTy {
    fn into_reducir_ty<I: ?Sized + MkReducIrTy>(self, ctx: &I) -> ReducIrTy;
}
impl IntoReducIrTy for ReducIrTy {
    fn into_reducir_ty<I: ?Sized + MkReducIrTy>(self, _ctx: &I) -> ReducIrTy {
        self
    }
}
impl IntoReducIrTy for ReducIrTyKind {
    fn into_reducir_ty<I: ?Sized + MkReducIrTy>(self, ctx: &I) -> ReducIrTy {
        ctx.mk_reducir_ty(self)
    }
}

impl<DB> MkReducIrTy for DB
where
    DB: ?Sized + crate::Db,
{
    fn mk_reducir_ty(&self, kind: ReducIrTyKind) -> ReducIrTy {
        ReducIrTy::new(self.as_ir_db(), kind)
    }

    fn mk_prod_ty(&self, elems: &[ReducIrTy]) -> ReducIrTy {
        self.mk_reducir_ty(ReducIrTyKind::ProductTy(elems.to_owned()))
    }

    fn mk_coprod_ty(&self, elems: &[ReducIrTy]) -> ReducIrTy {
        self.mk_reducir_ty(ReducIrTyKind::CoproductTy(elems.to_owned()))
    }
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

/// The kind of a type variable
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum Kind {
    Type,
    SimpleRow,
    ScopedRow,
}

impl<'a, D, A> Pretty<'a, D, A> for &Kind
where
    A: 'a,
    D: ?Sized + DocAllocator<'a, A>,
{
    fn pretty(self, a: &'a D) -> DocBuilder<'a, D, A> {
        match self {
            Kind::Type => a.text("Type"),
            Kind::SimpleRow => a.text("SimpleRow"),
            Kind::ScopedRow => a.text("ScopedRow"),
        }
    }
}

/// An ir type variable and it's kind
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct ReducIrVarTy {
    pub var: ReducIrTyVarId,
    pub kind: Kind,
}

impl<'a, D, A> Pretty<'a, D, A> for ReducIrVarTy
where
    A: 'a,
    D: ?Sized + DocAllocator<'a, A>,
{
    fn pretty(self, a: &'a D) -> DocBuilder<'a, D, A> {
        docs![a, "T", a.as_string(self.var.0)]
    }
}
impl ReducIrVarTy {
    fn pretty_with_kind<'a, D, A>(self, a: &'a D) -> DocBuilder<'a, D, A>
    where
        A: 'a,
        D: DocAllocator<'a, A>,
    {
        docs![a, "T", a.as_string(self.var.0), ":", a.space(), &self.kind,].parens()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ReducIrKind<IR = P<ReducIr>> {
    Int(usize),
    Var(ReducIrVar),
    Item(TermName),
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
    Tag(usize, IR),    // Intro
    Case(IR, Vec<IR>), // Elim
    // Delimited control
    // Generate a new prompt marker
    NewPrompt(ReducIrVar, IR),
    // Install a prompt for a marker
    Prompt(IR, IR),
    // Yield to a marker's prompt
    Yield(IR, IR),
}
use ReducIrKind::*;

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
            Item(_) => self.next(),
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
            TyAbs(_, a) | TyApp(a, _) | FieldProj(_, a) | Tag(_, a) => {
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

impl ReducIrVar {
    #[allow(dead_code)]
    fn pretty_with_ty<'a, D, A, DB>(&self, db: &DB, arena: &'a D) -> DocBuilder<'a, D, A>
    where
        A: 'a,
        D: DocAllocator<'a, A>,
        DB: ?Sized + crate::Db,
    {
        arena
            .text("V")
            .append(arena.text(self.var.0.to_string()))
            .append(arena.as_string(":"))
            .append(arena.space())
            .append(self.ty.pretty(db, arena))
            .parens()
    }
}

impl ReducIr {
    pub fn pretty<'a, A, D, DB>(&self, db: &DB, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        A: 'a,
        D: DocAllocator<'a, A>,
        DocBuilder<'a, D, A>: Clone,
        DB: ?Sized + crate::Db,
        D::Doc: pretty::Pretty<'a, D, A> + Clone,
    {
        self.kind.pretty(db, allocator)
    }
}

impl ReducIrKind {
    fn pretty<'a, D, A, DB>(&self, db: &DB, arena: &'a D) -> pretty::DocBuilder<'a, D, A>
    where
        A: 'a,
        D: DocAllocator<'a, A>,
        DocBuilder<'a, D, A>: Clone,
        DB: ?Sized + crate::Db,
        D::Doc: pretty::Pretty<'a, D, A> + Clone,
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
            Tag(tag, term) => docs![
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
            Item(name) => arena.text(name.name(db.as_core_db()).text(db.as_core_db()).clone()),
        }
    }
}
