use crate::id::{IrTyVarId, IrVarId};
use crate::memory::handle::RefHandle;
use std::fmt;
use std::ops::Deref;

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
    Row,
}

/// An ir type variable and it's kind
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct IrVarTy {
    pub var: IrTyVarId,
    pub kind: Kind,
}

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub enum IrTyKind<'ctx> {
    IntTy,
    // TODO: Should this have a different name?
    EvidenceVectorTy,
    VarTy(IrVarTy),
    FunTy(IrTy<'ctx>, IrTy<'ctx>),
    ForallTy(IrVarTy, IrTy<'ctx>),
    ProductTy(RefHandle<'ctx, [IrTy<'ctx>]>),
    CoproductTy(RefHandle<'ctx, [IrTy<'ctx>]>),
}
use IrTyKind::*;

impl<'ctx> fmt::Debug for IrTyKind<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VarTy(v) => f.debug_tuple("VarTy").field(v).finish(),
            IntTy => write!(f, "IntTy"),
            EvidenceVectorTy => write!(f, "EvidenceVectorTy"),
            FunTy(arg, ret) => f.debug_tuple("FunTy").field(arg).field(ret).finish(),
            ForallTy(ty_var, ty) => f.debug_tuple("ForallTy").field(ty_var).field(ty).finish(),
            ProductTy(elems) => f.debug_tuple("ProductTy").field(&elems.0).finish(),
            CoproductTy(elems) => f.debug_tuple("CoproductTy").field(&elems.0).finish(),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct IrTy<'ctx>(pub RefHandle<'ctx, IrTyKind<'ctx>>);

impl<'ctx> IrTy<'ctx> {
    pub fn new(handle: RefHandle<'ctx, IrTyKind<'ctx>>) -> Self {
        IrTy(handle)
    }
}
impl<'ctx> fmt::Debug for IrTy<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0 .0.fmt(f)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct IrVar<'ctx> {
    pub var: IrVarId,
    pub ty: IrTy<'ctx>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum IrKind<'ctx, IR = P<Ir<'ctx>>> {
    Int(usize),
    Var(IrVar<'ctx>),
    // Value abstraction and application
    Abs(IrVar<'ctx>, IR),
    App(IR, IR),
    // Type abstraction and application
    TyAbs(IrVarTy, IR),
    TyApp(IR, IrTy<'ctx>),
    // Trivial products
    Struct(Vec<IR>),      // Intro
    FieldProj(usize, IR), // Elim
    // Trivial coproducts
    Tag(usize, IR),    // Intro
    Case(IR, Vec<IR>), // Elim
    // Delimited control
    // Generate a new prompt marker
    NewPrompt(IrVar<'ctx>, IR),
    // Install a prompt for a marker
    Prompt(IR, IR),
    // Yield to a marker's prompt
    Yield(IR, IR),
    // Set index in an effect vector to value.
    VectorSet(IrVar<'ctx>, usize, IR),
    VectorGet(IrVar<'ctx>, usize),
}
use rustc_hash::FxHashSet;
use IrKind::*;
#[derive(PartialEq, Eq, Clone, Copy)]
struct IK<'a, 'ctx>(&'a IrKind<'ctx, IK<'a, 'ctx>>);
impl<'a, 'ctx> fmt::Debug for IK<'a, 'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// An Ir node
/// `Ir` is much more explicit than `Term`. It is based on System F with some modest
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
#[derive(Clone, PartialEq)]
pub struct Ir<'ctx> {
    pub kind: IrKind<'ctx>,
}

impl<'ctx> fmt::Debug for Ir<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}
impl<'ctx> Ir<'ctx> {
    pub fn new(kind: IrKind<'ctx>) -> Self {
        Self { kind }
    }

    pub fn kind<'a>(&'a self) -> &'a IrKind<'ctx> {
        &self.kind
    }

    pub fn var(var: IrVar<'ctx>) -> Self {
        Ir::new(Var(var))
    }

    pub fn app(head: Self, spine: impl IntoIterator<Item = Self>) -> Self {
        spine
            .into_iter()
            .fold(head, |func, arg| Ir::new(App(P::new(func), P::new(arg))))
    }

    pub fn abss<I>(vars: I, body: Ir<'ctx>) -> Self
    where
        I: IntoIterator,
        I::IntoIter: DoubleEndedIterator<Item = IrVar<'ctx>>,
    {
        vars.into_iter()
            .rfold(body, |body, var| Ir::new(Abs(var, P::new(body))))
    }

    pub fn case_on_var(var: IrVar<'ctx>, cases: impl IntoIterator<Item = Ir<'ctx>>) -> Self {
        Ir::new(Case(
            P::new(Ir::var(var)),
            cases.into_iter().map(P::new).collect(),
        ))
    }

    pub fn local(var: IrVar<'ctx>, value: Ir<'ctx>, body: Ir<'ctx>) -> Self {
        Ir::new(App(P::new(Ir::new(Abs(var, P::new(body)))), P::new(value)))
    }

    pub fn unbound_vars<'a>(&'a self) -> impl Iterator<Item = IrVar<'ctx>> + 'a {
        self.unbound_vars_with_bound(FxHashSet::default())
    }

    pub fn unbound_vars_with_bound<'a>(
        &'a self,
        bound: FxHashSet<IrVarId>,
    ) -> impl Iterator<Item = IrVar<'ctx>> + 'a {
        UnboundVars {
            bound,
            stack: vec![self],
        }
    }
}

struct UnboundVars<'a, 'ctx> {
    bound: FxHashSet<IrVarId>,
    stack: Vec<&'a Ir<'ctx>>,
}
impl<'ctx> Iterator for UnboundVars<'_, 'ctx> {
    type Item = IrVar<'ctx>;

    fn next(&mut self) -> Option<Self::Item> {
        self.stack.pop().and_then(|ir| match ir.kind() {
            Int(_) => self.next(),
            Var(v) | VectorGet(v, _) => (!self.bound.contains(&v.var))
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
            VectorSet(v, _, a) => {
                self.stack.push(a.deref());
                (!self.bound.contains(&v.var))
                    .then_some(*v)
                    .or_else(|| self.next())
            }
        })
    }
}

#[cfg(feature = "pretty")]
mod pretty_ir {
    use super::{IrKind::*, IrTyKind::*, *};
    use bumpalo::Bump;
    use pretty::*;

    impl<'a, D: ?Sized + DocAllocator<'a>> Pretty<'a, D> for &IrVarTy {
        fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, ()> {
            allocator
                .as_string(self.var.0)
                .append(allocator.text(":"))
                .append(allocator.space())
                .append(allocator.text(match self.kind {
                    Kind::Type => "Type",
                    Kind::Row => "Row",
                }))
                .parens()
        }
    }

    impl<'a, 'ctx, D> Pretty<'a, D> for &IrVar<'ctx>
    where
        D: ?Sized + DocAllocator<'a>,
    {
        fn pretty(self, arena: &'a D) -> DocBuilder<'a, D, ()> {
            arena
                .text("Var")
                .append(arena.text(self.var.0.to_string()).parens())
        }
    }

    impl<'a, 'ctx> Pretty<'a, pretty::Arena<'a>> for &IrKind<'ctx> {
        fn pretty(
            self,
            arena: &'a pretty::Arena<'a>,
        ) -> pretty::DocBuilder<'a, pretty::Arena<'a>, ()> {
            fn gather_abs<'a, 'ctx>(
                vars: &mut Vec<IrVar<'ctx>>,
                kind: &'a IrKind<'ctx>,
            ) -> &'a IrKind<'ctx> {
                match kind {
                    Abs(arg, body) => {
                        vars.push(*arg);
                        gather_abs(vars, &body.deref().kind)
                    }
                    _ => kind,
                }
            }
            fn gather_ty_abs<'a, 'ctx>(
                vars: &mut Vec<IrVarTy>,
                kind: &'a IrKind<'ctx>,
            ) -> &'a IrKind<'ctx> {
                match kind {
                    TyAbs(arg, body) => {
                        vars.push(*arg);
                        gather_ty_abs(vars, &body.deref().kind)
                    }
                    _ => kind,
                }
            }
            let paren_app_arg = |arg: &IrKind<'ctx>| match arg {
                App(_, _) => arg.pretty(arena).parens(),
                _ => arg.pretty(arena),
            };
            match self {
                Int(i) => i.to_string().pretty(arena),
                Var(v) => v.pretty(arena),
                Abs(arg, body) => {
                    let mut vars = vec![*arg];
                    let body = gather_abs(&mut vars, &body.deref().kind);
                    docs![
                        arena,
                        docs![
                            arena,
                            "fun",
                            arena.space(),
                            arena
                                .intersperse(
                                    vars.into_iter().map(|v| v.pretty(arena)),
                                    arena.text(",").append(arena.space()),
                                )
                                .group()
                                .parens()
                        ]
                        .group(),
                        arena.space(),
                        body.pretty(arena)
                            .enclose(arena.line(), arena.line())
                            .nest(2)
                            .braces(),
                        arena.softline(),
                    ]
                }
                App(func, arg) => {
                    let func_doc = match &func.deref().kind {
                        // Wrap lambda literals in parens so they're easier to read
                        f @ Abs(_, _) => f.pretty(arena).parens(),
                        f => f.pretty(arena),
                    };
                    func_doc
                        .append(arena.space())
                        .append(paren_app_arg(&arg.deref().kind))
                }
                TyAbs(tyvar, body) => {
                    let mut tyvars = vec![*tyvar];
                    let body = gather_ty_abs(&mut tyvars, &body.deref().kind);
                    docs![
                        arena,
                        docs![
                            arena,
                            "forall",
                            arena.space(),
                            arena.intersperse(
                                tyvars.into_iter().map(|tv| tv.pretty(arena)),
                                arena.space()
                            ),
                            arena.space(),
                        ]
                        .group(),
                        ".",
                        arena.softline().append(body.pretty(arena)).nest(2)
                    ]
                }
                Struct(elems) => arena
                    .intersperse(
                        elems.iter().map(|elem| elem.deref().kind.pretty(arena)),
                        arena.text(",").append(arena.softline()),
                    )
                    .enclose(arena.softline(), arena.softline())
                    .nest(2)
                    .braces(),
                FieldProj(idx, term) => term
                    .deref()
                    .kind
                    .pretty(arena)
                    .append(arena.as_string(idx).brackets()),
                Tag(tag, term) => docs![
                    arena,
                    arena.as_string(tag),
                    arena.text(":"),
                    arena.space(),
                    &term.deref().kind
                ]
                .angles(),
                Case(discr, branches) => docs![
                    arena,
                    "case",
                    arena.space(),
                    &discr.deref().kind,
                    arena.space(),
                    arena
                        .intersperse(
                            branches.iter().map(|b| b.deref().kind.pretty(arena)),
                            arena.hardline()
                        )
                        .nest(2)
                        .angles()
                ],
                TyApp(_, _) => todo!(),
                NewPrompt(p_var, body) => docs![
                    arena,
                    "new_prompt",
                    p_var.pretty(arena).parens(),
                    body.deref().kind.pretty(arena).braces()
                ],
                Prompt(marker, body) => docs![
                    arena,
                    "prompt",
                    marker.deref().kind.pretty(arena).parens(),
                    arena.space(),
                    body.deref()
                        .kind
                        .pretty(arena)
                        .group()
                        .enclose(arena.softline(), arena.softline())
                        .nest(2)
                        .braces(),
                ],
                Yield(marker, body) => docs![
                    arena,
                    "yield",
                    marker.deref().kind.pretty(arena).parens(),
                    arena.space(),
                    body.deref().kind.pretty(arena).nest(2).braces()
                ],
                VectorSet(vec, idx, value) => docs![
                    arena,
                    vec,
                    arena.as_string(idx).brackets(),
                    arena.space(),
                    ":=",
                    arena.softline(),
                    &value.deref().kind,
                ],
                VectorGet(vec, idx) => docs![arena, vec, arena.as_string(idx).brackets()],
            }
        }
    }
}
