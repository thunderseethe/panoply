use aiahr_core::id::MedIrVarId;
use aiahr_reducir::ReducIrTermName;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MedIrVar {
    pub id: MedIrVarId,
}

impl MedIrVar {
    pub fn new(id: MedIrVarId) -> Self {
        Self { id }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, PartialOrd, Ord, Hash)]
pub struct MedIrItemName(pub ReducIrTermName);

impl MedIrItemName {
    pub fn new(name: ReducIrTermName) -> Self {
        Self(name)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum Atom {
    Var(MedIrVar),
    Int(usize),
}
/// TODO: Document how this differs from ReducIr
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum MedIrKind {
    Atom(Atom),
    /// A flat sequence of values
    Blocks(Vec<Atom>),
    /// Index into a block producing the value stored
    BlockAccess(MedIrVar, usize),
    /// Switch on an int dispatching control to one of the branches based on the int's value.
    Switch(Atom, Vec<Locals>),
    /// Function call
    Call(Call, Vec<Atom>),
    /// Allocate a closure capturing the listed vars
    Closure(MedIrItemName, Vec<MedIrVar>),
}

#[derive(PartialEq, Eq, Clone)]
pub struct MedIr {
    pub kind: MedIrKind,
}
use std::fmt;
impl fmt::Debug for MedIr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}
impl MedIr {
    pub fn new(kind: MedIrKind) -> Self {
        MedIr { kind }
    }

    pub fn int(int: usize) -> Self {
        MedIr::new(MedIrKind::Atom(Atom::Int(int)))
    }

    pub fn var(var: MedIrVar) -> Self {
        MedIr::new(MedIrKind::Atom(Atom::Var(var)))
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Call {
    /// A known function call we can dispatch statically
    Known(MedIrItemName),
    /// An unknown function call that we have to apply
    Unknown(MedIrVar),
}

/// A series of binding that are scoped locally to body.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Locals {
    pub binds: Vec<(MedIrVar, MedIr)>,
    pub body: MedIr,
}

/// A top level definition is composed of a name, its parameters, and a Locals acting as the body
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Defn {
    pub name: MedIrItemName,
    pub params: Vec<MedIrVar>,
    pub body: Locals,
}

mod pretty {
    use aiahr_core::id::Id;
    use aiahr_core::pretty::PrettyWithCtx;
    use pretty::{DocAllocator, Pretty};

    use crate::{Atom, Call, Defn, Locals, MedIr, MedIrItemName, MedIrKind, MedIrVar};

    impl<DB: ?Sized + aiahr_reducir::Db> PrettyWithCtx<DB> for MedIrItemName {
        fn pretty<'a>(
            &self,
            ctx: &DB,
            alloc: &'a pretty::RcAllocator,
        ) -> pretty::DocBuilder<'a, pretty::RcAllocator> {
            self.0.pretty(ctx, alloc)
        }
    }

    impl<'a, D, A: 'a> pretty::Pretty<'a, D, A> for &MedIrVar
    where
        D: DocAllocator<'a, A>,
    {
        fn pretty(self, a: &'a D) -> pretty::DocBuilder<'a, D, A> {
            a.text("V").append(a.as_string(self.id.raw()))
        }
    }

    impl<'a, D, A: 'a> pretty::Pretty<'a, D, A> for &Atom
    where
        D: DocAllocator<'a, A>,
    {
        fn pretty(self, a: &'a D) -> pretty::DocBuilder<'a, D, A> {
            match self {
                Atom::Var(v) => v.pretty(a),
                Atom::Int(i) => a.as_string(i),
            }
        }
    }

    impl<DB: ?Sized + aiahr_reducir::Db> PrettyWithCtx<DB> for Call {
        fn pretty<'a>(
            &self,
            ctx: &DB,
            alloc: &'a pretty::RcAllocator,
        ) -> pretty::DocBuilder<'a, pretty::RcAllocator> {
            match self {
                Call::Known(item) => item.pretty(ctx, alloc),
                Call::Unknown(v) => alloc.text("apply_closure").append(v.pretty(alloc).parens()),
            }
        }
    }

    impl<DB: ?Sized + aiahr_reducir::Db> PrettyWithCtx<DB> for MedIrKind {
        fn pretty<'a>(
            &self,
            ctx: &DB,
            a: &'a pretty::RcAllocator,
        ) -> pretty::DocBuilder<'a, pretty::RcAllocator> {
            match self {
                MedIrKind::Atom(atom) => atom.pretty(a),
                MedIrKind::Blocks(elems) => a
                    .intersperse(elems.iter().map(|elem| elem.pretty(a)), a.text(", "))
                    .brackets(),
                MedIrKind::BlockAccess(v, indx) => v.pretty(a).append(a.as_string(indx).brackets()),
                MedIrKind::Switch(scrutinee, branches) => a
                    .text("switch")
                    .append(a.space())
                    .append(scrutinee.pretty(a))
                    .append("<")
                    .append(
                        a.line()
                            .append(a.intersperse(
                                branches.iter().enumerate().map(|(i, branch)| {
                                    a.text("branch")
                                        .append(a.space())
                                        .append(a.as_string(i))
                                        .append(a.space())
                                        .append(branch.pretty(ctx, a))
                                }),
                                a.line(),
                            ))
                            .nest(2),
                    )
                    .append(a.line())
                    .append(">"),
                MedIrKind::Call(fun, args) => fun.pretty(ctx, a).append(
                    a.intersperse(
                        args.iter().map(|arg| arg.pretty(a)),
                        a.text(",").append(a.space()),
                    )
                    .parens(),
                ),
                MedIrKind::Closure(item, env) => {
                    let env_doc = a
                        .intersperse(
                            env.iter().map(|v| v.pretty(a)),
                            a.text(",").append(a.space()),
                        )
                        .brackets();
                    a.text("make_closure").append(
                        item.pretty(ctx, a)
                            .append(a.text(","))
                            .append(env_doc)
                            .parens(),
                    )
                }
            }
        }
    }

    impl<DB: ?Sized + aiahr_reducir::Db> PrettyWithCtx<DB> for MedIr {
        fn pretty<'a>(
            &self,
            ctx: &DB,
            alloc: &'a pretty::RcAllocator,
        ) -> pretty::DocBuilder<'a, pretty::RcAllocator> {
            self.kind.pretty(ctx, alloc)
        }
    }

    impl<DB: ?Sized + aiahr_reducir::Db> PrettyWithCtx<DB> for Locals {
        fn pretty<'a>(
            &self,
            ctx: &DB,
            a: &'a pretty::RcAllocator,
        ) -> pretty::DocBuilder<'a, pretty::RcAllocator> {
            a.line()
                .append(a.concat(self.binds.iter().map(|(var, defn)| {
                    a.text("let")
                        .append(a.space())
                        .append(var.pretty(a))
                        .append(a.space())
                        .append("=")
                        .append(a.space())
                        .append(defn.pretty(ctx, a))
                        .append(a.text(";"))
                        .append(a.line())
                })))
                .append(self.body.pretty(ctx, a))
                .nest(2)
                .append(a.line())
                .braces()
        }
    }

    impl<DB: ?Sized + aiahr_reducir::Db> PrettyWithCtx<DB> for Defn {
        fn pretty<'a>(
            &self,
            ctx: &DB,
            a: &'a pretty::RcAllocator,
        ) -> pretty::DocBuilder<'a, pretty::RcAllocator> {
            a.text("defn")
                .append(a.space())
                .append(self.name.pretty(ctx, a))
                .append(
                    a.intersperse(
                        self.params.iter().map(|var| pretty::Pretty::pretty(var, a)),
                        a.text(",").append(a.space()),
                    )
                    .parens(),
                )
                .append(a.space())
                .append(self.body.pretty(ctx, a))
        }
    }
}
