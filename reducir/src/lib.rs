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

    fn as_mut(&mut self) -> &mut T {
        self.ptr.as_mut()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DelimCont {
    // Install a prompt for a marker
    Prompt(P<ReducIr<DelimCont>>, P<ReducIr<DelimCont>>),
    // Yield to a marker's prompt
    Yield(ReducIrTy, P<ReducIr<DelimCont>>, P<ReducIr<DelimCont>>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ReducIrKind<Ext = DelimCont> {
    Int(usize),
    Var(ReducIrVar),
    Item(TermName, ReducIrTy),
    // Value abstraction and application
    Abs(Box<[ReducIrVar]>, P<ReducIr<Ext>>),
    App(P<ReducIr<Ext>>, Vec<ReducIr<Ext>>),
    // Type abstraction and application
    TyAbs(ReducIrVarTy, P<ReducIr<Ext>>),
    TyApp(P<ReducIr<Ext>>, ReducIrTyApp),
    // Trivial products
    Struct(Vec<ReducIr<Ext>>),         // Intro
    FieldProj(usize, P<ReducIr<Ext>>), // Elim
    // Trivial coproducts
    Tag(ReducIrTy, usize, P<ReducIr<Ext>>), // Intro
    Case(ReducIrTy, P<ReducIr<Ext>>, Box<[ReducIr<Ext>]>), // Elim
    // Delimited control
    // Generate a new prompt marker
    NewPrompt(ReducIrVar, P<ReducIr<Ext>>),
    // Extensions
    X(Ext),
}
use ReducIrKind::*;

use crate::ty::ReducIrVarTy;

use self::ty::{Kind, MkReducIrTy, ReducIrTy, ReducIrTyApp};

/// A zip that does not own it's iterators and does not consume elements when only one iterator has
/// elements remaining.
struct LentZip<'a, A, B>
where
    A: Iterator,
    B: Iterator,
{
    a_iter: &'a mut std::iter::Peekable<A>,
    b_iter: &'a mut std::iter::Peekable<B>,
}
impl<'a, A, B> Iterator for LentZip<'a, A, B>
where
    A: Iterator,
    B: Iterator,
{
    type Item = (A::Item, B::Item);

    fn next(&mut self) -> Option<Self::Item> {
        match (self.a_iter.peek(), self.b_iter.peek()) {
            (Some(_), Some(_)) => Some((self.a_iter.next().unwrap(), self.b_iter.next().unwrap())),
            _ => None,
        }
    }
}

/// Zips two iterators without consuming them.
/// This allows for iterating over the overlap of two iterators, and then consuming whatever
/// remains in the two iterators separately after the fact.
trait ZipNonConsuming {
    type Iter: Iterator;

    fn zip_non_consuming<'a, J>(
        &'a mut self,
        other: &'a mut std::iter::Peekable<J>,
    ) -> LentZip<'a, Self::Iter, J>
    where
        J: Sized + Iterator;
}
impl<I: Iterator> ZipNonConsuming for std::iter::Peekable<I> {
    type Iter = I;

    fn zip_non_consuming<'a, J>(
        &'a mut self,
        other: &'a mut std::iter::Peekable<J>,
    ) -> LentZip<'a, I, J>
    where
        Self: Sized + Iterator,
        J: Sized + Iterator,
    {
        LentZip {
            a_iter: self,
            b_iter: other,
        }
    }
}

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
pub struct ReducIr<Ext = DelimCont> {
    pub kind: ReducIrKind<Ext>,
}
impl<Ext> fmt::Debug for ReducIr<Ext>
where
    Ext: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}
impl<Ext> ReducIr<Ext> {
    pub fn new(kind: ReducIrKind<Ext>) -> Self {
        Self { kind }
    }

    pub fn kind(&self) -> &ReducIrKind<Ext> {
        &self.kind
    }

    pub fn var(var: ReducIrVar) -> Self {
        ReducIr::new(Var(var))
    }

    pub fn ext(ext: Ext) -> Self {
        ReducIr::new(ReducIrKind::X(ext))
    }

    pub fn app(mut head: Self, spine: impl IntoIterator<Item = Self>) -> Self {
        match head.kind {
            App(_, ref mut in_place_spine) => {
                in_place_spine.extend(spine);
                head
            }
            _ => ReducIr::new(App(P::new(head), spine.into_iter().collect())),
        }
    }

    pub fn abss<I>(vars: I, body: Self) -> Self
    where
        I: IntoIterator<Item = ReducIrVar>,
    {
        let mut vars = vars.into_iter().peekable();
        // If vars is empty do not construct any abs
        if vars.peek().is_none() {
            body
        } else {
            // Otherwise try to prepend to an existing Abs
            let (vars, body) = match body.kind {
                Abs(ivars, body) => (vars.chain(ivars.iter().copied()).collect(), body),
                _ => (vars.collect(), P::new(body)),
            };
            ReducIr::new(Abs(vars, body))
        }
    }

    pub fn case_on_var(
        ty: ReducIrTy,
        var: ReducIrVar,
        cases: impl IntoIterator<Item = Self>,
    ) -> Self {
        ReducIr::new(Case(
            ty,
            P::new(ReducIr::var(var)),
            cases.into_iter().collect(),
        ))
    }

    pub fn local(var: ReducIrVar, value: Self, mut body: Self) -> Self {
        if let App(ref mut head, ref mut spine) = body.kind {
            if let Abs(ref mut vars, _) = head.as_mut().kind {
                *vars = std::iter::once(var).chain(vars.iter().copied()).collect();
                //spine.push(value);
                spine.insert(0, value);
                return body;
            }
        }
        if let Abs(ref mut vars, _) = body.kind {
            *vars = std::iter::once(var).chain(vars.iter().copied()).collect();
            return ReducIr::app(body, std::iter::once(value));
        }
        ReducIr::app(
            ReducIr::abss(std::iter::once(var), body),
            std::iter::once(value),
        )
    }

    pub fn field_proj(indx: usize, target: Self) -> Self {
        ReducIr::new(FieldProj(indx, P::new(target)))
    }
}
impl ReducIr {
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
            Abs(args, body) => {
                let ret_ty = body.type_check(ctx)?;
                Ok(ctx.mk_fun_ty(args.iter().map(|arg| arg.ty), ret_ty))
            }
            App(func, args) => {
                let func_ty = func.type_check(ctx)?;
                let mut arg_tys = args.iter().map(|arg| arg.type_check(ctx)).peekable();
                match func_ty.kind(ctx.as_ir_db()) {
                    FunTy(fun_arg_tys, ret_ty) => {
                        debug_assert!(args.len() <= fun_arg_tys.len());
                        let mut fun_args = fun_arg_tys.iter().peekable();
                        for (fun_arg_ty, arg_ty) in fun_args.zip_non_consuming(&mut arg_tys) {
                            let arg_ty = arg_ty?;
                            if *fun_arg_ty != arg_ty {
                                return Err(ReducIrTyErr::TyMismatch(*fun_arg_ty, arg_ty));
                            }
                        }
                        Ok(ctx.mk_fun_ty(fun_args.copied(), ret_ty))
                    }
                    _ => Err(ReducIrTyErr::ExpectedFunTy(func_ty)),
                }
            }
            TyAbs(ty_arg, body) => {
                let ret_ty = body.type_check(ctx)?;
                Ok(ctx.mk_reducir_ty(ForallTy(ty_arg.kind, ret_ty)))
            }
            TyApp(forall, ty_app) => {
                let forall_ty = forall.type_check(ctx)?;
                match forall_ty.kind(ctx) {
                    ForallTy(kind, ret_ty) => match (kind, ty_app) {
                        (Kind::Type, ReducIrTyApp::Ty(ty)) => Ok(ret_ty.subst_ty(ctx, *ty)),
                        (Kind::SimpleRow, ReducIrTyApp::DataRow(row))
                        | (Kind::ScopedRow, ReducIrTyApp::EffRow(row)) => {
                            Ok(ret_ty.subst_row(ctx, row.clone()))
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
            Case(case_ty, discr, branches) => {
                let coprod = discr.type_check(ctx)?;
                let tys = match coprod.kind(ctx) {
                    CoproductTy(tys) => tys,
                    _ => {
                        return Err(ReducIrTyErr::ExpectedCoprodTy(coprod));
                    }
                };
                for (branch, ty) in branches.iter().zip(tys.into_iter()) {
                    let branch_ty = branch.type_check(ctx)?;
                    match branch_ty.kind(ctx) {
                        FunTy(arg_tys, ret_ty) => {
                            debug_assert!(arg_tys.len() == 1);
                            if arg_tys[0] != ty {
                                return Err(ReducIrTyErr::TyMismatch(arg_tys[0], ty));
                            }
                            if ret_ty != *case_ty {
                                return Err(ReducIrTyErr::TyMismatch(ret_ty, *case_ty));
                            }
                        }
                        _ => {
                            return Err(ReducIrTyErr::ExpectedFunTy(branch_ty));
                        }
                    }
                }
                Ok(*case_ty)
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
            X(DelimCont::Prompt(marker, body)) => {
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
            X(DelimCont::Yield(ty, marker, body)) => {
                let marker_ty = marker.type_check(ctx)?;
                let IntTy = marker_ty.kind(ctx) else {
                    return Err(ReducIrTyErr::TyMismatch(
                        marker_ty,
                        ctx.mk_reducir_ty(IntTy),
                    ));
                };
                // We want to make sure body type checks but we don't actually use the result
                let _ = body.type_check(ctx)?;
                Ok(*ty)
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
        DocBuilder<'a, D>: Clone,
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
            Abs(vs, body) => {
                self.bound.extend(vs.iter().map(|v| v.var));
                self.stack.push(body.deref());
                self.next()
            }
            NewPrompt(v, body) => {
                self.bound.insert(v.var);
                self.stack.push(body.deref());
                self.next()
            }
            App(head, spine) => {
                self.stack.push(head);
                self.stack.extend(spine.iter());
                self.next()
            }
            X(DelimCont::Prompt(a, b)) | X(DelimCont::Yield(_, a, b)) => {
                self.stack.extend([a.deref(), b.deref()]);
                self.next()
            }
            TyAbs(_, a) | TyApp(a, _) | FieldProj(_, a) | Tag(_, _, a) => {
                self.stack.extend([a.deref()]);
                self.next()
            }
            Struct(irs) => {
                self.stack.extend(irs.iter());
                self.next()
            }
            Case(_, discr, branches) => {
                self.stack.push(discr.deref());
                self.stack.extend(branches.iter());
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
        arena.text("V").append(arena.as_string(self.var.0))
    }
}
impl ReducIrVar {
    #[allow(dead_code)]
    fn pretty_with_type<'a, D>(&self, db: &dyn crate::Db, arena: &'a D) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        DocBuilder<'a, D>: Clone,
    {
        arena
            .text("V")
            .append(arena.as_string(self.var.0))
            .append(arena.text(":"))
            .append(arena.softline())
            .append(self.ty.pretty(db, arena))
            .parens()
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
        match self {
            Int(i) => i.to_string().pretty(arena),
            Var(v) => v.pretty(arena),
            Abs(vars, body) => {
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
            App(func, args) => {
                match &func.deref().kind {
                    // If we see App(Abs(_, _), _) print this as a let binding
                    Abs(vars, body) => {
                        let pretty_binds = |binds: Vec<(&ReducIrVar, &ReducIr)>, body: &ReducIr| {
                            let binds_len = binds.len();
                            let mut binds_iter = binds.into_iter().map(|(var, defn)| {
                                var.pretty(arena)
                                    .append(arena.space())
                                    .append(defn.deref().pretty(db, arena))
                                    .parens()
                            });
                            let binds = if binds_len == 1 {
                                binds_iter.next().unwrap()
                            } else {
                                arena
                                    .space()
                                    .append(arena.intersperse(
                                        binds_iter,
                                        arena.line().append(",").append(arena.space()),
                                    ))
                                    .append(arena.line())
                                    .brackets()
                            };

                            docs![
                                arena,
                                "let",
                                arena.line().append(binds).nest(2).group(),
                                arena
                                    .line()
                                    .append(body.deref().pretty(db, arena))
                                    .nest(2)
                                    .group()
                            ]
                            .parens()
                        };

                        let mut binds = vec![];
                        let mut args_iter = args.iter().peekable();
                        let mut vars_iter = vars.iter().peekable();
                        loop {
                            match (args_iter.peek(), vars_iter.peek()) {
                                (Some(_), Some(_)) => {
                                    binds.push((
                                        vars_iter.next().unwrap(),
                                        args_iter.next().unwrap(),
                                    ));
                                }
                                (Some(_), None) => {
                                    let func = pretty_binds(binds, body);
                                    break func
                                        .append(
                                            arena
                                                .line()
                                                .append(arena.intersperse(
                                                    args_iter.map(|arg| arg.pretty(db, arena)),
                                                    arena.line(),
                                                ))
                                                .nest(2)
                                                .group(),
                                        )
                                        .parens();
                                }
                                (None, Some(_)) => {
                                    break pretty_binds(
                                        binds,
                                        &ReducIr::abss(vars_iter.copied(), body.deref().clone()),
                                    )
                                }
                                (None, None) => break pretty_binds(binds, body),
                            }
                        }
                    }
                    // Print application as normal
                    func => func
                        .pretty(db, arena)
                        .append(
                            arena
                                .line()
                                .append(
                                    arena
                                        .intersperse(
                                            args.iter().map(|arg| arg.pretty(db, arena)),
                                            arena.line(),
                                        )
                                        .align(),
                                )
                                .nest(2)
                                .group(),
                        )
                        .parens(),
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
            Case(_, discr, branches) => docs![
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
            X(DelimCont::Prompt(marker, body)) => arena
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
            X(DelimCont::Yield(_, marker, body)) => arena
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
