use aiahr_core::id::{ReducIrVarId, TermName};
use aiahr_core::pretty::PrettyWithCtx;
use rustc_hash::FxHashSet;
use std::borrow::Cow;
use std::convert::Infallible;
use std::fmt;
use std::ops::Deref;
use ReducIrKind::*;

use crate::ty::ReducIrVarTy;

use self::ty::{Kind, MkReducIrTy, ReducIrTy, ReducIrTyApp};

mod pretty;
pub mod ty;

pub mod zip_non_consuming;
use zip_non_consuming::ZipNonConsuming;

#[salsa::jar(db = Db)]
pub struct Jar(ty::ReducIrTy);
pub trait Db: salsa::DbWithJar<Jar> + aiahr_core::Db + aiahr_ty::Db {
    fn as_ir_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
    }
}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + aiahr_core::Db + aiahr_ty::Db {}

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
    // Delimited control
    // Generate a new prompt marker
    NewPrompt(ReducIrVar, P<ReducIr<DelimCont>>),
    // Install a prompt for a marker
    Prompt(
        P<ReducIr<DelimCont>>,
        P<ReducIr<DelimCont>>,
        P<ReducIr<DelimCont>>,
    ),
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
    // Extensions
    X(Ext),
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

    pub fn ty_app(head: Self, spine: impl IntoIterator<Item = ReducIrTyApp>) -> Self {
        spine.into_iter().fold(head, |head, ty_app| {
            ReducIr::new(TyApp(P::new(head), ty_app))
        })
    }

    pub fn app(mut head: Self, spine: impl IntoIterator<Item = Self>) -> Self {
        match head.kind {
            App(_, ref mut in_place_spine) => {
                in_place_spine.extend(spine);
                head
            }
            _ => {
                let mut spine_iter = spine.into_iter().peekable();
                if spine_iter.peek().is_some() {
                    ReducIr::new(App(P::new(head), spine_iter.collect()))
                } else {
                    // If application is empty we don't create an application node
                    head
                }
            }
        }
    }

    pub fn abss<I>(vars: I, mut body: Self) -> Self
    where
        I: IntoIterator<Item = ReducIrVar>,
    {
        let mut vars = vars.into_iter().peekable();
        // If vars is empty do not construct any abs
        if vars.peek().is_none() {
            body
        } else {
            // Otherwise try to prepend to an existing Abs
            if let Abs(ref mut ivars, _) = body.kind {
                *ivars = vars.chain(ivars.iter().copied()).collect();
                return body;
            }
            ReducIr::new(Abs(vars.collect(), P::new(body)))
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

pub trait ReducIrFold: Sized {
    type InExt;
    type OutExt;

    fn fold_ir(&mut self, kind: ReducIrKind<Self::OutExt>) -> ReducIr<Self::OutExt> {
        ReducIr { kind }
    }

    fn fold_ext(&mut self, ext: &Self::InExt) -> Self::OutExt;

    /// Controls traversal of ReducIr, by default will fold every node in the tree.
    /// Override with a custom implementation if you'd like to control which nodes are folded.
    fn traverse_ir(&mut self, ir: &ReducIr<Self::InExt>) -> ReducIr<Self::OutExt> {
        match ir.kind() {
            Abs(vars, body) => {
                let body = body.fold(self);
                self.fold_ir(Abs(vars.clone(), P::new(body)))
            }
            App(head, spine) => {
                let head = head.fold(self);
                let spine = spine.iter().map(|ir| ir.fold(self)).collect();
                self.fold_ir(App(P::new(head), spine))
            }
            TyAbs(ty_var, body) => {
                let body = body.fold(self);
                self.fold_ir(TyAbs(*ty_var, P::new(body)))
            }
            TyApp(body, ty_app) => {
                let body = body.fold(self);
                self.fold_ir(TyApp(P::new(body), ty_app.clone()))
            }
            Struct(elems) => {
                let elems = elems.iter().map(|e| e.fold(self)).collect();
                self.fold_ir(Struct(elems))
            }
            FieldProj(indx, body) => {
                let body = body.fold(self);
                self.fold_ir(FieldProj(*indx, P::new(body)))
            }
            Tag(ty, tag, body) => {
                let body = body.fold(self);
                self.fold_ir(Tag(*ty, *tag, P::new(body)))
            }
            Case(ty, discr, branches) => {
                let discr = discr.fold(self);
                let branches = branches.iter().map(|branch| branch.fold(self)).collect();
                self.fold_ir(Case(*ty, P::new(discr), branches))
            }
            Int(i) => self.fold_ir(Int(*i)),
            Var(v) => self.fold_ir(Var(*v)),
            Item(name, ty) => self.fold_ir(Item(*name, *ty)),

            X(in_ext) => {
                let out_ext = self.fold_ext(in_ext);
                self.fold_ir(X(out_ext))
            }
        }
    }
}

impl<F> ReducIrFold for &mut F
where
    F: ReducIrFold,
{
    type InExt = F::InExt;

    type OutExt = F::OutExt;

    fn fold_ir(&mut self, ir: ReducIrKind<Self::OutExt>) -> ReducIr<Self::OutExt> {
        F::fold_ir(self, ir)
    }

    fn fold_ext(&mut self, ext: &Self::InExt) -> Self::OutExt {
        F::fold_ext(self, ext)
    }
}

impl<Ext> ReducIr<Ext> {
    pub fn fold<F: ReducIrFold<InExt = Ext>>(&self, fold: &mut F) -> ReducIr<F::OutExt> {
        fold.traverse_ir(self)
    }
}

impl<Ext> ReducIr<Ext> {
    /// Allows "casting" to another Ext type by assuming `ReducIrKind::X` is not used anywhere in
    /// `self`.
    /// Panics if `ReducIrKind::X` is present in `self`.
    pub fn assume_no_ext(&self) -> ReducIr<Infallible> {
        struct AssumeNoExt<Ext>(std::marker::PhantomData<Ext>);
        impl<Ext> Default for AssumeNoExt<Ext> {
            fn default() -> Self {
                Self(std::marker::PhantomData)
            }
        }
        impl<Ext> ReducIrFold for AssumeNoExt<Ext> {
            type InExt = Ext;
            type OutExt = Infallible;

            fn fold_ext(&mut self, _: &Self::InExt) -> Self::OutExt {
                panic!("Assumed extension would not appear in ReducIr")
            }
        }

        self.fold(&mut AssumeNoExt::default())
    }
}
pub trait TypeCheck {
    type Ext: Clone;
    fn type_check<'a>(
        &'a self,
        ctx: &dyn crate::Db,
    ) -> Result<ReducIrTy, ReducIrTyErr<'a, Self::Ext>>;
}
impl TypeCheck for DelimCont {
    type Ext = Self;
    fn type_check(&self, ctx: &dyn crate::Db) -> Result<ReducIrTy, ReducIrTyErr<Self::Ext>> {
        use ty::ReducIrTyKind::*;
        match self {
            DelimCont::NewPrompt(prompt, body) => {
                if let MarkerTy(_) = prompt.ty.kind(ctx) {
                    body.type_check(ctx)
                } else {
                    Err(ReducIrTyErr::ExpectedMarkerTy(prompt.ty))
                }
            }
            DelimCont::Prompt(marker, _upd_handler, body) => {
                let marker_ty = marker.type_check(ctx)?;
                if let MarkerTy(_) = marker_ty.kind(ctx) {
                    body.type_check(ctx)
                } else {
                    Err(ReducIrTyErr::ExpectedMarkerTy(marker_ty))
                }
            }
            DelimCont::Yield(ty, marker, body) => {
                let marker_ty = marker.type_check(ctx)?;
                let MarkerTy(_) = marker_ty.kind(ctx) else {
                    return Err(ReducIrTyErr::ExpectedMarkerTy(
                        marker_ty,
                    ));
                };
                // We want to make sure body type checks but we don't actually use the result
                let _ = body.type_check(ctx)?;
                Ok(*ty)
            }
        }
    }
}
impl TypeCheck for Infallible {
    type Ext = Self;
    fn type_check(&self, _: &dyn crate::Db) -> Result<ReducIrTy, ReducIrTyErr<Self::Ext>> {
        unreachable!()
    }
}
impl<Ext: TypeCheck<Ext = Ext> + PrettyWithCtx<dyn crate::Db> + Clone> TypeCheck for ReducIr<Ext> {
    type Ext = Ext;

    fn type_check(&self, ctx: &dyn crate::Db) -> Result<ReducIrTy, ReducIrTyErr<Ext>> {
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
                let args_iter = args.iter();
                match func_ty.kind(ctx.as_ir_db()) {
                    FunTy(fun_arg_tys, ret_ty) => {
                        debug_assert!(args.len() <= fun_arg_tys.len());
                        let mut fun_args = fun_arg_tys.iter().peekable();
                        for (fun_arg_ty, (arg_index, arg)) in
                            fun_args.zip_non_consuming(&mut args_iter.enumerate().peekable())
                        {
                            let arg_ty = arg.type_check(ctx.as_ir_db())?;
                            if *fun_arg_ty != arg_ty {
                                return Err(ReducIrTyErr::TyMismatch {
                                    left_ty: ctx.mk_fun_ty(fun_args.copied(), ret_ty),
                                    left_ir: Cow::Owned(ReducIr::app(
                                        func.deref().clone(),
                                        args[0..arg_index].to_vec(),
                                    )),
                                    right_ty: arg_ty,
                                    right_ir: Cow::Borrowed(arg),
                                });
                            }
                        }

                        Ok(ctx.mk_fun_ty(fun_args.copied(), ret_ty))
                    }
                    _ => Err(ReducIrTyErr::ExpectedFunTy { ty: func_ty, func }),
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
                                return Err(ReducIrTyErr::TyMismatch {
                                    left_ir: Cow::Borrowed(branch),
                                    left_ty: arg_tys[0],
                                    right_ir: Cow::Borrowed(discr.deref()),
                                    right_ty: ty,
                                });
                            }
                            if ret_ty != *case_ty {
                                return Err(ReducIrTyErr::TyMismatch {
                                    left_ty: ret_ty,
                                    left_ir: Cow::Borrowed(branch),
                                    right_ty: *case_ty,
                                    right_ir: Cow::Borrowed(self),
                                });
                            }
                        }
                        _ => {
                            return Err(ReducIrTyErr::ExpectedFunTy {
                                ty: branch_ty,
                                func: branch,
                            });
                        }
                    }
                }
                Ok(*case_ty)
            }
            X(xt) => xt.type_check(ctx),
            Tag(ty, _, _) => Ok(*ty),
            Item(_, ty) => Ok(*ty),
        }
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
}

#[derive(Debug, PartialEq, Eq)]
pub enum ReducIrTyErr<'a, Ext: Clone> {
    TyMismatch {
        left_ty: ReducIrTy,
        left_ir: Cow<'a, ReducIr<Ext>>,
        right_ty: ReducIrTy,
        right_ir: Cow<'a, ReducIr<Ext>>,
    },
    KindMistmatch(Kind, Kind),
    ExpectedFunTy {
        ty: ReducIrTy,
        func: &'a ReducIr<Ext>,
    },
    ExpectedForallTy(ReducIrTy),
    ExpectedProdTy(ReducIrTy),
    ExpectedCoprodTy(ReducIrTy),
    ExpectedMarkerTy(ReducIrTy),
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
            App(head, spine) => {
                self.stack.push(head);
                self.stack.extend(spine.iter());
                self.next()
            }
            X(DelimCont::NewPrompt(v, body)) => {
                self.bound.insert(v.var);
                self.stack.push(body.deref());
                self.next()
            }
            X(DelimCont::Yield(_, a, b)) => {
                self.stack.extend([a.deref(), b.deref()]);
                self.next()
            }
            X(DelimCont::Prompt(a, b, c)) => {
                self.stack.extend([a.deref(), b.deref(), c.deref()]);
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
