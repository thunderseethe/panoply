use aiahr_core::id::{IdSupply, ReducIrTyVarId, ReducIrVarId, TermName};
use aiahr_core::ident::Ident;
use aiahr_core::modules::Module;
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

pub mod mon;
pub mod optimized;

#[salsa::jar(db = Db)]
pub struct Jar(
    ty::ReducIrTy,
    GeneratedReducIrName,
    ReducIrModule,
    ReducIrItem,
    ReducIrGenItem,
    ReducIrRowEv,
    mon::MonReducIrModule,
    mon::MonReducIrItem,
    mon::MonReducIrGenItem,
    mon::MonReducIrRowEv,
    optimized::OptimizedReducIrItem,
    optimized::OptimizedReducIrModule,
);
pub trait Db: salsa::DbWithJar<Jar> + aiahr_core::Db + aiahr_ty::Db {
    fn as_reducir_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
    }
}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + aiahr_core::Db + aiahr_ty::Db {}

#[salsa::interned]
pub struct GeneratedReducIrName {
    pub name: Ident,
    pub module: Module,
}

#[salsa::tracked]
pub struct ReducIrModule {
    #[id]
    pub module: Module,
    #[return_ref]
    pub items: Vec<ReducIrItem>,
}

#[salsa::tracked]
pub struct ReducIrGenItem {
    #[id]
    pub name: GeneratedReducIrName,
    #[return_ref]
    pub item: DelimReducIr,
}

#[salsa::tracked]
pub struct ReducIrRowEv {
    pub simple: ReducIrGenItem,
    pub scoped: ReducIrGenItem,
}

#[salsa::tracked]
pub struct ReducIrItem {
    #[id]
    pub name: TermName,
    #[return_ref]
    pub item: DelimReducIr,
    // List of row evidence items that this item references
    #[return_ref]
    pub row_evs: Vec<ReducIrRowEv>,
    #[return_ref]
    pub var_supply: IdSupply<ReducIrVarId>,
    #[return_ref]
    pub tyvar_supply: IdSupply<ReducIrTyVarId>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug)]
pub enum ReducIrTermName {
    // A term that existed in syntax and was most likely written by a user
    Term(TermName),
    // A generated term that only exists at the ReducIr level
    Gen(GeneratedReducIrName),
}
impl ReducIrTermName {
    pub fn term<DB: ?Sized + aiahr_core::Db>(db: &DB, name: impl ToString, module: Module) -> Self {
        ReducIrTermName::Term(TermName::new(
            db.as_core_db(),
            db.ident(name.to_string()),
            module,
        ))
    }

    pub fn gen<DB: ?Sized + crate::Db>(db: &DB, name: impl ToString, module: Module) -> Self {
        ReducIrTermName::Gen(GeneratedReducIrName::new(
            db.as_reducir_db(),
            db.ident(name.to_string()),
            module,
        ))
    }

    pub fn name<DB: ?Sized + crate::Db>(&self, db: &DB) -> Ident {
        match self {
            ReducIrTermName::Term(term) => term.name(db.as_core_db()),
            ReducIrTermName::Gen(gen) => gen.name(db.as_reducir_db()),
        }
    }

    pub fn module<DB: ?Sized + crate::Db>(&self, db: &DB) -> Module {
        match self {
            ReducIrTermName::Term(term) => term.module(db.as_core_db()),
            ReducIrTermName::Gen(gen) => gen.module(db.as_reducir_db()),
        }
    }
}

/// A ReducIrLocal variable, marked with it's top level term name for uniqueness
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct ReducIrLocal {
    pub top_level: ReducIrTermName,
    pub id: ReducIrVarId,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct ReducIrVar {
    pub var: ReducIrLocal,
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
pub struct Lets {
    pub binds: Vec<(ReducIrVar, ReducIr<Lets>)>,
    pub body: P<ReducIr<Lets>>,
    // TODO: Join points?
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ReducIrKind<Ext = Infallible> {
    Int(usize),
    Var(ReducIrVar),
    Item(ReducIrTermName, ReducIrTy),
    // Value abstraction and application
    Abs(Box<[ReducIrVar]>, P<ReducIr<Ext>>),
    App(P<ReducIr<Ext>>, Vec<ReducIr<Ext>>),
    // Type abstraction and application
    TyAbs(ReducIrVarTy, P<ReducIr<Ext>>),
    TyApp(P<ReducIr<Ext>>, Vec<ReducIrTyApp>),
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
pub struct ReducIr<Ext = Infallible> {
    pub kind: ReducIrKind<Ext>,
}

pub type DelimReducIr = ReducIr<DelimCont>;

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

    pub fn tag(ty: ReducIrTy, tag: usize, value: Self) -> Self {
        ReducIr::new(Tag(ty, tag, P::new(value)))
    }

    pub fn ext(ext: Ext) -> Self {
        ReducIr::new(ReducIrKind::X(ext))
    }

    pub fn ty_abs<II>(vars: II, body: Self) -> Self
    where
        II: IntoIterator<Item = ReducIrVarTy>,
        II::IntoIter: DoubleEndedIterator,
    {
        vars.into_iter()
            .rfold(body, |body, var| ReducIr::new(TyAbs(var, P::new(body))))
    }

    pub fn ty_app(mut head: Self, spine: impl IntoIterator<Item = ReducIrTyApp>) -> Self {
        if let ReducIrKind::TyApp(_, ty_apps) = &mut head.kind {
            ty_apps.extend(spine);
            return head;
        }
        let mut spine = spine.into_iter().peekable();
        if spine.peek().is_none() {
            // If iterator is empty don't allocate TyApp
            head
        } else {
            ReducIr::new(TyApp(P::new(head), spine.collect()))
        }
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

    pub fn abss_with_innermost(
        vars: impl IntoIterator<Item = ReducIrVar>,
        innermost_vars: impl IntoIterator<Item = ReducIrVar>,
        mut body: Self,
    ) -> Self {
        let mut vars = vars.into_iter().peekable();
        let mut innermost_vars = innermost_vars.into_iter().peekable();
        if vars.peek().is_none() && innermost_vars.peek().is_none() {
            body
        } else {
            if let Abs(ref mut ivars, _) = body.kind {
                *ivars = vars
                    .chain(ivars.iter().copied())
                    .chain(innermost_vars)
                    .collect();
                return body;
            }
            ReducIr::new(Abs(vars.chain(innermost_vars).collect(), P::new(body)))
        }
    }

    pub fn abss<I>(vars: I, body: Self) -> Self
    where
        I: IntoIterator<Item = ReducIrVar>,
    {
        Self::abss_with_innermost(vars, [], body)
    }

    pub fn map_within_abss(mut self, op: impl FnOnce(Self) -> Self) -> Self {
        if let Abs(_, inner) = &mut self.kind {
            let body = std::mem::replace(inner.as_mut(), ReducIr::new(ReducIrKind::Int(0)));
            *inner.as_mut() = op(body);
            self
        } else {
            op(self)
        }
    }

    pub fn map_within_lets(mut self, op: impl FnOnce(Self) -> Self) -> Self {
        if let App(head, _) = &mut self.kind {
            if let Abs(_, inner) = &mut head.as_mut().kind {
                let body = std::mem::replace(inner.as_mut(), ReducIr::new(ReducIrKind::Int(0)));
                *inner.as_mut() = op(body);
                return self;
            }
        }
        op(self)
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

    pub fn case(ty: ReducIrTy, discr: Self, cases: impl IntoIterator<Item = Self>) -> Self {
        ReducIr::new(Case(ty, P::new(discr), cases.into_iter().collect()))
    }

    fn contains_var(&self, var: &ReducIrVar) -> bool {
        match self.kind() {
            Int(_) | Item(_, _) => false,
            Var(v) => v == var,
            Abs(_, body)
            | TyAbs(_, body)
            | TyApp(body, _)
            | FieldProj(_, body)
            | Tag(_, _, body) => body.contains_var(var),
            App(head, spine) => {
                head.contains_var(var) || spine.iter().any(|ir| ir.contains_var(var))
            }
            Struct(elems) => elems.iter().any(|ir| ir.contains_var(var)),
            Case(_, discr, branches) => {
                discr.contains_var(var) || branches.iter().any(|ir| ir.contains_var(var))
            }
            X(_) => todo!(),
        }
    }

    pub fn local(var: ReducIrVar, value: Self, mut body: Self) -> Self {
        if let App(ref mut head, ref mut spine) = body.kind {
            if spine.iter().all(|arg| !arg.contains_var(&var)) {
                if let Abs(ref mut vars, _) = head.as_mut().kind {
                    *vars = std::iter::once(var).chain(vars.iter().copied()).collect();
                    spine.insert(0, value);
                    return body;
                }
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

    pub fn try_top_level_def(&self) -> Result<TopLevelDef<Ext>, &Self> {
        let mut ir = self;
        let mut ty_vars = vec![];
        while let TyAbs(ty_var, body) = ir.kind() {
            ty_vars.push(*ty_var);
            ir = body;
        }
        match ir.kind() {
            Abs(args, body) => Ok(TopLevelDef {
                ty_vars,
                vars: args,
                body: body.deref(),
            }),
            _ => Err(self),
        }
    }
}

pub struct TopLevelDef<'a, Ext> {
    pub ty_vars: Vec<ReducIrVarTy>,
    pub vars: &'a [ReducIrVar],
    pub body: &'a ReducIr<Ext>,
}

impl ReducIr<Lets> {
    pub fn locals(
        binds: impl IntoIterator<Item = (ReducIrVar, ReducIr<Lets>)>,
        body: ReducIr<Lets>,
    ) -> Self {
        let mut binds_iter = binds.into_iter().peekable();
        if binds_iter.peek().is_none() {
            body
        } else {
            ReducIr::new(X(Lets {
                binds: binds_iter.collect(),
                body: P::new(body),
            }))
        }
    }
}

pub trait TraverseExtInPlace {
    fn traverse_in_place<F: ?Sized + ReducIrInPlaceFold<Ext = Self>>(&mut self, fold: &mut F);
}

impl TraverseExtInPlace for Infallible {
    fn traverse_in_place<F: ?Sized + ReducIrInPlaceFold<Ext = Self>>(&mut self, _: &mut F) {
        unreachable!()
    }
}
impl TraverseExtInPlace for Lets {
    fn traverse_in_place<F: ?Sized + ReducIrInPlaceFold<Ext = Self>>(&mut self, fold: &mut F) {
        for (_, defn) in self.binds.iter_mut() {
            defn.fold_in_place(fold);
        }
        self.body.as_mut().fold_in_place(fold)
    }
}

pub trait ReducIrInPlaceFold {
    type Ext: TraverseExtInPlace;

    fn fold_ir_in_place(&mut self, _: &mut ReducIrKind<Self::Ext>) {}

    fn traverse_ir_in_place(&mut self, ir: &mut ReducIr<Self::Ext>) {
        match ir.kind {
            Case(_, ref mut discr, ref mut branches) => {
                discr.as_mut().fold_in_place(self);
                for branch in branches.iter_mut() {
                    branch.fold_in_place(self)
                }
            }
            App(ref mut head, ref mut spine) => {
                head.as_mut().fold_in_place(self);
                for arg in spine.iter_mut() {
                    arg.fold_in_place(self)
                }
            }
            Struct(ref mut elems) => {
                for elem in elems.iter_mut() {
                    elem.fold_in_place(self)
                }
            }
            Tag(_, _, ref mut body)
            | FieldProj(_, ref mut body)
            | Abs(_, ref mut body)
            | TyAbs(_, ref mut body)
            | TyApp(ref mut body, _) => {
                body.as_mut().fold_in_place(self);
            }
            X(ref mut ext) => ext.traverse_in_place(self),
            _ => {}
        };
        self.fold_ir_in_place(&mut ir.kind);
    }
}

pub trait ReducIrEndoFold: Sized {
    type Ext: Clone;

    fn endofold_ir(&mut self, kind: ReducIrKind<Self::Ext>) -> ReducIr<Self::Ext> {
        ReducIr::new(kind)
    }

    fn endotraverse_ir(&mut self, ir: &ReducIr<Self::Ext>) -> ReducIr<Self::Ext> {
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
impl<F: ReducIrEndoFold + Sized> ReducIrFold for F {
    type InExt = F::Ext;

    type OutExt = F::Ext;

    fn fold_ext(&mut self, ext: &Self::InExt) -> Self::OutExt {
        ext.clone()
    }

    fn fold_ir(&mut self, kind: ReducIrKind<Self::OutExt>) -> ReducIr<Self::OutExt> {
        self.endofold_ir(kind)
    }

    fn traverse_ir(&mut self, ir: &ReducIr<Self::InExt>) -> ReducIr<Self::OutExt> {
        self.endotraverse_ir(ir)
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

impl<Ext> ReducIr<Ext> {
    pub fn fold<F: ?Sized + ReducIrFold<InExt = Ext>>(&self, fold: &mut F) -> ReducIr<F::OutExt> {
        fold.traverse_ir(self)
    }

    pub fn fold_in_place<F: ?Sized + ReducIrInPlaceFold<Ext = Ext>>(&mut self, fold: &mut F) {
        fold.traverse_ir_in_place(self)
    }
}

impl<Ext: std::fmt::Debug> ReducIr<Ext> {
    // Unwrap top level item nodes (TyAbs and Abs) before applying fold in place.
    pub fn fold_in_place_inside_item<F: ?Sized + ReducIrInPlaceFold<Ext = Ext>>(
        &mut self,
        fold: &mut F,
    ) {
        let mut body = self;
        while let TyAbs(_, ref mut inner) = body.kind {
            body = inner.as_mut();
        }
        while let Abs(_, ref mut inner) = body.kind {
            body = inner.as_mut()
        }
        fold.traverse_ir_in_place(body)
    }
}

impl<Ext> ReducIr<Ext> {
    /// Allows "casting" to another Ext type by assuming `ReducIrKind::X` is not used anywhere in
    /// `self`.
    /// Panics if `ReducIrKind::X` is present in `self`.
    pub fn assume_no_ext(&self) -> ReducIr {
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

impl ReducIr<Lets> {
    fn free_var_aux(
        &self,
        in_scope: &mut FxHashSet<ReducIrLocal>,
        bound: &mut FxHashSet<ReducIrVar>,
    ) {
        match &self.kind {
            Int(_) | Item(_, _) => {}
            Var(v) => {
                if !in_scope.contains(&v.var) {
                    bound.insert(*v);
                }
            }
            Abs(vars, body) => {
                let tmp = in_scope.clone();
                in_scope.extend(vars.iter().map(|v| v.var));
                body.free_var_aux(in_scope, bound);
                *in_scope = tmp;
            }
            App(func, spine) => {
                func.free_var_aux(in_scope, bound);
                for arg in spine.iter() {
                    arg.free_var_aux(in_scope, bound);
                }
            }
            TyAbs(_, body) => body.free_var_aux(in_scope, bound),
            TyApp(body, _) => body.free_var_aux(in_scope, bound),
            Struct(elems) => {
                for e in elems.iter() {
                    e.free_var_aux(in_scope, bound);
                }
            }
            FieldProj(_, base) => base.free_var_aux(in_scope, bound),
            Tag(_, _, val) => val.free_var_aux(in_scope, bound),
            Case(_, discr, branches) => {
                discr.free_var_aux(in_scope, bound);
                for branch in branches.iter() {
                    branch.free_var_aux(in_scope, bound);
                }
            }
            X(Lets { binds, body }) => {
                for (var, defn) in binds.iter() {
                    defn.free_var_aux(in_scope, bound);
                    in_scope.insert(var.var);
                }
                body.free_var_aux(in_scope, bound);
            }
        }
    }

    pub fn free_var_set(&self) -> FxHashSet<ReducIrVar> {
        let mut free_vars = FxHashSet::default();
        let mut in_scope = FxHashSet::default();
        self.free_var_aux(&mut in_scope, &mut free_vars);
        free_vars
    }
}

impl ReducIr {
    pub fn free_vars<'a>(&'a self) -> Box<dyn Iterator<Item = ReducIrVar> + 'a> {
        match &self.kind {
            Int(_) | Item(_, _) => Box::new(std::iter::empty()),
            Var(v) => Box::new(std::iter::once(*v)),
            Abs(vars, body) => Box::new(body.free_vars().filter(move |v| !vars.contains(v))),
            App(func, args) => Box::new(
                func.free_vars()
                    .chain(args.iter().flat_map(|arg| arg.free_vars())),
            ),
            TyAbs(_, body) => Box::new(body.free_vars()),
            TyApp(head, _) => Box::new(head.free_vars()),
            Struct(elems) => Box::new(elems.iter().flat_map(|e| e.free_vars())),
            FieldProj(_, base) => Box::new(base.free_vars()),
            Tag(_, _, base) => Box::new(base.free_vars()),
            Case(_, discr, branches) => Box::new(
                discr
                    .free_vars()
                    .chain(branches.iter().flat_map(|b| b.free_vars())),
            ),
            X(_) => unreachable!(),
        }
    }
}

pub trait TypeCheck {
    type Ext: Clone;
    fn type_check<'a, DB: ?Sized + crate::Db>(
        &'a self,
        ctx: &DB,
    ) -> Result<ReducIrTy, ReducIrTyErr<'a, Self::Ext>>;
}
impl TypeCheck for DelimCont {
    type Ext = Self;
    fn type_check<DB: ?Sized + crate::Db>(
        &self,
        ctx: &DB,
    ) -> Result<ReducIrTy, ReducIrTyErr<Self::Ext>> {
        use ty::ReducIrTyKind::*;
        match self {
            DelimCont::NewPrompt(prompt, body) => {
                if let MarkerTy(_) = prompt.ty.kind(ctx.as_reducir_db()) {
                    body.type_check(ctx)
                } else {
                    Err(ReducIrTyErr::ExpectedMarkerTy(prompt.ty))
                }
            }
            DelimCont::Prompt(marker, _upd_handler, body) => {
                let marker_ty = marker.type_check(ctx)?;
                if let MarkerTy(_) = marker_ty.kind(ctx.as_reducir_db()) {
                    body.type_check(ctx)
                } else {
                    Err(ReducIrTyErr::ExpectedMarkerTy(marker_ty))
                }
            }
            DelimCont::Yield(ty, marker, body) => {
                let marker_ty = marker.type_check(ctx)?;
                let MarkerTy(_) = marker_ty.kind(ctx.as_reducir_db()) else {
                    return Err(ReducIrTyErr::ExpectedMarkerTy(marker_ty));
                };
                // We want to make sure body type checks but we don't actually use the result
                let _ = body.type_check(ctx)?;
                Ok(*ty)
            }
        }
    }
}
impl TypeCheck for Lets {
    type Ext = Self;

    fn type_check<'a, DB: ?Sized + crate::Db>(
        &'a self,
        ctx: &DB,
    ) -> Result<ReducIrTy, ReducIrTyErr<'a, Self::Ext>> {
        for (var, defn) in self.binds.iter() {
            let defn_ty = defn.type_check(ctx)?;
            if var.ty != defn_ty {
                return Err(ReducIrTyErr::TyMismatch {
                    left_ty: var.ty,
                    left_ir: Cow::Owned(ReducIr::var(*var)),
                    right_ty: defn_ty,
                    right_ir: Cow::Borrowed(defn),
                });
            }
        }
        self.body.type_check(ctx)
    }
}
impl TypeCheck for Infallible {
    type Ext = Self;
    fn type_check<DB: ?Sized + crate::Db>(
        &self,
        _: &DB,
    ) -> Result<ReducIrTy, ReducIrTyErr<Self::Ext>> {
        unreachable!()
    }
}
impl<Ext: TypeCheck<Ext = Ext> + Clone> TypeCheck for ReducIr<Ext> {
    type Ext = Ext;

    fn type_check<DB: ?Sized + crate::Db>(&self, ctx: &DB) -> Result<ReducIrTy, ReducIrTyErr<Ext>> {
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
                match func_ty.kind(ctx.as_reducir_db()) {
                    FunTy(fun_arg_tys, ret_ty) => {
                        debug_assert!(args.len() <= fun_arg_tys.len());
                        let mut fun_args = fun_arg_tys.iter().peekable();
                        for (fun_arg_ty, (arg_index, arg)) in
                            fun_args.zip_non_consuming(&mut args_iter.enumerate().peekable())
                        {
                            let arg_ty = arg.type_check(ctx.as_reducir_db())?;
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
            TyApp(forall, ty_apps) => {
                let forall_ty = forall.type_check(ctx)?;
                ty_apps.iter().try_fold(forall_ty, |forall_ty, ty_app| {
                    match forall_ty.kind(ctx.as_reducir_db()) {
                        ForallTy(kind, ret_ty) => match (kind, ty_app) {
                            (Kind::Type, ReducIrTyApp::Ty(ty)) => {
                                Ok(ret_ty.subst_single(ctx.as_reducir_db(), *ty))
                            }
                            (Kind::SimpleRow, ReducIrTyApp::DataRow(row))
                            | (Kind::ScopedRow, ReducIrTyApp::EffRow(row)) => {
                                Ok(ret_ty.subst_single(ctx.as_reducir_db(), row.clone()))
                            }
                            (k, ReducIrTyApp::Ty(_)) => {
                                Err(ReducIrTyErr::KindMistmatch(k, Kind::Type))
                            }
                            (k, ReducIrTyApp::DataRow(_)) => {
                                Err(ReducIrTyErr::KindMistmatch(k, Kind::SimpleRow))
                            }
                            (k, ReducIrTyApp::EffRow(_)) => {
                                Err(ReducIrTyErr::KindMistmatch(k, Kind::ScopedRow))
                            }
                        },
                        _ => Err(ReducIrTyErr::ExpectedForallTy(forall_ty)),
                    }
                })
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
                match strukt_ty.kind(ctx.as_reducir_db()) {
                    ProductTy(tys) => Ok(tys[*indx]),
                    _ => Err(ReducIrTyErr::ExpectedProdTy(
                        strukt_ty,
                        Cow::Borrowed(strukt),
                    )),
                }
            }
            Case(case_ty, discr, branches) => {
                let coprod = discr.type_check(ctx)?;
                let tys = match coprod.kind(ctx.as_reducir_db()) {
                    CoproductTy(tys) => tys,
                    // We have to coerce control to an enum type here
                    // This is the toil we pay for special casing control
                    ControlTy(m_ty, a_ty) => {
                        let exists_b = ctx.mk_reducir_ty(VarTy(2));
                        let exists_m = ctx.mk_reducir_ty(VarTy(1));
                        let exists_r = ctx.mk_reducir_ty(VarTy(0));
                        let exists_mon_m_r = ctx.mk_mon_ty(exists_m, exists_r);
                        let exists_body_fun_ty = ctx.mk_mon_ty(m_ty, a_ty).shift(ctx, 3);
                        let yield_ty = ctx.mk_forall_ty(
                            [Kind::Type, Kind::Type, Kind::Type],
                            ProductTy(vec![
                                ctx.mk_reducir_ty(MarkerTy(exists_r)),
                                ctx.mk_fun_ty(
                                    [ctx.mk_fun_ty([exists_b], exists_mon_m_r)],
                                    exists_mon_m_r,
                                ),
                                ctx.mk_fun_ty([exists_b], exists_body_fun_ty),
                            ]),
                        );
                        vec![a_ty, yield_ty]
                    }
                    _ => {
                        return Err(ReducIrTyErr::ExpectedCoprodTy(coprod, Cow::Borrowed(discr)));
                    }
                };
                for (branch, ty) in branches.iter().zip(tys.into_iter()) {
                    let branch_ty = branch.type_check(ctx)?;
                    match branch_ty.kind(ctx.as_reducir_db()) {
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

impl DelimReducIr {
    pub fn unbound_vars(&self) -> impl Iterator<Item = ReducIrVar> + '_ {
        self.unbound_vars_with_bound(FxHashSet::default())
    }

    pub fn unbound_vars_with_bound(
        &self,
        bound: FxHashSet<ReducIrLocal>,
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
    ExpectedProdTy(ReducIrTy, Cow<'a, ReducIr<Ext>>),
    ExpectedCoprodTy(ReducIrTy, Cow<'a, ReducIr<Ext>>),
    ExpectedMarkerTy(ReducIrTy),
}

struct UnboundVars<'a> {
    bound: FxHashSet<ReducIrLocal>,
    stack: Vec<&'a DelimReducIr>,
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
