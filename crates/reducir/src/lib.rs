use base::{
  id::{IdSupply, ReducIrTyVarId, ReducIrVarId, TermName},
  ident::Ident,
  modules::Module,
  pretty::PrettyWithCtx,
};
use rustc_hash::FxHashSet;
use std::borrow::Cow;
use std::convert::Infallible;
use std::fmt;
use std::ops::Deref;
use ReducIrKind::*;

use crate::ty::ReducIrVarTy;

use self::{
  subst::SubstTy,
  ty::{Kind, MkReducIrTy, ReducIrTy, ReducIrTyApp, Subst},
};

mod pretty;
pub mod ty;

pub mod zip_non_consuming;
use zip_non_consuming::ZipNonConsuming;

pub mod mon;
pub mod optimized;

mod subst {
  use std::convert::Infallible;

  use crate::ty::{ReducIrRow, ReducIrTy, ReducIrTyApp, Subst};
  use crate::{
    default_endotraverse_ir, Bind, ReducIr, ReducIrEndoFold, ReducIrFold, ReducIrKind, ReducIrVar,
    P,
  };

  pub(crate) struct SubstTy<'a> {
    pub(crate) db: &'a dyn crate::Db,
    pub(crate) subst: Subst,
  }

  impl<'a> SubstTy<'a> {
    pub fn new<DB: ?Sized + crate::Db>(db: &'a DB, subst: Subst) -> Self {
      Self {
        db: db.as_reducir_db(),
        subst,
      }
    }

    fn subst(&self, ty: ReducIrTy) -> ReducIrTy {
      ty.subst(self.db.as_reducir_db(), self.subst.clone())
    }
  }

  impl ReducIrEndoFold for SubstTy<'_> {
    type Ext = Infallible;

    fn endofold_ir(&mut self, kind: ReducIrKind<Self::Ext>) -> ReducIr<Self::Ext> {
      match kind {
        ReducIrKind::TyApp(body, ty_apps) => {
          let ty_apps = ty_apps
            .into_iter()
            .map(|ty_app| match ty_app {
              ReducIrTyApp::Ty(ty) => ReducIrTyApp::Ty(self.subst(ty)),
              // Don't do anything for these
              ReducIrTyApp::DataRow(ReducIrRow::Open(_))
              | ReducIrTyApp::EffRow(ReducIrRow::Open(_)) => ty_app,
              ReducIrTyApp::EffRow(ReducIrRow::Closed(row)) => ReducIrTyApp::EffRow(
                ReducIrRow::Closed(row.into_iter().map(|ty| self.subst(ty)).collect()),
              ),
              ReducIrTyApp::DataRow(ReducIrRow::Closed(row)) => ReducIrTyApp::DataRow(
                ReducIrRow::Closed(row.into_iter().map(|ty| self.subst(ty)).collect()),
              ),
            })
            .collect();
          ReducIr::new(ReducIrKind::TyApp(body, ty_apps))
        }
        ReducIrKind::Var(var) => ReducIr::var(ReducIrVar::new(var.var, self.subst(var.ty))),
        ReducIrKind::Item(occ) => ReducIr::new(ReducIrKind::Item(occ.map_ty(|ty| self.subst(ty)))),
        ReducIrKind::Abs(vars, body) => ReducIr::new(ReducIrKind::Abs(
          vars
            .iter()
            .map(|v| ReducIrVar::new(v.var, self.subst(v.ty)))
            .collect(),
          body,
        )),
        ReducIrKind::Locals(binds, body) => ReducIr::new(ReducIrKind::Locals(
          binds
            .into_iter()
            .map(|bind| {
              Bind::new(
                ReducIrVar::new(bind.var.var, self.subst(bind.var.ty)),
                bind.defn,
              )
            })
            .collect(),
          body,
        )),
        ReducIrKind::Tag(ty, tag, value) => {
          ReducIr::new(ReducIrKind::Tag(self.subst(ty), tag, value))
        }
        ReducIrKind::Case(ty, discr, branches) => {
          ReducIr::new(ReducIrKind::Case(self.subst(ty), discr, branches))
        }
        ReducIrKind::Coerce(ty, body) => ReducIr::new(ReducIrKind::Coerce(self.subst(ty), body)),
        kind => ReducIr::new(kind),
      }
    }

    fn endotraverse_ir(&mut self, ir: &ReducIr<Self::Ext>) -> ReducIr<Self::Ext> {
      use ReducIrKind::*;
      match ir.kind() {
        TyAbs(ty_var, body) => {
          let subst = self.subst.clone();
          let subst = std::mem::replace(&mut self.subst, subst.lift());
          let body = body.fold(self);
          self.subst = subst;
          self.fold_ir(TyAbs(*ty_var, P::new(body)))
        }
        _ => default_endotraverse_ir(self, ir),
      }
    }
  }
}

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
pub trait Db: salsa::DbWithJar<Jar> + base::Db + ::ty::Db {
  fn as_reducir_db(&self) -> &dyn crate::Db {
    <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
  }
}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + base::Db + ::ty::Db {}

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
  pub fn term<DB: ?Sized + base::Db>(db: &DB, name: impl ToString, module: Module) -> Self {
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

impl ReducIrLocal {
  pub fn new(top_level: ReducIrTermName, id: ReducIrVarId) -> Self {
    Self { top_level, id }
  }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct ReducIrVar {
  pub var: ReducIrLocal,
  pub ty: ReducIrTy,
  pub is_join_point: bool,
}

impl ReducIrVar {
  pub fn new(var: ReducIrLocal, ty: ReducIrTy) -> Self {
    Self {
      var,
      ty,
      is_join_point: false,
    }
  }

  pub fn with_join(var: ReducIrLocal, ty: ReducIrTy) -> Self {
    Self {
      var,
      ty,
      is_join_point: true,
    }
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
  Prompt {
    marker: P<ReducIr<DelimCont>>,
    upd_evv: P<ReducIr<DelimCont>>,
    body: P<ReducIr<DelimCont>>,
  },
  // Yield to a marker's prompt
  Yield {
    ret_ty: ReducIrTy,
    marker: P<ReducIr<DelimCont>>,
    body: P<ReducIr<DelimCont>>,
  },
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// A local binding
pub struct Bind<Ext> {
  pub var: ReducIrVar,
  pub defn: ReducIr<Ext>,
}
impl<Ext> Bind<Ext> {
  pub fn new(var: ReducIrVar, defn: ReducIr<Ext>) -> Self {
    Self { var, defn }
  }

  fn fold<F: ?Sized + ReducIrFold<InExt = Ext>>(&self, fold: &mut F) -> Bind<F::OutExt> {
    Bind {
      var: self.var,
      defn: self.defn.fold(fold),
    }
  }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct ReducIrItemOccurence {
  pub name: ReducIrTermName,
  pub ty: ReducIrTy,
  /// Can this item occurence be inlined
  pub inline: bool,
}

impl ReducIrItemOccurence {
  pub fn new(name: ReducIrTermName, ty: ReducIrTy) -> Self {
    Self {
      name,
      ty,
      inline: true,
    }
  }

  pub fn with_inline(name: ReducIrTermName, ty: ReducIrTy, inline: bool) -> Self {
    Self { name, ty, inline }
  }

  pub fn map_ty(self, ty_op: impl FnOnce(ReducIrTy) -> ReducIrTy) -> Self {
    Self {
      ty: ty_op(self.ty),
      ..self
    }
  }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ReducIrKind<Ext = Infallible> {
  // Values
  Int(usize),
  Var(ReducIrVar),
  Item(ReducIrItemOccurence),
  // Value abstraction and application
  Abs(Box<[ReducIrVar]>, P<ReducIr<Ext>>),
  App(P<ReducIr<Ext>>, Vec<ReducIr<Ext>>),
  Locals(Vec<Bind<Ext>>, P<ReducIr<Ext>>),
  // Type abstraction and application
  TyAbs(ReducIrVarTy, P<ReducIr<Ext>>),
  TyApp(P<ReducIr<Ext>>, Vec<ReducIrTyApp>),
  // Trivial products
  Struct(Vec<ReducIr<Ext>>),         // Intro
  FieldProj(usize, P<ReducIr<Ext>>), // Elim
  // Trivial coproducts
  Tag(ReducIrTy, usize, P<ReducIr<Ext>>), // Intro
  Case(ReducIrTy, P<ReducIr<Ext>>, Box<[ReducIr<Ext>]>), // Elim
  // TODO: It'd be nice to not need this.
  Coerce(ReducIrTy, P<ReducIr<Ext>>),
  // Extensions
  X(Ext),
}
impl<Ext> ReducIrKind<Ext> {
  pub fn item(name: ReducIrTermName, ty: ReducIrTy) -> Self {
    Self::Item(ReducIrItemOccurence::new(name, ty))
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
    vars
      .into_iter()
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

  pub fn app(head: Self, spine: impl IntoIterator<Item = Self>) -> Self {
    // This just exists because we might call app recursively if head is an Abs.
    // If we actually call `app` recursively we evoke a recursive IntoIterator check that fails.
    // So instead we call this inner function and don't recurse.
    fn app_inner<Ext>(
      mut head: ReducIr<Ext>,
      spine: impl IntoIterator<Item = ReducIr<Ext>>,
    ) -> ReducIr<Ext> {
      if let App(_, ref mut in_place_spine) = &mut head.kind {
        in_place_spine.extend(spine);
        head
      } else {
        let mut spine_iter = spine.into_iter().peekable();
        if spine_iter.peek().is_some() {
          ReducIr::new(App(P::new(head), spine_iter.collect()))
        } else {
          // If application is empty we don't create an application node
          head
        }
      }
    }

    if let Abs(vars, body) = head.kind {
      let mut vars = vars.iter().copied().peekable();
      let mut spine = spine.into_iter().peekable();
      let mut binds = vec![];
      let body = loop {
        match (vars.peek(), spine.peek()) {
          (None, None) => break body.into_inner(),
          (None, Some(_)) => break app_inner(body.into_inner(), spine),
          (Some(_), None) => break ReducIr::abss(vars, body.into_inner()),
          (Some(_), Some(_)) => binds.push(Bind::new(vars.next().unwrap(), spine.next().unwrap())),
        }
      };
      ReducIr::locals(binds, body)
    } else {
      app_inner(head, spine)
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

  pub fn field_proj(indx: usize, target: Self) -> Self {
    ReducIr::new(FieldProj(indx, P::new(target)))
  }

  pub fn locals(binds: impl IntoIterator<Item = Bind<Ext>>, mut body: Self) -> Self {
    if let Locals(ref mut body_binds, _) = &mut body.kind {
      let mut binds = binds.into_iter().collect::<Vec<_>>();
      binds.append(body_binds);
      *body_binds = binds;
      body
    } else {
      let mut binds = binds.into_iter().peekable();
      if binds.peek().is_none() {
        body
      } else {
        ReducIr::new(Locals(binds.collect(), P::new(body)))
      }
    }
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

impl<Ext> ReducIr<Ext> {
  pub fn local(var: ReducIrVar, value: Self, body: Self) -> Self {
    Self::locals(std::iter::once(Bind::new(var, value)), body)
  }
}

pub struct TopLevelDef<'a, Ext> {
  pub ty_vars: Vec<ReducIrVarTy>,
  pub vars: &'a [ReducIrVar],
  pub body: &'a ReducIr<Ext>,
}

impl ReducIr<Infallible> {
  /// Apply a subst to all types within self
  pub fn subst<DB: ?Sized + crate::Db>(&self, db: &DB, subst: Subst) -> Self {
    self.fold(&mut SubstTy::new(db, subst))
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

pub fn default_endotraverse_ir<F: ReducIrEndoFold>(
  fold: &mut F,
  ir: &ReducIr<F::Ext>,
) -> ReducIr<F::Ext> {
  match ir.kind() {
    Abs(vars, body) => {
      let body = body.fold(fold);
      // We do it this way to reuse smart constructor logic
      fold.fold_ir(ReducIr::abss(vars.iter().copied(), body).kind)
    }
    App(head, spine) => {
      let head = head.fold(fold);
      let spine = spine.iter().map(|ir| ir.fold(fold));
      // We do it this way to reuse smart constructor logic
      let kind = ReducIr::app(head, spine).kind;
      fold.fold_ir(kind)
    }
    Locals(binds, body) => {
      let body = body.fold(fold);
      let binds = binds.iter().map(|local| local.fold(fold));
      let kind = ReducIr::locals(binds, body).kind;
      fold.fold_ir(kind)
    }
    TyAbs(ty_var, body) => {
      let body = body.fold(fold);
      fold.fold_ir(TyAbs(*ty_var, P::new(body)))
    }
    TyApp(body, ty_app) => {
      let body = body.fold(fold);
      let kind = ReducIr::ty_app(body, ty_app.iter().cloned()).kind;
      fold.fold_ir(kind)
    }
    Struct(elems) => {
      let elems = elems.iter().map(|e| e.fold(fold)).collect();
      fold.fold_ir(Struct(elems))
    }
    FieldProj(indx, body) => {
      let body = body.fold(fold);
      fold.fold_ir(FieldProj(*indx, P::new(body)))
    }
    Tag(ty, tag, body) => {
      let body = body.fold(fold);
      fold.fold_ir(Tag(*ty, *tag, P::new(body)))
    }
    Case(ty, discr, branches) => {
      let discr = discr.fold(fold);
      let branches = branches.iter().map(|branch| branch.fold(fold)).collect();
      fold.fold_ir(Case(*ty, P::new(discr), branches))
    }
    Coerce(ty, ir) => {
      let ir = ir.fold(fold);
      fold.fold_ir(Coerce(*ty, P::new(ir)))
    }
    Int(i) => fold.fold_ir(Int(*i)),
    Var(v) => fold.fold_ir(Var(*v)),
    Item(occ) => fold.fold_ir(Item(*occ)),

    X(in_ext) => {
      let out_ext = fold.fold_ext(in_ext);
      fold.fold_ir(X(out_ext))
    }
  }
}

pub trait ReducIrEndoFold: Sized {
  type Ext: Clone;

  fn endofold_ir(&mut self, kind: ReducIrKind<Self::Ext>) -> ReducIr<Self::Ext> {
    ReducIr::new(kind)
  }

  fn endotraverse_ir(&mut self, ir: &ReducIr<Self::Ext>) -> ReducIr<Self::Ext> {
    default_endotraverse_ir::<Self>(self, ir)
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
      Locals(binds, body) => {
        let binds = binds.iter().map(|local| local.fold(self)).collect();
        let body = body.fold(self);
        self.fold_ir(Locals(binds, P::new(body)))
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
      Coerce(ty, ir) => {
        let ir = ir.fold(self);
        self.fold_ir(Coerce(*ty, P::new(ir)))
      }
      Int(i) => self.fold_ir(Int(*i)),
      Var(v) => self.fold_ir(Var(*v)),
      Item(occ) => self.fold_ir(Item(*occ)),

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

impl ReducIr {
  fn free_var_aux(
    &self,
    in_scope: &mut FxHashSet<ReducIrLocal>,
    bound: &mut FxHashSet<ReducIrVar>,
  ) {
    match &self.kind {
      Int(_) | Item(_) => {}
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
      Locals(binds, body) => {
        for Bind { var, defn } in binds.iter() {
          defn.free_var_aux(in_scope, bound);
          in_scope.insert(var.var);
        }
        body.free_var_aux(in_scope, bound);
      }
      Coerce(_, body) => {
        body.free_var_aux(in_scope, bound);
      }
      X(_) => unreachable!(),
    }
  }

  pub fn free_var_set(&self) -> FxHashSet<ReducIrVar> {
    let mut free_vars = FxHashSet::default();
    let mut in_scope = FxHashSet::default();
    self.free_var_aux(&mut in_scope, &mut free_vars);
    free_vars
  }
}

pub trait TypeCheck {
  type Ext: Clone;
  fn type_check<'a, DB: ?Sized + crate::Db>(
    &'a self,
    ctx: &DB,
  ) -> Result<ReducIrTy, ReducIrTyErr<'a, Self::Ext>>
  where
    Self::Ext: PrettyWithCtx<DB>;
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
      DelimCont::Prompt { marker, body, .. } => {
        let marker_ty = marker.type_check(ctx)?;
        if let MarkerTy(_) = marker_ty.kind(ctx.as_reducir_db()) {
          body.type_check(ctx)
        } else {
          Err(ReducIrTyErr::ExpectedMarkerTy(marker_ty))
        }
      }
      DelimCont::Yield {
        ret_ty,
        marker,
        body,
      } => {
        let marker_ty = marker.type_check(ctx)?;
        let MarkerTy(_) = marker_ty.kind(ctx.as_reducir_db()) else {
          return Err(ReducIrTyErr::ExpectedMarkerTy(marker_ty));
        };
        // We want to make sure body type checks but we don't actually use the result
        let _ = body.type_check(ctx)?;
        Ok(*ret_ty)
      }
    }
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

  fn type_check<DB: ?Sized + crate::Db>(&self, ctx: &DB) -> Result<ReducIrTy, ReducIrTyErr<Ext>>
  where
    Ext: PrettyWithCtx<DB>,
  {
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
            let mut fun_args = fun_arg_tys.iter().peekable();
            for (fun_arg_ty, (arg_index, arg)) in
              fun_args.zip_non_consuming(&mut args_iter.enumerate().peekable())
            {
              let arg_ty = arg.type_check(ctx)?;
              if *fun_arg_ty != arg_ty {
                return Err(ReducIrTyErr::TyMismatch {
                  left_ty: ctx.mk_fun_ty(
                    std::iter::once(*fun_arg_ty).chain(fun_args.copied()),
                    ret_ty,
                  ),
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
      Locals(binds, body) => {
        for Bind { var, defn } in binds.iter() {
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
        body.type_check(ctx)
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
              (k, ReducIrTyApp::Ty(_)) => Err(ReducIrTyErr::KindMistmatch(k, Kind::Type)),
              (k, ReducIrTyApp::DataRow(_)) => Err(ReducIrTyErr::KindMistmatch(k, Kind::SimpleRow)),
              (k, ReducIrTyApp::EffRow(_)) => Err(ReducIrTyErr::KindMistmatch(k, Kind::ScopedRow)),
            },
            _ => Err(ReducIrTyErr::ExpectedForallTy {
              forall: Cow::Borrowed(forall),
              forall_ty,
              ty_apps: ty_apps.as_slice(),
            }),
          }
        })
      }
      Struct(elems) => {
        let elems = elems
          .iter()
          .map(|e| e.type_check(ctx))
          .collect::<Result<Vec<_>, _>>()?;
        Ok(ctx.mk_prod_ty(elems))
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
            /*let exists_b = ctx.mk_reducir_ty(VarTy(2));
            let exists_m = ctx.mk_reducir_ty(VarTy(1));
            let exists_r = ctx.mk_reducir_ty(VarTy(0));
            let exists_mon_m_r = ctx.mk_mon_ty(exists_m, exists_r);
            let exists_body_fun_ty = ctx.mk_mon_ty(m_ty, a_ty).shift(ctx, 3);
            let yield_ty = ctx.mk_forall_ty(
              [Kind::Type, Kind::Type, Kind::Type],
              ProductTy(vec![
                ctx.mk_reducir_ty(MarkerTy(exists_r)),
                ctx.mk_fun_ty([ctx.mk_fun_ty([exists_b], exists_mon_m_r)], exists_mon_m_r),
                ctx.mk_fun_ty([exists_b], exists_body_fun_ty),
              ]),
            );*/
            vec![a_ty, ctx.mk_yield_ty(m_ty, a_ty)]
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
      Coerce(ty, body) => {
        let _ = body.type_check(ctx)?;
        Ok(*ty)
      }
      X(xt) => xt.type_check(ctx),
      Tag(ty, _, _) => Ok(*ty),
      Item(occ) => Ok(occ.ty),
    }
  }
}

impl DelimReducIr {
  /*pub fn unbound_vars(&self) -> impl Iterator<Item = ReducIrVar> + '_ {
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
  }*/
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
  ExpectedForallTy {
    forall: Cow<'a, ReducIr<Ext>>,
    forall_ty: ReducIrTy,
    ty_apps: &'a [ReducIrTyApp],
  },
  ExpectedProdTy(ReducIrTy, Cow<'a, ReducIr<Ext>>),
  ExpectedCoprodTy(ReducIrTy, Cow<'a, ReducIr<Ext>>),
  ExpectedMarkerTy(ReducIrTy),
}

/*struct UnboundVars<'a> {
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
}*/
