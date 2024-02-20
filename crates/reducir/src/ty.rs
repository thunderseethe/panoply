use base::id::ReducIrTyVarId;
use ty::row::{RowSema, Scoped, Simple};

mod subst;
use salsa::DebugWithDb;

use subst::SubstFold;
pub use subst::{IntoPayload, Subst};

mod pretty;

/// The kind of a type variable
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum Kind {
  Type,
  SimpleRow,
  ScopedRow,
}

pub trait RowReducIrKind: RowSema {
  fn kind() -> Kind;
}
impl RowReducIrKind for Simple {
  fn kind() -> Kind {
    Kind::SimpleRow
  }
}
impl RowReducIrKind for Scoped {
  fn kind() -> Kind {
    Kind::ScopedRow
  }
}

/// An ir type variable and it's kind
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct ReducIrVarTy {
  pub var: ReducIrTyVarId,
  pub kind: Kind,
}

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub enum ReducIrTyKind {
  IntTy,
  VarTy(i32),
  ProdVarTy(i32),
  CoprodVarTy(i32),
  FunTy(Box<[ReducIrTy]>, ReducIrTy),
  ForallTy(Kind, ReducIrTy),
  ProductTy(Vec<ReducIrTy>),
  CoproductTy(Vec<ReducIrTy>),
  // TODO: Figure out how to not build this in
  MarkerTy(ReducIrTy),
  /// Our delimited continuation monad type.
  /// It's specialized as a type to handle recursion without full support for recursive types.
  ControlTy(ReducIrTy, ReducIrTy),
  FunETy(ReducIrTy, ReducIrTy, ReducIrTy),
}

#[salsa::interned]
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct ReducIrTy {
  pub kind: ReducIrTyKind,
}

pub struct UnwrapMonTy {
  pub evv_ty: ReducIrTy,
  pub a_ty: ReducIrTy,
}
impl ReducIrTy {
  /// Unwrap a monadic type into it's evv type and value type.
  pub fn try_unwrap_monadic<Db: ?Sized + crate::Db>(self, db: &Db) -> Result<UnwrapMonTy, Self> {
    // Monadic type is evv -> Control evv a
    // Unwrap and return (evv, a) from Control type
    let reducir_db = db.as_reducir_db();
    match self.kind(reducir_db) {
      ReducIrTyKind::FunTy(args, ret) if args.len() == 1 => match ret.kind(reducir_db) {
        ReducIrTyKind::ControlTy(evv_ty, a_ty) if args[0] == evv_ty => {
          Ok(UnwrapMonTy { evv_ty, a_ty })
        }
        _ => Err(self),
      },
      _ => Err(self),
    }
  }

  pub fn unwrap_foralls<Db: ?Sized + crate::Db>(self, db: &Db) -> Self {
    match self.kind(db.as_reducir_db()) {
      ReducIrTyKind::ForallTy(_, ty) => ty.unwrap_foralls(db),
      _ => self,
    }
  }

  /// Checks if a function type returns a monadic type
  pub fn try_fun_returns_monadic(self, db: &dyn crate::Db) -> Result<(usize, UnwrapMonTy), Self> {
    match self.kind(db) {
      ReducIrTyKind::FunTy(args, ret) => match ret.kind(db) {
        ReducIrTyKind::ControlTy(evv_ty, a_ty) if args.last() == Some(&evv_ty) => {
          // Don't count the evv parameter since it's part of monadic type
          Ok((args.len() - 1, UnwrapMonTy { evv_ty, a_ty }))
        }
        _ => Err(self),
      },
      _ => Err(self),
    }
  }

  /// Drops n args off of self if self is a function type. Returns Err if self is not a function type.
  pub fn drop_args(self, db: &dyn crate::Db, n_args: usize) -> Result<Self, Self> {
    if n_args == 0 {
      return Ok(self);
    }
    match self.kind(db) {
      ReducIrTyKind::FunTy(args, ret) => Ok(db.mk_fun_ty(args.iter().skip(n_args).copied(), ret)),
      ReducIrTyKind::FunETy(_arg, _, ret) => ret.drop_args(db, n_args - 1),
      _ => Err(self),
    }
  }

  pub fn split_args<DB: ?Sized + crate::Db>(
    self,
    db: &DB,
    n_args: usize,
  ) -> Result<(Vec<Self>, Self), Self> {
    match self.kind(db.as_reducir_db()) {
      ReducIrTyKind::FunTy(args, ret) => {
        let (args, ret_args) = args.split_at(n_args);
        Ok((args.to_vec(), db.mk_fun_ty(ret_args.iter().copied(), ret)))
      }
      _ => Err(self),
    }
  }

  /// Arity of this type for use in medir.
  /// If the type isn't a function this arity is 0
  pub fn medir_arity<DB: ?Sized + crate::Db>(&self, db: &DB) -> Option<u32> {
    match self.kind(db.as_reducir_db()) {
      ReducIrTyKind::FunTy(params, _) => Some(params.len().try_into().unwrap()),
      ReducIrTyKind::ForallTy(_, ty) => ty.medir_arity(db),
      _ => None,
    }
  }
}

pub fn default_fold_tykind<'db>(
  fold: &mut (impl FoldReducIrTy<'db> + ?Sized),
  kind: ReducIrTyKind,
) -> ReducIrTy {
  let kind = match kind {
    ReducIrTyKind::IntTy => ReducIrTyKind::IntTy,
    ReducIrTyKind::VarTy(v) => return fold.fold_ty_var(v),
    ReducIrTyKind::ProdVarTy(v) => return fold.fold_prod_var(v),
    ReducIrTyKind::CoprodVarTy(v) => return fold.fold_coprod_var(v),
    ReducIrTyKind::FunTy(args, ret_ty) => {
      let ret_ty = fold.fold_ty(ret_ty);
      match ret_ty.kind(fold.db()) {
        ReducIrTyKind::FunTy(b_args, ret) => ReducIrTyKind::FunTy(
          args
            .iter()
            .map(|a| fold.fold_ty(*a))
            .chain(b_args.iter().copied())
            .collect(),
          ret,
        ),
        _ => ReducIrTyKind::FunTy(args.iter().map(|a| fold.fold_ty(*a)).collect(), ret_ty),
      }
    }
    ReducIrTyKind::ForallTy(kind, ty) => {
      let ty = fold.fold_ty(ty);
      ReducIrTyKind::ForallTy(kind, ty)
    }
    ReducIrTyKind::ProductTy(elems) => {
      let elems = elems.iter().map(|e| fold.fold_ty(*e)).collect();
      ReducIrTyKind::ProductTy(elems)
    }
    ReducIrTyKind::CoproductTy(elems) => {
      let elems = elems.iter().map(|e| fold.fold_ty(*e)).collect();
      ReducIrTyKind::CoproductTy(elems)
    }
    ReducIrTyKind::MarkerTy(a_ty) => {
      let a_ty = fold.fold_ty(a_ty);
      ReducIrTyKind::MarkerTy(a_ty)
    }
    ReducIrTyKind::ControlTy(evv_ty, a_ty) => {
      let evv_ty = fold.fold_ty(evv_ty);
      let a_ty = fold.fold_ty(a_ty);
      ReducIrTyKind::ControlTy(evv_ty, a_ty)
    }
    ReducIrTyKind::FunETy(arg, eff, ret) => {
      let arg = fold.fold_ty(arg);
      let eff = fold.fold_ty(eff);
      let ret = fold.fold_ty(ret);
      ReducIrTyKind::FunETy(arg, eff, ret)
    }
  };
  fold.mk_ty(kind)
}

pub trait FoldReducIrTy<'db> {
  fn db(&self) -> &'db dyn crate::Db;

  fn mk_ty(&self, kind: ReducIrTyKind) -> ReducIrTy {
    self.db().mk_reducir_ty(kind)
  }

  fn fold_ty_var(&mut self, var: i32) -> ReducIrTy {
    self.mk_ty(ReducIrTyKind::VarTy(var))
  }

  fn fold_prod_var(&mut self, var: i32) -> ReducIrTy {
    self.mk_ty(ReducIrTyKind::ProdVarTy(var))
  }

  fn fold_coprod_var(&mut self, var: i32) -> ReducIrTy {
    self.mk_ty(ReducIrTyKind::CoprodVarTy(var))
  }

  fn fold_ty_kind(&mut self, kind: ReducIrTyKind) -> ReducIrTy {
    default_fold_tykind(self, kind)
  }

  fn fold_ty(&mut self, ty: ReducIrTy) -> ReducIrTy {
    self.fold_ty_kind(ty.kind(self.db()))
  }
}

impl ReducIrTy {
  fn fold<'db>(self, f: &mut impl FoldReducIrTy<'db>) -> Self {
    f.fold_ty(self)
  }
  /// Assume `self` is a forall and reduce it by applying `ty` as it's argument.
  /// This applies type substitution without having to create a TyApp ReducIr node.
  pub fn reduce_forall<DB: ?Sized + crate::Db>(self, db: &DB, ty_app: ReducIrTyApp) -> ReducIrTy {
    match (self.kind(db.as_reducir_db()), ty_app) {
      (ReducIrTyKind::ForallTy(Kind::Type, ret_ty), ReducIrTyApp::Ty(ty)) => {
        ret_ty.subst_single(db, ty)
      }
      (ReducIrTyKind::ForallTy(Kind::ScopedRow, ret_ty), ReducIrTyApp::EffRow(row)) => {
        ret_ty.subst_single(db, row)
      }
      (ReducIrTyKind::ForallTy(Kind::SimpleRow, ret_ty), ReducIrTyApp::DataRow(row)) => {
        ret_ty.subst_single(db, row)
      }
      _ => panic!("reduce_forall called on non forall type"),
    }
  }

  pub fn subst<DB: ?Sized + crate::Db>(self, db: &DB, subst: Subst) -> Self {
    self.fold(&mut SubstFold {
      db: db.as_reducir_db(),
      subst,
    })
  }

  pub fn subst_single<DB: ?Sized + crate::Db>(self, db: &DB, payload: impl IntoPayload) -> Self {
    self.subst(db, Subst::single(payload.into_payload(db)))
  }

  /// Shift all the variables in a term by delta.
  pub fn shift<DB: ?Sized + crate::Db>(self, db: &DB, delta: i32) -> Self {
    self.subst(db.as_reducir_db(), Subst::Inc(delta))
  }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum ReducIrRow {
  Open(i32),
  Closed(Vec<ReducIrTy>),
}

impl DebugWithDb<dyn crate::Db> for ReducIrRow {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
    db: &dyn crate::Db,
    include_all_fields: bool,
  ) -> std::fmt::Result {
    match self {
      ReducIrRow::Open(var) => f.debug_tuple("Open").field(var).finish(),
      ReducIrRow::Closed(tys) => f
        .debug_tuple("Closed")
        .field(&tys.debug_with(db, include_all_fields))
        .finish(),
    }
  }
}

impl ReducIrRow {
  pub fn shift(self, db: &dyn crate::Db, delta: i32) -> Self {
    match self {
      ReducIrRow::Open(var) => ReducIrRow::Open(var + delta),
      ReducIrRow::Closed(row) => {
        ReducIrRow::Closed(row.into_iter().map(|ty| ty.shift(db, delta)).collect())
      }
    }
  }

  fn into_prod_ty<DB: ?Sized + crate::Db>(self, db: &DB) -> ReducIrTy {
    match self {
      ReducIrRow::Open(var) => db.mk_reducir_ty(ReducIrTyKind::ProdVarTy(var)),
      ReducIrRow::Closed(tys) => db.mk_prod_ty(tys),
    }
  }

  fn into_coprod_ty<DB: ?Sized + crate::Db>(self, db: &DB) -> ReducIrTy {
    match self {
      ReducIrRow::Open(var) => db.mk_reducir_ty(ReducIrTyKind::CoprodVarTy(var)),
      ReducIrRow::Closed(tys) => db.mk_coprod_ty(tys),
    }
  }
}

// We allow Rows in type applications because they might show up in constraints.
// But we want to ensure they don't appear in our ReducIr types outside of that so we make a specific type
// for it
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum ReducIrTyApp {
  Ty(ReducIrTy),
  DataRow(ReducIrRow),
  EffRow(ReducIrRow),
}

pub trait MkReducIrTy {
  fn mk_reducir_ty(&self, kind: ReducIrTyKind) -> ReducIrTy;
  fn mk_fun_ty(
    &self,
    args: impl IntoIterator<Item = impl IntoReducIrTy>,
    ret: impl IntoReducIrTy,
  ) -> ReducIrTy;

  fn mk_prod_ty_ref(&self, elems: &[ReducIrTy]) -> ReducIrTy {
    self.mk_prod_ty(elems.to_owned())
  }

  fn mk_prod_ty(&self, elems: Vec<ReducIrTy>) -> ReducIrTy {
    if elems.len() == 1 {
      elems.into_iter().next().unwrap()
    } else {
      self.mk_reducir_ty(ReducIrTyKind::ProductTy(elems))
    }
  }

  fn mk_coprod_ty_ref(&self, elems: &[ReducIrTy]) -> ReducIrTy {
    self.mk_coprod_ty(elems.to_owned())
  }

  fn mk_coprod_ty(&self, elems: Vec<ReducIrTy>) -> ReducIrTy {
    if elems.len() == 1 {
      elems.into_iter().next().unwrap()
    } else {
      self.mk_reducir_ty(ReducIrTyKind::CoproductTy(elems))
    }
  }

  fn mk_forall_ty<I>(&self, kinds: I, ty: impl IntoReducIrTy) -> ReducIrTy
  where
    I: IntoIterator<Item = Kind>,
    I::IntoIter: DoubleEndedIterator,
  {
    kinds
      .into_iter()
      .rfold(ty.into_reducir_ty(self), |ty, kind| {
        self.mk_reducir_ty(ReducIrTyKind::ForallTy(kind, ty))
      })
  }

  fn mk_mon_ty(&self, evv_ty: impl IntoReducIrTy, a_ty: impl IntoReducIrTy) -> ReducIrTy {
    let evv_ty = evv_ty.into_reducir_ty(self);
    let a_ty = a_ty.into_reducir_ty(self);
    self.mk_fun_ty(
      [evv_ty],
      self.mk_reducir_ty(ReducIrTyKind::ControlTy(evv_ty, a_ty)),
    )
  }

  fn mk_yield_ty(&self, evv_ty: impl IntoReducIrTy, a_ty: impl IntoReducIrTy) -> ReducIrTy;
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
    ReducIrTy::new(self.as_reducir_db(), kind)
  }

  fn mk_fun_ty(
    &self,
    args: impl IntoIterator<Item = impl IntoReducIrTy>,
    ret: impl IntoReducIrTy,
  ) -> ReducIrTy {
    let mut args = args
      .into_iter()
      .map(|arg| arg.into_reducir_ty(self))
      .peekable();
    let ret = ret.into_reducir_ty(self);
    // If we have no args don't output a function type
    if args.peek().is_none() {
      ret
    } else {
      match ret.kind(self.as_reducir_db()) {
        ReducIrTyKind::FunTy(iargs, ret) => self.mk_reducir_ty(ReducIrTyKind::FunTy(
          args.chain(iargs.iter().copied()).collect(),
          ret,
        )),
        _ => self.mk_reducir_ty(ReducIrTyKind::FunTy(args.collect(), ret)),
      }
    }
  }

  fn mk_yield_ty(&self, evv_ty: impl IntoReducIrTy, a_ty: impl IntoReducIrTy) -> ReducIrTy {
    let exists_b = self.mk_reducir_ty(ReducIrTyKind::VarTy(2));
    let exists_m = self.mk_reducir_ty(ReducIrTyKind::VarTy(1));
    let exists_r = self.mk_reducir_ty(ReducIrTyKind::VarTy(0));
    let exists_mon_m_r = self.mk_mon_ty(exists_m, exists_r);
    let exists_body_fun_ty = self
      .mk_mon_ty(evv_ty, a_ty)
      .subst(self.as_reducir_db(), Subst::Inc(3));
    self.mk_forall_ty(
      [Kind::Type, Kind::Type, Kind::Type],
      self.mk_prod_ty(vec![
        self.mk_reducir_ty(ReducIrTyKind::MarkerTy(exists_r)),
        self.mk_fun_ty([self.mk_fun_ty([exists_b], exists_mon_m_r)], exists_mon_m_r),
        self.mk_fun_ty([exists_b], exists_body_fun_ty),
      ]),
    )
  }
}
