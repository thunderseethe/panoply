use base::id::TyVarId;
use salsa::DebugWithDb;
use std::fmt::{self, Debug};
use std::hash::Hash;
use std::ops::Deref;

mod alloc;
pub use alloc::{
  db::{InDb, RowFields, RowValues, TyData},
  AccessTy, MkTy, ScopedRowVarOf, SimpleRowVarOf, TypeAlloc, TypeVarOf,
};

mod evidence;
pub use evidence::Evidence;

mod fold;
use self::fold::DefaultFold;
use self::row::{RowOps, Scoped, ScopedRow, Simple, SimpleRow};
pub use fold::{FallibleEndoTypeFold, FallibleTypeFold, TypeFoldable};

pub mod row;
use row::{Row, SimpleClosedRow};

#[cfg(feature = "type_infer")]
pub mod infer;

#[salsa::jar(db = Db)]
pub struct Jar(TyData, RowFields, RowValues);
pub trait Db: salsa::DbWithJar<Jar> + base::Db {
  fn as_ty_db(&self) -> &dyn crate::Db {
    <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
  }
}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + base::Db {}

mod pretty;

/// A monomorphic type.
///
/// This is just a wrapper around an interned reference to the `TypeKind` which contains the actual
/// data.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ty<A: TypeAlloc = InDb>(pub A::TypeData);

impl<A: TypeAlloc> Copy for Ty<A>
where
  A: Clone,
  A::TypeData: Copy,
{
}

impl<A: TypeAlloc> Debug for Ty<A> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.0.fmt(f)
  }
}
impl<Db> DebugWithDb<Db> for Ty<InDb>
where
  Db: ?Sized + crate::Db,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Db, include_all_fields: bool) -> fmt::Result {
    self
      .0
      .kind(db.as_ty_db())
      .debug_with(db, include_all_fields)
      .fmt(f)
  }
}

impl Ty<InDb> {
  pub fn try_as_prod_row<'a>(
    self,
    db: &(impl ?Sized + AccessTy<'a, InDb>),
  ) -> Result<SimpleRow<InDb>, Self> {
    match db.kind(&self) {
      TypeKind::ProdTy(Row::Closed(row)) | TypeKind::RowTy(row) => Ok(Row::Closed(*row)),
      TypeKind::ProdTy(Row::Open(var)) | TypeKind::VarTy(var) => Ok(Row::Open(*var)),
      _ => Err(self),
    }
  }

  pub fn try_as_sum_row<'a>(
    self,
    db: &(impl ?Sized + AccessTy<'a, InDb>),
  ) -> Result<SimpleRow<InDb>, Self> {
    match db.kind(&self) {
      TypeKind::SumTy(Row::Closed(row)) | TypeKind::RowTy(row) => Ok(Row::Closed(*row)),
      TypeKind::SumTy(Row::Open(var)) | TypeKind::VarTy(var) => Ok(Row::Open(*var)),
      _ => Err(self),
    }
  }

  pub fn try_as_fn_ty<'a>(
    self,
    db: &(impl ?Sized + AccessTy<'a, InDb>),
  ) -> Result<(Self, ScopedRow<InDb>, Self), Self> {
    match db.kind(&self) {
      TypeKind::FunTy(arg, eff, ret) => Ok((*arg, *eff, *ret)),
      _ => Err(self),
    }
  }

  /// Iterate over all the type variables that are used in a row position
  pub fn row_vars<'db>(&self, db: &'db dyn crate::Db) -> impl Iterator<Item = TyVarId> + 'db {
    TyInDbDfs::new(db, *self).filter_map(|ty| match ty.0.kind(db) {
      TypeKind::ProdTy(Row::Open(row_var)) | TypeKind::SumTy(Row::Open(row_var)) => Some(*row_var),
      _ => None,
    })
  }

  pub fn try_as_eff_row_val<DB: ?Sized + crate::Db>(
    self,
    db: &DB,
  ) -> Result<(ScopedRow, Ty), Self> {
    let ty_db = db.as_ty_db();
    match ty_db.kind(&self) {
      TypeKind::ProdTy(Row::Closed(row)) => {
        let [eff, ret] = row.values(ty_db) else {
          return Err(self);
        };
        let eff = match ty_db.kind(eff) {
          TypeKind::FunTy(_, eff, _) => eff,
          _ => return Err(self),
        };
        Ok((*eff, *ret))
      }
      _ => Err(self),
    }
  }
}

impl<A: TypeAlloc> Ty<A>
where
  Self: Copy,
  ScopedRow<A>: Copy,
{
  pub fn transform_to_cps_handler_ty<'a, 'b, C>(
    &self,
    acc: &'b C,
    ret_ty: Self,
  ) -> Result<Self, (Self, Self)>
  where
    A: 'a,
    C: ?Sized + MkTy<A>,
    &'b C: AccessTy<'a, A>,
  {
    // Transform our ty into the type a handler should have
    // This means it should take a resume parameter that is a function returning `ret` and return `ret` itself.
    match acc.kind(self) {
      TypeKind::FunTy(a, eff, b) => {
        let kont_ty = acc.mk_ty(TypeKind::FunTy(*b, *eff, ret_ty));
        Ok(acc.mk_ty(TypeKind::FunTy(
          *a,
          *eff,
          acc.mk_ty(TypeKind::FunTy(kont_ty, *eff, ret_ty)),
        )))
      }
      _ => {
        // TODO: report a better error here.
        // We should specialize the error so it's clear it was an
        // effect signature with an invalid type that caused it
        Err((
          *self,
          acc.mk_ty(TypeKind::FunTy(
            acc.mk_ty(TypeKind::ErrorTy),
            Row::Closed(acc.empty_row()),
            acc.mk_ty(TypeKind::ErrorTy),
          )),
        ))
      }
    }
  }
}

struct TyInDbDfs<'a, Db: ?Sized> {
  db: &'a Db,
  stack: Vec<Ty<InDb>>,
}

impl<'a, Db: ?Sized> TyInDbDfs<'a, Db> {
  fn new(db: &'a Db, root: Ty<InDb>) -> Self {
    Self {
      db,
      stack: vec![root],
    }
  }
}
impl<'a, DB> Iterator for TyInDbDfs<'a, DB>
where
  DB: ?Sized + crate::Db,
{
  type Item = Ty<InDb>;

  fn next(&mut self) -> Option<Self::Item> {
    self.stack.pop().map(|ty| {
      match ty.0.kind(self.db.as_ty_db()) {
        TypeKind::ErrorTy => {}
        TypeKind::IntTy => {}
        TypeKind::VarTy(_) => {}
        TypeKind::RowTy(row) => {
          self.stack.extend_from_slice(row.values(self.db));
        }
        TypeKind::FunTy(arg, _, ret) => {
          self.stack.extend([arg, ret]);
        }
        TypeKind::ProdTy(row) => match row {
          Row::Open(_) => {}
          Row::Closed(row) => {
            self.stack.extend_from_slice(row.values(self.db));
          }
        },
        TypeKind::SumTy(row) => match row {
          Row::Open(_) => {}
          Row::Closed(row) => {
            self.stack.extend_from_slice(row.values(self.db.as_ty_db()));
          }
        },
      };
      ty
    })
  }
}

impl<A: TypeAlloc> Deref for Ty<A>
where
  A::TypeData: Deref,
{
  type Target = <A::TypeData as Deref>::Target;

  fn deref(&self) -> &Self::Target {
    self.0.deref()
  }
}

/// Data for `Ty`.
/// `TypeKind` is interned to produce a `Ty`.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeKind<A: TypeAlloc = InDb> {
  /// Marker that signifies an operation produced an error. This exists so that we can try to
  /// gracefully recover from a type checking error and produce a list of errors at the end of type checking
  ErrorTy,
  /// Type of integers.
  //TODO: This should be part of some BaseTy or LiteralTy as opposed to a member of Type directly
  IntTy,
  /// A type variable, during type checking this may be either a unifier or a proper type variable
  VarTy(A::TypeVar),
  /// A row type, this is specifically a closed row. Open rows are represented as VarTy
  RowTy(SimpleClosedRow<A>),
  /// A function type
  FunTy(Ty<A>, ScopedRow<A>, Ty<A>),
  /// A product type. This is purely a wrapper type to coerce a row type to be a product.
  ProdTy(Row<Simple, A>),
  /// A sum type. This is purely a wrapper type to coerce a row type to be a sum.
  SumTy(Row<Simple, A>),
}
impl<A: TypeAlloc> Copy for TypeKind<A>
where
  A: Clone,
  A::TypeVar: Copy,
  SimpleClosedRow<A>: Copy,
  Row<Simple, A>: Copy,
  Row<Scoped, A>: Copy,
  Ty<A>: Copy,
{
}
impl Debug for TypeKind<InDb> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("Use DebugWithDb for TypeKind<InDb>")
  }
}
impl<Db> DebugWithDb<Db> for TypeKind<InDb>
where
  Db: ?Sized + crate::Db,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Db, _include_all_fields: bool) -> fmt::Result {
    match self {
      TypeKind::ErrorTy => f.debug_tuple("ErrorTy").finish(),
      TypeKind::IntTy => f.debug_tuple("IntTy").finish(),
      TypeKind::VarTy(var) => f.debug_tuple("VarTy").field(var).finish(),
      TypeKind::RowTy(row) => f.debug_tuple("RowTy").field(&row.debug(db)).finish(),
      TypeKind::FunTy(arg, eff, ret) => f
        .debug_tuple("FunTy")
        .field(&arg.debug(db))
        .field(&eff.debug(db))
        .field(&ret.debug(db))
        .finish(),
      TypeKind::ProdTy(row) => f.debug_tuple("ProdTy").field(&row.debug(db)).finish(),
      TypeKind::SumTy(row) => f.debug_tuple("SumTy").field(&row.debug(db)).finish(),
    }
  }
}

impl<'ctx, A: TypeAlloc + 'ctx> fold::DefaultFold<'ctx> for TypeKind<A>
where
  A: Clone,
{
  type In = A;

  fn try_default_fold<F: FallibleTypeFold<'ctx, In = Self::In>>(
    &self,
    fold: &mut F,
  ) -> Result<Ty<F::Out>, F::Error> {
    Ok(match &self {
      TypeKind::VarTy(ref var) => fold.try_fold_var(var.clone())?,
      TypeKind::IntTy => fold.ctx().mk_ty(TypeKind::IntTy),
      TypeKind::ErrorTy => fold.ctx().mk_ty(TypeKind::ErrorTy),
      TypeKind::FunTy(ref arg, ref eff, ref ret) => {
        let arg_ = arg.clone().try_fold_with(fold)?;
        let eff_ = eff.clone().try_fold_with(fold)?;
        let ret_ = ret.clone().try_fold_with(fold)?;
        fold
          .ctx()
          .mk_ty(TypeKind::<F::Out>::FunTy(arg_, eff_, ret_))
      }
      TypeKind::RowTy(ref row) => {
        let row_ = row.clone().try_fold_with(fold)?;
        fold.ctx().mk_ty(TypeKind::<F::Out>::RowTy(row_))
      }
      TypeKind::ProdTy(ref row) => {
        let row_ = row.clone().try_fold_with(fold)?;
        fold.ctx().mk_ty(TypeKind::ProdTy(row_))
      }
      TypeKind::SumTy(ref row) => {
        let row_ = row.clone().try_fold_with(fold)?;
        fold.ctx().mk_ty(TypeKind::SumTy(row_))
      }
    })
  }
}

impl<'ctx, A: TypeAlloc + Clone + 'ctx> TypeFoldable<'ctx> for Ty<A>
where
  A: Clone,
{
  type Alloc = A;
  type Out<B: TypeAlloc> = Ty<B>;

  fn try_fold_with<F: FallibleTypeFold<'ctx, In = Self::Alloc>>(
    self,
    fold: &mut F,
  ) -> Result<Self::Out<F::Out>, F::Error> {
    fold.access().kind(&self).try_default_fold(fold)
  }
}

/// A type scheme (also know as a polymorphic type).
/// Type schemes wrap a monomorphic type in any number of foralls binding the free variables within
/// the monomorphic type. They may also assert constraints on the bound type variables.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyScheme<A: TypeAlloc = InDb> {
  pub bound_ty: Vec<A::TypeVar>,
  pub bound_data_row: Vec<A::SimpleRowVar>,
  pub bound_eff_row: Vec<A::ScopedRowVar>,
  pub constrs: Vec<Evidence<A>>,
  pub eff: ScopedRow<A>,
  pub ty: Ty<A>,
}
impl<Db> DebugWithDb<Db> for TyScheme
where
  Db: ?Sized + crate::Db,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Db, _include_all_fields: bool) -> fmt::Result {
    f.debug_struct("TyScheme")
      .field("bound_ty", &self.bound_ty)
      .field("bound_data", &self.bound_data_row)
      .field("bound_eff", &self.bound_eff_row)
      .field("constrs", &self.constrs)
      .field("eff", &self.eff.debug(db))
      .field("ty", &self.ty.debug(db))
      .finish()
  }
}

#[derive(Eq, PartialEq, Debug)]
pub struct Wrapper<A: TypeAlloc = InDb> {
  pub tys: Vec<Ty<A>>,
  pub data_rows: Vec<SimpleRow<A>>,
  pub eff_rows: Vec<ScopedRow<A>>,
  pub constrs: Vec<Evidence<A>>,
}
