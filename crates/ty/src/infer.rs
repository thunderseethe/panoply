//! Implementation details that are specific to type inference.
//! These are kept in their own module because they should not be used outside of the type checking module.
use std::{
  convert::Infallible,
  fmt::{self, Debug},
  ops::Deref,
};

use ena::unify::{EqUnifyValue, UnifyKey, UnifyValue};
use pretty::DocAllocator;
use salsa::DebugWithDb;

use crate::{
  row::{
    Row, RowInternals, RowLabel, RowOps, ScopedClosedRow, ScopedRow, SimpleClosedRow, SimpleRow,
  },
  Evidence, FallibleTypeFold, Ty, TypeAlloc, TypeFoldable, TypeKind, Wrapper,
};

#[derive(Debug, PartialEq, Eq)]
pub struct UnifierToTcVarError {
  index: u32,
}
#[derive(Debug, PartialEq, Eq)]
pub struct TcVarToUnifierError {
  index: u32,
}

mod seal_unifier_kind {
  pub trait SealUnifierKind {}
}
use seal_unifier_kind::SealUnifierKind;

pub trait UnifierKind: SealUnifierKind + Copy + Clone + std::fmt::Debug + PartialEq {
  type UnifyValue<'ctx>: UnifyValue;
}
/// Kind of TcUnifierVar that is mapped to a type
#[derive(Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct TypeK;
impl SealUnifierKind for TypeK {}
impl UnifierKind for TypeK {
  type UnifyValue<'ctx> = InferTy<'ctx>;
}
/// Kind of TcUnifierVar that is mapped to a simple row
#[derive(Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct SimpleRowK;
impl SealUnifierKind for SimpleRowK {}
impl UnifierKind for SimpleRowK {
  type UnifyValue<'ctx> = SimpleClosedRow<InArena<'ctx>>;
}
/// Kind of TcUnifierVar that is mapped to a scoped row
#[derive(Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct ScopedRowK;
impl SealUnifierKind for ScopedRowK {}
impl UnifierKind for ScopedRowK {
  type UnifyValue<'ctx> = ScopedClosedRow<InArena<'ctx>>;
}

/// A unifier variable.
/// These are produced during the type checking process and MUST NOT persist outside the type
/// checker. They may not appear in the AST once type checking is completed and are removed by
/// zonking.
/// Conversely to the untouchable TcVar, these are "touchable" and will be modified by the type
/// checker.
#[derive(Default, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct TcUnifierVar<'ctx, Kind: UnifierKind = TypeK> {
  id: u32,
  _marker: std::marker::PhantomData<&'ctx Kind>,
}
impl<'ctx, Kind: UnifierKind> fmt::Debug for TcUnifierVar<'ctx, Kind> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let kind = std::any::type_name::<Kind>();
    f.debug_tuple(&format!("TcUnifierVar<{kind}>"))
      .field(&self.id)
      .finish()
  }
}
impl<'ctx, Kind: UnifierKind> From<Infallible> for TcUnifierVar<'ctx, Kind> {
  fn from(_: Infallible) -> Self {
    unreachable!()
  }
}

impl<'a, 'ctx, A, D, Kind> pretty::Pretty<'a, D, A> for TcUnifierVar<'ctx, Kind>
where
  Kind: UnifierKind,
  A: 'a,
  D: ?Sized + DocAllocator<'a, A>,
{
  fn pretty(self, a: &'a D) -> pretty::DocBuilder<'a, D, A> {
    "tv".pretty(a).append(a.as_string(self.id).angles()).group()
  }
}

impl<'ctx, Kind: UnifierKind + Debug> UnifyKey for TcUnifierVar<'ctx, Kind> {
  type Value = Option<Kind::UnifyValue<'ctx>>;

  fn index(&self) -> u32 {
    self.id
  }

  fn from_index(id: u32) -> Self {
    Self {
      id,
      _marker: std::marker::PhantomData,
    }
  }

  fn tag() -> &'static str {
    "TcUnifierVar<Type>"
  }
}

/// During inference our type variables are all unification variables.
/// This is an alias to make inference types easy to talk about.
pub type InferTy<'ctx> = Ty<InArena<'ctx>>;

impl<'ctx> EqUnifyValue for Ty<InArena<'ctx>> {}

pub(crate) mod arena {
  use crate::{
    alloc::IteratorSorted,
    row::{NewRow, RowLabel},
    AccessTy, MkTy, Ty, TypeAlloc, TypeKind,
  };
  use base::memory::{
    handle::RefHandle,
    intern::{Interner, InternerByRef, SyncInterner},
  };
  use bumpalo::Bump;

  use super::{ScopedRowK, SimpleRowK, TcUnifierVar, TypeK};

  pub struct TyCtx<'ctx> {
    tys: SyncInterner<'ctx, TypeKind<InArena<'ctx>>, Bump>,
    row_fields: SyncInterner<'ctx, [RowLabel], Bump>,
    row_values: SyncInterner<'ctx, [Ty<InArena<'ctx>>], Bump>,
    db: &'ctx dyn crate::Db,
  }

  /// Allocate our type structs in an Arena.
  #[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
  pub struct InArena<'ctx>(std::marker::PhantomData<&'ctx ()>);
  impl Copy for InArena<'_>
  where
    <Self as TypeAlloc>::TypeData: Copy,
    <Self as TypeAlloc>::TypeVar: Copy,
    <Self as TypeAlloc>::RowFields: Copy,
    <Self as TypeAlloc>::RowValues: Copy,
  {
  }

  impl<'ctx> TypeAlloc<TypeKind<Self>> for InArena<'ctx> {
    type TypeData = RefHandle<'ctx, TypeKind<Self>>;

    type RowFields = RefHandle<'ctx, [RowLabel]>;

    type RowValues = RefHandle<'ctx, [Ty<Self>]>;

    type TypeVar = TcUnifierVar<'ctx, TypeK>;

    type SimpleRowVar = TcUnifierVar<'ctx, SimpleRowK>;

    type ScopedRowVar = TcUnifierVar<'ctx, ScopedRowK>;
  }

  impl<'ctx> TyCtx<'ctx> {
    pub fn new(db: &'ctx dyn crate::Db, arena: &'ctx Bump) -> Self {
      Self {
        tys: SyncInterner::new(arena),
        row_fields: SyncInterner::new(arena),
        row_values: SyncInterner::new(arena),
        db,
      }
    }
  }

  impl<'ctx> MkTy<InArena<'ctx>> for TyCtx<'ctx>
  where
    TypeKind<InArena<'ctx>>: Copy,
  {
    fn mk_ty(&self, kind: TypeKind<InArena<'ctx>>) -> Ty<InArena<'ctx>> {
      Ty(self.tys.intern(kind))
    }

    fn mk_label(&self, label: &str) -> RowLabel {
      self.db.ident_str(label)
    }

    fn mk_row<R: NewRow<InArena<'ctx>>>(
      &self,
      fields: &[RowLabel],
      values: &[Ty<InArena<'ctx>>],
    ) -> R {
      debug_assert!(
        fields.len() == values.len(),
        "Expected row fields and valuse to be the same length"
      );
      debug_assert!(
        fields.iter().considered_sorted(),
        "Expected row fields to be sorted"
      );
      R::new(
        self.row_fields.intern_by_ref(fields),
        self.row_values.intern_by_ref(values),
      )
    }

    fn mk_row_vec<R: NewRow<InArena<'ctx>>>(
      &self,
      fields: Vec<RowLabel>,
      values: Vec<Ty<InArena<'ctx>>>,
    ) -> R {
      self.mk_row(fields.as_slice(), values.as_slice())
    }
  }

  impl<'ctx> AccessTy<'ctx, InArena<'ctx>> for TyCtx<'ctx> {
    fn kind(&self, ty: &Ty<InArena<'ctx>>) -> &'ctx TypeKind<InArena<'ctx>> {
      let handle = ty.0;
      handle.0
    }

    fn row_fields(&self, fields: &<InArena<'ctx> as TypeAlloc>::RowFields) -> &'ctx [RowLabel] {
      fields.0
    }

    fn row_values(
      &self,
      values: &<InArena<'ctx> as TypeAlloc>::RowValues,
    ) -> &'ctx [Ty<InArena<'ctx>>] {
      values.0
    }
  }
  // Technically with arena alloc it's all refs so we don't need any context to access data.
  impl<'ctx> AccessTy<'ctx, InArena<'ctx>> for () {
    fn kind(&self, ty: &Ty<InArena<'ctx>>) -> &'ctx TypeKind<InArena<'ctx>> {
      (ty.0).0
    }

    fn row_fields(&self, fields: &<InArena<'ctx> as TypeAlloc>::RowFields) -> &'ctx [RowLabel] {
      fields.0
    }

    fn row_values(
      &self,
      values: &<InArena<'ctx> as TypeAlloc>::RowValues,
    ) -> &'ctx [Ty<InArena<'ctx>>] {
      values.0
    }
  }
}
pub use arena::{InArena, TyCtx};

impl<'ctx, DB> DebugWithDb<DB> for SimpleClosedRow<InArena<'ctx>>
where
  DB: ?Sized + crate::Db,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &DB, include_all_fields: bool) -> fmt::Result {
    f.debug_map()
      .entries(self.iter().map(|(k, v)| {
        (
          k.text(db.as_core_db()).as_str(),
          v.debug_with(db, include_all_fields),
        )
      }))
      .finish()
  }
}

impl<'ctx, DB> DebugWithDb<DB> for SimpleRow<InArena<'ctx>>
where
  DB: ?Sized + crate::Db,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &DB, include_all_fields: bool) -> fmt::Result {
    match self {
      Row::Open(var) => var.fmt(f),
      Row::Closed(row) => row.debug_with(db, include_all_fields).fmt(f),
    }
  }
}

impl<'ctx, DB> DebugWithDb<DB> for ScopedClosedRow<InArena<'ctx>>
where
  DB: ?Sized + crate::Db,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &DB, include_all_fields: bool) -> fmt::Result {
    f.debug_map()
      .entries(self.iter().map(|(k, v)| {
        (
          k.text(db.as_core_db()).as_str(),
          v.debug_with(db, include_all_fields),
        )
      }))
      .finish()
  }
}
impl<'ctx, DB> DebugWithDb<DB> for ScopedRow<InArena<'ctx>>
where
  DB: ?Sized + crate::Db,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &DB, include_all_fields: bool) -> fmt::Result {
    match self {
      Row::Open(var) => var.fmt(f),
      Row::Closed(row) => row.debug_with(db, include_all_fields).fmt(f),
    }
  }
}

impl<'ctx, DB> DebugWithDb<DB> for Evidence<InArena<'ctx>>
where
  DB: ?Sized + crate::Db,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &DB, include_all_fields: bool) -> fmt::Result {
    match self {
      Evidence::DataRow { left, right, goal } => f
        .debug_struct("DataRow")
        .field("left", &left.debug_with(db, include_all_fields))
        .field("right", &right.debug_with(db, include_all_fields))
        .field("goal", &goal.debug_with(db, include_all_fields))
        .finish(),
      Evidence::EffRow { left, right, goal } => f
        .debug_struct("EffRow")
        .field("left", &left.debug_with(db, include_all_fields))
        .field("right", &right.debug_with(db, include_all_fields))
        .field("goal", &goal.debug_with(db, include_all_fields))
        .finish(),
    }
  }
}

impl<'ctx, DB> DebugWithDb<DB> for TypeKind<InArena<'ctx>>
where
  DB: ?Sized + crate::Db,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &DB, include_all_fields: bool) -> fmt::Result {
    match self {
      TypeKind::ErrorTy => f.debug_tuple("ErrorTy").finish(),
      TypeKind::IntTy => f.debug_tuple("IntTy").finish(),
      TypeKind::VarTy(var) => f.debug_tuple("VarTy").field(var).finish(),
      TypeKind::RowTy(row) => f
        .debug_tuple("RowTy")
        .field(&row.debug_with(db, include_all_fields))
        .finish(),
      TypeKind::FunTy(arg, eff, ret) => f
        .debug_tuple("FunTy")
        .field(&arg.debug_with(db, include_all_fields))
        .field(&eff.debug_with(db, include_all_fields))
        .field(&ret.debug_with(db, include_all_fields))
        .finish(),
      TypeKind::ProdTy(row) => f
        .debug_tuple("ProdTy")
        .field(&row.debug_with(db, include_all_fields))
        .finish(),
      TypeKind::SumTy(row) => f
        .debug_tuple("SumTy")
        .field(&row.debug_with(db, include_all_fields))
        .finish(),
    }
  }
}

impl<'ctx> fmt::Debug for TypeKind<InArena<'ctx>> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      TypeKind::ErrorTy => f.debug_tuple("ErrorTy").finish(),
      TypeKind::IntTy => f.debug_tuple("IntTy").finish(),
      TypeKind::VarTy(var) => f.debug_tuple("VarTy").field(var).finish(),
      TypeKind::RowTy(row) => f.debug_tuple("RowTy").field(row).finish(),
      TypeKind::FunTy(arg, eff, ret) => f
        .debug_tuple("FunTy")
        .field(arg)
        .field(eff)
        .field(ret)
        .finish(),
      TypeKind::ProdTy(row) => f.debug_tuple("ProdTy").field(row).finish(),
      TypeKind::SumTy(row) => f.debug_tuple("SumTy").field(row).finish(),
    }
  }
}

impl<'ctx> Ty<InArena<'ctx>> {
  pub fn try_as_fn_ty(self) -> Result<(Self, ScopedRow<InArena<'ctx>>, Self), Self> {
    match self.deref() {
      TypeKind::FunTy(arg, eff, ret) => Ok((*arg, *eff, *ret)),
      _ => Err(self),
    }
  }
}

impl<'ctx> EqUnifyValue for SimpleClosedRow<InArena<'ctx>> {}
impl<'ctx> EqUnifyValue for ScopedClosedRow<InArena<'ctx>> {}

pub type SimpleInferRow<'ctx> = SimpleRow<InArena<'ctx>>;
pub type ScopedInferRow<'ctx> = ScopedRow<InArena<'ctx>>;

impl<'ctx> UnifyValue for SimpleInferRow<'ctx> {
  type Error = (Self, Self);

  fn unify_values(left: &Self, right: &Self) -> Result<Self, Self::Error> {
    match (left, right) {
      (Row::Open(left_var), Row::Open(right_var)) => {
        Ok(Row::Open(std::cmp::min(*left_var, *right_var)))
      }
      // Prefer the more solved row if possible
      (Row::Open(_), Row::Closed(_)) => Ok(*right),
      (Row::Closed(_), Row::Open(_)) => Ok(*left),
      (Row::Closed(left_row), Row::Closed(right_row)) => (left_row == right_row)
        .then_some(*left)
        .ok_or((*left, *right)),
    }
  }
}

impl<'ctx> ScopedClosedRow<InArena<'ctx>> {
  /// Checks if we can attempt to unify two rows.
  /// If two rows have different lenghts or different field labels we know they cannot unify so
  /// we don't need to attempt the more expensive unification on their row values.
  pub fn is_unifiable(self, right: Self) -> bool {
    self.0.fields == right.0.fields
  }

  pub fn iter(&self) -> impl Iterator<Item = (&RowLabel, &Ty<InArena<'ctx>>)> {
    self.0.fields.iter().zip(self.0.values.iter())
  }
}
impl<'ctx> SimpleClosedRow<InArena<'ctx>> {
  /// Create a new row that contains all self fields that are not present in sub.
  pub fn difference(self, sub: Self) -> RowInternals<InArena<'ctx>> {
    Self::difference_rowlikes((self.fields(()), self.values(())), sub.fields(()))
  }

  /// Checks if we can attempt to unify two rows.
  /// If two rows have different lenghts or different field labels we know they cannot unify so
  /// we don't need to attempt the more expensive unification on their row values.
  pub fn is_unifiable(self, right: Self) -> bool {
    self.0.fields == right.0.fields
  }

  pub fn iter(&self) -> impl Iterator<Item = (&RowLabel, &Ty<InArena<'ctx>>)> {
    self.0.fields.iter().zip(self.0.values.iter())
  }
}

impl<'ctx, A: 'ctx + Clone + TypeAlloc> TypeFoldable<'ctx> for Wrapper<A> {
  type Alloc = A;

  type Out<B: TypeAlloc> = Wrapper<B>;

  fn try_fold_with<F: FallibleTypeFold<'ctx, In = Self::Alloc>>(
    self,
    fold: &mut F,
  ) -> Result<Self::Out<F::Out>, F::Error> {
    Ok(Wrapper {
      tys: self.tys.try_fold_with(fold)?,
      data_rows: self.data_rows.try_fold_with(fold)?,
      eff_rows: self.eff_rows.try_fold_with(fold)?,
      constrs: self.constrs.try_fold_with(fold)?,
    })
  }
}

#[cfg(test)]
mod tests {
  use crate::{row::Row, MkTy, TypeKind::*};
  use base::{
    id::TyVarId,
    pretty::{PrettyPrint, PrettyWithCtx},
  };

  #[derive(Default)]
  #[salsa::db(crate::Jar, base::Jar)]
  struct TestDatabase {
    storage: salsa::Storage<Self>,
  }
  impl salsa::Database for TestDatabase {}

  #[test]
  fn test_ty_pretty_printing() {
    let db = TestDatabase::default();

    let int = db.mk_ty(IntTy);
    let row = db.mk_row(
      &[db.mk_label("x"), db.mk_label("y"), db.mk_label("z")],
      &[int, int, int],
    );

    let ty = db.mk_ty(FunTy(
      db.mk_ty(ProdTy(Row::Closed(row))),
      Row::Closed(db.empty_row()),
      db.mk_ty(VarTy(TyVarId(0))),
    ));
    let out = ty.pretty_with(&(&db, &db)).pprint().pretty(32).to_string();
    assert_eq!(
      out,
      r#"{ x |> Int, y |> Int, z |> Int }
  -> ∅ ty_var<0>"#
    );
    let out = ty.pretty_with(&(&db, &db)).pprint().pretty(10).to_string();
    assert_eq!(
      out,
      r#"{ x |> Int
, y |> Int
, z |> Int
} ->
  ∅ ty_var<0>"#
    );
  }
}
