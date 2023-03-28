use pretty::{docs, DocAllocator, DocBuilder, Pretty};
use salsa::DebugWithDb;
use std::fmt::{self, Debug};
use std::hash::Hash;
use std::ops::Deref;

mod alloc;
use self::fold::DefaultFold;
pub use alloc::{
    db::{InDb, SalsaRowFields, SalsaRowValues, TyData},
    AccessTy, MkTy, TypeAlloc, TypeVarOf,
};

mod evidence;
pub use evidence::Evidence;

mod fold;
pub use fold::{FallibleEndoTypeFold, FallibleTypeFold, TypeFoldable};

pub mod row;
use row::{ClosedRow, Row};

#[cfg(feature = "type_infer")]
pub mod infer;

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
    Db: crate::Db,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Db, _include_all_fields: bool) -> fmt::Result {
        self.0.kind(db).debug(db).fmt(f)
    }
}
impl DebugWithDb<dyn crate::Db + '_> for Ty<InDb> {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        db: &dyn crate::Db,
        _include_all_fields: bool,
    ) -> fmt::Result {
        self.0.kind(db).debug(db).fmt(f)
    }
}

impl Ty<InDb> {
    pub fn try_as_prod_row<'a>(self, db: &impl AccessTy<'a, InDb>) -> Result<Row<InDb>, Self> {
        match db.kind(&self) {
            TypeKind::ProdTy(Row::Closed(row)) | TypeKind::RowTy(row) => Ok(Row::Closed(*row)),
            TypeKind::ProdTy(Row::Open(var)) | TypeKind::VarTy(var) => Ok(Row::Open(*var)),
            _ => Err(self),
        }
    }

    pub fn try_as_sum_row<'a>(self, db: &impl AccessTy<'a, InDb>) -> Result<Row<InDb>, Self> {
        match db.kind(&self) {
            TypeKind::SumTy(Row::Closed(row)) | TypeKind::RowTy(row) => Ok(Row::Closed(*row)),
            TypeKind::SumTy(Row::Open(var)) | TypeKind::VarTy(var) => Ok(Row::Open(*var)),
            _ => Err(self),
        }
    }

    pub fn try_as_fn_ty<'a>(self, db: &impl AccessTy<'a, InDb>) -> Result<(Self, Self), Self> {
        match db.kind(&self) {
            TypeKind::FunTy(arg, ret) => Ok((*arg, *ret)),
            _ => Err(self),
        }
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
    RowTy(ClosedRow<A>),
    /// A function type
    FunTy(Ty<A>, Ty<A>),
    /// A product type. This is purely a wrapper type to coerce a row type to be a product.
    ProdTy(Row<A>),
    /// A sum type. This is purely a wrapper type to coerce a row type to be a sum.
    SumTy(Row<A>),
}
impl<A: TypeAlloc> Copy for TypeKind<A>
where
    A: Clone,
    A::TypeVar: Copy,
    ClosedRow<A>: Copy,
    Row<A>: Copy,
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
    Db: crate::Db,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Db, _include_all_fields: bool) -> fmt::Result {
        match self {
            TypeKind::ErrorTy => f.debug_tuple("ErrorTy").finish(),
            TypeKind::IntTy => f.debug_tuple("IntTy").finish(),
            TypeKind::VarTy(var) => f.debug_tuple("VarTy").field(var).finish(),
            TypeKind::RowTy(row) => f.debug_tuple("RowTy").field(&row.debug(db)).finish(),
            TypeKind::FunTy(arg, ret) => f
                .debug_tuple("FunTy")
                .field(&arg.debug(db))
                .field(&ret.debug(db))
                .finish(),
            TypeKind::ProdTy(row) => f.debug_tuple("ProdTy").field(&row.debug(db)).finish(),
            TypeKind::SumTy(row) => f.debug_tuple("SumTy").field(&row.debug(db)).finish(),
        }
    }
}
impl DebugWithDb<dyn crate::Db + '_> for TypeKind<InDb> {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        db: &dyn crate::Db,
        _include_all_fields: bool,
    ) -> fmt::Result {
        match self {
            TypeKind::ErrorTy => f.debug_tuple("ErrorTy").finish(),
            TypeKind::IntTy => f.debug_tuple("IntTy").finish(),
            TypeKind::VarTy(var) => f.debug_tuple("VarTy").field(var).finish(),
            TypeKind::RowTy(row) => f.debug_tuple("RowTy").field(&row.debug(db)).finish(),
            TypeKind::FunTy(arg, ret) => f
                .debug_tuple("FunTy")
                .field(&arg.debug(db))
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
            TypeKind::FunTy(ref arg, ref ret) => {
                let arg_ = arg.clone().try_fold_with(fold)?;
                let ret_ = ret.clone().try_fold_with(fold)?;
                fold.ctx().mk_ty(TypeKind::<F::Out>::FunTy(arg_, ret_))
            }
            TypeKind::RowTy(ref row) => {
                let row_: ClosedRow<F::Out> = row.clone().try_fold_with(fold)?;
                fold.ctx().mk_ty(TypeKind::<F::Out>::RowTy(row_))
            }
            TypeKind::ProdTy(ref row) => {
                let row_: Row<F::Out> = row.clone().try_fold_with(fold)?;
                fold.ctx().mk_ty(TypeKind::ProdTy(row_))
            }
            TypeKind::SumTy(ref row) => {
                let row_: Row<F::Out> = row.clone().try_fold_with(fold)?;
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

impl<A: TypeAlloc> Ty<A> {
    pub fn pretty<'a, 'b, D>(
        &self,
        a: &'a D,
        db: &dyn crate::Db,
        acc: &impl AccessTy<'b, A>,
    ) -> DocBuilder<'a, D>
    where
        D: ?Sized + DocAllocator<'a>,
        D::Doc: pretty::Pretty<'a, D> + Clone,
        A::TypeVar: Pretty<'a, D>,
        A: 'b,
    {
        acc.kind(self).pretty(a, db, acc)
    }
}

impl<A: TypeAlloc> TypeKind<A> {
    pub fn pretty<'a, 'b, D>(
        &self,
        a: &'a D,
        db: &dyn crate::Db,
        acc: &impl AccessTy<'b, A>,
    ) -> DocBuilder<'a, D>
    where
        D: ?Sized + DocAllocator<'a>,
        D::Doc: pretty::Pretty<'a, D> + Clone,
        A::TypeVar: Pretty<'a, D>,
        A: 'b,
    {
        match self {
            TypeKind::ErrorTy => a.as_string("Error"),
            TypeKind::IntTy => a.as_string("Int"),
            TypeKind::VarTy(tv) => pretty::Pretty::pretty(tv.clone(), a),
            TypeKind::RowTy(closed_row) => closed_row.pretty(a, db, acc).nest(2).parens().group(),
            TypeKind::FunTy(arg, ret) => arg
                .pretty(a, db, acc)
                .append(docs![a, a.softline(), "->", a.softline(), ret.pretty(a, db, acc)].nest(2)),
            TypeKind::ProdTy(row) => row
                .pretty(a, db, acc)
                .enclose(a.softline(), a.softline())
                .braces()
                .group(),
            TypeKind::SumTy(row) => row
                .pretty(a, db, acc)
                .enclose(a.softline(), a.softline())
                .angles()
                .group(),
        }
    }
}

/// A type scheme (also know as a polymorphic type).
/// Type schemes wrap a monomorphic type in any number of foralls binding the free variables within
/// the monomorphic type. They may also assert constraints on the bound type variables.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyScheme<A: TypeAlloc = InDb> {
    pub bound: Vec<A::TypeVar>,
    pub constrs: Vec<Evidence<A>>,
    pub eff: Row<A>,
    pub ty: Ty<A>,
}
