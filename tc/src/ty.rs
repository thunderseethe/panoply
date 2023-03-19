use std::fmt::{self, Debug};
use std::hash::Hash;
use std::ops::Deref;

pub mod row;
use row::*;

pub mod fold;
use fold::*;
use salsa::DebugWithDb;

use crate::InDb;

use self::alloc::{AccessTy, MkTy, TypeAlloc};

#[derive(Debug, PartialEq, Eq)]
pub struct UnifierToTcVarError {
    index: u32,
}
#[derive(Debug, PartialEq, Eq)]
pub struct TcVarToUnifierError {
    index: u32,
}

pub mod alloc {
    use aiahr_core::ident::Ident;

    use crate::{ty::row::RowLabel, ClosedRow, Ty, TypeKind};
    use std::cmp::Ordering;
    use std::fmt::Debug;
    use std::hash::Hash;

    /// Allow our type structs (Ty, ClosedRow, etc.) to be generic in how they are allocated
    ///
    /// We have two allocation strategies we'd like to support. During unification we will allocate
    /// everything in an arena that is dropped at the end of unification. This is to prevent any
    /// unifiers from escaping unification which is an ICE.
    ///
    /// Once unification completes we zonk all our types, generalizing them back to use type variables
    /// where allowed. Once this is done we want to store them in the salsa database so that they are
    /// incrementally recomputed.
    ///
    /// We aim to achieve this by parameterizing our structs by this allocation trait, and then we can
    /// implement unification only against these structs instantiated with our unification allocator.
    pub trait TypeAlloc<TK = TypeKind<Self>> {
        type TypeData: Clone + PartialEq + Eq + PartialOrd + Ord + Hash + Debug;
        type RowFields: Clone + PartialEq + Eq + PartialOrd + Ord + Hash + Debug;
        type RowValues: Clone + PartialEq + Eq + PartialOrd + Ord + Hash + Debug;
        type TypeVar: Clone + PartialEq + Eq + PartialOrd + Ord + Hash + Debug;
    }

    pub(crate) type TypeVarOf<A> = <A as TypeAlloc>::TypeVar;

    /// A trait for allocators that can make types and related data types.
    pub trait MkTy<A: TypeAlloc<TypeKind<A>>> {
        fn mk_ty(&self, kind: TypeKind<A>) -> Ty<A>;
        fn mk_label(&self, label: &str) -> RowLabel;
        fn mk_row(&self, fields: &[RowLabel], values: &[Ty<A>]) -> ClosedRow<A>;

        fn empty_row(&self) -> ClosedRow<A> {
            self.mk_row(&[], &[])
        }
        fn empty_row_ty(&self) -> Ty<A> {
            self.mk_ty(TypeKind::RowTy(self.empty_row()))
        }
        fn single_row(&self, label: Ident, value: Ty<A>) -> ClosedRow<A> {
            self.mk_row(&[label], &[value])
        }
        fn single_row_ty(&self, label: Ident, value: Ty<A>) -> Ty<A> {
            self.mk_ty(TypeKind::RowTy(self.single_row(label, value)))
        }

        fn construct_row(&self, mut row: Vec<(RowLabel, Ty<A>)>) -> ClosedRow<A> {
            row.sort_by(|a, b| Ident::cmp(&a.0, &b.0));

            let mut fields = Vec::with_capacity(row.len());
            let mut values = Vec::with_capacity(row.len());
            for (k, v) in row {
                fields.push(k);
                values.push(v);
            }

            self.mk_row(&fields, &values)
        }
    }

    pub trait AccessTy<'a, A: TypeAlloc<TypeKind<A>>> {
        fn kind(&self, ty: &Ty<A>) -> &'a TypeKind<A>;
        fn row_fields(&self, row: &A::RowFields) -> &'a [RowLabel];
        fn row_values(&self, row: &A::RowValues) -> &'a [Ty<A>];
    }

    // TODO: Replace this once `is_sorted` is stabilized
    pub(crate) trait IteratorSorted: Iterator {
        // Is this iterator sorted based on compare function
        fn considered_sorted_by<F>(self, compare: F) -> bool
        where
            Self: Sized,
            F: FnMut(&Self::Item, &Self::Item) -> Option<Ordering>;

        // Is this iterator sorted based on it's PartialOrd instance
        fn considered_sorted(self) -> bool
        where
            Self: Sized,
            Self::Item: PartialOrd,
        {
            self.considered_sorted_by(PartialOrd::partial_cmp)
        }
    }

    impl<I: Iterator> IteratorSorted for I {
        fn considered_sorted_by<F>(mut self, mut compare: F) -> bool
        where
            Self: Sized,
            F: FnMut(&Self::Item, &Self::Item) -> Option<Ordering>,
        {
            let mut last = match self.next() {
                Some(e) => e,
                None => return true,
            };

            self.all(move |curr| {
                if let Some(Ordering::Greater) | None = compare(&last, &curr) {
                    return false;
                }
                last = curr;
                true
            })
        }
    }

    pub mod db {

        use aiahr_core::{id::TyVarId, ident::Ident};

        use crate::{
            ty::{alloc::IteratorSorted, row::RowLabel},
            ClosedRow, MkTy, Ty, TypeKind,
        };

        use super::{AccessTy, TypeAlloc};

        #[salsa::interned]
        #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
        pub struct TyData {
            #[return_ref]
            pub kind: TypeKind<InDb>,
        }

        #[salsa::interned]
        #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
        pub struct SalsaRowFields {
            #[return_ref]
            pub fields: Vec<Ident>,
        }
        #[salsa::interned]
        pub struct SalsaRowValues {
            #[return_ref]
            pub values: Vec<Ty<InDb>>,
        }

        /// Allocate our types in salsa database.
        #[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
        pub struct InDb;
        impl TypeAlloc for InDb {
            type TypeData = TyData;

            type RowFields = SalsaRowFields;

            type RowValues = SalsaRowValues;

            type TypeVar = TyVarId;
        }
        impl Copy for InDb
        where
            <Self as TypeAlloc>::TypeData: Copy,
            <Self as TypeAlloc>::RowFields: Copy,
            <Self as TypeAlloc>::RowValues: Copy,
            <Self as TypeAlloc>::TypeVar: Copy,
        {
        }

        impl MkTy<InDb> for dyn crate::Db + '_ {
            fn mk_ty(&self, kind: TypeKind<InDb>) -> Ty<InDb> {
                Ty(TyData::new(self, kind))
            }

            fn mk_label(&self, label: &str) -> RowLabel {
                self.ident_str(label)
            }

            fn mk_row(&self, fields: &[RowLabel], values: &[Ty<InDb>]) -> ClosedRow<InDb> {
                debug_assert_eq!(fields.len(), values.len());
                debug_assert!(fields.iter().considered_sorted());

                ClosedRow {
                    fields: SalsaRowFields::new(self, fields.to_vec()),
                    values: SalsaRowValues::new(self, values.to_vec()),
                }
            }
        }

        impl<DB> MkTy<InDb> for DB
        where
            DB: crate::Db,
        {
            fn mk_ty(&self, kind: TypeKind<InDb>) -> Ty<InDb> {
                Ty(TyData::new(self, kind))
            }

            fn mk_label(&self, label: &str) -> RowLabel {
                self.ident_str(label)
            }

            fn mk_row(&self, fields: &[RowLabel], values: &[Ty<InDb>]) -> ClosedRow<InDb> {
                debug_assert_eq!(fields.len(), values.len());
                debug_assert!(fields.iter().considered_sorted());

                ClosedRow {
                    fields: SalsaRowFields::new(self, fields.to_vec()),
                    values: SalsaRowValues::new(self, values.to_vec()),
                }
            }
        }

        impl<'db, DB> AccessTy<'db, InDb> for &'db DB
        where
            DB: crate::Db,
        {
            fn kind(&self, ty: &Ty<InDb>) -> &'db TypeKind<InDb> {
                ty.0.kind(*self)
            }

            fn row_fields(&self, row: &<InDb as TypeAlloc>::RowFields) -> &'db [RowLabel] {
                row.fields(*self).as_slice()
            }

            fn row_values(&self, row: &<InDb as TypeAlloc>::RowValues) -> &'db [Ty<InDb>] {
                row.values(*self).as_slice()
            }
        }
        impl<'db> AccessTy<'db, InDb> for &'db (dyn crate::Db + '_) {
            fn kind(&self, ty: &Ty<InDb>) -> &'db TypeKind<InDb> {
                ty.0.kind(*self)
            }

            fn row_fields(&self, row: &<InDb as TypeAlloc>::RowFields) -> &'db [RowLabel] {
                row.fields(*self)
            }

            fn row_values(&self, row: &<InDb as TypeAlloc>::RowValues) -> &'db [Ty<InDb>] {
                row.values(*self)
            }
        }
    }
}

/// A monomorphic type.
///
/// This is just a wrapper around an interned reference to the `TypeKind` which contains the actual
/// data.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ty<A: TypeAlloc>(pub A::TypeData);

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
pub enum TypeKind<A: TypeAlloc> {
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

impl<'ctx, A: TypeAlloc + 'ctx> DefaultFold<'ctx> for TypeKind<A>
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

#[cfg(test)]
mod tests {
    use bumpalo::Bump;
    use ena::unify::UnifyKey;

    use crate::infer_ty::TcUnifierVar;
    use crate::tests::TestDatabase;
    use crate::TypeKind::*;
    use crate::{MkTy, Row, TyCtx};

    #[test]
    fn test_ty_pretty_printing() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let ctx: TyCtx<'_> = TyCtx::new(&db, &arena);

        let int = ctx.mk_ty(IntTy);
        let row = ctx.mk_row(
            &[ctx.mk_label("x"), ctx.mk_label("y"), ctx.mk_label("z")],
            &[int, int, int],
        );

        let ty = ctx.mk_ty(FunTy(
            ctx.mk_ty(ProdTy(Row::Closed(row))),
            ctx.mk_ty(VarTy(TcUnifierVar::from_index(0))),
        ));
        let arena: pretty::Arena<'_, ()> = pretty::Arena::new();
        let mut out = String::new();
        ty.pretty(&arena, &db)
            .into_doc()
            .render_fmt(32, &mut out)
            .unwrap();
        assert_eq!(
            out,
            r#"{ x |> Int, y |> Int, z |> Int }
  -> tv<0>"#
        );
        let mut out = String::new();
        ty.pretty(&arena, &db)
            .into_doc()
            .render_fmt(10, &mut out)
            .unwrap();
        assert_eq!(
            out,
            r#"{ x |> Int
, y |> Int
, z |> Int
} -> tv<0>"#
        );
    }
}
