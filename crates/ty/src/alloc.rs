use base::ident::Ident;

use crate::{
    row::{NewRow, RowLabel, SimpleClosedRow},
    Ty, TypeKind,
};
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
    type SimpleRowVar: Clone + PartialEq + Eq + PartialOrd + Ord + Hash + Debug;
    type ScopedRowVar: Clone + PartialEq + Eq + PartialOrd + Ord + Hash + Debug;
}

pub type TypeVarOf<A> = <A as TypeAlloc>::TypeVar;
pub type SimpleRowVarOf<A> = <A as TypeAlloc>::SimpleRowVar;
pub type ScopedRowVarOf<A> = <A as TypeAlloc>::ScopedRowVar;

/// A trait for allocators that can make types and related data types.
pub trait MkTy<A: TypeAlloc<TypeKind<A>>> {
    fn mk_ty(&self, kind: TypeKind<A>) -> Ty<A>;
    fn mk_label(&self, label: &str) -> RowLabel;

    fn mk_row<R: NewRow<A>>(&self, fields: &[RowLabel], values: &[Ty<A>]) -> R;
    fn mk_row_vec<R: NewRow<A>>(&self, fields: Vec<RowLabel>, values: Vec<Ty<A>>) -> R;

    fn mk_row_iter<R: NewRow<A>>(
        &self,
        fields: impl Iterator<Item = RowLabel>,
        values: impl Iterator<Item = Ty<A>>,
    ) -> R {
        self.mk_row_vec(fields.collect(), values.collect())
    }

    fn empty_row<R: NewRow<A>>(&self) -> R {
        self.mk_row(&[], &[])
    }
    fn empty_row_ty(&self) -> Ty<A> {
        self.mk_ty(TypeKind::RowTy(self.empty_row()))
    }
    fn single_row<R: NewRow<A>>(&self, label: Ident, value: Ty<A>) -> R {
        self.mk_row(&[label], &[value])
    }
    fn single_row_ty(&self, label: Ident, value: Ty<A>) -> Ty<A> {
        self.mk_ty(TypeKind::RowTy(self.single_row(label, value)))
    }

    fn construct_simple_row(&self, mut row: Vec<(RowLabel, Ty<A>)>) -> SimpleClosedRow<A> {
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
    use crate::{alloc::IteratorSorted, row::RowLabel, MkTy, Ty, TypeKind};
    use base::{id::TyVarId, ident::Ident};

    use super::{AccessTy, TypeAlloc};

    #[salsa::interned]
    #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
    pub struct TyData {
        #[return_ref]
        pub kind: TypeKind<InDb>,
    }

    #[salsa::interned]
    #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
    pub struct RowFields {
        #[return_ref]
        pub fields: Vec<Ident>,
    }
    #[salsa::interned]
    pub struct RowValues {
        #[return_ref]
        pub values: Vec<Ty<InDb>>,
    }

    /// Allocate our types in salsa database.
    #[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
    pub struct InDb;
    impl TypeAlloc for InDb {
        type TypeData = TyData;

        type RowFields = RowFields;

        type RowValues = RowValues;

        type TypeVar = TyVarId;

        type SimpleRowVar = TyVarId;

        type ScopedRowVar = TyVarId;
    }
    impl Copy for InDb
    where
        <Self as TypeAlloc>::TypeData: Copy,
        <Self as TypeAlloc>::RowFields: Copy,
        <Self as TypeAlloc>::RowValues: Copy,
        <Self as TypeAlloc>::TypeVar: Copy,
    {
    }

    impl<DB> MkTy<InDb> for DB
    where
        DB: ?Sized + crate::Db,
    {
        fn mk_ty(&self, kind: TypeKind<InDb>) -> Ty<InDb> {
            Ty(TyData::new(self.as_ty_db(), kind))
        }

        fn mk_label(&self, label: &str) -> RowLabel {
            self.ident_str(label)
        }

        fn mk_row_vec<R: crate::row::NewRow<InDb>>(
            &self,
            fields: Vec<RowLabel>,
            values: Vec<Ty<InDb>>,
        ) -> R {
            debug_assert_eq!(fields.len(), values.len());
            debug_assert!(fields.iter().considered_sorted());

            R::new(
                RowFields::new(self.as_ty_db(), fields),
                RowValues::new(self.as_ty_db(), values),
            )
        }

        fn mk_row<R: crate::row::NewRow<InDb>>(
            &self,
            fields: &[RowLabel],
            values: &[Ty<InDb>],
        ) -> R {
            self.mk_row_vec(fields.to_vec(), values.to_vec())
        }
    }

    impl<'db, DB> AccessTy<'db, InDb> for &'db DB
    where
        DB: ?Sized + crate::Db,
    {
        fn kind(&self, ty: &Ty<InDb>) -> &'db TypeKind<InDb> {
            ty.0.kind(self.as_ty_db())
        }

        fn row_fields(&self, row: &<InDb as TypeAlloc>::RowFields) -> &'db [RowLabel] {
            row.fields(self.as_ty_db()).as_slice()
        }

        fn row_values(&self, row: &<InDb as TypeAlloc>::RowValues) -> &'db [Ty<InDb>] {
            row.values(self.as_ty_db()).as_slice()
        }
    }
}
