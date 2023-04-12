use aiahr_core::ident::Ident;

use crate::{
    row::{ClosedRow, RowLabel},
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
}

pub type TypeVarOf<A> = <A as TypeAlloc>::TypeVar;

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
    use crate::alloc::IteratorSorted;
    use aiahr_core::{id::TyVarId, ident::Ident};

    use crate::{
        row::{ClosedRow, RowLabel},
        MkTy, Ty, TypeKind,
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

        fn mk_row(&self, fields: &[RowLabel], values: &[Ty<InDb>]) -> ClosedRow<InDb> {
            debug_assert_eq!(fields.len(), values.len());
            debug_assert!(fields.iter().considered_sorted());

            ClosedRow {
                fields: RowFields::new(self.as_ty_db(), fields.to_vec()),
                values: RowValues::new(self.as_ty_db(), values.to_vec()),
            }
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
