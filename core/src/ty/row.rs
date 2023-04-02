use crate::ident::Ident;
use pretty::{docs, DocAllocator, DocBuilder, Pretty};
use salsa::DebugWithDb;

use std::convert::Infallible;
use std::fmt::{self, Debug};
use std::hash::Hash;

use super::{alloc::MkTy, AccessTy, FallibleTypeFold, InDb, Ty, TypeAlloc, TypeFoldable, TypeKind};

/// A label of a row field
pub type RowLabel = Ident;

/// A closed row is a map of labels to types where all labels are known.
/// Counterpart to an open row where the set of labels is polymorphic
///
/// Because our closed row is an interned map, some important invariants are maintained
/// by the construction of ClosedRow:
/// 1. fields and values are the same length
/// 2. The field at index i is the key for the type at index i in values
/// 3. fields is sorted lexographically
#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub struct ClosedRow<A: TypeAlloc = InDb> {
    pub fields: A::RowFields,
    pub values: A::RowValues,
}
impl<A: TypeAlloc + Copy> Copy for ClosedRow<A>
where
    A::RowFields: Copy,
    A::RowValues: Copy,
{
}
impl<A: TypeAlloc> ClosedRow<A> {
    pub fn is_empty<'a>(&self, acc: &(impl ?Sized + AccessTy<'a, A>)) -> bool {
        acc.row_fields(&self.fields).is_empty()
    }

    pub fn len<'a>(&self, acc: &(impl ?Sized + AccessTy<'a, A>)) -> usize {
        // Because fields.len() must equal values.len() it doesn't matter which we use here
        acc.row_fields(&self.fields).len()
    }

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
        let docs = acc
            .row_fields(&self.fields)
            .iter()
            .zip(acc.row_values(&self.values).iter())
            .map(|(field, value)| {
                docs![
                    a,
                    a.as_string(field.text(db.as_core_db())),
                    a.space(),
                    "|>",
                    a.softline(),
                    value.pretty(a, db, acc)
                ]
                .group()
            });
        a.intersperse(
            docs,
            a.concat([a.softline_(), a.as_string(","), a.space()])
                .into_doc(),
        )
    }
}

impl<A: TypeAlloc> ClosedRow<A>
where
    Ty<A>: PartialEq,
{
    /// Return true if `self` is a sub row of `row`, false otherwise.
    /// A row is a sub row if all it's fields are within the super row, and all values for those
    /// fields equal the values in the super row.
    pub fn is_sub_row<'a>(&self, acc: &dyn AccessTy<'a, A>, row: &Self) -> bool
    where
        A: 'a,
    {
        let row_fields = acc.row_fields(&row.fields);
        let row_values = acc.row_values(&row.values);
        acc.row_fields(&self.fields)
            .iter()
            .zip(acc.row_values(&self.values))
            .all(|(field, value)| {
                row_fields
                    .binary_search(field)
                    .map(|indx| value == &row_values[indx])
                    .unwrap_or(false)
            })
    }
}

/// Internal representation of a row.
/// Sometimes we need this to pass values that will become a row
/// around before we're able to intern them into a row.
pub type RowInternals<A> = (Box<[RowLabel]>, Box<[Ty<A>]>);

impl<A: TypeAlloc> ClosedRow<A>
where
    Ty<A>: Clone,
{
    pub fn _disjoint_union<'a, E>(
        self,
        right: Self,
        acc: &dyn AccessTy<'a, A>,
        mk_err: impl FnOnce(Self, Self, &RowLabel) -> E,
    ) -> Result<RowInternals<A>, E>
    where
        A: 'a,
    {
        use std::cmp::Ordering::*;

        let goal_len = self.len(acc) + right.len(acc);
        let mut left_fields = acc.row_fields(&self.fields).iter().peekable();
        let mut left_values = acc.row_values(&self.values).iter();
        let mut right_fields = acc.row_fields(&right.fields).iter().peekable();
        let mut right_values = acc.row_values(&right.values).iter();

        let (mut fields, mut values): (Vec<RowLabel>, Vec<Ty<A>>) =
            (Vec::with_capacity(goal_len), Vec::with_capacity(goal_len));
        // Because we know our rows are each individually sorted, we can optimistically merge them here
        loop {
            match (left_fields.peek(), right_fields.peek()) {
                (Some(left_lbl), Some(right_lbl)) => {
                    // This ensures we don't use Handle::ord on accident
                    match left_lbl.cmp(right_lbl) {
                        // Push left
                        Less => {
                            fields.push(*left_fields.next().unwrap());
                            values.push(left_values.next().unwrap().clone());
                        }
                        // Because these are disjoint rows overlapping labels are an error
                        Equal => return Err(mk_err(self, right, left_lbl)),
                        // Push right
                        Greater => {
                            fields.push(*right_fields.next().unwrap());
                            values.push(right_values.next().unwrap().clone());
                        }
                    }
                }
                // Right row bigger than left
                (None, Some(_)) => {
                    fields.extend(right_fields);
                    values.extend(right_values.cloned());
                    break;
                }
                // Left row bigger than right
                (Some(_), None) => {
                    fields.extend(left_fields);
                    values.extend(left_values.cloned());
                    break;
                }
                (None, None) => break,
            }
        }

        fields.shrink_to_fit();
        values.shrink_to_fit();

        Ok((fields.into_boxed_slice(), values.into_boxed_slice()))
    }
}

impl ClosedRow<InDb> {
    /// Invariant: These rows have already been typed checked so we cannot fail at union.
    pub fn disjoint_union(self, acc: &dyn AccessTy<InDb>, right: Self) -> RowInternals<InDb> {
        self._disjoint_union::<Infallible>(right, acc, |_, _, _| unreachable!())
            .unwrap()
    }
}
impl<Db> DebugWithDb<Db> for ClosedRow<InDb>
where
    Db: crate::Db,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Db, _include_all_fields: bool) -> fmt::Result {
        f.debug_map()
            .entries(
                self.fields
                    .fields(db)
                    .iter()
                    .map(|handle| handle.text(db.as_core_db()))
                    .zip(self.values.values(db).iter()),
            )
            .finish()
    }
}
impl DebugWithDb<dyn crate::Db + '_> for ClosedRow<InDb> {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        db: &dyn crate::Db,
        _include_all_fields: bool,
    ) -> fmt::Result {
        f.debug_map()
            .entries(
                self.fields
                    .fields(db)
                    .iter()
                    .map(|handle| handle.text(db.as_core_db()))
                    .zip(self.values.values(db).iter()),
            )
            .finish()
    }
}

impl<'ctx, A: TypeAlloc + Clone + 'ctx> TypeFoldable<'ctx> for ClosedRow<A> {
    type Alloc = A;
    type Out<B: TypeAlloc> = ClosedRow<B>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, In = Self::Alloc>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::Out>, F::Error> {
        let values = fold
            .access()
            .row_values(&self.values)
            .iter()
            .map(|ty| ty.clone().try_fold_with(fold))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(fold
            .ctx()
            .mk_row(fold.access().row_fields(&self.fields), values.as_slice()))
    }
}

/// A row is our representaion of data, it maps fields to values.
/// Rows come in two flavors: Open and Closed.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub enum Row<A: TypeAlloc = InDb> {
    /// An open row is a polymorphic set of data. Used to allow generic row programming.
    Open(A::TypeVar),
    /// A closed row is a concrete mapping from fields to values.
    Closed(ClosedRow<A>),
}
impl<A: TypeAlloc> Copy for Row<A>
where
    A: Clone,
    A::TypeVar: Copy,
    ClosedRow<A>: Copy,
{
}
impl<Db> DebugWithDb<Db> for Row<InDb>
where
    Db: crate::Db,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Db, _include_all_fields: bool) -> fmt::Result {
        match self {
            Row::Open(var) => f.debug_tuple("Open").field(var).finish(),
            Row::Closed(row) => f.debug_tuple("Closed").field(&row.debug(db)).finish(),
        }
    }
}
impl DebugWithDb<dyn crate::Db + '_> for Row<InDb> {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        db: &dyn crate::Db,
        _include_all_fields: bool,
    ) -> fmt::Result {
        match self {
            Row::Open(var) => f.debug_tuple("Open").field(var).finish(),
            Row::Closed(row) => f.debug_tuple("Closed").field(&row.debug(db)).finish(),
        }
    }
}
impl<A: TypeAlloc> Row<A> {
    pub fn pretty<'a, 'b, D>(
        &self,
        allocator: &'a D,
        db: &dyn crate::Db,
        acc: &impl AccessTy<'b, A>,
    ) -> pretty::DocBuilder<'a, D>
    where
        D: ?Sized + DocAllocator<'a>,
        D::Doc: pretty::Pretty<'a, D> + Clone,
        A::TypeVar: Pretty<'a, D>,
        A: 'b,
    {
        match self {
            Row::Open(tv) => pretty::Pretty::pretty(tv.clone(), allocator),
            Row::Closed(row) => row.pretty(allocator, db, acc),
        }
    }
}

impl<A: TypeAlloc> Row<A> {
    pub fn to_ty<I: MkTy<A>>(self, ctx: &I) -> Ty<A> {
        match self {
            Row::Open(var) => ctx.mk_ty(TypeKind::VarTy(var)),
            Row::Closed(row) => ctx.mk_ty(TypeKind::RowTy(row)),
        }
    }
}

impl<'ctx, A: TypeAlloc + Clone + 'ctx> TypeFoldable<'ctx> for Row<A> {
    type Alloc = A;
    type Out<B: TypeAlloc> = Row<B>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, In = Self::Alloc>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::Out>, F::Error> {
        match self {
            Row::Open(var) => fold.try_fold_row_var(var),
            Row::Closed(crow) => crow.try_fold_with(fold).map(Row::Closed),
        }
    }
}
