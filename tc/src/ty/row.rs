use aiahr_core::ident::Ident;
use ena::unify::{EqUnifyValue, UnifyValue};
use pretty::{docs, DocAllocator, DocBuilder};
use salsa::DebugWithDb;

use crate::{diagnostic::TypeCheckError, Evidence, InArena, InDb, Ty, TypeKind};

use crate::ty::TypeFoldable;
use std::cmp::Ordering;
use std::convert::Infallible;
use std::fmt::{self, Debug};
use std::hash::Hash;

use super::{AccessTy, FallibleTypeFold, MkTy, TypeAlloc};

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
pub struct ClosedRow<A: TypeAlloc> {
    pub fields: A::RowFields,
    pub values: A::RowValues,
}
impl<A: TypeAlloc + Copy> Copy for ClosedRow<A>
where
    A::RowFields: Copy,
    A::RowValues: Copy,
{
}
impl<'ctx> ClosedRow<InArena<'ctx>> {
    pub(crate) fn pretty<'a, D>(&self, a: &'a D, db: &dyn crate::Db) -> DocBuilder<'a, D>
    where
        D: ?Sized + DocAllocator<'a>,
        D::Doc: pretty::Pretty<'a, D> + Clone,
    {
        let docs = self
            .fields
            .iter()
            .zip(self.values.iter())
            .map(|(field, value)| {
                docs![
                    a,
                    a.as_string(field.text(db.as_core_db())),
                    a.space(),
                    "|>",
                    a.softline(),
                    value.pretty(a, db)
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
impl<'ctx> EqUnifyValue for ClosedRow<InArena<'ctx>> {}
impl<A: TypeAlloc> ClosedRow<A> {
    pub fn is_empty<'a>(&self, acc: &impl AccessTy<'a, A>) -> bool {
        acc.row_fields(&self.fields).is_empty()
    }

    pub fn len<'a>(&self, acc: &dyn AccessTy<'a, A>) -> usize {
        // Because fields.len() must equal values.len() it doesn't matter which we use here
        acc.row_fields(&self.fields).len()
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
    fn _disjoint_union<'a, E>(
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
                            fields.push(left_fields.next().unwrap().clone());
                            values.push(left_values.next().unwrap().clone());
                        }
                        // Because these are disjoint rows overlapping labels are an error
                        Equal => return Err(mk_err(self, right, left_lbl)),
                        // Push right
                        Greater => {
                            fields.push(right_fields.next().unwrap().clone());
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

impl<'ctx> ClosedRow<InArena<'ctx>> {
    /// Create a new row that contains all self fields that are not present in sub.
    pub fn difference(self, sub: Self) -> (Box<[RowLabel]>, Box<[Ty<InArena<'ctx>>]>) {
        let out_row = self
            .fields
            .iter()
            .zip(self.values.iter())
            .filter(|(field, _)| sub.fields.binary_search_by(|lbl| lbl.cmp(field)).is_err());

        let (mut fields, mut values) = (Vec::new(), Vec::new());
        for (field, value) in out_row {
            fields.push(*field);
            values.push(*value);
        }
        (fields.into_boxed_slice(), values.into_boxed_slice())
    }

    /// Combine two disjoint rows into a new row.
    /// This maintains the row invariants in the resulting row.
    /// If called on two overlapping rows an error is thrown.
    pub fn disjoint_union(
        self,
        right: Self,
    ) -> Result<RowInternals<InArena<'ctx>>, TypeCheckError<'ctx>> {
        self._disjoint_union(right, &(), |left, right, lbl| {
            TypeCheckError::RowsNotDisjoint(left, right, lbl.clone())
        })
    }
}

impl ClosedRow<InDb> {
    /// Invariant: These rows have already been typed checked so we cannot fail at union.
    pub fn disjoint_union(
        self,
        acc: &dyn AccessTy<InDb>,
        right: Self,
    ) -> RowInternals<crate::InDb> {
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
pub enum Row<A: TypeAlloc> {
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
    pub fn to_ty<I: MkTy<A>>(self, ctx: &I) -> Ty<A> {
        match self {
            Row::Open(var) => ctx.mk_ty(TypeKind::VarTy(var)),
            Row::Closed(row) => ctx.mk_ty(TypeKind::RowTy(row)),
        }
    }
}
impl<'ctx> Row<InArena<'ctx>> {
    pub(crate) fn pretty<'a, D>(
        &self,
        allocator: &'a D,
        db: &dyn crate::Db,
    ) -> pretty::DocBuilder<'a, D>
    where
        D: ?Sized + DocAllocator<'a>,
        D::Doc: pretty::Pretty<'a, D> + Clone,
    {
        match self {
            Row::Open(tv) => pretty::Pretty::pretty(tv.clone(), allocator),
            Row::Closed(row) => row.pretty(allocator, db),
        }
    }
}

pub(crate) type InferRow<'ctx> = Row<InArena<'ctx>>;
impl<'ctx> UnifyValue for InferRow<'ctx> {
    type Error = TypeCheckError<'ctx>;

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
                .ok_or(TypeCheckError::RowsNotEqual(*left, *right)),
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

/// Represents the value of a unification variable that is the component of a row combination.
/// If this is the value in the unification table for row variable uv, then we can imagine it forms
/// the row combination `uv * other ~ goal`
#[derive(PartialEq, Eq, Hash, Clone)]
pub(crate) struct CombineInto<A: TypeAlloc> {
    pub(crate) other: Row<A>,
    pub(crate) goal: Row<A>,
}
impl<'ctx, A: TypeAlloc + Clone + 'ctx> TypeFoldable<'ctx> for CombineInto<A> {
    type Alloc = A;
    type Out<B: TypeAlloc> = CombineInto<B>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, In = Self::Alloc>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::Out>, F::Error> {
        Ok(CombineInto {
            other: self.other.try_fold_with(fold)?,
            goal: self.goal.try_fold_with(fold)?,
        })
    }
}

/// TODO: WIP Name
/// What we want to capture here is two fold:
///  1. Row components are ordered in a standard way so comparison is easy (we don't have to check
///     any permutations)
///
///  2. We can store at most one closed row. Two closed rows is considered invalid, unlike if we
///     stored a `(Row<'ctx, TV>, Row<'ctx, TV>)`).
#[derive(Hash, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum OrderedRowXorRow<A: TypeAlloc> {
    ClosedOpen(ClosedRow<A>, A::TypeVar),
    OpenOpen { min: A::TypeVar, max: A::TypeVar },
}
impl<A: TypeAlloc> Copy for OrderedRowXorRow<A>
where
    A: Clone,
    A::TypeVar: Copy,
    ClosedRow<A>: Copy,
{
}
impl<A: TypeAlloc> OrderedRowXorRow<A> {
    pub(crate) fn with_open_open(l: A::TypeVar, r: A::TypeVar) -> Self
    where
        A::TypeVar: Ord,
    {
        debug_assert!(l != r, "Expected l != r in OpenOpen variant");
        if l < r {
            Self::OpenOpen { min: l, max: r }
        } else {
            Self::OpenOpen { min: r, max: l }
        }
    }
}
impl<'ctx> From<OrderedRowXorRow<InArena<'ctx>>> for (Row<InArena<'ctx>>, Row<InArena<'ctx>>) {
    fn from(val: OrderedRowXorRow<InArena<'ctx>>) -> Self {
        match val {
            OrderedRowXorRow::ClosedOpen(row, var) => (Row::Closed(row), Row::Open(var)),
            OrderedRowXorRow::OpenOpen { min, max } => (Row::Open(min), Row::Open(max)),
        }
    }
}
impl<'ctx> TryFrom<(Row<InArena<'ctx>>, Row<InArena<'ctx>>)> for OrderedRowXorRow<InArena<'ctx>> {
    type Error = (ClosedRow<InArena<'ctx>>, ClosedRow<InArena<'ctx>>);

    fn try_from(value: (Row<InArena<'ctx>>, Row<InArena<'ctx>>)) -> Result<Self, Self::Error> {
        match value {
            (Row::Open(l), Row::Open(r)) => Ok(Self::with_open_open(l, r)),
            (Row::Open(tv), Row::Closed(row)) | (Row::Closed(row), Row::Open(tv)) => {
                Ok(Self::ClosedOpen(row, tv))
            }
            (Row::Closed(l), Row::Closed(r)) => Err((l, r)),
        }
    }
}
impl<'ctx, A: TypeAlloc + Clone + 'ctx> TypeFoldable<'ctx> for OrderedRowXorRow<A> {
    type Alloc = A;
    type Out<B: TypeAlloc> = (Row<B>, Row<B>);

    fn try_fold_with<F: FallibleTypeFold<'ctx, In = Self::Alloc>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::Out>, F::Error> {
        Ok(match self {
            OrderedRowXorRow::ClosedOpen(row, tv) => (
                Row::Closed(row.try_fold_with(fold)?),
                fold.try_fold_row_var(tv)?,
            ),
            OrderedRowXorRow::OpenOpen { min, max } => {
                (fold.try_fold_row_var(min)?, fold.try_fold_row_var(max)?)
            }
        })
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct ClosedGoal<A: TypeAlloc> {
    pub(crate) goal: ClosedRow<A>,
    pub(crate) min: A::TypeVar,
    pub(crate) max: A::TypeVar,
}
impl<A: TypeAlloc> Copy for ClosedGoal<A>
where
    A: Clone,
    ClosedRow<A>: Copy,
    A::TypeVar: Copy,
{
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct OpenGoal<A: TypeAlloc> {
    pub(crate) goal: A::TypeVar,
    pub(crate) orxr: OrderedRowXorRow<A>,
}
impl<A: TypeAlloc> Copy for OpenGoal<A>
where
    A: Clone,
    A::TypeVar: Copy,
    OrderedRowXorRow<A>: Copy,
{
}

pub(crate) enum UnsolvedRowEquation<A: TypeAlloc> {
    ClosedGoal(ClosedGoal<A>),
    OpenGoal(OpenGoal<A>),
}
impl<A: TypeAlloc> Clone for UnsolvedRowEquation<A>
where
    OpenGoal<A>: Clone,
    ClosedGoal<A>: Clone,
{
    fn clone(&self) -> Self {
        match self {
            UnsolvedRowEquation::ClosedGoal(closed) => Self::ClosedGoal(closed.clone()),
            UnsolvedRowEquation::OpenGoal(open) => Self::OpenGoal(open.clone()),
        }
    }
}
impl<A: TypeAlloc> Copy for UnsolvedRowEquation<A>
where
    OpenGoal<A>: Copy,
    ClosedGoal<A>: Copy,
{
}
impl<'ctx> From<UnsolvedRowEquation<InArena<'ctx>>> for Evidence<InArena<'ctx>> {
    fn from(eq: UnsolvedRowEquation<InArena<'ctx>>) -> Self {
        match eq {
            UnsolvedRowEquation::ClosedGoal(cand) => Evidence::Row {
                left: Row::Open(cand.min),
                right: Row::Open(cand.max),
                goal: Row::Closed(cand.goal),
            },
            UnsolvedRowEquation::OpenGoal(cand) => match cand.orxr {
                OrderedRowXorRow::ClosedOpen(closed, open) => Evidence::Row {
                    left: Row::Closed(closed),
                    right: Row::Open(open),
                    goal: Row::Open(cand.goal),
                },
                OrderedRowXorRow::OpenOpen { min, max } => Evidence::Row {
                    left: Row::Open(min),
                    right: Row::Open(max),
                    goal: Row::Open(cand.goal),
                },
            },
        }
    }
}
impl<A: TypeAlloc> PartialEq for UnsolvedRowEquation<A>
where
    OpenGoal<A>: PartialEq,
    ClosedGoal<A>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (UnsolvedRowEquation::OpenGoal(a), UnsolvedRowEquation::OpenGoal(b)) => a == b,
            (UnsolvedRowEquation::ClosedGoal(a), UnsolvedRowEquation::ClosedGoal(b)) => a == b,
            _ => false,
        }
    }
}
impl<A: TypeAlloc> Eq for UnsolvedRowEquation<A>
where
    OpenGoal<A>: Eq,
    ClosedGoal<A>: Eq,
{
}

impl<A: TypeAlloc> PartialOrd for UnsolvedRowEquation<A>
where
    OpenGoal<A>: PartialOrd,
    ClosedGoal<A>: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (UnsolvedRowEquation::ClosedGoal(left), UnsolvedRowEquation::ClosedGoal(right)) => {
                left.partial_cmp(right)
            }
            (UnsolvedRowEquation::OpenGoal(left), UnsolvedRowEquation::OpenGoal(right)) => {
                left.partial_cmp(right)
            }
            (UnsolvedRowEquation::ClosedGoal(_), UnsolvedRowEquation::OpenGoal(_)) => {
                Some(Ordering::Greater)
            }
            (UnsolvedRowEquation::OpenGoal(_), UnsolvedRowEquation::ClosedGoal(_)) => {
                Some(Ordering::Less)
            }
        }
    }
}
impl<A: TypeAlloc> Ord for UnsolvedRowEquation<A>
where
    OpenGoal<A>: Ord,
    ClosedGoal<A>: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (UnsolvedRowEquation::ClosedGoal(left), UnsolvedRowEquation::ClosedGoal(right)) => {
                left.cmp(right)
            }
            (UnsolvedRowEquation::OpenGoal(left), UnsolvedRowEquation::OpenGoal(right)) => {
                left.cmp(right)
            }
            (UnsolvedRowEquation::ClosedGoal(_), UnsolvedRowEquation::OpenGoal(_)) => {
                Ordering::Greater
            }
            (UnsolvedRowEquation::OpenGoal(_), UnsolvedRowEquation::ClosedGoal(_)) => {
                Ordering::Less
            }
        }
    }
}
