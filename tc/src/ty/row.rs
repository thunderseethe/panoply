use aiahr_core::id::TyVarId;
use aiahr_core::memory::handle::RefHandle;
use ena::unify::{EqUnifyValue, UnifyValue};

use crate::TypeCheckError;

use crate::ty::{InferTy, TcUnifierVar, Ty, TypeFoldable};
use std::cmp::Ordering;
use std::convert::Infallible;
use std::fmt::{self, Debug};

use super::{FallibleTypeFold, MkTy, TypeKind};

/// A label of a row field
pub type RowLabel<'ctx> = RefHandle<'ctx, str>;

/// A closed row is a map of labels to types where all labels are known.
/// Counterpart to an open row where the set of labels is polymorphic
///
/// Because our closed row is an interned map, some important invariants are maintained
/// by the construction of ClosedRow:
/// 1. fields and values are the same length
/// 2. The field at index i is the key for the type at index i in values
/// 3. fields is sorted lexographically
#[derive(Hash)]
pub struct ClosedRow<'ctx, TV> {
    pub fields: RefHandle<'ctx, [RowLabel<'ctx>]>,
    pub values: RefHandle<'ctx, [Ty<'ctx, TV>]>,
}
impl<'ctx, TV: Debug> EqUnifyValue for ClosedRow<'ctx, TV> {}
impl<'ctx, TV> PartialEq for ClosedRow<'ctx, TV> {
    fn eq(&self, other: &Self) -> bool {
        self.fields == other.fields && self.values == other.values
    }
}
impl<'ctx, TV> Eq for ClosedRow<'ctx, TV> {}
impl<'ctx, TV> PartialOrd for ClosedRow<'ctx, TV> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl<'ctx, TV> Ord for ClosedRow<'ctx, TV> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.fields.cmp(&other.fields)
    }
}
impl<'ctx, TV> ClosedRow<'ctx, TV> {
    pub fn is_empty(&self) -> bool {
        self.fields.is_empty()
    }

    pub fn len(&self) -> usize {
        // Because fields.len() must equal values.len() it doesn't matter which we use here
        self.fields.len()
    }

    /// Return true if `self` is a sub row of `row`, false otherwise.
    /// A row is a sub row if all it's fields are within the super row, and all values for those
    /// fields equal the values in the super row.
    pub fn is_sub_row(&self, row: &ClosedRow<'ctx, TV>) -> bool {
        self.fields
            .iter()
            .zip(self.values.iter())
            .all(|(field, value)| {
                row.fields
                    .as_ref()
                    .binary_search(field)
                    .map(|indx| value == &row.values[indx])
                    .unwrap_or(false)
            })
    }
}

/// Internal representation of a row.
/// Sometimes we need this to pass values that will become a row
/// around before we're able to intern them into a row.
pub type RowInternals<'ctx, TV> = (Box<[RowLabel<'ctx>]>, Box<[Ty<'ctx, TV>]>);

impl<'ctx, TV> ClosedRow<'ctx, TV> {
    fn _disjoint_union<E>(
        self,
        right: Self,
        mk_err: impl FnOnce(Self, Self) -> E,
    ) -> Result<RowInternals<'ctx, TV>, E> {
        use std::cmp::Ordering::*;

        let goal_len = self.len() + right.len();
        let mut left_fields = self.fields.iter().peekable();
        let mut left_values = self.values.iter();
        let mut right_fields = right.fields.iter().peekable();
        let mut right_values = right.values.iter();

        let (mut fields, mut values): (Vec<RowLabel<'ctx>>, Vec<Ty<'ctx, TV>>) =
            (Vec::with_capacity(goal_len), Vec::with_capacity(goal_len));
        // Because we know our rows are each individually sorted, we can optimistically merge them here
        loop {
            match (left_fields.peek(), right_fields.peek()) {
                (Some(left_lbl), Some(right_lbl)) => {
                    // This ensures we don't use Handle::ord on accident
                    match str::cmp(left_lbl, right_lbl) {
                        // Push left
                        Less => {
                            fields.push(*left_fields.next().unwrap());
                            values.push(*left_values.next().unwrap());
                        }
                        // Because these are disjoint rows overlapping labels are an error
                        Equal => return Err(mk_err(self, right)),
                        // Push right
                        Greater => {
                            fields.push(*right_fields.next().unwrap());
                            values.push(*right_values.next().unwrap());
                        }
                    }
                }
                // Right row bigger than left
                (None, Some(_)) => {
                    fields.extend(right_fields);
                    values.extend(right_values);
                    break;
                }
                // Left row bigger than right
                (Some(_), None) => {
                    fields.extend(left_fields);
                    values.extend(left_values);
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

impl<'ctx> ClosedRow<'ctx, TcUnifierVar<'ctx>> {
    /// Create a new row that contains all self fields that are not present in sub.
    pub fn difference(self, sub: Self) -> (Box<[RowLabel<'ctx>]>, Box<[InferTy<'ctx>]>) {
        let out_row = self
            .fields
            .iter()
            .zip(self.values.iter())
            .filter(|(field, _)| sub.fields.binary_search(field).is_err());

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
    ) -> Result<RowInternals<'ctx, TcUnifierVar<'ctx>>, TypeCheckError<'ctx>> {
        self._disjoint_union(right, |left, right| {
            TypeCheckError::RowsNotDisjoint(left, right)
        })
    }
}

impl<'ctx> ClosedRow<'ctx, TyVarId> {
    /// Invariant: These rows have already been typed checked so we cannot fail at union.
    pub fn disjoint_union(self, right: Self) -> RowInternals<'ctx, TyVarId> {
        self._disjoint_union::<Infallible>(right, |_, _| unreachable!())
            .unwrap()
    }
}

impl<'ctx, TV> Clone for ClosedRow<'ctx, TV> {
    fn clone(&self) -> Self {
        ClosedRow {
            fields: self.fields,
            values: self.values,
        }
    }
}
impl<'ctx, TV> Copy for ClosedRow<'ctx, TV> {}

impl<'ctx, TV: Debug> Debug for ClosedRow<'ctx, TV> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(
                self.fields
                    .iter()
                    .map(|handle| &handle.0)
                    .zip(self.values.iter()),
            )
            .finish()
    }
}

impl<'ctx, TV: Clone> TypeFoldable<'ctx> for ClosedRow<'_, TV> {
    type TypeVar = TV;
    type Out<T: 'ctx> = ClosedRow<'ctx, T>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::TypeVar>, F::Error> {
        let labels = self
            .fields
            .iter()
            .map(|lbl| fold.ctx().mk_label(lbl))
            .collect::<Vec<_>>();
        let values = self
            .values
            .iter()
            .map(|ty| ty.try_fold_with(fold))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(fold.ctx().mk_row(labels.as_slice(), values.as_slice()))
    }
}

/// A row is our representaion of data, it maps fields to values.
/// Rows come in two flavors: Open and Closed.
#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
pub enum Row<'ctx, TV> {
    /// An open row is a polymorphic set of data. Used to allow generic row programming.
    Open(TV),
    /// A closed row is a concrete mapping from fields to values.
    Closed(ClosedRow<'ctx, TV>),
}
impl<'ctx, TV: PartialOrd> PartialOrd for Row<'ctx, TV> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Row::Open(_), Row::Closed(_)) => Some(Ordering::Greater),
            (Row::Closed(_), Row::Open(_)) => Some(Ordering::Less),
            (Row::Open(a), Row::Open(b)) => a.partial_cmp(b),
            (Row::Closed(a), Row::Closed(b)) => Some(a.cmp(b)),
        }
    }
}
impl<'ctx, TV: Ord> Ord for Row<'ctx, TV> {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Row::Open(_), Row::Closed(_)) => Ordering::Greater,
            (Row::Closed(_), Row::Open(_)) => Ordering::Less,
            (Row::Open(a), Row::Open(b)) => a.cmp(b),
            (Row::Closed(a), Row::Closed(b)) => a.cmp(b),
        }
    }
}

impl<'ctx, TV> From<TV> for Row<'ctx, TV> {
    fn from(var: TV) -> Self {
        Row::Open(var)
    }
}
impl<'ctx, TV: Copy> Row<'ctx, TV> {
    pub fn to_ty<I: MkTy<'ctx, TV>>(self, ctx: &I) -> Ty<'ctx, TV> {
        match self {
            Row::Open(var) => ctx.mk_ty(TypeKind::VarTy(var)),
            Row::Closed(row) => ctx.mk_ty(TypeKind::RowTy(row)),
        }
    }
}

pub type InferRow<'infer> = Row<'infer, TcUnifierVar<'infer>>;
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
impl<'ctx, TV: Clone> TypeFoldable<'ctx> for Row<'_, TV> {
    type TypeVar = TV;
    type Out<T: 'ctx> = Row<'ctx, T>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::TypeVar>, F::Error> {
        match self {
            Row::Open(var) => fold.try_fold_row_var(var),
            Row::Closed(crow) => Ok(Row::Closed(crow.try_fold_with(fold)?)),
        }
    }
}

/// Represents the value of a unification variable that is the component of a row combination.
/// If this is the value in the unification table for row variable uv, then we can imagine it forms
/// the row combination `uv * other ~ goal`
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub(crate) struct CombineInto<'ctx, TV> {
    pub(crate) other: Row<'ctx, TV>,
    pub(crate) goal: Row<'ctx, TV>,
}
impl<'ctx, TV: Clone> TypeFoldable<'ctx> for CombineInto<'_, TV> {
    type TypeVar = TV;
    type Out<T: 'ctx> = CombineInto<'ctx, T>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::TypeVar>, F::Error> {
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
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug, PartialOrd, Ord)]
pub enum OrderedRowXorRow<'ctx, TV> {
    ClosedOpen(ClosedRow<'ctx, TV>, TV),
    OpenOpen { min: TV, max: TV },
}
pub trait Unifiable<Rhs> {
    fn unifiable(&self, other: &Rhs) -> bool;
}
impl<'ctx, TV: PartialEq> Unifiable<(ClosedRow<'ctx, TV>, TV)> for OrderedRowXorRow<'ctx, TV> {
    fn unifiable(&self, (closed, open): &(ClosedRow<'ctx, TV>, TV)) -> bool {
        match self {
            OrderedRowXorRow::ClosedOpen(c, o) => c.fields == closed.fields || o == open,
            OrderedRowXorRow::OpenOpen { .. } => false,
        }
    }
}
impl<'ctx, TV: PartialEq> Unifiable<TV> for OrderedRowXorRow<'ctx, TV> {
    fn unifiable(&self, other: &TV) -> bool {
        match self {
            OrderedRowXorRow::ClosedOpen(_, open) => open == other,
            OrderedRowXorRow::OpenOpen { min, max } => min == other || max == other,
        }
    }
}
impl<'ctx, TV: Copy + Debug + Ord> UnifyValue for OrderedRowXorRow<'ctx, TV> {
    type Error = (Self, Self);

    fn unify_values(left: &Self, right: &Self) -> Result<Self, Self::Error> {
        Ok(match (left, right) {
            (
                OrderedRowXorRow::ClosedOpen(left_row, left_var),
                OrderedRowXorRow::ClosedOpen(right_row, right_var),
            ) => OrderedRowXorRow::ClosedOpen(
                ClosedRow::unify_values(left_row, right_row).map_err(|_| (*left, *right))?,
                std::cmp::min(*left_var, *right_var),
            ),
            (OrderedRowXorRow::ClosedOpen(_, _), OrderedRowXorRow::OpenOpen { .. }) => *left,
            (OrderedRowXorRow::OpenOpen { .. }, OrderedRowXorRow::ClosedOpen(_, _)) => *right,
            (
                OrderedRowXorRow::OpenOpen {
                    min: left_min,
                    max: left_max,
                },
                OrderedRowXorRow::OpenOpen {
                    min: right_min,
                    max: right_max,
                },
            ) => OrderedRowXorRow::with_open_open(
                *std::cmp::min(left_min, right_min),
                *std::cmp::min(left_max, right_max),
            ),
        })
    }
}
impl<'ctx, TV> OrderedRowXorRow<'ctx, TV> {
    pub(crate) fn with_open_open(l: TV, r: TV) -> Self
    where
        TV: Ord,
    {
        debug_assert!(l != r, "Expected l != r in OpenOpen variant");
        if l < r {
            Self::OpenOpen { min: l, max: r }
        } else {
            Self::OpenOpen { min: r, max: l }
        }
    }
}
impl<'ctx, TV> From<OrderedRowXorRow<'ctx, TV>> for (Row<'ctx, TV>, Row<'ctx, TV>) {
    fn from(val: OrderedRowXorRow<'ctx, TV>) -> Self {
        match val {
            OrderedRowXorRow::ClosedOpen(row, var) => (Row::Closed(row), Row::Open(var)),
            OrderedRowXorRow::OpenOpen { min, max } => (Row::Open(min), Row::Open(max)),
        }
    }
}
impl<'ctx, TV: Ord> TryFrom<(Row<'ctx, TV>, Row<'ctx, TV>)> for OrderedRowXorRow<'ctx, TV> {
    type Error = (ClosedRow<'ctx, TV>, ClosedRow<'ctx, TV>);

    fn try_from(value: (Row<'ctx, TV>, Row<'ctx, TV>)) -> Result<Self, Self::Error> {
        match value {
            (Row::Open(l), Row::Open(r)) => Ok(Self::with_open_open(l, r)),
            (Row::Open(tv), Row::Closed(row)) | (Row::Closed(row), Row::Open(tv)) => {
                Ok(Self::ClosedOpen(row, tv))
            }
            (Row::Closed(l), Row::Closed(r)) => Err((l, r)),
        }
    }
}
impl<'ctx, TV: Clone> TypeFoldable<'ctx> for OrderedRowXorRow<'_, TV> {
    type TypeVar = TV;
    type Out<T: 'ctx> = (Row<'ctx, T>, Row<'ctx, T>);

    fn try_fold_with<F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::TypeVar>, F::Error> {
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
