use aiahr_core::memory::handle::RefHandle;
use ena::unify::UnifyValue;

use crate::TypeCheckError;

use crate::ty::{InferTy, TcUnifierVar, Ty, TypeFoldable};
use std::convert::Infallible;
use std::fmt::{self, Debug};

use super::{FallibleTypeFold, MkTy, TypeKind};

/// A label of a row field
pub type RowLabel<'ctx> = RefHandle<'ctx, str>;

/// A closed row is a map of labels to values where all labels are known.
/// Counterpart to an open row where the set of labels is polymorphic
///
/// Because our closed row is basically an interned map, some important invariants are maintained
/// by the construction of ClosedRow:
/// 1. fields and values are the same length
/// 2. The field at index i is the key for the type at index i in values
/// 3. fields is sorted lexographically
#[derive(PartialEq, Eq, Hash)]
pub struct ClosedRow<'ctx, TV> {
    pub fields: RefHandle<'ctx, [RowLabel<'ctx>]>,
    pub values: RefHandle<'ctx, [Ty<'ctx, TV>]>,
}

impl<'ctx, TV> ClosedRow<'ctx, TV> {
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
    ) -> Result<(Box<[RowLabel<'ctx>]>, Box<[InferTy<'ctx>]>), TypeCheckError<'ctx>> {
        use std::cmp::Ordering::*;

        let goal_len = self.len() + right.len();
        let mut left_fields = self.fields.iter().peekable();
        let mut left_values = self.values.iter();
        let mut right_fields = right.fields.iter().peekable();
        let mut right_values = right.values.iter();

        let (mut fields, mut values): (Vec<RowLabel<'ctx>>, Vec<InferTy<'ctx>>) =
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
                        Equal => return Err(TypeCheckError::RowsNotDisjoint(self, right)),
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

impl<'ctx, TV> From<TV> for Row<'ctx, TV> {
    fn from(var: TV) -> Self {
        Row::Open(var)
    }
}
impl<'ctx, TV> Row<'ctx, TV> {
    pub fn to_ty<I: MkTy<'ctx, TV>>(self, ctx: &I) -> Ty<'ctx, TV> {
        match self {
            Row::Open(var) => ctx.mk_ty(TypeKind::VarTy(var)),
            Row::Closed(row) => ctx.mk_ty(TypeKind::RowTy(row)),
        }
    }

    pub(crate) fn expect_closed(self, msg: &str) -> ClosedRow<'ctx, TV> {
        match self {
            Row::Closed(row) => row,
            Row::Open(_) => panic!("{msg}"),
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
            (Row::Open(_), Row::Closed(_)) => Ok(right.clone()),
            (Row::Closed(_), Row::Open(_)) => Ok(left.clone()),
            (Row::Closed(left_row), Row::Closed(right_row)) => (left_row == right_row)
                .then(|| left.clone())
                .ok_or_else(|| TypeCheckError::RowsNotEqual(*left, *right)),
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

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum PartialRow<'ctx, TV> {
    /// This row represents the goal of a RowCombine constraint
    OpenGoal {
        left: Row<'ctx, TV>,
        right: Row<'ctx, TV>,
    },
    /// This row represents the left subrow of a RowCombine constraint
    OpenLeft {
        goal: Row<'ctx, TV>,
        right: Row<'ctx, TV>,
    },
    /// This row represents the right subrow of a RowCombine constraint
    OpenRight {
        goal: Row<'ctx, TV>,
        left: Row<'ctx, TV>,
    },
}

impl<'ctx, TV: Clone> TypeFoldable<'ctx> for PartialRow<'_, TV> {
    type TypeVar = TV;
    type Out<T: 'ctx> = PartialRow<'ctx, T>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::TypeVar>, F::Error> {
        match self {
            PartialRow::OpenGoal { left, right } => {
                let left = left.try_fold_with(fold)?;
                let right = right.try_fold_with(fold)?;
                Ok(PartialRow::OpenGoal { left, right })
            }
            PartialRow::OpenLeft { goal, right } => {
                let goal = goal.try_fold_with(fold)?;
                let right = right.try_fold_with(fold)?;
                Ok(PartialRow::OpenLeft { goal, right })
            }
            PartialRow::OpenRight { goal, left } => {
                let goal = goal.try_fold_with(fold)?;
                let left = left.try_fold_with(fold)?;
                Ok(PartialRow::OpenRight { goal, left })
            }
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug, Default)]
pub struct RowSet<'ctx, TV> {
    rows: Vec<PartialRow<'ctx, TV>>,
}

impl<'ctx, TV> RowSet<'ctx, TV> {
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = &'a PartialRow<'ctx, TV>> {
        self.rows.iter()
    }
}

impl<'ctx, TV> From<PartialRow<'ctx, TV>> for RowSet<'ctx, TV> {
    fn from(partial: PartialRow<'ctx, TV>) -> Self {
        RowSet {
            rows: vec![partial],
        }
    }
}

impl<'ctx, TV> IntoIterator for RowSet<'ctx, TV> {
    type Item = PartialRow<'ctx, TV>;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.rows.into_iter()
    }
}

impl<'ctx, TV: Clone> TypeFoldable<'ctx> for RowSet<'_, TV> {
    type TypeVar = TV;
    type Out<T: 'ctx> = RowSet<'ctx, T>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::TypeVar>, F::Error> {
        let rows: Vec<PartialRow<'ctx, F::TypeVar>> = self
            .rows
            .into_iter()
            .map(|row| row.try_fold_with(fold))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(RowSet { rows })
    }
}

impl<'ctx, TV: Clone + Debug + PartialEq> UnifyValue for RowSet<'ctx, TV> {
    type Error = Infallible;

    fn unify_values(left: &Self, right: &Self) -> Result<Self, Self::Error> {
        let mut rows = Vec::with_capacity(left.rows.len() + right.rows.len());
        rows.extend_from_slice(left.rows.as_slice());
        rows.extend_from_slice(right.rows.as_slice());
        rows.dedup_by(|a, b| a.eq(&b));
        Ok(Self { rows })
    }
}
