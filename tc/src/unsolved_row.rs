//! Contains structs and functions for working with unsolved rows during type inference.
use std::cmp::Ordering;

use aiahr_ty::{
    infer::{InArena, SimpleInferRow},
    row::{Row, RowSema, Scoped, ScopedRow, Simple, SimpleClosedRow, SimpleRow},
    Evidence, FallibleTypeFold, TypeAlloc, TypeFoldable,
};

impl<'ctx, Sema: RowSema> From<OrderedRowXorRow<InArena<'ctx>, Sema>>
    for (Row<InArena<'ctx>, Sema>, Row<InArena<'ctx>, Sema>)
{
    fn from(val: OrderedRowXorRow<InArena<'ctx>, Sema>) -> Self {
        match val {
            OrderedRowXorRow::ClosedOpen(row, var) => (Row::Closed(row), Row::Open(var)),
            OrderedRowXorRow::OpenOpen { min, max } => (Row::Open(min), Row::Open(max)),
        }
    }
}
impl<'ctx> TryFrom<(SimpleInferRow<'ctx>, SimpleInferRow<'ctx>)>
    for OrderedRowXorRow<InArena<'ctx>, Simple>
{
    type Error = (
        SimpleClosedRow<InArena<'ctx>>,
        SimpleClosedRow<InArena<'ctx>>,
    );

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
#[derive(Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum OrderedRowXorRow<A: TypeAlloc, Sema: RowSema> {
    ClosedOpen(Sema::Closed<A>, Sema::Open<A>),
    OpenOpen {
        min: Sema::Open<A>,
        max: Sema::Open<A>,
    },
}
impl<A: TypeAlloc, Sema: RowSema> Clone for OrderedRowXorRow<A, Sema>
where
    Sema::Open<A>: Clone,
    Sema::Closed<A>: Clone,
{
    fn clone(&self) -> Self {
        match self {
            OrderedRowXorRow::ClosedOpen(closed, open) => {
                Self::ClosedOpen(closed.clone(), open.clone())
            }
            OrderedRowXorRow::OpenOpen { min, max } => Self::OpenOpen {
                min: min.clone(),
                max: max.clone(),
            },
        }
    }
}
impl<A: TypeAlloc, Sema: RowSema> Copy for OrderedRowXorRow<A, Sema>
where
    A: Clone,
    Sema::Open<A>: Copy,
    Sema::Closed<A>: Copy,
{
}
impl<A: TypeAlloc, Sema: RowSema> OrderedRowXorRow<A, Sema> {
    pub(crate) fn with_open_open(l: Sema::Open<A>, r: Sema::Open<A>) -> Self
    where
        Sema::Open<A>: Ord,
    {
        debug_assert!(l != r, "Expected l != r in OpenOpen variant");
        if l < r {
            Self::OpenOpen { min: l, max: r }
        } else {
            Self::OpenOpen { min: r, max: l }
        }
    }
}
impl<'ctx, A: TypeAlloc + Clone + 'ctx> TypeFoldable<'ctx> for OrderedRowXorRow<A, Simple> {
    type Alloc = A;
    type Out<B: TypeAlloc> = (SimpleRow<B>, SimpleRow<B>);

    fn try_fold_with<F: FallibleTypeFold<'ctx, In = Self::Alloc>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::Out>, F::Error> {
        Ok(match self {
            OrderedRowXorRow::ClosedOpen(row, tv) => (
                Row::Closed(row.try_fold_with(fold)?),
                fold.try_fold_simple_row_var(tv)?,
            ),
            OrderedRowXorRow::OpenOpen { min, max } => (
                fold.try_fold_simple_row_var(min)?,
                fold.try_fold_simple_row_var(max)?,
            ),
        })
    }
}
impl<'ctx, A: TypeAlloc + Clone + 'ctx> TypeFoldable<'ctx> for OrderedRowXorRow<A, Scoped> {
    type Alloc = A;
    type Out<B: TypeAlloc> = (ScopedRow<B>, ScopedRow<B>);

    fn try_fold_with<F: FallibleTypeFold<'ctx, In = Self::Alloc>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::Out>, F::Error> {
        Ok(match self {
            OrderedRowXorRow::ClosedOpen(row, tv) => (
                Row::Closed(row.try_fold_with(fold)?),
                fold.try_fold_scoped_row_var(tv)?,
            ),
            OrderedRowXorRow::OpenOpen { min, max } => (
                fold.try_fold_scoped_row_var(min)?,
                fold.try_fold_scoped_row_var(max)?,
            ),
        })
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct ClosedGoal<A: TypeAlloc, Sema: RowSema> {
    pub(crate) goal: Sema::Closed<A>,
    pub(crate) min: Sema::Open<A>,
    pub(crate) max: Sema::Open<A>,
}
impl<A: TypeAlloc, Sema: RowSema> Clone for ClosedGoal<A, Sema>
where
    Sema::Closed<A>: Clone,
    Sema::Open<A>: Clone,
{
    fn clone(&self) -> Self {
        Self {
            goal: self.goal.clone(),
            min: self.min.clone(),
            max: self.max.clone(),
        }
    }
}
impl<A: TypeAlloc, Sema: RowSema> Copy for ClosedGoal<A, Sema>
where
    A: Clone,
    Sema::Closed<A>: Copy,
    Sema::Open<A>: Copy,
{
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct OpenGoal<A: TypeAlloc, Sema: RowSema> {
    pub(crate) goal: Sema::Open<A>,
    pub(crate) orxr: OrderedRowXorRow<A, Sema>,
}
impl<A: TypeAlloc, Sema: RowSema> Clone for OpenGoal<A, Sema>
where
    A: Clone,
    Sema::Open<A>: Clone,
    OrderedRowXorRow<A, Sema>: Clone,
{
    fn clone(&self) -> Self {
        Self {
            goal: self.goal.clone(),
            orxr: self.orxr.clone(),
        }
    }
}
impl<A: TypeAlloc, Sema: RowSema> Copy for OpenGoal<A, Sema>
where
    A: Clone,
    Sema::Open<A>: Copy,
    OrderedRowXorRow<A, Sema>: Copy,
{
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Operatives<A: TypeAlloc> {
    OpenOpen {
        left: <Scoped as RowSema>::Open<A>,
        right: <Scoped as RowSema>::Open<A>,
    },
    OpenClosed {
        left: <Scoped as RowSema>::Open<A>,
        right: <Scoped as RowSema>::Closed<A>,
    },
    ClosedOpen {
        left: <Scoped as RowSema>::Closed<A>,
        right: <Scoped as RowSema>::Open<A>,
    },
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct ScopedOpenGoal<A: TypeAlloc> {
    pub(crate) goal: <Scoped as RowSema>::Open<A>,
    pub(crate) ops: Operatives<A>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum ScopedRowEquation<A: TypeAlloc> {
    ClosedGoal(ClosedGoal<A, Scoped>),
    OpenGoal(ScopedOpenGoal<A>),
}

pub(crate) enum UnsolvedRowEquation<A: TypeAlloc> {
    ClosedGoal(ClosedGoal<A, Simple>),
    OpenGoal(OpenGoal<A, Simple>),
}
impl<A: TypeAlloc> Clone for UnsolvedRowEquation<A>
where
    OpenGoal<A, Simple>: Clone,
    ClosedGoal<A, Simple>: Clone,
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
    OpenGoal<A, Simple>: Copy,
    ClosedGoal<A, Simple>: Copy,
{
}
impl<A: TypeAlloc> PartialEq for UnsolvedRowEquation<A>
where
    OpenGoal<A, Simple>: PartialEq,
    ClosedGoal<A, Simple>: PartialEq,
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
    OpenGoal<A, Simple>: Eq,
    ClosedGoal<A, Simple>: Eq,
{
}

impl<A: TypeAlloc> PartialOrd for UnsolvedRowEquation<A>
where
    OpenGoal<A, Simple>: PartialOrd,
    ClosedGoal<A, Simple>: PartialOrd,
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
    OpenGoal<A, Simple>: Ord,
    ClosedGoal<A, Simple>: Ord,
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
