//! Contains structs and functions for working with unsolved rows during type inference.
use std::cmp::Ordering;

use aiahr_ty::{
    infer::InArena,
    row::{Row, SimpleClosedRow},
    Evidence, FallibleTypeFold, TypeAlloc, TypeFoldable,
};

impl<'ctx> From<OrderedRowXorRow<InArena<'ctx>>> for (Row<InArena<'ctx>>, Row<InArena<'ctx>>) {
    fn from(val: OrderedRowXorRow<InArena<'ctx>>) -> Self {
        match val {
            OrderedRowXorRow::ClosedOpen(row, var) => (Row::Closed(row), Row::Open(var)),
            OrderedRowXorRow::OpenOpen { min, max } => (Row::Open(min), Row::Open(max)),
        }
    }
}
impl<'ctx> TryFrom<(Row<InArena<'ctx>>, Row<InArena<'ctx>>)> for OrderedRowXorRow<InArena<'ctx>> {
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
#[derive(Hash, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum OrderedRowXorRow<A: TypeAlloc, Closed = SimpleClosedRow<A>> {
    ClosedOpen(Closed, A::TypeVar),
    OpenOpen { min: A::TypeVar, max: A::TypeVar },
}
impl<A: TypeAlloc, Closed> Copy for OrderedRowXorRow<A, Closed>
where
    A: Clone,
    A::TypeVar: Copy,
    Closed: Copy,
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
pub(crate) struct ClosedGoal<A: TypeAlloc, Closed = SimpleClosedRow<A>> {
    pub(crate) goal: Closed,
    pub(crate) min: A::TypeVar,
    pub(crate) max: A::TypeVar,
}
impl<A: TypeAlloc, Closed> Copy for ClosedGoal<A, Closed>
where
    A: Clone,
    Closed: Copy,
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
