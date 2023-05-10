//! Contains structs and functions for working with unsolved rows during type inference.
use std::cmp::Ordering;

use aiahr_ty::{
    infer::InArena,
    row::{Row, RowSema, Simple},
    Evidence, FallibleTypeFold, TypeAlloc, TypeFoldable,
};

impl<'ctx> From<UnsolvedRowEquation<InArena<'ctx>, Simple>> for Evidence<InArena<'ctx>> {
    fn from(eq: UnsolvedRowEquation<InArena<'ctx>, Simple>) -> Self {
        match eq {
            UnsolvedRowEquation::ClosedGoal(cand) => Evidence::Row {
                left: Row::Open(cand.left),
                right: Row::Open(cand.right),
                goal: Row::Closed(cand.goal),
            },
            UnsolvedRowEquation::OpenGoal(cand) => match cand.ops {
                Operatives::OpenOpen { left, right } => Evidence::Row {
                    left: Row::Open(left),
                    right: Row::Open(right),
                    goal: Row::Open(cand.goal),
                },
                Operatives::OpenClosed { left, right } => Evidence::Row {
                    left: Row::Open(left),
                    right: Row::Closed(right),
                    goal: Row::Open(cand.goal),
                },
                Operatives::ClosedOpen { left, right } => Evidence::Row {
                    left: Row::Closed(left),
                    right: Row::Open(right),
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

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct ClosedGoal<A: TypeAlloc, Sema: RowSema> {
    pub(crate) goal: Sema::Closed<A>,
    pub(crate) left: Sema::Open<A>,
    pub(crate) right: Sema::Open<A>,
}
impl<A: TypeAlloc, Sema: RowSema> Clone for ClosedGoal<A, Sema>
where
    Sema::Closed<A>: Clone,
    Sema::Open<A>: Clone,
{
    fn clone(&self) -> Self {
        Self {
            goal: self.goal.clone(),
            left: self.left.clone(),
            right: self.right.clone(),
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
pub(crate) enum Operatives<A: TypeAlloc, Sema: RowSema> {
    OpenOpen {
        left: Sema::Open<A>,
        right: Sema::Open<A>,
    },
    OpenClosed {
        left: Sema::Open<A>,
        right: Sema::Closed<A>,
    },
    ClosedOpen {
        left: Sema::Closed<A>,
        right: Sema::Open<A>,
    },
}
impl<A: TypeAlloc, Sema: RowSema> Clone for Operatives<A, Sema>
where
    Sema::Open<A>: Clone,
    Sema::Closed<A>: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Operatives::OpenOpen { left, right } => Operatives::OpenOpen {
                left: left.clone(),
                right: right.clone(),
            },
            Operatives::OpenClosed { left, right } => Operatives::OpenClosed {
                left: left.clone(),
                right: right.clone(),
            },
            Operatives::ClosedOpen { left, right } => Operatives::ClosedOpen {
                left: left.clone(),
                right: right.clone(),
            },
        }
    }
}
impl<A: TypeAlloc, Sema: RowSema> Copy for Operatives<A, Sema>
where
    Sema::Open<A>: Copy,
    Sema::Closed<A>: Copy,
{
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct OpenGoal<A: TypeAlloc, Sema: RowSema> {
    pub(crate) goal: Sema::Open<A>,
    pub(crate) ops: Operatives<A, Sema>,
}
impl<A: TypeAlloc, Sema: RowSema> Clone for OpenGoal<A, Sema>
where
    Sema::Open<A>: Clone,
    Sema::Closed<A>: Clone,
{
    fn clone(&self) -> Self {
        Self {
            goal: self.goal.clone(),
            ops: self.ops.clone(),
        }
    }
}
impl<A: TypeAlloc, Sema: RowSema> Copy for OpenGoal<A, Sema>
where
    Sema::Open<A>: Copy,
    Sema::Closed<A>: Copy,
{
}

pub(crate) enum UnsolvedRowEquation<A: TypeAlloc, Sema: RowSema> {
    ClosedGoal(ClosedGoal<A, Sema>),
    OpenGoal(OpenGoal<A, Sema>),
}
impl<A: TypeAlloc, Sema: RowSema> Clone for UnsolvedRowEquation<A, Sema>
where
    OpenGoal<A, Sema>: Clone,
    ClosedGoal<A, Sema>: Clone,
{
    fn clone(&self) -> Self {
        match self {
            UnsolvedRowEquation::ClosedGoal(closed) => Self::ClosedGoal(closed.clone()),
            UnsolvedRowEquation::OpenGoal(open) => Self::OpenGoal(open.clone()),
        }
    }
}
impl<A: TypeAlloc, Sema: RowSema> Copy for UnsolvedRowEquation<A, Sema>
where
    A: Clone,
    ClosedGoal<A, Sema>: Copy,
    OpenGoal<A, Sema>: Copy,
{
}
impl<A: TypeAlloc, Sema: RowSema> PartialEq for UnsolvedRowEquation<A, Sema>
where
    OpenGoal<A, Sema>: PartialEq,
    ClosedGoal<A, Sema>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (UnsolvedRowEquation::OpenGoal(a), UnsolvedRowEquation::OpenGoal(b)) => a == b,
            (UnsolvedRowEquation::ClosedGoal(a), UnsolvedRowEquation::ClosedGoal(b)) => a == b,
            _ => false,
        }
    }
}
impl<A: TypeAlloc, Sema: RowSema> Eq for UnsolvedRowEquation<A, Sema>
where
    OpenGoal<A, Sema>: Eq,
    ClosedGoal<A, Sema>: Eq,
{
}

impl<A: TypeAlloc, Sema: RowSema> PartialOrd for UnsolvedRowEquation<A, Sema>
where
    OpenGoal<A, Sema>: PartialOrd,
    ClosedGoal<A, Sema>: PartialOrd,
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
impl<A: TypeAlloc, Sema: RowSema> Ord for UnsolvedRowEquation<A, Sema>
where
    OpenGoal<A, Sema>: Ord,
    ClosedGoal<A, Sema>: Ord,
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
