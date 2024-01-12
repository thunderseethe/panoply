//! Contains structs and functions for working with unsolved rows during type inference.
use std::cmp::Ordering;

use ty::{
  infer::InArena,
  row::{Row, RowSema, Scoped, Simple},
  Evidence, TypeAlloc,
};

/// An unsolved row equation.
/// This covers all the cases, and only those cases, where we don't have enough information to
/// solve a row equation. It is used during unification to ensure that we only store row equations
/// that we can't make progress on. If we learn new information that lets us solve one of our
/// pending unsolved row equations, we convert it to a row combination and unify it with the
/// updated information.
///
/// If our row equation has a closed goal, this means our left and right must both be open. If
/// either one was closed we could infer the other by taking the difference with goal.
///
/// If our row equation has an open goal there are more possibilities:
///  * Left row open right row open
///  * Left row open, right row closed
///  * Right row open, left row closed
///  Importantly, we cannot have two closed rows with an open goal. Because then we could combine
///  our left and right row to infer the goal row.
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
      (UnsolvedRowEquation::ClosedGoal(_), UnsolvedRowEquation::OpenGoal(_)) => Ordering::Greater,
      (UnsolvedRowEquation::OpenGoal(_), UnsolvedRowEquation::ClosedGoal(_)) => Ordering::Less,
    }
  }
}

impl<'ctx> From<UnsolvedRowEquation<InArena<'ctx>, Simple>> for Evidence<InArena<'ctx>> {
  fn from(eq: UnsolvedRowEquation<InArena<'ctx>, Simple>) -> Self {
    into_ev(eq, |left, right, goal| Evidence::DataRow {
      left,
      right,
      goal,
    })
  }
}

impl<'ctx> From<UnsolvedRowEquation<InArena<'ctx>, Scoped>> for Evidence<InArena<'ctx>> {
  fn from(eq: UnsolvedRowEquation<InArena<'ctx>, Scoped>) -> Self {
    into_ev(eq, |left, right, goal| Evidence::EffRow {
      left,
      right,
      goal,
    })
  }
}

fn into_ev<'ctx, Sema: RowSema>(
  eq: UnsolvedRowEquation<InArena<'ctx>, Sema>,
  ev_constr: impl FnOnce(
    Row<Sema, InArena<'ctx>>,
    Row<Sema, InArena<'ctx>>,
    Row<Sema, InArena<'ctx>>,
  ) -> Evidence<InArena<'ctx>>,
) -> Evidence<InArena<'ctx>> {
  match eq {
    UnsolvedRowEquation::ClosedGoal(cand) => ev_constr(
      Row::Open(cand.left),
      Row::Open(cand.right),
      Row::Closed(cand.goal),
    ),
    UnsolvedRowEquation::OpenGoal(cand) => match cand.ops {
      Operatives::OpenOpen { left, right } => {
        ev_constr(Row::Open(left), Row::Open(right), Row::Open(cand.goal))
      }
      Operatives::OpenClosed { left, right } => {
        ev_constr(Row::Open(left), Row::Closed(right), Row::Open(cand.goal))
      }
      Operatives::ClosedOpen { left, right } => {
        ev_constr(Row::Closed(left), Row::Open(right), Row::Open(cand.goal))
      }
    },
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
