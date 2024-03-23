use std::cmp::Ordering;

use crate::row::{Scoped, Simple};

use super::{row::Row, FallibleTypeFold, InDb, TypeAlloc, TypeFoldable};
use salsa::DebugWithDb;

/// Evidence proving a piece of type level information.
/// This allows type schemes to express constraints on it's type variables that type checking must
/// prove true for the the scheme to type check.
/// This will be things like row combinations or type class constraints.
/// Once we drop down to the IR level where everything is explicitly typed each piece of required
/// evidence will appear as a parameter to it's term, and passing the parameter provides a witness
/// taht the evidence is true.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Evidence<A: TypeAlloc = InDb> {
  DataRow {
    left: Row<Simple, A>,
    right: Row<Simple, A>,
    goal: Row<Simple, A>,
  },
  EffRow {
    left: Row<Scoped, A>,
    right: Row<Scoped, A>,
    goal: Row<Scoped, A>,
  },
}
impl<A: TypeAlloc> Copy for Evidence<A>
where
  A: Clone,
  Row<Simple, A>: Copy,
  Row<Scoped, A>: Copy,
{
}

impl<A: TypeAlloc + Eq> PartialOrd for Evidence<A>
where
  Row<Simple, A>: Ord,
  Row<Scoped, A>: Ord,
{
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}

/// We use ordering of evidence to determine order of parameters at the IR level.
impl<A: TypeAlloc + Eq> Ord for Evidence<A>
where
  Row<Simple, A>: Ord,
  Row<Scoped, A>: Ord,
{
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    match (self, other) {
      // Two solved rows compare normally
      (
        Evidence::DataRow {
          left: Row::Closed(a_left),
          right: Row::Closed(a_right),
          goal: Row::Closed(a_goal),
        },
        Evidence::DataRow {
          left: Row::Closed(b_left),
          right: Row::Closed(b_right),
          goal: Row::Closed(b_goal),
        },
      ) => a_goal
        .cmp(b_goal)
        .then(a_left.cmp(b_left))
        .then(a_right.cmp(b_right)),
      // solved `cmp` unsolved = Less
      (
        Evidence::DataRow {
          left: Row::Closed(_),
          right: Row::Closed(_),
          goal: Row::Closed(_),
        },
        Evidence::DataRow { .. },
      ) => Ordering::Less,
      // unsolved `cmp` solved = Greater
      (
        Evidence::DataRow { .. },
        Evidence::DataRow {
          left: Row::Closed(_),
          right: Row::Closed(_),
          goal: Row::Closed(_),
        },
      ) => Ordering::Greater,
      // Two unsolved rows compare normally
      (
        Evidence::DataRow {
          left: a_left,
          right: a_right,
          goal: a_goal,
        },
        Evidence::DataRow {
          left: b_left,
          right: b_right,
          goal: b_goal,
        },
      ) => a_goal
        .cmp(b_goal)
        .then(a_left.cmp(b_left))
        .then(a_right.cmp(b_right)),
      // Two solved rows compare normally
      (
        Evidence::EffRow {
          left: Row::Closed(a_left),
          right: Row::Closed(a_right),
          goal: Row::Closed(a_goal),
        },
        Evidence::EffRow {
          left: Row::Closed(b_left),
          right: Row::Closed(b_right),
          goal: Row::Closed(b_goal),
        },
      ) => a_goal
        .cmp(b_goal)
        .then(a_left.cmp(b_left))
        .then(a_right.cmp(b_right)),
      // solved `cmp` unsolved = Less
      (
        Evidence::EffRow {
          left: Row::Closed(_),
          right: Row::Closed(_),
          goal: Row::Closed(_),
        },
        Evidence::EffRow { .. },
      ) => Ordering::Less,
      // unsolved `cmp` solved = Greater
      (
        Evidence::EffRow { .. },
        Evidence::EffRow {
          left: Row::Closed(_),
          right: Row::Closed(_),
          goal: Row::Closed(_),
        },
      ) => Ordering::Greater,
      // Two unsolved rows compare normally
      (
        Evidence::EffRow {
          left: a_left,
          right: a_right,
          goal: a_goal,
        },
        Evidence::EffRow {
          left: b_left,
          right: b_right,
          goal: b_goal,
        },
      ) => a_goal
        .cmp(b_goal)
        .then(a_left.cmp(b_left))
        .then(a_right.cmp(b_right)),
      (Evidence::EffRow { .. }, Evidence::DataRow { .. }) => Ordering::Less,
      (Evidence::DataRow { .. }, Evidence::EffRow { .. }) => Ordering::Greater,
    }
  }
}

impl<Db> DebugWithDb<Db> for Evidence<InDb>
where
  Db: ?Sized + crate::Db,
{
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
    db: &Db,
    include_all_fields: bool,
  ) -> std::fmt::Result {
    match self {
      Evidence::DataRow { left, right, goal } => f
        .debug_struct("Evidence::DataRow")
        .field("left", &left.debug_with(db, include_all_fields))
        .field("right", &right.debug_with(db, include_all_fields))
        .field("goal", &goal.debug_with(db, include_all_fields))
        .finish(),
      Evidence::EffRow { left, right, goal } => f
        .debug_struct("Evidence::EffRow")
        .field("left", &left.debug_with(db, include_all_fields))
        .field("right", &right.debug_with(db, include_all_fields))
        .field("goal", &goal.debug_with(db, include_all_fields))
        .finish(),
    }
  }
}

impl<'ctx, A: TypeAlloc + Clone + 'ctx> TypeFoldable<'ctx> for Evidence<A> {
  type Alloc = A;
  type Out<B: TypeAlloc> = Evidence<B>;

  fn try_fold_with<F: FallibleTypeFold<'ctx, In = Self::Alloc>>(
    self,
    fold: &mut F,
  ) -> Result<Self::Out<F::Out>, F::Error> {
    match self {
      Evidence::DataRow { left, right, goal } => Ok(Evidence::DataRow {
        left: left.try_fold_with(fold)?,
        right: right.try_fold_with(fold)?,
        goal: goal.try_fold_with(fold)?,
      }),
      Evidence::EffRow { left, right, goal } => Ok(Evidence::EffRow {
        left: left.try_fold_with(fold)?,
        right: right.try_fold_with(fold)?,
        goal: goal.try_fold_with(fold)?,
      }),
    }
  }
}
