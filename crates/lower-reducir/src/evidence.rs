use std::ops::Index;

use reducir::ReducIrVar;
use ty::{
  row::{Row, Scoped, ScopedRow, Simple, SimpleClosedRow},
  Evidence, InDb, RowFields, RowValues,
};

use rustc_hash::FxHashMap;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub(crate) enum PartialEv {
  Data {
    other: Row<Simple>,
    goal: Row<Simple>,
  },
  ScopedLeft {
    left: Row<Scoped>,
    goal: Row<Scoped>,
  },
  ScopedRight {
    right: Row<Scoped>,
    goal: Row<Scoped>,
  },
}
impl Copy for PartialEv where InDb: Copy {}

#[derive(Default, Debug)]
pub(crate) struct EvidenceMap {
  /// Unique list of parameters we've generated so far
  params: Vec<(ReducIrVar, Evidence)>,
  // Find evidence when we only have partial information about it.
  // Like when we encounter a Project or Inject node.
  partial_map: FxHashMap<PartialEv, usize>,
  complete_map: FxHashMap<Evidence, usize>,
}
impl EvidenceMap {
  pub(crate) fn insert(&mut self, ev: Evidence, param: ReducIrVar) {
    let idx = self
      .params
      .iter()
      .position(|p| p.0 == param)
      .unwrap_or_else(|| {
        let idx = self.params.len();
        self.params.push((param, ev));
        idx
      });

    match &ev {
      Evidence::DataRow { left, right, goal } => {
        self.partial_map.insert(
          PartialEv::Data {
            other: *left,
            goal: *goal,
          },
          idx,
        );
        self.partial_map.insert(
          PartialEv::Data {
            other: *right,
            goal: *goal,
          },
          idx,
        );
      }
      Evidence::EffRow { left, right, goal } => {
        self.partial_map.insert(
          PartialEv::ScopedLeft {
            left: *left,
            goal: *goal,
          },
          idx,
        );
        self.partial_map.insert(
          PartialEv::ScopedRight {
            right: *right,
            goal: *goal,
          },
          idx,
        );
      }
    }

    self.complete_map.insert(ev, idx);
  }

  /// Lookup a `PartialEvidence::ScopedRight` where we know the field of our right row, but not
  /// it's value. Because we keep our return value in our effect row, we can't know what it is
  /// ahead of time.
  pub(crate) fn match_right_eff_ev(
    &self,
    right: RowFields,
    goal: ScopedRow,
  ) -> Option<(RowValues, ReducIrVar, &Evidence)> {
    self
      .partial_map
      .iter()
      .find_map(|(ev, indx)| {
        if let PartialEv::ScopedRight {
          right: Row::Closed(right_ev),
          goal: goal_ev,
        } = ev
        {
          (right_ev.raw_fields() == right && *goal_ev == goal)
            .then_some((right_ev.raw_values(), *indx))
        } else {
          None
        }
      })
      .map(|(vals, indx)| {
        let (id, ev) = &self.params[indx];
        (vals, *id, ev)
      })
  }
}
impl Index<&Evidence> for EvidenceMap {
  type Output = ReducIrVar;

  fn index(&self, index: &Evidence) -> &Self::Output {
    &self.params[*self.complete_map.get(index).unwrap_or_else(|| {
      panic!(
        "Cound not find complete ev: {:?} in \n{:#?}",
        index, self.complete_map
      )
    })]
    .0
  }
}
impl Index<&PartialEv> for EvidenceMap {
  type Output = (ReducIrVar, Evidence);

  fn index(&self, index: &PartialEv) -> &Self::Output {
    &self.params[*self.partial_map.get(index).unwrap_or_else(|| {
      panic!(
        "Could not find partial ev: {:?} in\n{:#?}",
        index, self.partial_map
      )
    })]
  }
}

/// Row evidence where every row is closed.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub(crate) struct SolvedRowEv {
  pub(crate) goal: SimpleClosedRow,
  pub(crate) left: SimpleClosedRow,
  pub(crate) right: SimpleClosedRow,
}
impl From<SolvedRowEv> for Evidence {
  fn from(val: SolvedRowEv) -> Self {
    Evidence::DataRow {
      left: Row::Closed(val.left),
      right: Row::Closed(val.right),
      goal: Row::Closed(val.goal),
    }
  }
}
