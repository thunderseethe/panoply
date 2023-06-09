use std::ops::Index;

use aiahr_reducir::ReducIrVar;
use aiahr_ty::row::{Scoped, Simple, SimpleClosedRow};
use aiahr_ty::{row::Row, Evidence, InDb};

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
    params: Vec<ReducIrVar>,
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
            .position(|p| p == &param)
            .unwrap_or_else(|| {
                let idx = self.params.len();
                self.params.push(param);
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
}
impl Index<&Evidence> for EvidenceMap {
    type Output = ReducIrVar;

    fn index(&self, index: &Evidence) -> &Self::Output {
        &self.params[self.complete_map[index]]
    }
}
impl Index<&PartialEv> for EvidenceMap {
    type Output = ReducIrVar;

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
