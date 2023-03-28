use std::ops::Index;

use aiahr_core::ir::IrVar;
use aiahr_core::ty::{
    row::{ClosedRow, Row},
    Evidence, InDb,
};

use rustc_hash::FxHashMap;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub(crate) struct PartialEv {
    pub(crate) other: Row,
    pub(crate) goal: Row,
}
impl Copy for PartialEv where InDb: Copy {}

#[derive(Default, Debug)]
pub(crate) struct EvidenceMap<'ctx> {
    /// Unique list of parameters we've generated so far
    params: Vec<IrVar<'ctx>>,
    // Find evidence when we only have partial information about it.
    // Like when we encounter a Project or Inject node.
    partial_map: FxHashMap<PartialEv, usize>,
    complete_map: FxHashMap<Evidence, usize>,
}
impl<'ctx> EvidenceMap<'ctx> {
    pub(crate) fn insert(&mut self, ev: Evidence, param: IrVar<'ctx>) {
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
            Evidence::Row { left, right, goal } => {
                self.partial_map.insert(
                    PartialEv {
                        other: *left,
                        goal: *goal,
                    },
                    idx,
                );
                self.partial_map.insert(
                    PartialEv {
                        other: *right,
                        goal: *goal,
                    },
                    idx,
                );
            }
        }

        self.complete_map.insert(ev, idx);
    }
}
impl<'ctx> Index<&Evidence> for EvidenceMap<'ctx> {
    type Output = IrVar<'ctx>;

    fn index(&self, index: &Evidence) -> &Self::Output {
        &self.params[self.complete_map[index]]
    }
}
impl<'ctx> Index<&PartialEv> for EvidenceMap<'ctx> {
    type Output = IrVar<'ctx>;

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
    pub(crate) goal: ClosedRow,
    pub(crate) left: ClosedRow,
    pub(crate) right: ClosedRow,
}
impl From<SolvedRowEv> for Evidence {
    fn from(val: SolvedRowEv) -> Self {
        Evidence::Row {
            left: Row::Closed(val.left),
            right: Row::Closed(val.right),
            goal: Row::Closed(val.goal),
        }
    }
}
impl SolvedRowEv {
    pub(crate) fn new(left: ClosedRow, right: ClosedRow, goal: ClosedRow) -> Self {
        Self { goal, left, right }
    }
}
