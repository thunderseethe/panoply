use std::ops::Index;

use aiahr_core::ir::IrVar;
use aiahr_tc::{ClosedRow, Evidence, InDb, Row};
use rustc_hash::FxHashMap;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub(crate) struct PartialEv {
    pub(crate) other: Row<InDb>,
    pub(crate) goal: Row<InDb>,
}
impl Copy for PartialEv where InDb: Copy {}

#[derive(Default, Debug)]
pub(crate) struct EvidenceMap<'ctx> {
    /// Unique list of parameters we've generated so far
    params: Vec<IrVar<'ctx>>,
    // Find evidence when we only have partial information about it.
    // Like when we encounter a Project or Inject node.
    partial_map: FxHashMap<PartialEv, usize>,
    complete_map: FxHashMap<Evidence<InDb>, usize>,
}
impl<'ctx> EvidenceMap<'ctx> {
    pub(crate) fn insert(&mut self, ev: Evidence<InDb>, param: IrVar<'ctx>) {
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
impl<'ctx> Index<&Evidence<InDb>> for EvidenceMap<'ctx> {
    type Output = IrVar<'ctx>;

    fn index(&self, index: &Evidence<InDb>) -> &Self::Output {
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
    pub(crate) goal: ClosedRow<InDb>,
    pub(crate) left: ClosedRow<InDb>,
    pub(crate) right: ClosedRow<InDb>,
}
impl From<SolvedRowEv> for Evidence<InDb> {
    fn from(val: SolvedRowEv) -> Self {
        Evidence::Row {
            left: Row::Closed(val.left),
            right: Row::Closed(val.right),
            goal: Row::Closed(val.goal),
        }
    }
}
impl SolvedRowEv {
    pub(crate) fn new(
        left: ClosedRow<InDb>,
        right: ClosedRow<InDb>,
        goal: ClosedRow<InDb>,
    ) -> Self {
        Self { goal, left, right }
    }
}
