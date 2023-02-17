use std::iter;

/// A node in a directed graph.
pub trait Graph {
    type Neighbors<'a>: 'a + Iterator<Item = Self>
    where
        Self: 'a;

    /// Returns an iterator over the node's neighbors, i.e. those nodes that `self` has edges to.
    fn neighbors<'a>(&'a self) -> Self::Neighbors<'a>;
}

/// A marker trait to indicate that a graph is a tree.
pub trait Tree: Graph {
    /// Finds the deepest node for which the predicate is true.
    ///
    /// If multiple children of a given node satisfy the predicate, the leftmost child is searched.
    fn find_left<P>(self, mut predicate: P) -> Option<Self>
    where
        Self: Sized,
        P: FnMut(&Self) -> bool,
    {
        iter::successors(predicate(&self).then_some(self), |node| {
            node.neighbors().find(&mut predicate)
        })
        .last()
    }
}
