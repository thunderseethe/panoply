use std::{
    collections::HashMap,
    hash::{BuildHasher, Hash},
};

use crate::{graph::Tree, span::Span};

/// A context that associated spans to values of a given type.
pub trait Spanner<T> {
    /// Gets the span associated with a given value.
    fn get_span(&self, value: &T) -> Option<Span>;

    /// When the value type has a tree structure, finds the deepest node whose span includes the
    /// given one.
    ///
    /// This function requires that the spans of a given node's children are all included in its own
    /// span. If multiple children of a given node have a span containing the given one, only the
    /// first such child will be searched.
    fn find_by_span(&self, root: T, span: Span) -> Option<T>
    where
        T: Eq + Hash + Tree,
    {
        root.find_left(|t| {
            self.get_span(t)
                .is_some_and(|sp| span.by_inclusion() <= sp.by_inclusion())
        })
    }
}

impl<T, S> Spanner<T> for HashMap<T, Span, S>
where
    T: Eq + Hash,
    S: BuildHasher,
{
    fn get_span(&self, value: &T) -> Option<Span> {
        self.get(value).copied()
    }
}
