use std::ops::Deref;

use aiahr_core::{
    id::{Id, IdGen, Ids},
    ident::Ident,
    span::SpanOf,
};

/// The result of an insert into a name layer.
#[derive(Clone, Copy, Debug)]
pub struct InsertResult<I> {
    /// The ID of the inserted name.
    pub id: I,

    /// If the name already existed in the layer, the ID and span associated with the existing name.
    pub existing: Option<SpanOf<I>>,
}

impl<I> InsertResult<I> {
    /// Indicates that the new name does not conflict with an existing name.
    pub fn ok(id: I) -> InsertResult<I> {
        InsertResult { id, existing: None }
    }

    /// Indicates that the new name conflicts with an existing name.
    pub fn err(id: I, existing: SpanOf<I>) -> InsertResult<I> {
        InsertResult {
            id,
            existing: Some(existing),
        }
    }
}

/// Trait for name layers to lookup IDs.
pub trait IdOps<I> {
    /// Gets the string associated with a given ID.
    fn get(&self, id: I) -> SpanOf<Ident>;
}

impl<I: Id> IdOps<I> for Ids<I, SpanOf<Ident>> {
    fn get(&self, id: I) -> SpanOf<Ident> {
        self[id]
    }
}

impl<I: Id, T> IdOps<I> for T
where
    T: Deref<Target = Ids<I, SpanOf<Ident>>>,
{
    fn get(&self, id: I) -> SpanOf<Ident> {
        self[id]
    }
}

/// Trait for bundles of generators to get and push by ID kind.
pub(crate) trait GensOps<I>: IdOps<I> {
    /// Generates an ID for a new string.
    fn push(&mut self, name: SpanOf<Ident>) -> I;
}

impl<I: Id> GensOps<I> for IdGen<I, SpanOf<Ident>> {
    fn push(&mut self, name: SpanOf<Ident>) -> I {
        self.push(name)
    }
}

/// Trait for match structs to construct and offer mutable access by ID kind.
pub(crate) trait MatchesOps<I> {
    /// Constructs a new match set consisting solely of the given ID.
    fn new(id: I) -> Self;

    /// Returns a mutable accessor to the match field for this ID type.
    fn get_mut(&mut self) -> &mut Option<I>;
}
