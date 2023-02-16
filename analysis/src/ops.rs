use aiahr_core::{
    diagnostic::{
        nameres::{NameKind, NameResolutionError},
        DiagnosticSink,
    },
    memory::handle::RefHandle,
    span::{SpanOf, Spanned},
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

    /// Emits a duplicate definition error if necessary, then returns the contained ID.
    pub fn emit_and_unwrap<'s, E>(
        self,
        name: SpanOf<RefHandle<'s, str>>,
        kind: NameKind,
        errors: &mut E,
    ) -> SpanOf<I>
    where
        E: DiagnosticSink<NameResolutionError<'s>>,
    {
        if let Some(existing) = self.existing {
            errors.add(NameResolutionError::Duplicate {
                name: name.value,
                kind,
                original: existing.span(),
                duplicate: name.span(),
            });
        }
        name.span().of(self.id)
    }
}

/// Trait for name layers to lookup IDs.
pub trait IdOps<'s, I> {
    /// Gets the string associated with a given ID.
    fn get(&self, id: I) -> SpanOf<RefHandle<'s, str>>;
}

/// Trait for bundles of generators to get and push by ID kind.
pub(crate) trait GensOps<'s, I>: IdOps<'s, I> {
    /// Generates an ID for a new string.
    fn push(&mut self, name: SpanOf<RefHandle<'s, str>>) -> I;
}

/// Trait for match structs to construct and offer mutable access by ID kind.
pub(crate) trait MatchesOps<I> {
    /// Constructs a new match set consisting solely of the given ID.
    fn new(id: I) -> Self;

    /// Returns a mutable accessor to the match field for this ID type.
    fn get_mut(&mut self) -> &mut Option<I>;
}
