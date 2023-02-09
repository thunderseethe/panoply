use crate::{
    base::BaseNames,
    module::{ModuleNames, ModuleNamesBuilder},
    ops::InsertResult,
};
use aiahr_core::{
    cst::Item,
    diagnostic::{
        nameres::{NameKind, NameResolutionError},
        DiagnosticSink,
    },
    id::ModuleId,
    modules::ModuleTree,
    span::Spanned,
};
use bumpalo::Bump;
use rustc_hash::FxHashMap;

/// Accumulates and publishes top-level names.
#[derive(Debug)]
pub struct BaseBuilder<'s> {
    builder: ModuleNamesBuilder<'s>,
}

impl<'s> BaseBuilder<'s> {
    /// Constructs an empty set of top-level names.
    pub fn new() -> BaseBuilder<'s> {
        BaseBuilder {
            builder: Default::default(),
        }
    }

    /// Accumulates names from the given top-level items.
    pub fn add_slice<E>(mut self, items: &[Item<'_, 's>], errors: &mut E) -> Self
    where
        E: DiagnosticSink<NameResolutionError<'s>>,
    {
        for item in items.iter() {
            match item {
                Item::Term { name, .. } => {
                    if let InsertResult {
                        existing: Some(old),
                        ..
                    } = self.builder.insert_item(*name)
                    {
                        errors.add(NameResolutionError::Duplicate {
                            name: name.value,
                            kind: NameKind::Item,
                            original: name.span(),
                            duplicate: old.span(),
                        })
                    }
                }
            }
        }
        self
    }

    /// Builds a [`BaseNames`] for the given module.
    pub fn build<'a, 'b>(
        self,
        arena: &'a Bump,
        me: ModuleId,
        modules: &'b ModuleTree<'s>,
        module_names: &'b mut FxHashMap<ModuleId, &'a ModuleNames<'s>>,
    ) -> BaseNames<'b, 'a, 's> {
        module_names.insert(me, arena.alloc(self.builder.build()));
        BaseNames::new(me, modules, module_names)
    }
}
