use crate::{
    base::BaseNames,
    effect::EffectBuilder,
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

impl<'s> Default for BaseBuilder<'s> {
    fn default() -> Self {
        Self::new()
    }
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
                Item::Effect { name, ops, .. } => {
                    let mut effect = EffectBuilder::default();
                    for op in ops.iter() {
                        if let InsertResult {
                            existing: Some(old),
                            ..
                        } = effect.insert_op(op.name)
                        {
                            errors.add(NameResolutionError::Duplicate {
                                name: op.name.value,
                                kind: NameKind::EffectOp,
                                original: old.span(),
                                duplicate: op.name.span(),
                            })
                        }
                    }

                    if let InsertResult {
                        existing: Some(old),
                        ..
                    } = self.builder.insert_effect(*name, effect.build())
                    {
                        errors.add(NameResolutionError::Duplicate {
                            name: name.value,
                            kind: NameKind::Effect,
                            original: old.span(),
                            duplicate: name.span(),
                        })
                    }
                }
                Item::Term { name, .. } => {
                    if let InsertResult {
                        existing: Some(old),
                        ..
                    } = self.builder.insert_item(*name)
                    {
                        errors.add(NameResolutionError::Duplicate {
                            name: name.value,
                            kind: NameKind::Item,
                            original: old.span(),
                            duplicate: name.span(),
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
