use crate::{
    base::BaseNames,
    effect::EffectBuilder,
    module::{ModuleNames, ModuleNamesBuilder},
    ops::InsertResult,
};
use aiahr_core::{
    diagnostic::{
        nameres::{NameKind, NameResolutionError},
        DiagnosticSink,
    },
    modules::Module,
    span::Spanned,
};
use aiahr_cst::Item;
use bumpalo::Bump;
use rustc_hash::FxHashMap;

/// Accumulates and publishes top-level names.
#[derive(Default)]
pub struct BaseBuilder {
    builder: ModuleNamesBuilder,
}

impl BaseBuilder {
    /// Constructs an empty set of top-level names.
    pub fn new() -> Self {
        Self::default()
    }

    /// Accumulates names from the given top-level items.
    pub fn add_slice<E>(mut self, items: &[Item], errors: &mut E) -> Self
    where
        E: DiagnosticSink<NameResolutionError>,
    {
        for item in items.iter() {
            match item {
                Item::Effect(ref eff) => {
                    let mut effect = EffectBuilder::default();
                    for op in eff.ops.iter() {
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
                    } = self.builder.insert_effect(eff.name, effect.build())
                    {
                        errors.add(NameResolutionError::Duplicate {
                            name: eff.name.value,
                            kind: NameKind::Effect,
                            original: old.span(),
                            duplicate: eff.name.span(),
                        })
                    }
                }
                Item::Term(term) => {
                    if let InsertResult {
                        existing: Some(old),
                        ..
                    } = self.builder.insert_item(term.name)
                    {
                        errors.add(NameResolutionError::Duplicate {
                            name: term.name.value,
                            kind: NameKind::Item,
                            original: old.span(),
                            duplicate: term.name.span(),
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
        me: Module,
        db: &'b dyn crate::Db,
        module_names: &'b mut FxHashMap<Module, &'a ModuleNames>,
    ) -> BaseNames<'b, 'a> {
        module_names.insert(me, arena.alloc(self.builder.build()));
        BaseNames::new(me, db, module_names)
    }
}
