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
pub struct BaseBuilder<'a> {
    db: &'a dyn crate::Db,
    builder: ModuleNamesBuilder,
}

impl<'base> BaseBuilder<'base> {
    /// Constructs an empty set of top-level names.
    pub fn new(db: &'base dyn crate::Db) -> BaseBuilder {
        BaseBuilder {
            db,
            builder: Default::default(),
        }
    }

    /// Accumulates names from the given top-level items.
    pub fn add_slice<'s, E>(mut self, items: &[Item<'_, 's>], errors: &mut E) -> Self
    where
        E: DiagnosticSink<NameResolutionError>,
    {
        for item in items.iter() {
            match item {
                Item::Effect { name, ops, .. } => {
                    let mut effect = EffectBuilder::default();
                    for op in ops.iter() {
                        let name = op.name.map(|v| self.db.ident(v.to_string()));
                        if let InsertResult {
                            existing: Some(old),
                            ..
                        } = effect.insert_op(name)
                        {
                            errors.add(NameResolutionError::Duplicate {
                                name: name.value,
                                kind: NameKind::EffectOp,
                                original: old.span(),
                                duplicate: op.name.span(),
                            })
                        }
                    }

                    let name = name.map(|v| self.db.ident(v.to_string()));
                    if let InsertResult {
                        existing: Some(old),
                        ..
                    } = self.builder.insert_effect(name, effect.build())
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
                    let name = name.map(|v| self.db.ident(v.to_string()));
                    if let InsertResult {
                        existing: Some(old),
                        ..
                    } = self.builder.insert_item(name)
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
        modules: &'b ModuleTree,
        module_names: &'b mut FxHashMap<ModuleId, &'a ModuleNames>,
    ) -> BaseNames<'b, 'a> {
        module_names.insert(me, arena.alloc(self.builder.build()));
        BaseNames::new(me, modules, module_names)
    }
}
