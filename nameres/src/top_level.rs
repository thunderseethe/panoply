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
    id::EffectName,
    modules::Module,
    span::Spanned,
};
use aiahr_cst::Item;
use rustc_hash::FxHashMap;

/// Accumulates and publishes top-level names.
pub struct BaseBuilder<'a> {
    db: &'a dyn crate::Db,
    builder: ModuleNamesBuilder<'a>,
}

impl<'a> BaseBuilder<'a> {
    /// Constructs an empty set of top-level names.
    pub fn new(db: &'a dyn crate::Db) -> Self {
        Self {
            db,
            builder: ModuleNamesBuilder::new(db),
        }
    }

    /// Accumulates names from the given top-level items.
    pub fn add_slice<E>(mut self, module: Module, items: &[Item], errors: &mut E) -> Self
    where
        E: DiagnosticSink<NameResolutionError>,
    {
        for item in items.iter() {
            match item {
                Item::Effect(ref eff) => {
                    let mut effect = EffectBuilder::new(self.db);
                    let effect_name = eff
                        .name
                        .map(|name| EffectName::new(self.db.as_core_db(), name, module));
                    for op in eff.ops.iter() {
                        if let InsertResult {
                            existing: Some(old),
                            ..
                        } = effect.insert_op(effect_name.value, op.name)
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
                    } = self.builder.insert_effect(effect_name, effect.build())
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
                    } = self.builder.insert_term(module, term.name)
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
    pub fn build<'b>(
        self,
        me: Module,
        db: &'b dyn crate::Db,
        module_names: &'b mut FxHashMap<Module, ModuleNames>,
    ) -> BaseNames<'b> {
        module_names.insert(me, self.builder.build());
        BaseNames::new(me, db, module_names)
    }
}
