use crate::{
  base::BaseNames,
  effect::EffectBuilder,
  module::{ModuleNames, ModuleNamesBuilder},
  ops::InsertResult,
};
use base::{
  diagnostic::{
    nameres::{NameKind, NameResolutionError},
    DiagnosticSink,
  },
  id::EffectName,
  modules::Module,
  span::Spanned,
};
use parser::{HasName, Item};
use rowan::ast::AstChildren;
use rustc_hash::FxHashMap;
use stack_graphs::graph::StackGraph;

/// Accumulates and publishes top-level names.
pub struct BaseBuilder<'a> {
  db: &'a dyn crate::Db,
  builder: ModuleNamesBuilder<'a>,
  graph: StackGraph
}

impl<'a> BaseBuilder<'a> {
  /// Constructs an empty set of top-level names.
  pub fn new(db: &'a dyn crate::Db) -> Self {
    Self {
      db,
      builder: ModuleNamesBuilder::new(db),
      graph: StackGraph::default(),
    }
  }

  /// Accumulates names from the given top-level items.
  pub fn add_slice<E>(mut self, module: Module, items: AstChildren<Item>, errors: &mut E) -> Self
  where
    E: DiagnosticSink<NameResolutionError>,
  {
    for item in items {
      match item {
        Item::Effect(ref eff) => {
          let mut effect = EffectBuilder::new(self.db);
          let effect_name = eff
            .name()
            .map(|name| {
                let ident = self.db.ident(name.text());
                EffectName::new(self.db.as_core_db(), ident, module)
            })
            .expect("Effect missing a name");
          for op in eff.ops().into_iter().flat_map(|ops| ops) {
            let op_name = op.name().expect("Effect operation missing name");
            if let Err((_, old, old_span)) = effect.insert_op(effect_name, op_name)
            {
              errors.add(NameResolutionError::Duplicate {
                name: effect_name.name(self.db.as_core_db()),
                kind: NameKind::EffectOp,
                original: old_span.into(),
                duplicate: op_name.span().into(),
              })
            }
          }

          /*if let InsertResult {
            existing: Some(old),
            ..
          } = self.builder.insert_effect(effect_name, effect.build())
          {
            let eff_name = eff.name();
            errors.add(NameResolutionError::Duplicate {
              name: eff_name,
              kind: NameKind::Effect,
              original: 
              duplicate: eff.name().map(|id| id.span().into()).expect("Effect missing a name"),
            })
          }*/
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
