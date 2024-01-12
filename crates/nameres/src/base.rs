use std::slice::Iter;

use ::base::{
  file::FileId,
  id::EffectName,
  ident::Ident,
  loc::Loc,
  modules::Module,
  span::{Span, SpanOf},
};
use rustc_hash::FxHashMap;

use crate::{
  effect::EffectNames,
  module::ModuleNames,
  name::{BaseName, ModuleName},
  ops::IdOps,
};

// Wraps a module ID in its canonical span.
fn canonical_span(file: FileId) -> Span {
  Span::zero(Loc::start(file))
}

/// A collection of names visible throughout a module. Also accumulates the local variable IDs of
/// the module.
pub struct BaseNames<'b> {
  me: Module,
  db: &'b dyn crate::Db,
  module_names: &'b FxHashMap<Module, ModuleNames>,
}
impl std::fmt::Debug for BaseNames<'_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("BaseNames")
      .field("me", &self.me)
      .field("module_names", &self.module_names)
      .finish_non_exhaustive()
  }
}

impl<'b> BaseNames<'b> {
  /// Creates a new `BaseNames`.
  pub fn new(
    me: Module,
    db: &'b dyn crate::Db,
    module_names: &'b FxHashMap<Module, ModuleNames>,
  ) -> Self {
    Self {
      me,
      db,
      module_names,
    }
  }

  /// The source module.
  pub fn me(&self) -> Module {
    self.me
  }

  /// Gets the effect corresponding to the given ID.
  pub fn get_effect(&self, effect: &EffectName) -> &EffectNames {
    self.module_names[&effect.module(self.db.as_core_db())].get_effect(effect)
  }

  /// Finds the correct ID associated with the given string.
  pub fn find(&self, name: Ident) -> impl '_ + Iterator<Item = SpanOf<BaseName>> {
    self.find_in(self.me, name)
  }

  /// Finds the correct ID associated with the given string in the given module.
  pub fn find_in(
    &self,
    module: Module,
    name: Ident,
  ) -> impl '_ + Iterator<Item = SpanOf<BaseName>> {
    self.module_names[&module]
      .find(name)
      .map(move |sn| sn.map(BaseName::from))
  }

  /// Iterates over all IDs defined in this module in the order that they were generated.
  pub fn iter(&self) -> Iter<'_, ModuleName> {
    self.module_names[&self.me].iter()
  }
}

impl<'b, I> IdOps<I> for BaseNames<'b>
where
  BaseName: From<I>,
{
  fn get(&self, id: I) -> SpanOf<Ident> {
    match BaseName::from(id) {
      BaseName::Module(m) => {
        canonical_span(m.uri(self.db.as_core_db())).of(m.name(self.db.as_core_db()))
      }
      BaseName::Effect(e) => self.module_names[&e.module(self.db.as_core_db())].get(e),
      BaseName::EffectOp(o) => {
        let core_db = self.db.as_core_db();
        let e = o.effect(core_db);
        let m = e.module(core_db);
        self.module_names[&m].get_effect(&e).get(o)
      }
      BaseName::Item(t) => self.module_names[&t.module(self.db.as_core_db())].get(t),
    }
  }
}
