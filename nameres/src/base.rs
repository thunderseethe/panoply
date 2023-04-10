use std::slice::Iter;

use aiahr_core::{
    file::FileId,
    id::{EffectId, ModuleId},
    ident::Ident,
    loc::Loc,
    modules::ModuleTree,
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
#[derive(Debug)]
pub struct BaseNames<'b, 'a> {
    me: ModuleId,
    modules: &'b ModuleTree,
    module_names: &'b FxHashMap<ModuleId, &'a ModuleNames>,
}

impl<'b, 'a> BaseNames<'b, 'a> {
    /// Creates a new `BaseNames`.
    pub fn new(
        me: ModuleId,
        modules: &'b ModuleTree,
        module_names: &'b FxHashMap<ModuleId, &'a ModuleNames>,
    ) -> Self {
        Self {
            me,
            modules,
            module_names,
        }
    }

    /// The source module.
    pub fn me(&self) -> ModuleId {
        self.me
    }

    /// Gets the effect corresponding to the given ID.
    pub fn get_effect(&self, module: ModuleId, effect: EffectId) -> &EffectNames {
        self.module_names[&module].get_effect(effect)
    }

    /// Finds the correct ID associated with the given string.
    pub fn find(&self, name: Ident) -> impl '_ + Iterator<Item = SpanOf<BaseName>> {
        self.find_in(self.me, name)
    }

    /// Finds the correct ID associated with the given string in the given module.
    pub fn find_in(
        &self,
        module: ModuleId,
        name: Ident,
    ) -> impl '_ + Iterator<Item = SpanOf<BaseName>> {
        self.module_names[&module]
            .find(name)
            .map(move |sn| sn.map(|n| n.based_in(module)))
    }

    /// Iterates over all IDs defined in this module in the order that they were generated.
    pub fn iter<'c>(&'c self) -> Iter<'a, ModuleName> {
        self.module_names[&self.me].iter()
    }
}

impl<'b, 'a, I> IdOps<I> for BaseNames<'b, 'a>
where
    BaseName: From<I>,
{
    fn get(&self, id: I) -> SpanOf<Ident> {
        match BaseName::from(id) {
            BaseName::Module(_m) => {
                canonical_span(todo!("Map ModuleId m back to FileId")).of(self.modules.get_name(_m))
            }
            BaseName::Effect(m, e) => self.module_names[&m].get(e),
            BaseName::EffectOp(m, e, o) => self.module_names[&m].get_effect(e).get(o),
            BaseName::Item(m, i) => self.module_names[&m].get(i),
        }
    }
}
