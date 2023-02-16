use std::slice::Iter;

use aiahr_core::{
    id::ModuleId,
    loc::Loc,
    memory::handle::RefHandle,
    modules::ModuleTree,
    span::{Span, SpanOf},
};
use rustc_hash::FxHashMap;

use crate::{
    module::ModuleNames,
    name::{BaseName, ModuleName},
    ops::IdOps,
};

// Wraps a module ID in its canonical span.
fn canonical_span(m: ModuleId) -> Span {
    Span::zero(Loc::start(m))
}

/// A collection of names visible throughout a module. Also accumulates the local variable IDs of
/// the module.
#[derive(Debug)]
pub struct BaseNames<'b, 'a, 's> {
    me: ModuleId,
    modules: &'b ModuleTree<'s>,
    module_names: &'b FxHashMap<ModuleId, &'a ModuleNames<'s>>,
}

impl<'b, 'a, 's> BaseNames<'b, 'a, 's> {
    /// Creates a new `BaseNames`.
    pub fn new(
        me: ModuleId,
        modules: &'b ModuleTree<'s>,
        module_names: &'b FxHashMap<ModuleId, &'a ModuleNames<'s>>,
    ) -> BaseNames<'b, 'a, 's> {
        BaseNames {
            me,
            modules,
            module_names,
        }
    }

    /// Finds the correct ID associated with the given string.
    pub fn find<'c>(
        &'c self,
        name: RefHandle<'s, str>,
    ) -> impl 'c + Iterator<Item = SpanOf<BaseName>> {
        self.find_in(self.me, name)
    }

    /// Finds the correct ID associated with the given string in the given module.
    pub fn find_in<'c>(
        &'c self,
        module: ModuleId,
        name: RefHandle<'s, str>,
    ) -> impl 'c + Iterator<Item = SpanOf<BaseName>> {
        self.module_names[&module]
            .find(name)
            .map(move |sn| sn.map(|n| n.based_in(module)))
    }

    /// Iterates over all IDs defined in this module in the order that they were generated.
    pub fn iter<'c>(&'c self) -> Iter<'a, ModuleName> {
        self.module_names[&self.me].iter()
    }
}

impl<'b, 'a, 's, I> IdOps<'s, I> for BaseNames<'b, 'a, 's>
where
    BaseName: From<I>,
{
    fn get(&self, id: I) -> SpanOf<RefHandle<'s, str>> {
        match BaseName::from(id) {
            BaseName::Module(m) => canonical_span(m).of(self.modules.get_name(m)),
            BaseName::Item(m, i) => self.module_names[&m].get(i),
        }
    }
}
