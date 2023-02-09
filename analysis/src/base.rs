use std::slice::Iter;

use aiahr_core::{
    diagnostic::nameres::{RejectionReason, Suggestion},
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

    fn extend_suggestions<T, F>(
        &self,
        module: ModuleId,
        mut f: F,
        mut suggestions: Vec<Suggestion<'s>>,
    ) -> Result<T, Vec<Suggestion<'s>>>
    where
        F: FnMut(BaseName) -> Result<T, RejectionReason>,
    {
        f(BaseName::Module(module)).map_err(|why_not| {
            suggestions.push(Suggestion {
                name: canonical_span(module).of(self.modules.get_name(module)),
                why_not,
            });
            suggestions
        })
    }

    /// Finds the correct ID associated with the given string.
    pub fn find<T, F>(&self, name: RefHandle<'s, str>, mut f: F) -> Result<T, Vec<Suggestion<'s>>>
    where
        F: FnMut(BaseName) -> Result<T, RejectionReason>,
    {
        self.module_names[&self.me]
            .find(name, |n| f(n.based_in(self.me)))
            .or_else(|suggestions| {
                if let Some(m) = self.modules.find_package(name) {
                    self.extend_suggestions(m, f, suggestions)
                } else {
                    Err(suggestions)
                }
            })
    }

    /// Finds the correct ID associated with the given string in the given module.
    pub fn find_in<T, F>(
        &self,
        module: ModuleId,
        name: RefHandle<'s, str>,
        mut f: F,
    ) -> Result<T, Vec<Suggestion<'s>>>
    where
        F: FnMut(BaseName) -> Result<T, RejectionReason>,
    {
        self.module_names[&module]
            .find(name, |n| f(n.based_in(module)))
            .or_else(|suggestions| {
                if let Some(m) = self.modules.find_package(name) {
                    self.extend_suggestions(m, f, suggestions)
                } else {
                    Err(suggestions)
                }
            })
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
