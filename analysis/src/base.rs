use std::collections::HashMap;

use aiahr_core::{
    id::{ItemId, ModuleId},
    memory::handle::RefHandle,
};

use crate::modules::{Member, ModuleTree};

/// A collection of names visible throughout a module. Also accumulates the local variable IDs of
/// the module.
#[derive(Debug)]
pub struct BaseNames<'a, 's> {
    this: ModuleId,
    modules: &'a ModuleTree<'a, 's>,
    inames: &'a HashMap<RefHandle<'s, str>, ItemId>,
}

impl<'a, 's> BaseNames<'a, 's> {
    /// Creates a new `BaseNames` with no local variables.
    pub fn new(
        this: ModuleId,
        modules: &'a ModuleTree<'a, 's>,
        inames: &'a HashMap<RefHandle<'s, str>, ItemId>,
    ) -> BaseNames<'a, 's> {
        BaseNames {
            this,
            modules,
            inames,
        }
    }

    /// Gets the ID of the owning module.
    pub fn this(&self) -> ModuleId {
        self.this.clone()
    }

    /// Gets the symbol associated with the given name.
    pub fn get(&self, name: RefHandle<'s, str>) -> Option<Member> {
        self.inames
            .get(&name)
            .copied()
            .map(Member::Item)
            .or_else(|| self.modules.get_package(name).map(Member::Module))
    }

    /// Gets the given member in the module.
    pub fn get_in(&self, module: ModuleId, name: RefHandle<'s, str>) -> Option<Member> {
        self.modules.get_in(module, name)
    }
}
