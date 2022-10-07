use std::{cell::RefCell, collections::HashMap};

use aiahr_core::{
    id::{ItemId, ModuleId, VarId},
    memory::handle::RefHandle,
    span::SpanOf,
};

use crate::modules::{Member, ModuleTree};

/// A collection of names visible throughout a module. Also accumulates the local variable IDs of
/// the module.
#[derive(Debug)]
pub struct BaseNames<'a, 's> {
    this: ModuleId,
    modules: &'a ModuleTree<'a, 's>,
    inames: &'a HashMap<RefHandle<'s, str>, ItemId>,
    // TODO: see if we can get rid of RefCell here and just use static analysis
    vars: RefCell<Vec<SpanOf<RefHandle<'s, str>>>>,
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
            vars: RefCell::new(Vec::new()),
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

    /// Creates an ID for a variable.
    pub fn make_id(&self, name: SpanOf<RefHandle<'s, str>>) -> VarId {
        let mut vars = RefCell::borrow_mut(&self.vars);
        let id = vars.len();
        vars.push(name);
        VarId(id)
    }

    /// Returns the name and location of a variable.
    pub fn var(&self, id: VarId) -> Option<SpanOf<RefHandle<'s, str>>> {
        self.vars.borrow().get(id.0).copied()
    }

    /// Consumes this object and returns the variable vector.
    pub fn into_vars(self) -> Vec<SpanOf<RefHandle<'s, str>>> {
        RefCell::into_inner(self.vars)
    }
}
