use std::{cell::RefCell, collections::HashMap};

use aiahr_core::{
    id::{ItemId, ModuleId, VarId},
    span::SpanOf,
};

use crate::modules::{Member, ModuleTree};

/// A collection of names visible throughout a module. Also accumulates the local variable IDs of
/// the module.
#[derive(Debug)]
pub struct BaseNames<'a, 'i> {
    this: ModuleId,
    modules: &'a ModuleTree<'a, 'i>,
    inames: &'a HashMap<&'i str, ItemId>,
    // TODO: see if we can get rid of RefCell here and just use static analysis
    vars: RefCell<Vec<SpanOf<&'i str>>>,
}

impl<'a, 'i> BaseNames<'a, 'i> {
    /// Creates a new `BaseNames` with no local variables.
    pub fn new(
        this: ModuleId,
        modules: &'a ModuleTree<'a, 'i>,
        inames: &'a HashMap<&'i str, ItemId>,
    ) -> BaseNames<'a, 'i> {
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
    pub fn get(&self, name: &str) -> Option<Member> {
        self.inames
            .get(name)
            .copied()
            .map(Member::Item)
            .or_else(|| self.modules.get_package(name).map(Member::Module))
    }

    /// Gets the given member in the module.
    pub fn get_in(&self, module: ModuleId, name: &str) -> Option<Member> {
        self.modules.get_in(module, name)
    }

    /// Creates an ID for a variable.
    pub fn make_id(&self, name: SpanOf<&'i str>) -> VarId {
        let mut vars = RefCell::borrow_mut(&self.vars);
        let id = vars.len();
        vars.push(name);
        VarId(id)
    }

    /// Consumes this object and returns the variable vector.
    pub fn into_vars(self) -> Vec<SpanOf<&'i str>> {
        RefCell::into_inner(self.vars)
    }
}
