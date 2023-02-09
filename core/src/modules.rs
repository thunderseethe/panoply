use rustc_hash::FxHashMap;

use crate::{
    id::{IdGen, ModuleId},
    memory::handle::RefHandle,
};

#[derive(Debug)]
struct ModuleData<'s> {
    name: RefHandle<'s, str>,
    submodules: FxHashMap<RefHandle<'s, str>, ModuleId>,
}

impl<'s> ModuleData<'s> {
    fn new(name: RefHandle<'s, str>) -> ModuleData<'s> {
        ModuleData {
            name,
            submodules: FxHashMap::default(),
        }
    }
}

/// A tree of modules.
#[derive(Debug)]
pub struct ModuleTree<'s> {
    packages: FxHashMap<RefHandle<'s, str>, ModuleId>,
    modules: IdGen<ModuleId, ModuleData<'s>>,
}

impl<'s> ModuleTree<'s> {
    /// Constructs an empty module tree.
    pub fn new() -> ModuleTree<'s> {
        ModuleTree {
            packages: FxHashMap::default(),
            modules: IdGen::new(),
        }
    }

    /// Adds a new package, returning its module ID.
    pub fn add_package(&mut self, name: RefHandle<'s, str>) -> ModuleId {
        let id = self.modules.push(ModuleData::new(name));
        self.packages.insert(name, id);
        id
    }

    /// Adds a new module with the given parent, returning the new module's ID.
    pub fn add_module(&mut self, name: RefHandle<'s, str>, parent: ModuleId) -> ModuleId {
        let id = self.modules.push(ModuleData::new(name));
        self.modules[parent].submodules.insert(name, id);
        id
    }

    /// Gets the name of the given module.
    pub fn get_name(&self, module: ModuleId) -> RefHandle<'s, str> {
        self.modules[module].name
    }

    /// Finds the root module associated with the given package name.
    pub fn find_package(&self, name: RefHandle<'s, str>) -> Option<ModuleId> {
        self.packages.get(&name).copied()
    }

    /// Finds the given submodule in the module.
    pub fn find_submodule(&self, module: ModuleId, name: RefHandle<'s, str>) -> Option<ModuleId> {
        let data = &self.modules[module];
        data.submodules.get(&name).copied()
    }
}
