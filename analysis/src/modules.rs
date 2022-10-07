use std::collections::HashMap;

use aiahr_core::{
    id::{ItemId, ModuleId},
    memory::handle::RefHandle,
};

#[derive(Debug)]
enum ModuleKind<'a, 's> {
    #[allow(dead_code)]
    Dir(HashMap<RefHandle<'s, str>, ModuleId>),
    Leaf(&'a HashMap<RefHandle<'s, str>, ItemId>),
}

/// A member of a module.
#[derive(Clone, Copy, Debug)]
pub enum Member {
    Module(ModuleId),
    Item(ItemId),
}

/// A tree of modules and their members.
#[derive(Debug)]
pub struct ModuleTree<'a, 's> {
    packages: HashMap<RefHandle<'s, str>, ModuleId>,
    modules: HashMap<ModuleId, ModuleKind<'a, 's>>,
}

impl<'a, 's> ModuleTree<'a, 's> {
    /// Returns an empty module tree.
    pub fn new() -> ModuleTree<'a, 's> {
        ModuleTree {
            packages: HashMap::new(),
            modules: HashMap::new(),
        }
    }

    /// Adds a top-level item listing for the given module. May only be called once per module, and
    /// only for leaf modules.
    pub fn add_items(&mut self, module: ModuleId, items: &'a HashMap<RefHandle<'s, str>, ItemId>) {
        if self.modules.contains_key(&module) {
            // TODO: we can do better than panicking
            panic!("Module {:?} has already been processed", module);
        }
        self.modules.insert(module, ModuleKind::Leaf(items));
    }

    /// Gets the root module associated with the given package name.
    pub fn get_package(&self, name: RefHandle<'s, str>) -> Option<ModuleId> {
        self.packages.get(&name).copied()
    }

    /// Gets the given member in the module.
    pub fn get_in(&self, module: ModuleId, name: RefHandle<'s, str>) -> Option<Member> {
        self.modules.get(&module).and_then(|kind| match kind {
            ModuleKind::Dir(dir) => dir.get(&name).copied().map(Member::Module),
            ModuleKind::Leaf(items) => items.get(&name).copied().map(Member::Item),
        })
    }
}
