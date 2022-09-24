use std::collections::HashMap;

use aiahr_core::id::{ItemId, ModuleId};

#[derive(Debug)]
enum ModuleKind<'a, 'i> {
    #[allow(dead_code)]
    Dir(HashMap<&'i str, ModuleId>),
    Leaf(&'a HashMap<&'i str, ItemId>),
}

/// A member of a module.
#[derive(Clone, Copy, Debug)]
pub enum Member {
    Module(ModuleId),
    Item(ItemId),
}

/// A tree of modules and their members.
#[derive(Debug)]
pub struct ModuleTree<'a, 'i> {
    packages: HashMap<&'i str, ModuleId>,
    modules: HashMap<ModuleId, ModuleKind<'a, 'i>>,
}

impl<'a, 'i> ModuleTree<'a, 'i> {
    /// Returns an empty module tree.
    pub fn new() -> ModuleTree<'a, 'i> {
        ModuleTree {
            packages: HashMap::new(),
            modules: HashMap::new(),
        }
    }

    /// Adds a top-level item listing for the given module. May only be called once per module, and
    /// only for leaf modules.
    pub fn add_items(&mut self, module: ModuleId, items: &'a HashMap<&'i str, ItemId>) {
        if self.modules.contains_key(&module) {
            // TODO: we can do better than panicking
            panic!("Module {:?} has already been processed", module);
        }
        self.modules.insert(module, ModuleKind::Leaf(items));
    }

    /// Gets the root module associated with the given package name.
    pub fn get_package(&self, name: &str) -> Option<ModuleId> {
        self.packages.get(name).copied()
    }

    /// Gets the given member in the module.
    pub fn get_in(&self, module: ModuleId, name: &str) -> Option<Member> {
        self.modules.get(&module).and_then(|kind| match kind {
            ModuleKind::Dir(dir) => dir.get(name).copied().map(Member::Module),
            ModuleKind::Leaf(items) => items.get(name).copied().map(Member::Item),
        })
    }
}
