use rustc_hash::FxHashMap;

use crate::{
    displayer::Displayer,
    id::{IdGen, Ids, ModuleId},
    ident::Ident,
};

#[derive(Debug)]
struct ModuleData {
    name: Ident,
    submodules: FxHashMap<Ident, ModuleId>,
}

impl ModuleData {
    fn new(name: Ident) -> ModuleData {
        ModuleData {
            name,
            submodules: FxHashMap::default(),
        }
    }
}

/// A tree of modules.
#[derive(Debug)]
pub struct ModuleTree {
    packages: FxHashMap<Ident, ModuleId>,
    modules: IdGen<ModuleId, ModuleData>,
}

impl Default for ModuleTree {
    fn default() -> Self {
        Self::new()
    }
}

impl ModuleTree {
    /// Constructs an empty module tree.
    pub fn new() -> ModuleTree {
        ModuleTree {
            packages: FxHashMap::default(),
            modules: IdGen::new(),
        }
    }

    /// Adds a new package, returning its module ID.
    pub fn add_package(&mut self, name: Ident) -> ModuleId {
        let id = self.modules.push(ModuleData::new(name));
        self.packages.insert(name, id);
        id
    }

    /// Adds a new module with the given parent, returning the new module's ID.
    pub fn add_module(&mut self, name: Ident, parent: ModuleId) -> ModuleId {
        let id = self.modules.push(ModuleData::new(name));
        self.modules[parent].submodules.insert(name, id);
        id
    }

    /// Gets the name of the given module.
    pub fn get_name(&self, module: ModuleId) -> Ident {
        self.modules[module].name
    }

    /// Finds the root module associated with the given package name.
    pub fn find_package(&self, name: Ident) -> Option<ModuleId> {
        self.packages.get(&name).copied()
    }

    /// Finds the given submodule in the module.
    pub fn find_submodule(&self, module: ModuleId, name: Ident) -> Option<ModuleId> {
        let data = &self.modules[module];
        data.submodules.get(&name).copied()
    }
}

impl Displayer<ModuleId> for ModuleTree {
    type Output<'a> = &'a str;

    fn show<'a>(&self, _value: &'a ModuleId) -> Self::Output<'a> {
        todo!("Thread a Db instance through to here so we can read out Ident instances")
    }
}

/// A tracked module.
///
/// This stores input for a module and ties together the metadata produced by each pass for a given
/// module.
#[salsa::tracked]
pub struct Module {
    #[return_ref]
    name: Ident,

    #[return_ref]
    uri: std::path::PathBuf,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SalsaModuleData {
    name: Ident,
    data: Module,
    submodules: FxHashMap<Ident, ModuleId>,
}

#[salsa::tracked]
pub struct SalsaModuleTree {
    #[return_ref]
    packages: FxHashMap<Ident, ModuleId>,
    #[return_ref]
    modules: Box<Ids<ModuleId, SalsaModuleData>>,
}

#[salsa::tracked]
pub fn all_modules(
    _db: &dyn crate::Db, /* TODO: this should take some kind of config file? or be an input */
) -> SalsaModuleTree {
    todo!()
}

#[salsa::tracked]
pub fn module_of(db: &dyn crate::Db, _unused: crate::Top, module_id: ModuleId) -> Module {
    let tree = all_modules(db);

    let (_, module_data) = tree
        .modules(db)
        .iter_enumerate()
        .find(|(id, _)| id == &module_id)
        .unwrap_or_else(|| {
            panic!(
                "Constructed ModuleId {:?} without associating it to a Module",
                module_id
            )
        });

    module_data.data
}
