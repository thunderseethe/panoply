use rustc_hash::FxHashMap;
use salsa::DebugWithDb;

use crate::{
    displayer::Displayer,
    file::{FileId, SourceFileSet},
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
#[derive(DebugWithDb)]
pub struct Module {
    pub name: ModuleId,

    pub uri: FileId,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SalsaModuleData {
    pub data: Module,
}

#[salsa::tracked]
pub struct SalsaModuleTree {
    #[return_ref]
    pub modules: Box<Ids<ModuleId, Module>>,
}

#[salsa::tracked]
pub fn all_modules(db: &dyn crate::Db) -> SalsaModuleTree {
    let source_file_set = SourceFileSet::get(db);

    let mut files = source_file_set.files(db);
    // Ensure that our SourceFiles are in order before re-assigning them a ModuleId
    files.sort_by_key(|module| module.module(db));
    let modules = files
        .into_iter()
        .map(|file| Module::new(db, file.module(db), file.path(db)))
        .collect::<IdGen<ModuleId, Module>>();

    SalsaModuleTree::new(db, modules.into_boxed_ids())
}

#[salsa::tracked]
pub fn module_id_of(db: &dyn crate::Db, module: Module) -> ModuleId {
    let tree = all_modules(db);

    let (module_id, _) = tree
        .modules(db)
        .iter_enumerate()
        .find(|(_, mod_data)| (*mod_data) == &module)
        .unwrap_or_else(|| {
            panic!(
                "ICE: Constructed Module {:?} but did not store it in Module Tree",
                module.debug(db)
            )
        });

    module_id
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

    *module_data
}
