use aiahr_core::id::ItemId;
use aiahr_core::modules::Module;

use self::names::LocalIds;

pub mod base;
pub mod effect;
pub mod module;
pub mod name;
pub mod names;
pub mod ops;
pub mod resolve;
pub mod top_level;

#[salsa::jar(db = Db)]
pub struct Jar(NameResModule, SalsaItem);
pub trait Db: salsa::DbWithJar<Jar> + aiahr_core::Db {
    fn as_analysis_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<crate::Jar>>::as_jar_db(self)
    }
}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + aiahr_core::Db {}

#[salsa::tracked]
pub struct SalsaItem {
    #[id]
    pub name: ItemId,
    #[return_ref]
    pub data: aiahr_core::nst::indexed::Item,
    #[return_ref]
    pub alloc: aiahr_core::nst::indexed::NstIndxAlloc,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SalsaModuleResolution {
    pub local_ids: LocalIds,
    pub resolved_items: Vec<Option<SalsaItem>>,
}

#[salsa::tracked]
pub struct NameResModule {
    #[id]
    pub module: Module,
    #[return_ref]
    pub names: module::ModuleNames,
    #[return_ref]
    pub items: SalsaModuleResolution,
}
