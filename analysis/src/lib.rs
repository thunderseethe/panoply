use aiahr_core::id::{EffectId, EffectOpId, ItemId, ModuleId};
use aiahr_core::ident::Ident;
use aiahr_core::modules::{module_of, Module};
use rustc_hash::FxHashMap;

use crate::ops::IdOps;

use self::module::ModuleNames;
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
pub struct Jar(
    NameResModule,
    SalsaItem,
    nameres_module_of,
    effect_name,
    effect_member_name,
    effect_members,
    lookup_effect_by_member_names,
    lookup_effect_by_name,
);
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
    // This is a map of all in scope names, from BaseNames
    #[return_ref]
    pub names: FxHashMap<ModuleId, ModuleNames>,
    #[return_ref]
    pub items: SalsaModuleResolution,
}

#[salsa::tracked]
pub fn nameres_module_of(_db: &dyn crate::Db, _module: Module) -> NameResModule {
    // TODO
    // 1. Get the parsed module for `module`
    // 2. Perform name res on that module
    // 3. Return the name resolved module
    todo!()
}

#[salsa::tracked]
pub fn effect_name(
    db: &dyn crate::Db,
    top: aiahr_core::Top,
    module_id: ModuleId,
    effect_id: EffectId,
) -> Ident {
    let module = module_of(db.as_core_db(), top, module_id);
    let name_res = nameres_module_of(db, module);

    name_res.names(db)[&module_id].get(effect_id).value
}

#[salsa::tracked]
pub fn effect_member_name(
    db: &dyn crate::Db,
    top: aiahr_core::Top,
    module_id: ModuleId,
    eff_id: EffectId,
    op_id: EffectOpId,
) -> Ident {
    let module = module_of(db.as_core_db(), top, module_id);
    let name_res = nameres_module_of(db, module);

    name_res.names(db)[&module_id]
        .get_effect(eff_id)
        .ops
        .get(op_id)
        .value
}

#[salsa::tracked]
pub fn effect_members(
    db: &dyn crate::Db,
    top: aiahr_core::Top,
    module_id: ModuleId,
    effect_id: EffectId,
) -> Vec<EffectOpId> {
    let module = module_of(db.as_core_db(), top, module_id);
    let name_res = nameres_module_of(db, module);

    name_res.names(db)[&module_id]
        .get_effect(effect_id)
        .iter()
        .collect()
}

#[salsa::tracked]
pub fn lookup_effect_by_member_names(
    db: &dyn crate::Db,
    top: aiahr_core::Top,
    module_id: ModuleId,
    members: Box<[Ident]>,
) -> Option<(ModuleId, EffectId)> {
    let mut members: Vec<Ident> = members.as_ref().to_vec();
    // TODO: Consider making it invariant that members must be sorted to avoid this allocation.
    members.sort();

    lookup_effect_by(db, top, module_id, |(_, eff_names)|
            // if length's don't match up we don't need to iterate
            members.len() == eff_names.ops.len()
                && eff_names
                    .ops
                    .iter_enumerate()
                    .map(|(_, name)| name.value)
                    .all(|id| members.binary_search(&id).is_ok()))
}

#[salsa::tracked]
pub fn lookup_effect_by_name(
    db: &dyn crate::Db,
    top: aiahr_core::Top,
    module_id: ModuleId,
    effect_name: Ident,
) -> Option<(ModuleId, EffectId)> {
    lookup_effect_by(db, top, module_id, |(name, _)| name.value == effect_name)
}

fn lookup_effect_by(
    db: &dyn crate::Db,
    top: aiahr_core::Top,
    module_id: ModuleId,
    mut find_by: impl FnMut(&(aiahr_core::span::SpanOf<Ident>, effect::EffectNames)) -> bool,
) -> Option<(ModuleId, EffectId)> {
    let module = module_of(db.as_core_db(), top, module_id);
    let name_res = nameres_module_of(db, module);

    let names = name_res.names(db);
    names
        .iter()
        .flat_map(|(mod_id, names)| {
            names
                .effects
                .iter_enumerate()
                .map(|(eff_id, names)| ((*mod_id, eff_id), names))
        })
        .find(|(_, names)| find_by(*names))
        .map(|(ids, _)| ids)
}
