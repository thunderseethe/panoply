use aiahr_core::cst::indexed::CstRefAlloc;
use aiahr_core::id::{EffectId, EffectOpId, ModuleId};
use aiahr_core::ident::Ident;
use aiahr_core::indexed::{IndexedAllocate, ReferenceAllocate};
use aiahr_core::modules::{all_modules, module_of, Module, ModuleTree};
use aiahr_core::nst::indexed::NstIndxAlloc;
use aiahr_core::Top;
use aiahr_parser::ParseModule;
use rustc_hash::FxHashMap;

use crate::name::ModuleName;
use crate::ops::IdOps;
use crate::resolve::resolve_module;
use crate::top_level::BaseBuilder;

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
    all_effects,
    nameres_module,
    nameres_module_of,
    effect_name,
    effect_member_name,
    effect_members,
    effect_handler_order,
    effect_handler_return_index,
    effect_handler_op_index,
    effect_vector_index,
    module_effects,
    lookup_effect_by_member_names,
    lookup_effect_by_name,
);
pub trait Db: salsa::DbWithJar<Jar> + aiahr_core::Db + aiahr_parser::Db {
    fn as_analysis_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<crate::Jar>>::as_jar_db(self)
    }
}
impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> + aiahr_core::Db + aiahr_parser::Db {}

#[salsa::tracked]
pub struct SalsaItem {
    #[id]
    pub name: ModuleName,
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
pub fn nameres_module(db: &dyn crate::Db, parse_module: ParseModule) -> NameResModule {
    let arena = bumpalo::Bump::new();

    let cst_module = parse_module.data(db.as_parser_db());
    let mod_id = parse_module.module(db.as_parser_db()).name(db.as_core_db());
    let mut ref_alloc = CstRefAlloc {
        arena: &arena,
        // TODO: Take this as a ref not an owned value
        indices: cst_module.indices.clone(),
    };
    let ref_cst_module = cst_module.ref_alloc(&mut ref_alloc);

    let modules = ModuleTree::default();
    let mut errors: Vec<aiahr_core::diagnostic::nameres::NameResolutionError> = vec![];
    let mut module_names = FxHashMap::default();
    let base = BaseBuilder::new()
        .add_slice(ref_cst_module.items, &mut errors)
        .build(&arena, *mod_id, &modules, &mut module_names);
    let mod_resolution = resolve_module(&arena, ref_cst_module, base, &mut errors);

    let salsa_resolution = SalsaModuleResolution {
        local_ids: mod_resolution.locals,
        resolved_items: mod_resolution
            .resolved_items
            .iter()
            .map(|oi| {
                oi.map(|item| {
                    let mut alloc = NstIndxAlloc::default();
                    let item = item.alloc(&mut alloc);
                    let name = match item {
                        aiahr_core::nst::indexed::Item::Effect { name, .. } => {
                            ModuleName::from(name.value)
                        }
                        aiahr_core::nst::indexed::Item::Term { name, .. } => {
                            ModuleName::from(name.value)
                        }
                    };
                    SalsaItem::new(db, name, item, alloc)
                })
            })
            .collect(),
    };

    NameResModule::new(
        db,
        parse_module.module(db.as_parser_db()),
        module_names
            .into_iter()
            .map(|(key, value)| (key, value.clone()))
            .collect(),
        salsa_resolution,
    )
}

#[salsa::tracked]
pub fn nameres_module_of(_db: &dyn crate::Db, _module: Module) -> NameResModule {
    // Replace this method with one that relies on a concept of module set.
    unimplemented!()
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

#[salsa::tracked(return_ref)]
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

#[salsa::tracked(return_ref)]
fn effect_handler_order(
    db: &dyn crate::Db,
    top: Top,
    mod_id: ModuleId,
    eff_id: EffectId,
) -> Vec<Ident> {
    let mut members = effect_members(db, top, mod_id, eff_id)
        .iter()
        .map(|op_id| effect_member_name(db, top, mod_id, eff_id, *op_id))
        .collect::<Vec<_>>();

    // Insert `return` so it get's ordered as well.
    members.push(db.ident_str("return"));
    members.sort();

    return members;
}

#[salsa::tracked]
pub fn effect_handler_return_index(
    db: &dyn crate::Db,
    top: Top,
    module_id: ModuleId,
    effect_id: EffectId,
) -> usize {
    let return_id = db.ident_str("return");
    effect_handler_order(db, top, module_id, effect_id)
        .binary_search(&return_id)
        .unwrap_or_else(|_| {
            panic!(
                "ICE: Created handler order for effect {:?} that did not contain `return`",
                effect_id
            )
        })
}

#[salsa::tracked]
pub fn effect_handler_op_index(
    db: &dyn crate::Db,
    top: Top,
    module_id: ModuleId,
    effect_id: EffectId,
    op_id: EffectOpId,
) -> usize {
    let member_id = effect_member_name(db, top, module_id, effect_id, op_id);
    effect_handler_order(db, top, module_id, effect_id)
        .binary_search(&member_id)
        .unwrap_or_else(|_| {
            panic!(
                "ICE: Created handler order for effect {:?} that did not contain member {:?}",
                effect_id, op_id
            )
        })
}

#[salsa::tracked(return_ref)]
pub fn module_effects(db: &dyn crate::Db, module: Module) -> Vec<EffectId> {
    let nameres_module = nameres_module_of(db, module);
    nameres_module
        .items(db)
        .resolved_items
        .iter()
        .filter_map(|item| item.as_ref())
        .filter_map(|item| match item.data(db) {
            aiahr_core::nst::indexed::Item::Term { .. } => None,
            aiahr_core::nst::indexed::Item::Effect { name, .. } => Some(name.value),
        })
        .collect()
}

#[salsa::tracked(return_ref)]
pub fn all_effects(db: &dyn crate::Db) -> Vec<(ModuleId, EffectId)> {
    let core_db = db.as_core_db();
    let module_tree = all_modules(core_db);
    let mut effects: Vec<(ModuleId, EffectId)> = module_tree
        .modules(core_db)
        .iter_enumerate()
        .flat_map(|(mod_id, module)| {
            module_effects(db, module.data)
                .iter()
                .map(move |eff_id| (mod_id, *eff_id))
        })
        .collect();

    effects.sort();
    effects
}

#[salsa::tracked]
pub fn effect_vector_index(
    db: &dyn crate::Db,
    _top: Top,
    module_id: ModuleId,
    effect_id: EffectId,
) -> usize {
    let effects = all_effects(db);
    effects
        .binary_search(&(module_id, effect_id))
        .unwrap_or_else(|_| {
            panic!(
                "ICE: {:?} expected effect to exist but it was not found",
                (module_id, effect_id)
            )
        })
}
