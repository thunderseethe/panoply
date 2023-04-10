use aiahr_core::diagnostic::aiahr::{AiahrcError, AiahrcErrors};
use aiahr_core::file::SourceFileSet;
use aiahr_core::id::{EffectId, EffectOpId, ItemId, ModuleId};
use aiahr_core::ident::Ident;
use aiahr_core::modules::{all_modules, Module, ModuleTree};
use aiahr_core::span::{Span, Spanned};
use aiahr_core::Top;
use aiahr_cst::nameres as nst;
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
    effect_name,
    effect_member_name,
    effect_members,
    effect_handler_order,
    effect_handler_return_index,
    effect_handler_op_index,
    effect_vector_index,
    id_for_name,
    item_name,
    lookup_effect_by_member_names,
    lookup_effect_by_name,
    module_effects,
    nameres_module,
    nameres_module_of,
);
pub trait Db: salsa::DbWithJar<Jar> + aiahr_parser::Db {
    fn as_nameres_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<crate::Jar>>::as_jar_db(self)
    }

    fn nameres_module_of(&self, mod_id: ModuleId) -> NameResModule {
        nameres_module_of(self.as_nameres_db(), self.top(), mod_id)
    }

    fn item_name(&self, mod_id: ModuleId, item_id: ItemId) -> Ident {
        item_name(self.as_nameres_db(), self.top(), mod_id, item_id)
    }

    fn id_for_name(&self, mod_id: ModuleId, name: Ident) -> Option<ItemId> {
        id_for_name(self.as_nameres_db(), self.top(), mod_id, name)
    }

    fn nameres_errors(&self) -> Vec<AiahrcError> {
        let file_set = SourceFileSet::get(self.as_core_db());
        file_set
            .files(self.as_core_db())
            .into_iter()
            .flat_map(|file| {
                nameres_module_of::accumulated::<AiahrcErrors>(
                    self.as_nameres_db(),
                    self.top(),
                    file.module(self.as_core_db()),
                )
                .into_iter()
            })
            .collect()
    }
}
impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> + aiahr_parser::Db {}

#[salsa::tracked]
pub struct SalsaItem {
    #[id]
    pub name: ModuleName,
    #[return_ref]
    pub data: nst::Item,
    #[return_ref]
    pub alloc: nst::NstIndxAlloc,
}

impl SalsaItem {
    pub fn span_of(&self, db: &dyn crate::Db) -> Span {
        match self.data(db) {
            nst::Item::Effect { effect, rbrace, .. } => Span::join(effect, rbrace),
            nst::Item::Term { value, .. } => {
                let alloc = self.alloc(db);
                alloc[*value].spanned(alloc).span()
            }
        }
    }
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

    let modules = ModuleTree::default();
    let mut errors: Vec<aiahr_core::diagnostic::nameres::NameResolutionError> = vec![];
    let mut module_names = FxHashMap::default();
    let base = BaseBuilder::new()
        .add_slice(&cst_module.items, &mut errors)
        .build(&arena, mod_id, &modules, &mut module_names);
    let mod_resolution = resolve_module(&arena, cst_module, base, &mut errors);

    let salsa_resolution = SalsaModuleResolution {
        local_ids: mod_resolution.locals,
        resolved_items: mod_resolution
            .resolved_items
            .into_iter()
            .map(|oi| {
                oi.map(|item| {
                    let name = match &item.item {
                        nst::Item::Effect { name, .. } => ModuleName::from(name.value),
                        nst::Item::Term { name, .. } => ModuleName::from(name.value),
                    };
                    SalsaItem::new(db, name, item.item, item.alloc)
                })
            })
            .collect(),
    };

    for error in errors {
        AiahrcErrors::push(db.as_core_db(), AiahrcError::from(error));
    }
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
fn nameres_module_of(db: &dyn crate::Db, _top: Top, mod_id: ModuleId) -> NameResModule {
    let parse_module = db.parse_module_of(mod_id);
    nameres_module(db, parse_module)
}

#[salsa::tracked]
fn item_name(db: &dyn crate::Db, _top: Top, mod_id: ModuleId, item_id: ItemId) -> Ident {
    let module = db.nameres_module_of(mod_id);
    module.names(db)[&mod_id].get(item_id).value
}

#[salsa::tracked]
fn id_for_name(db: &dyn crate::Db, _top: Top, mod_id: ModuleId, name: Ident) -> Option<ItemId> {
    let module = db.nameres_module_of(mod_id);
    module.names(db)[&mod_id]
        .find(name)
        .find_map(|module_name| match module_name.value {
            ModuleName::Effect(_) => None,
            ModuleName::Item(item_id) => Some(item_id),
        })
}

#[salsa::tracked]
pub fn effect_name(
    db: &dyn crate::Db,
    _top: aiahr_core::Top,
    module_id: ModuleId,
    effect_id: EffectId,
) -> Ident {
    let name_res = db.nameres_module_of(module_id);

    name_res.names(db)[&module_id].get(effect_id).value
}

#[salsa::tracked]
pub fn effect_member_name(
    db: &dyn crate::Db,
    _top: aiahr_core::Top,
    module_id: ModuleId,
    eff_id: EffectId,
    op_id: EffectOpId,
) -> Ident {
    let name_res = db.nameres_module_of(module_id);

    name_res.names(db)[&module_id]
        .get_effect(eff_id)
        .ops
        .get(op_id)
        .value
}

#[salsa::tracked(return_ref)]
pub fn effect_members(
    db: &dyn crate::Db,
    _top: aiahr_core::Top,
    module_id: ModuleId,
    effect_id: EffectId,
) -> Vec<EffectOpId> {
    let name_res = db.nameres_module_of(module_id);

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
    _top: aiahr_core::Top,
    module_id: ModuleId,
    mut find_by: impl FnMut(&(aiahr_core::span::SpanOf<Ident>, effect::EffectNames)) -> bool,
) -> Option<(ModuleId, EffectId)> {
    let name_res = db.nameres_module_of(module_id);

    let names = name_res.names(db);
    names
        .iter()
        .flat_map(|(mod_id, names)| {
            names
                .effects
                .iter_enumerate()
                .map(|(eff_id, names)| ((*mod_id, eff_id), names))
        })
        .find(|(_, names)| find_by(names))
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
    members
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
    let nameres_module = db.nameres_module_of(module.name(db.as_core_db()));
    nameres_module
        .items(db)
        .resolved_items
        .iter()
        .filter_map(|item| item.as_ref())
        .filter_map(|item| match item.data(db) {
            nst::Item::Term { .. } => None,
            nst::Item::Effect { name, .. } => Some(name.value),
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
            module_effects(db, *module)
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
