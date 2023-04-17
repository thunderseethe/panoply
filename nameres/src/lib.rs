use aiahr_core::diagnostic::aiahr::{AiahrcError, AiahrcErrors};
use aiahr_core::file::{FileId, SourceFile};
use aiahr_core::id::{EffectName, EffectOpName, TermName};
use aiahr_core::ident::Ident;
use aiahr_core::modules::Module;
use aiahr_core::span::{Span, Spanned};
use aiahr_cst::nameres::{self as nst, LocalIds};
use aiahr_parser::ParseFile;
use rustc_hash::FxHashMap;

use crate::name::ModuleName;
use crate::resolve::{resolve_module, ModuleResolution};
use crate::top_level::BaseBuilder;

use self::module::ModuleNames;

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
    NameResTerm,
    NameResEffect,
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

    fn nameres_module_of(&self, module: Module) -> NameResModule {
        nameres_module_of(self.as_nameres_db(), module)
    }

    fn nameres_module_for_file(&self, file: SourceFile) -> NameResModule {
        let parse_file = self.parse_module(file);
        nameres_module(self.as_nameres_db(), parse_file)
    }

    fn nameres_module_for_file_id(&self, file_id: FileId) -> NameResModule {
        let file = aiahr_core::file::file_for_id(self.as_core_db(), file_id);
        self.nameres_module_for_file(file)
    }

    fn item_name(&self, term: TermName) -> Ident {
        item_name(self.as_nameres_db(), term)
    }

    fn id_for_name(&self, module: Module, name: Ident) -> Option<TermName> {
        id_for_name(self.as_nameres_db(), module, name)
    }

    fn nameres_errors(&self) -> Vec<AiahrcError> {
        self.all_modules()
            .iter()
            .flat_map(|module| {
                nameres_module_of::accumulated::<AiahrcErrors>(self.as_nameres_db(), *module)
                    .into_iter()
            })
            .collect()
    }
}
impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> + aiahr_parser::Db {}

#[salsa::tracked]
pub struct NameResTerm {
    #[id]
    pub name: TermName,
    #[return_ref]
    pub data: nst::TermDefn,
    #[return_ref]
    pub alloc: nst::NstIndxAlloc,
    #[return_ref]
    pub locals: LocalIds,
}

impl NameResTerm {
    pub fn span_of(&self, db: &dyn crate::Db) -> Span {
        let alloc = self.alloc(db);
        alloc[self.data(db).value].spanned(alloc).span()
    }
}

#[salsa::tracked]
pub struct NameResEffect {
    #[id]
    pub name: EffectName,
    #[return_ref]
    pub data: nst::EffectDefn,
    #[return_ref]
    pub alloc: nst::NstIndxAlloc,
    #[return_ref]
    pub locals: LocalIds,
}

#[salsa::tracked]
pub struct NameResModule {
    #[id]
    pub module: Module,
    // This is a map of all in scope names, from BaseNames
    #[return_ref]
    pub names: FxHashMap<Module, ModuleNames>,
    #[return_ref]
    pub terms: Vec<Option<NameResTerm>>,
    #[return_ref]
    pub effects: Vec<Option<NameResEffect>>,
}

#[salsa::tracked]
pub fn nameres_module(db: &dyn crate::Db, parse_module: ParseFile) -> NameResModule {
    let arena = bumpalo::Bump::new();

    let cst_module = parse_module.data(db.as_parser_db());
    let module = parse_module.module(db.as_parser_db());

    let mut errors: Vec<aiahr_core::diagnostic::nameres::NameResolutionError> = vec![];
    let mut module_names = FxHashMap::default();
    let base = BaseBuilder::new(db)
        .add_slice(module, &cst_module.items, &mut errors)
        .build(module, db, &mut module_names);
    let ModuleResolution { terms, effects } = resolve_module(&arena, cst_module, base, &mut errors);

    for error in errors {
        AiahrcErrors::push(db.as_core_db(), AiahrcError::from(error));
    }
    NameResModule::new(
        db,
        parse_module.module(db.as_parser_db()),
        module_names
            .into_iter()
            .map(|(key, value)| (key, value))
            .collect(),
        terms
            .into_iter()
            .map(|ot| {
                ot.map(|term| {
                    NameResTerm::new(
                        db,
                        term.item.name.value,
                        term.item,
                        term.alloc,
                        term.local_ids,
                    )
                })
            })
            .collect(),
        effects
            .into_iter()
            .map(|oe| {
                oe.map(|effect| {
                    NameResEffect::new(
                        db,
                        effect.item.name.value,
                        effect.item,
                        effect.alloc,
                        effect.local_ids,
                    )
                })
            })
            .collect(),
    )
}

#[salsa::tracked]
fn nameres_module_of(db: &dyn crate::Db, module: Module) -> NameResModule {
    let parse_module = db.parse_module_of(module);
    nameres_module(db, parse_module)
}

#[salsa::tracked]
fn item_name(db: &dyn crate::Db, term: TermName) -> Ident {
    term.name(db.as_core_db())
}

#[salsa::tracked]
fn id_for_name(db: &dyn crate::Db, module: Module, name: Ident) -> Option<TermName> {
    let nr_module = db.nameres_module_of(module);
    nr_module.names(db)[&module]
        .find(name)
        .find_map(|module_name| match module_name.value {
            ModuleName::Effect(_) => None,
            ModuleName::Item(item_id) => Some(item_id),
        })
}

#[salsa::tracked]
pub fn effect_name(db: &dyn crate::Db, effect: EffectName) -> Ident {
    effect.name(db.as_core_db())
}

#[salsa::tracked]
pub fn effect_member_name(db: &dyn crate::Db, effect_op: EffectOpName) -> Ident {
    effect_op.name(db.as_core_db())
}

#[salsa::tracked(return_ref)]
pub fn effect_members(db: &dyn crate::Db, effect: EffectName) -> Vec<EffectOpName> {
    let module = effect.module(db.as_core_db());
    let name_res = db.nameres_module_of(module);

    name_res.names(db)[&module]
        .get_effect(&effect)
        .iter()
        .collect()
}

#[salsa::tracked]
pub fn lookup_effect_by_member_names(
    db: &dyn crate::Db,
    module: Module,
    members: Box<[Ident]>,
) -> Option<EffectName> {
    let mut members: Vec<Ident> = members.as_ref().to_vec();
    // TODO: Consider making it invariant that members must be sorted to avoid this allocation.
    members.sort();

    lookup_effect_by(db, module, |(_, eff_names)|
            // if length's don't match up we don't need to iterate
            members.len() == eff_names.ops.len()
                && eff_names
                    .ops.values()
                    .all(|name| members.binary_search(&name.value).is_ok()))
}

#[salsa::tracked]
pub fn lookup_effect_by_name(
    db: &dyn crate::Db,
    module: Module,
    effect_name: Ident,
) -> Option<EffectName> {
    lookup_effect_by(db, module, |(name, _)| name.value == effect_name)
}

fn lookup_effect_by(
    db: &dyn crate::Db,
    module: Module,
    mut find_by: impl FnMut(&(aiahr_core::span::SpanOf<Ident>, effect::EffectNames)) -> bool,
) -> Option<EffectName> {
    let name_res = db.nameres_module_of(module);

    let names = name_res.names(db);
    names
        .iter()
        .flat_map(|(_, names)| {
            names
                .effects
                .iter()
                .map(|(eff_name, names)| (*eff_name, names))
        })
        .find(|(_, names)| find_by(names))
        .map(|(ids, _)| ids)
}

#[salsa::tracked(return_ref)]
fn effect_handler_order(db: &dyn crate::Db, eff_name: EffectName) -> Vec<Ident> {
    let mut members = effect_members(db, eff_name)
        .iter()
        .map(|eff_op| effect_member_name(db, *eff_op))
        .collect::<Vec<_>>();

    // Insert `return` so it get's ordered as well.
    members.push(db.ident_str("return"));
    members.sort();
    members
}

#[salsa::tracked]
pub fn effect_handler_return_index(db: &dyn crate::Db, eff: EffectName) -> usize {
    let return_id = db.ident_str("return");
    effect_handler_order(db, eff)
        .binary_search(&return_id)
        .unwrap_or_else(|_| {
            panic!(
                "ICE: Created handler order for effect {:?} that did not contain `return`",
                eff.name(db.as_core_db()),
            )
        })
}

#[salsa::tracked]
pub fn effect_handler_op_index(db: &dyn crate::Db, eff_op: EffectOpName) -> usize {
    let member_id = effect_member_name(db, eff_op);
    let eff = eff_op.effect(db.as_core_db());
    effect_handler_order(db, eff)
        .binary_search(&member_id)
        .unwrap_or_else(|_| {
            panic!(
                "ICE: Created handler order for effect {:?} that did not contain member {:?}",
                eff.name(db.as_core_db()).text(db.as_core_db()),
                eff_op.name(db.as_core_db()).text(db.as_core_db())
            )
        })
}

#[salsa::tracked(return_ref)]
pub fn module_effects(db: &dyn crate::Db, module: Module) -> Vec<EffectName> {
    let nameres_module = db.nameres_module_of(module);
    nameres_module
        .effects(db)
        .iter()
        .filter_map(|item| item.as_ref())
        .map(|effect| effect.name(db))
        .collect()
}

#[salsa::tracked(return_ref)]
pub fn all_effects(db: &dyn crate::Db) -> Vec<EffectName> {
    let module_tree = db.all_modules();
    let mut effects = module_tree
        .iter()
        .flat_map(|module| module_effects(db, *module).iter().copied())
        .collect::<Vec<_>>();

    effects.sort();
    effects
}

#[salsa::tracked]
pub fn effect_vector_index(db: &dyn crate::Db, effect: EffectName) -> usize {
    let effects = all_effects(db);
    effects.binary_search(&effect).unwrap_or_else(|_| {
        panic!(
            "ICE: {:?} expected effect to exist but it was not found",
            effect.name(db.as_core_db()).text(db.as_core_db())
        )
    })
}
