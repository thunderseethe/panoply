use std::{collections::hash_map::Entry, slice::Iter};

use ::base::{
    id::{EffectName, TermName},
    ident::Ident,
    modules::Module,
    span::{SpanOf, Spanned},
};
use rustc_hash::FxHashMap;

use crate::{
    effect::EffectNames,
    name::ModuleName,
    ops::{IdOps, InsertResult, MatchesOps},
};

#[derive(Debug, Default)]
struct Gens {
    effects: FxHashMap<EffectName, (SpanOf<Ident>, EffectNames)>,
    terms: FxHashMap<TermName, SpanOf<Ident>>,
}

impl IdOps<EffectName> for Gens {
    fn get(&self, id: EffectName) -> SpanOf<Ident> {
        self.effects[&id].0
    }
}

impl IdOps<TermName> for Gens {
    fn get(&self, id: TermName) -> SpanOf<Ident> {
        self.terms[&id]
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
struct Matches {
    effect: Option<EffectName>,
    item: Option<TermName>,
}

impl Matches {
    fn iter(&self) -> impl '_ + Iterator<Item = ModuleName> {
        self.effect
            .into_iter()
            .map(ModuleName::Effect)
            .chain(self.item.into_iter().map(ModuleName::Item))
    }
}

impl MatchesOps<EffectName> for Matches {
    fn new(id: EffectName) -> Self {
        Matches {
            effect: Some(id),
            ..Default::default()
        }
    }

    fn get_mut(&mut self) -> &mut Option<EffectName> {
        &mut self.effect
    }
}

impl MatchesOps<TermName> for Matches {
    fn new(id: TermName) -> Self {
        Matches {
            item: Some(id),
            ..Default::default()
        }
    }

    fn get_mut(&mut self) -> &mut Option<TermName> {
        &mut self.item
    }
}

/// An accumulator for top-level names in a module.
pub struct ModuleNamesBuilder<'a> {
    db: &'a dyn crate::Db,
    gens: Gens,
    names: FxHashMap<Ident, Matches>,
    id_order: Vec<ModuleName>,
}

impl<'a> ModuleNamesBuilder<'a> {
    pub fn new(db: &'a dyn crate::Db) -> Self {
        Self {
            db,
            gens: Gens::default(),
            names: FxHashMap::default(),
            id_order: Vec::default(),
        }
    }

    /// Inserts an effect into the top-level scope.
    pub fn insert_effect(
        &mut self,
        span_eff_name: SpanOf<EffectName>,
        ops: EffectNames,
    ) -> InsertResult<EffectName> {
        let eff_name = span_eff_name.value;
        let span_id = span_eff_name.map(|eff_name| eff_name.name(self.db.as_core_db()));
        self.gens.effects.insert(eff_name, (span_id, ops));
        self.id_order.push(ModuleName::Effect(eff_name));
        match self.names.entry(eff_name.name(self.db.as_core_db())) {
            Entry::Occupied(mut occ) => {
                let old = occ.get_mut().get_mut().get_or_insert(eff_name);
                if *old == eff_name {
                    InsertResult::ok(eff_name)
                } else {
                    InsertResult::err(eff_name, self.gens.effects[old].0.span().of(*old))
                }
            }
            Entry::Vacant(vac) => {
                vac.insert(Matches::new(eff_name));
                InsertResult::ok(eff_name)
            }
        }
    }

    /// Inserts an item into the top-level scope.
    pub fn insert_term(&mut self, module: Module, name: SpanOf<Ident>) -> InsertResult<TermName> {
        let id = TermName::new(self.db.as_core_db(), name.value, module);
        self.gens.terms.insert(id, name);
        self.id_order.push(ModuleName::from(id));
        match self.names.entry(name.value) {
            Entry::Occupied(mut occ) => {
                let old = *occ.get_mut().get_mut().get_or_insert(id);
                if old == id {
                    InsertResult::ok(id)
                } else {
                    InsertResult::err(id, self.gens.get(old).span().of(old))
                }
            }
            Entry::Vacant(vac) => {
                vac.insert(Matches::new(id));
                InsertResult::ok(id)
            }
        }
    }

    /// Finalizes the names.
    pub fn build(self) -> ModuleNames {
        ModuleNames {
            effects: self.gens.effects,
            items: self.gens.terms,
            names: self.names,
            id_order: self.id_order.into_boxed_slice(),
        }
    }
}

/// A leaf module in the module tree. Holds top-level names.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleNames {
    pub(crate) effects: FxHashMap<EffectName, (SpanOf<Ident>, EffectNames)>,
    items: FxHashMap<TermName, SpanOf<Ident>>,
    names: FxHashMap<Ident, Matches>,
    id_order: Box<[ModuleName]>,
}

impl ModuleNames {
    /// Gets the operations associated with an effect.
    pub fn get_effect(&self, id: &EffectName) -> &EffectNames {
        &self.effects[id].1
    }

    /// Finds the correct ID associated with the given string.
    pub fn find(&self, name: Ident) -> impl '_ + Iterator<Item = SpanOf<ModuleName>> {
        self.names
            .get(&name)
            .into_iter()
            .flat_map(|ms| ms.iter())
            .map(|n| self.get(n).span().of(n))
    }

    /// Iterates over all IDs in the order that they were generated.
    pub fn iter(&self) -> Iter<'_, ModuleName> {
        self.id_order.iter()
    }
}

impl<I> IdOps<I> for ModuleNames
where
    ModuleName: From<I>,
{
    fn get(&self, id: I) -> SpanOf<Ident> {
        match ModuleName::from(id) {
            ModuleName::Effect(e) => self.effects[&e].0,
            ModuleName::Item(i) => self.items[&i],
        }
    }
}
