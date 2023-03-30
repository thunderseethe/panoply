use std::slice::Iter;

use aiahr_core::{
    id::{EffectId, IdGen, Ids, ItemId},
    ident::Ident,
    span::{SpanOf, Spanned},
};
use rustc_hash::FxHashMap;

use crate::{
    effect::EffectNames,
    name::ModuleName,
    ops::{GensOps, IdOps, InsertResult, MatchesOps},
};

#[derive(Debug, Default)]
struct Gens {
    effects: IdGen<EffectId, (SpanOf<Ident>, EffectNames)>,
    items: IdGen<ItemId, SpanOf<Ident>>,
}

impl IdOps<EffectId> for Gens {
    fn get(&self, id: EffectId) -> SpanOf<Ident> {
        self.effects[id].0
    }
}

impl IdOps<ItemId> for Gens {
    fn get(&self, id: ItemId) -> SpanOf<Ident> {
        self.items[id]
    }
}

impl GensOps<ItemId> for Gens {
    fn push(&mut self, name: SpanOf<Ident>) -> ItemId {
        self.items.push(name)
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
struct Matches {
    effect: Option<EffectId>,
    item: Option<ItemId>,
}

impl Matches {
    fn iter(&self) -> impl '_ + Iterator<Item = ModuleName> {
        self.effect
            .into_iter()
            .map(ModuleName::Effect)
            .chain(self.item.into_iter().map(ModuleName::Item))
    }
}

impl MatchesOps<EffectId> for Matches {
    fn new(id: EffectId) -> Self {
        Matches {
            effect: Some(id),
            ..Default::default()
        }
    }

    fn get_mut(&mut self) -> &mut Option<EffectId> {
        &mut self.effect
    }
}

impl MatchesOps<ItemId> for Matches {
    fn new(id: ItemId) -> Self {
        Matches {
            item: Some(id),
            ..Default::default()
        }
    }

    fn get_mut(&mut self) -> &mut Option<ItemId> {
        &mut self.item
    }
}

/// An accumulator for top-level names in a module.
#[derive(Debug, Default)]
pub struct ModuleNamesBuilder {
    gens: Gens,
    names: FxHashMap<Ident, Matches>,
    id_order: Vec<ModuleName>,
}

impl ModuleNamesBuilder {
    fn insert<I>(&mut self, name: SpanOf<Ident>) -> InsertResult<I>
    where
        I: Copy,
        ModuleName: From<I>,
        Gens: GensOps<I>,
        Matches: MatchesOps<I>,
    {
        let id = self.gens.push(name);
        self.id_order.push(ModuleName::from(id));
        if let Some(ms) = self.names.get_mut(&name.value) {
            if let Some(old) = ms.get_mut() {
                InsertResult::err(id, self.gens.get(*old).span().of(*old))
            } else {
                *ms.get_mut() = Some(id);
                InsertResult::ok(id)
            }
        } else {
            self.names.insert(name.value, Matches::new(id));
            InsertResult::ok(id)
        }
    }

    /// Inserts an effect into the top-level scope.
    pub fn insert_effect(
        &mut self,
        name: SpanOf<Ident>,
        ops: EffectNames,
    ) -> InsertResult<EffectId> {
        let id = self.gens.effects.push((name, ops));
        self.id_order.push(ModuleName::Effect(id));
        if let Some(ms) = self.names.get_mut(&name.value) {
            if let Some(old) = ms.get_mut() {
                InsertResult::err(id, self.gens.effects[*old].0.span().of(*old))
            } else {
                *ms.get_mut() = Some(id);
                InsertResult::ok(id)
            }
        } else {
            self.names.insert(name.value, Matches::new(id));
            InsertResult::ok(id)
        }
    }

    /// Inserts an item into the top-level scope.
    pub fn insert_item(&mut self, name: SpanOf<Ident>) -> InsertResult<ItemId> {
        self.insert(name)
    }

    /// Finalizes the names.
    pub fn build(self) -> ModuleNames {
        ModuleNames {
            effects: self.gens.effects.into_boxed_ids(),
            items: self.gens.items.into_boxed_ids(),
            names: self.names,
            id_order: self.id_order.into_boxed_slice(),
        }
    }
}

/// A leaf module in the module tree. Holds top-level names.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleNames {
    pub(crate) effects: Box<Ids<EffectId, (SpanOf<Ident>, EffectNames)>>,
    items: Box<Ids<ItemId, SpanOf<Ident>>>,
    names: FxHashMap<Ident, Matches>,
    id_order: Box<[ModuleName]>,
}

impl ModuleNames {
    /// Gets the operations associated with an effect.
    pub fn get_effect(&self, id: EffectId) -> &EffectNames {
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
            ModuleName::Effect(e) => self.effects[e].0,
            ModuleName::Item(i) => self.items[i],
        }
    }
}
