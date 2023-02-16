use std::slice::Iter;

use aiahr_core::{
    id::{IdGen, Ids, ItemId},
    memory::handle::RefHandle,
    span::{SpanOf, Spanned},
};
use rustc_hash::FxHashMap;

use crate::{
    name::ModuleName,
    ops::{GensOps, IdOps, InsertResult, MatchesOps},
};

#[derive(Debug, Default)]
struct Gens<'s> {
    items: IdGen<ItemId, SpanOf<RefHandle<'s, str>>>,
}

impl<'s> IdOps<'s, ItemId> for Gens<'s> {
    fn get(&self, id: ItemId) -> SpanOf<RefHandle<'s, str>> {
        self.items[id]
    }
}

impl<'s> GensOps<'s, ItemId> for Gens<'s> {
    fn push(&mut self, name: SpanOf<RefHandle<'s, str>>) -> ItemId {
        self.items.push(name)
    }
}

#[derive(Clone, Copy, Debug, Default)]
struct Matches {
    item: Option<ItemId>,
}

impl Matches {
    fn iter<'a>(&'a self) -> impl 'a + Iterator<Item = ModuleName> {
        self.item.into_iter().map(ModuleName::Item)
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
pub struct ModuleNamesBuilder<'s> {
    gens: Gens<'s>,
    names: FxHashMap<RefHandle<'s, str>, Matches>,
    id_order: Vec<ModuleName>,
}

impl<'s> ModuleNamesBuilder<'s> {
    fn insert<I>(&mut self, name: SpanOf<RefHandle<'s, str>>) -> InsertResult<I>
    where
        I: Copy,
        ModuleName: From<I>,
        Gens<'s>: GensOps<'s, I>,
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

    /// Inserts an item into the top-level scope.
    pub fn insert_item(&mut self, name: SpanOf<RefHandle<'s, str>>) -> InsertResult<ItemId> {
        self.insert(name)
    }

    /// Finalizes the names.
    pub fn build(self) -> ModuleNames<'s> {
        ModuleNames {
            items: self.gens.items.into_boxed_ids(),
            names: self.names,
            id_order: self.id_order.into_boxed_slice(),
        }
    }
}

/// A leaf module in the module tree. Holds top-level names.
#[derive(Debug)]
pub struct ModuleNames<'s> {
    items: Box<Ids<ItemId, SpanOf<RefHandle<'s, str>>>>,
    names: FxHashMap<RefHandle<'s, str>, Matches>,
    id_order: Box<[ModuleName]>,
}

impl<'s> ModuleNames<'s> {
    /// Finds the correct ID associated with the given string.
    pub fn find<'b>(
        &'b self,
        name: RefHandle<'s, str>,
    ) -> impl 'b + Iterator<Item = SpanOf<ModuleName>> {
        self.names
            .get(&name)
            .into_iter()
            .flat_map(|ms| ms.iter())
            .map(|n| self.get(n).span().of(n))
    }

    /// Iterates over all IDs in the order that they were generated.
    pub fn iter<'a>(&'a self) -> Iter<'a, ModuleName> {
        self.id_order.iter()
    }
}

impl<'s, I> IdOps<'s, I> for ModuleNames<'s>
where
    ModuleName: From<I>,
{
    fn get(&self, id: I) -> SpanOf<RefHandle<'s, str>> {
        match ModuleName::from(id) {
            ModuleName::Item(i) => self.items[i],
        }
    }
}
