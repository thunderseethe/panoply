use std::{collections::hash_map::Entry, iter::FusedIterator};

use base::{
    id::{EffectName, EffectOpName},
    ident::Ident,
    span::{SpanOf, Spanned},
};
use rustc_hash::FxHashMap;

use crate::ops::{IdOps, InsertResult};

/// Accumulates operations of an effect definition.
pub struct EffectBuilder<'a> {
    db: &'a dyn crate::Db,
    ops: FxHashMap<EffectOpName, SpanOf<Ident>>,
    names: FxHashMap<Ident, EffectOpName>,
}

impl<'a> EffectBuilder<'a> {
    pub fn new(db: &'a dyn crate::Db) -> Self {
        Self {
            db,
            ops: FxHashMap::default(),
            names: FxHashMap::default(),
        }
    }

    /// Adds an operation to the effect.
    pub fn insert_op(
        &mut self,
        effect: EffectName,
        name: SpanOf<Ident>,
    ) -> InsertResult<EffectOpName> {
        let span_id = name.map(|ident| EffectOpName::new(self.db.as_core_db(), ident, effect));
        let id = span_id.value;
        self.ops.insert(id, name);
        match self.names.entry(name.value) {
            Entry::Occupied(occ) => {
                InsertResult::err(id, self.ops[occ.get()].span().of(*occ.get()))
            }
            Entry::Vacant(vac) => {
                vac.insert(id);
                InsertResult::ok(id)
            }
        }
    }

    /// Finalizes the definition.
    pub fn build(self) -> EffectNames {
        EffectNames {
            ops: self.ops,
            names: self.names,
        }
    }
}

/// The operation names of an effect.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EffectNames {
    pub(crate) ops: FxHashMap<EffectOpName, SpanOf<Ident>>,
    names: FxHashMap<Ident, EffectOpName>,
}

impl EffectNames {
    /// An iterator over all operations matching the given name.
    pub fn find(&self, name: Ident) -> impl '_ + Iterator<Item = SpanOf<EffectOpName>> {
        self.names
            .get(&name)
            .map(|n| self.get(*n).span().of(*n))
            .into_iter()
    }

    /// All effect operations, in definition order.
    pub fn iter(
        &self,
    ) -> impl '_ + Iterator<Item = EffectOpName> + ExactSizeIterator + FusedIterator {
        self.ops.keys().copied()
    }
}

impl IdOps<EffectOpName> for EffectNames {
    fn get(&self, id: EffectOpName) -> SpanOf<Ident> {
        self.ops[&id]
    }
}
