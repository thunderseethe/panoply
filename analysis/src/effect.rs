use std::iter::FusedIterator;

use aiahr_core::{
    id::{EffectOpId, IdGen, Ids},
    ident::Ident,
    span::{SpanOf, Spanned},
};
use rustc_hash::FxHashMap;

use crate::ops::{IdOps, InsertResult};

/// Accumulates operations of an effect definition.
#[derive(Debug, Default)]
pub struct EffectBuilder {
    ops: IdGen<EffectOpId, SpanOf<Ident>>,
    names: FxHashMap<Ident, EffectOpId>,
}

impl EffectBuilder {
    /// Adds an operation to the effect.
    pub fn insert_op(&mut self, name: SpanOf<Ident>) -> InsertResult<EffectOpId> {
        let id = self.ops.push(name);
        if let Some(old) = self.names.get(&name.value) {
            InsertResult::err(id, self.ops[*old].span().of(*old))
        } else {
            self.names.insert(name.value, id);
            InsertResult::ok(id)
        }
    }

    /// Finalizes the definition.
    pub fn build(self) -> EffectNames {
        EffectNames {
            ops: self.ops.into_boxed_ids(),
            names: self.names,
        }
    }
}

/// The operation names of an effect.
#[derive(Debug, PartialEq, Eq)]
pub struct EffectNames {
    pub(crate) ops: Box<Ids<EffectOpId, SpanOf<Ident>>>,
    names: FxHashMap<Ident, EffectOpId>,
}

impl EffectNames {
    /// An iterator over all operations matching the given name.
    pub fn find(&self, name: Ident) -> impl '_ + Iterator<Item = SpanOf<EffectOpId>> {
        self.names
            .get(&name)
            .map(|n| self.get(*n).span().of(*n))
            .into_iter()
    }

    /// All effect operations, in definition order.
    pub fn iter(
        &self,
    ) -> impl '_ + Iterator<Item = EffectOpId> + DoubleEndedIterator + ExactSizeIterator + FusedIterator
    {
        self.ops.iter_enumerate().map(|(id, _)| id)
    }
}

impl IdOps<EffectOpId> for EffectNames {
    fn get(&self, id: EffectOpId) -> SpanOf<Ident> {
        self.ops[id]
    }
}
