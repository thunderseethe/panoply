use std::iter::FusedIterator;

use aiahr_core::{
    id::{EffectOpId, IdGen, Ids},
    memory::handle::RefHandle,
    span::{SpanOf, Spanned},
};
use rustc_hash::FxHashMap;

use crate::ops::{IdOps, InsertResult};

/// Accumulates operations of an effect definition.
#[derive(Debug, Default)]
pub struct EffectBuilder<'s> {
    ops: IdGen<EffectOpId, SpanOf<RefHandle<'s, str>>>,
    names: FxHashMap<RefHandle<'s, str>, EffectOpId>,
}

impl<'s> EffectBuilder<'s> {
    /// Adds an operation to the effect.
    pub fn insert_op(&mut self, name: SpanOf<RefHandle<'s, str>>) -> InsertResult<EffectOpId> {
        let id = self.ops.push(name);
        if let Some(old) = self.names.get(&name.value) {
            InsertResult::err(id, self.ops[*old].span().of(*old))
        } else {
            self.names.insert(name.value, id);
            InsertResult::ok(id)
        }
    }

    /// Finalizes the definition.
    pub fn build(self) -> EffectNames<'s> {
        EffectNames {
            ops: self.ops.into_boxed_ids(),
            names: self.names,
        }
    }
}

/// The operation names of an effect.
#[derive(Debug)]
pub struct EffectNames<'s> {
    ops: Box<Ids<EffectOpId, SpanOf<RefHandle<'s, str>>>>,
    names: FxHashMap<RefHandle<'s, str>, EffectOpId>,
}

impl<'s> EffectNames<'s> {
    /// An iterator over all operations matching the given name.
    pub fn find<'a>(
        &'a self,
        name: RefHandle<'s, str>,
    ) -> impl 'a + Iterator<Item = SpanOf<EffectOpId>> {
        self.names
            .get(&name)
            .map(|n| self.get(*n).span().of(*n))
            .into_iter()
    }

    /// All effect operations, in definition order.
    pub fn iter<'a>(
        &'a self,
    ) -> impl 'a + Iterator<Item = EffectOpId> + DoubleEndedIterator + ExactSizeIterator + FusedIterator
    {
        self.ops.iter_enumerate().map(|(id, _)| id)
    }
}

impl<'s> IdOps<'s, EffectOpId> for EffectNames<'s> {
    fn get(&self, id: EffectOpId) -> SpanOf<RefHandle<'s, str>> {
        self.ops[id]
    }
}
