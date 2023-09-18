use aiahr_core::id::{Id, IdSupply};
use rustc_hash::FxHashMap;

pub(crate) struct IdConverter<VarIn, VarOut> {
    cache: FxHashMap<VarIn, VarOut>,
    gen: IdSupply<VarOut>,
}
impl<VarIn, VarOut> IdConverter<VarIn, VarOut>
where
    VarIn: std::hash::Hash + Eq,
    VarOut: Id + Copy,
{
    pub(crate) fn new() -> Self {
        Self {
            cache: FxHashMap::default(),
            gen: IdSupply::default(),
        }
    }

    pub(crate) fn convert(&mut self, var_id: VarIn) -> VarOut {
        *self
            .cache
            .entry(var_id)
            .or_insert_with(|| self.gen.supply_id())
    }

    pub(crate) fn generate(&mut self) -> VarOut {
        self.gen.supply_id()
    }
}

impl<In, Out> From<IdConverter<In, Out>> for IdSupply<Out> {
    fn from(value: IdConverter<In, Out>) -> Self {
        value.gen
    }
}
