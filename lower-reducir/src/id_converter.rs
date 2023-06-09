use aiahr_core::id::{Id, IdGen};
use rustc_hash::FxHashMap;

pub(crate) struct IdConverter<VarIn, VarOut> {
    cache: FxHashMap<VarIn, VarOut>,
    gen: IdGen<VarOut, ()>,
}
impl<VarIn, VarOut> IdConverter<VarIn, VarOut>
where
    VarIn: std::hash::Hash + Eq,
    VarOut: Id + Copy,
{
    pub(crate) fn new() -> Self {
        Self {
            cache: FxHashMap::default(),
            gen: IdGen::new(),
        }
    }

    pub(crate) fn convert(&mut self, var_id: VarIn) -> VarOut {
        *self
            .cache
            .entry(var_id)
            .or_insert_with(|| self.gen.push(()))
    }

    pub(crate) fn generate(&mut self) -> VarOut {
        self.gen.generate()
    }
}
