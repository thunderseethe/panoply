use crate::id::{Id, IdSupply};
use rustc_hash::FxHashMap;

pub struct IdConverter<VarIn, VarOut> {
  cache: FxHashMap<VarIn, VarOut>,
  gen: IdSupply<VarOut>,
}
impl<VarIn, VarOut> IdConverter<VarIn, VarOut>
where
  VarIn: std::hash::Hash + Eq,
  VarOut: Id + Copy,
{
  pub fn new() -> Self {
    Self::default()
  }

  pub fn convert(&mut self, var_id: VarIn) -> VarOut {
    *self
      .cache
      .entry(var_id)
      .or_insert_with(|| self.gen.supply_id())
  }

  pub fn generate(&mut self) -> VarOut {
    self.gen.supply_id()
  }
}

impl<VarIn, VarOut> Default for IdConverter<VarIn, VarOut>
where
  VarIn: std::hash::Hash + Eq,
  VarOut: Id + Copy,
{
  fn default() -> Self {
    Self {
      cache: FxHashMap::default(),
      gen: IdSupply::default(),
    }
  }
}

impl<In, Out> From<IdConverter<In, Out>> for IdSupply<Out> {
  fn from(value: IdConverter<In, Out>) -> Self {
    value.gen
  }
}
