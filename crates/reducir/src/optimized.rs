use base::modules::Module;

use crate::{ReducIr, ReducIrTermName};

#[salsa::tracked]
pub struct OptimizedReducIrItem<'db> {
  pub name: ReducIrTermName,
  #[returns(ref)]
  pub item: ReducIr,
}

#[salsa::tracked]
pub struct OptimizedReducIrModule<'db> {
  pub module: Module,
  #[returns(ref)]
  pub items: Vec<OptimizedReducIrItem<'db>>,
}
