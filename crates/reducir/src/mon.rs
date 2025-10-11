use base::{
  id::{IdSupply, ReducIrVarId, TermName},
  modules::Module,
};

use crate::{GeneratedReducIrName, ReducIr};

#[salsa::tracked]
pub struct MonReducIrModule<'db> {
  pub module: Module,
  #[returns(ref)]
  pub items: Vec<MonReducIrItem<'db>>,
}

#[salsa::tracked]
pub struct MonReducIrItem<'db> {
  pub name: TermName,
  #[returns(ref)]
  pub item: ReducIr,
  #[returns(ref)]
  pub row_evs: Vec<MonReducIrGenItem<'db>>,
  #[returns(ref)]
  pub var_supply: IdSupply<ReducIrVarId>,
}

#[salsa::tracked]
pub struct MonReducIrGenItem<'db> {
  pub name: GeneratedReducIrName,
  #[returns(ref)]
  pub item: ReducIr,
  #[returns(ref)]
  pub var_supply: IdSupply<ReducIrVarId>,
}
