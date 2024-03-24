use base::{
  id::{IdSupply, ReducIrVarId, TermName},
  modules::Module,
};

use crate::{GeneratedReducIrName, ReducIr};

#[salsa::tracked]
pub struct MonReducIrModule {
  #[id]
  pub module: Module,
  #[return_ref]
  pub items: Vec<MonReducIrItem>,
}

#[salsa::tracked]
pub struct MonReducIrItem {
  #[id]
  pub name: TermName,
  #[return_ref]
  pub item: ReducIr,
  #[return_ref]
  pub row_evs: Vec<MonReducIrGenItem>,
  #[return_ref]
  pub var_supply: IdSupply<ReducIrVarId>,
}

#[salsa::tracked]
pub struct MonReducIrGenItem {
  #[id]
  pub name: GeneratedReducIrName,
  #[return_ref]
  pub item: ReducIr,
  #[return_ref]
  pub var_supply: IdSupply<ReducIrVarId>,
}
