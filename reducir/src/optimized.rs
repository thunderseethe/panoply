use aiahr_core::modules::Module;

use crate::{Lets, ReducIr, ReducIrTermName};

#[salsa::tracked]
pub struct OptimizedReducIrItem {
    #[id]
    pub name: ReducIrTermName,
    #[return_ref]
    pub item: ReducIr<Lets>,
}

#[salsa::tracked]
pub struct OptimizedReducIrModule {
    #[id]
    pub module: Module,
    #[return_ref]
    pub items: Vec<OptimizedReducIrItem>,
}
