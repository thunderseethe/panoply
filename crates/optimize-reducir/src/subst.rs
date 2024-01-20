use std::convert::Infallible;

use reducir::ty::Subst;
use reducir::{default_endotraverse_ir, ReducIr, ReducIrEndoFold, ReducIrKind, ReducIrLocal, P};
use rustc_hash::FxHashMap;

pub(crate) struct Inline<'a, DB: ?Sized> {
  pub(crate) db: &'a DB,
  pub(crate) env: &'a FxHashMap<ReducIrLocal, ReducIr>,
  pub(crate) subst: Subst,
}

impl<'a, DB: ?Sized> Inline<'a, DB> {
  pub(crate) fn new(db: &'a DB, env: &'a FxHashMap<ReducIrLocal, ReducIr>) -> Self {
    Self {
      db,
      env,
      subst: Subst::nil(),
    }
  }
}
impl<DB: ?Sized + crate::Db> ReducIrEndoFold for Inline<'_, DB> {
  type Ext = Infallible;

  fn endofold_ir(&mut self, kind: ReducIrKind<Self::Ext>) -> ReducIr<Self::Ext> {
    match kind {
      ReducIrKind::Var(v) => match self.env.get(&v.var) {
        Some(val) => val.subst(self.db, self.subst.clone()),
        None => ReducIr::new(kind),
      },
      _ => ReducIr::new(kind),
    }
  }

  fn endotraverse_ir(&mut self, ir: &ReducIr<Self::Ext>) -> ReducIr<Self::Ext> {
    match ir.kind() {
      ReducIrKind::TyAbs(ty_var, body) => {
        let subst = self.subst.clone();
        self.subst = self.subst.clone().inc1();
        let body = body.fold(self);
        self.subst = subst;
        self.endofold_ir(ReducIrKind::TyAbs(*ty_var, P::new(body)))
      }
      _ => default_endotraverse_ir(self, ir),
    }
  }
}
