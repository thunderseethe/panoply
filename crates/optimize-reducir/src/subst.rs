use std::convert::Infallible;

use base::id::ReducIrVarId;
use reducir::ty::Subst;
use reducir::{default_endotraverse_ir, ReducIr, ReducIrEndoFold, ReducIrKind, ReducIrLocal, P};
use rustc_hash::FxHashMap;

pub(crate) struct Inline<'a, DB: ?Sized> {
  pub(crate) db: &'a DB,
  pub(crate) env: &'a mut FxHashMap<ReducIrLocal, ReducIr>,
  pub(crate) evv_var_id: ReducIrVarId,
  pub(crate) subst: Subst,
}

impl<'a, DB: ?Sized> Inline<'a, DB> {
  pub(crate) fn new(
    db: &'a DB,
    env: &'a mut FxHashMap<ReducIrLocal, ReducIr>,
    evv_var_id: ReducIrVarId,
  ) -> Self {
    Self {
      db,
      env,
      subst: Subst::nil(),
      evv_var_id,
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
      // Check for evv in our abs and remove it from subst if present
      ReducIrKind::Abs(vars, body) if vars.iter().any(|var| var.var.id == self.evv_var_id) => {
        let put_these_back = self
          .env
          .keys()
          .filter(|local| local.id == self.evv_var_id)
          .copied()
          .collect::<Vec<_>>();
        let put_these_back = put_these_back
          .into_iter()
          .filter_map(|local| self.env.remove_entry(&local))
          .collect::<Vec<_>>();
        let body = body.fold(self);
        self.env.extend(put_these_back);
        ReducIr::abss(vars.iter().copied(), body)
      }
      ReducIrKind::Locals(binds, body) => {
        let mut put_these_back = vec![];
        let body = body.fold(self);
        let binds = binds.iter().map(|local| {
          if let Some(entry) = self.env.remove_entry(&local.var.var) {
            put_these_back.push(entry);
          };
          local.fold(self)
        });
        let kind = ReducIr::locals(binds, body).kind;
        let ir = self.endofold_ir(kind);
        self.env.extend(put_these_back);
        ir
      }
      _ => default_endotraverse_ir(self, ir),
    }
  }
}
