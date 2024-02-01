pub(crate) mod occurs_check {
  use ty::{
    infer::{InArena, ScopedRowK, SimpleRowK, TcUnifierVar, TypeK, UnifierKind},
    row::{Row, ScopedRow},
    FallibleEndoTypeFold, MkTy, TypeVarOf,
  };

  use ty::{AccessTy, Ty, TypeKind};

  /// Check that a unification variable does not appear within the type the unification variable is
  /// mapped to. This prevents unification from solving to a substitution with a cycle.
  pub(crate) struct OccursCheck<'a, 'inf, I, K: UnifierKind> {
    pub(crate) ctx: &'a I,
    pub(crate) var: TcUnifierVar<'inf, K>,
  }
  impl<'a, 'inf, I> FallibleEndoTypeFold<'inf> for OccursCheck<'a, 'inf, I, TypeK>
  where
    I: MkTy<InArena<'inf>> + AccessTy<'inf, InArena<'inf>>,
  {
    type Alloc = InArena<'inf>;
    type Error = TcUnifierVar<'inf, TypeK>;

    type TyCtx = I;

    fn endo_ctx(&self) -> &Self::TyCtx {
      self.ctx
    }

    fn try_endofold_var(
      &mut self,
      var: TypeVarOf<Self::Alloc>,
    ) -> Result<Ty<Self::Alloc>, Self::Error> {
      if var == self.var {
        Err(var)
      } else {
        Ok(self.ctx.mk_ty(TypeKind::VarTy(var)))
      }
    }
  }
  impl<'a, 'inf, I> FallibleEndoTypeFold<'inf> for OccursCheck<'a, 'inf, I, SimpleRowK>
  where
    I: MkTy<InArena<'inf>> + AccessTy<'inf, InArena<'inf>>,
  {
    type Alloc = InArena<'inf>;
    type Error = TcUnifierVar<'inf, SimpleRowK>;

    type TyCtx = I;

    fn endo_ctx(&self) -> &Self::TyCtx {
      self.ctx
    }

    fn try_endofold_simple_row_var(
      &mut self,
      var: ty::SimpleRowVarOf<Self::Alloc>,
    ) -> Result<ty::row::SimpleRow<Self::Alloc>, Self::Error> {
      if var == self.var {
        Err(var)
      } else {
        Ok(Row::Open(var))
      }
    }
  }

  impl<'a, 'inf, I> FallibleEndoTypeFold<'inf> for OccursCheck<'a, 'inf, I, ScopedRowK>
  where
    I: MkTy<InArena<'inf>> + AccessTy<'inf, InArena<'inf>>,
  {
    type Alloc = InArena<'inf>;
    type Error = TcUnifierVar<'inf, ScopedRowK>;

    type TyCtx = I;

    fn endo_ctx(&self) -> &Self::TyCtx {
      self.ctx
    }

    fn try_endofold_scoped_row_var(
      &mut self,
      var: ty::ScopedRowVarOf<Self::Alloc>,
    ) -> Result<ScopedRow<Self::Alloc>, Self::Error> {
      if var == self.var {
        Err(var)
      } else {
        Ok(Row::Open(var))
      }
    }
  }
}

pub(crate) mod normalize {
  use std::convert::Infallible;

  use ena::unify::InPlaceUnificationTable;
  use ty::{
    infer::{InArena, ScopedRowK, SimpleRowK, TcUnifierVar, TypeK},
    row::{Row, ScopedRow, SimpleRow},
    FallibleEndoTypeFold, ScopedRowVarOf, SimpleRowVarOf, TypeFoldable, TypeVarOf,
  };

  use ty::{AccessTy, MkTy, Ty, TypeKind};

  /// Normalize a type for unification.
  /// Walks a type and checks any variables it contains against current unifiers. Replacing
  /// unification variables by their value when present.
  pub(crate) struct Normalize<'a, 'inf, I> {
    pub(crate) ctx: &'a I,
    pub(crate) ty_unifiers: &'a mut InPlaceUnificationTable<TcUnifierVar<'inf, TypeK>>,
    pub(crate) datarow_unifiers: &'a mut InPlaceUnificationTable<TcUnifierVar<'inf, SimpleRowK>>,
    pub(crate) effrow_unifiers: &'a mut InPlaceUnificationTable<TcUnifierVar<'inf, ScopedRowK>>,
  }

  impl<'inf, I> FallibleEndoTypeFold<'inf> for Normalize<'_, 'inf, I>
  where
    I: MkTy<InArena<'inf>> + AccessTy<'inf, InArena<'inf>>,
  {
    type Alloc = InArena<'inf>;
    type Error = Infallible;

    type TyCtx = I;

    fn endo_ctx(&self) -> &Self::TyCtx {
      self.ctx
    }

    fn try_endofold_var(
      &mut self,
      var: TypeVarOf<Self::Alloc>,
    ) -> Result<Ty<Self::Alloc>, Self::Error> {
      match self.ty_unifiers.probe_value(var) {
        Some(ty) => ty.try_fold_with(self),
        _ => Ok(self.endo_ctx().mk_ty(TypeKind::VarTy(var))),
      }
    }

    fn try_endofold_simple_row_var(
      &mut self,
      var: SimpleRowVarOf<Self::Alloc>,
    ) -> Result<SimpleRow<Self::Alloc>, Self::Error> {
      match self.datarow_unifiers.probe_value(var) {
        Some(row) => row.try_fold_with(self).map(Row::Closed),
        _ => Ok(Row::Open(var)),
      }
    }

    fn try_endofold_scoped_row_var(
      &mut self,
      var: ScopedRowVarOf<Self::Alloc>,
    ) -> Result<ScopedRow<Self::Alloc>, Self::Error> {
      match self.effrow_unifiers.probe_value(var) {
        Some(row) => row.try_fold_with(self).map(Row::Closed),
        _ => Ok(Row::Open(var)),
      }
    }
  }
}

pub(crate) mod tyvar_subst {
  use std::convert::Infallible;

  use base::id::TyVarId;
  use rustc_hash::FxHashMap;
  use ty::row::{Row, ScopedRow, SimpleRow};
  use ty::{FallibleTypeFold, InDb, MkTy, ScopedRowVarOf, SimpleRowVarOf, Ty, TypeKind, TypeVarOf};

  pub(crate) struct TyVarIdSubst<'a, DB: ?Sized> {
    pub(crate) db: &'a DB,
    pub(crate) subst: FxHashMap<TyVarId, TyVarId>,
  }
  impl<'a, DB: ?Sized + crate::Db> FallibleTypeFold<'a> for TyVarIdSubst<'a, DB> {
    type In = InDb;

    type Out = InDb;

    type Error = Infallible;

    type AccessTy = &'a DB;

    type MkTy = DB;

    fn access(&self) -> &Self::AccessTy {
      &self.db
    }

    fn ctx(&self) -> &Self::MkTy {
      self.db
    }

    fn try_fold_var(&mut self, var: TypeVarOf<Self::In>) -> Result<Ty<Self::Out>, Self::Error> {
      let var = self.subst.get(&var).copied().unwrap_or(var);
      Ok(self.ctx().mk_ty(TypeKind::VarTy(var)))
    }

    fn try_fold_simple_row_var(
      &mut self,
      var: SimpleRowVarOf<Self::In>,
    ) -> Result<SimpleRow<Self::Out>, Self::Error> {
      let var = self.subst.get(&var).copied().unwrap_or(var);
      Ok(Row::Open(var))
    }

    fn try_fold_scoped_row_var(
      &mut self,
      var: ScopedRowVarOf<Self::In>,
    ) -> Result<ScopedRow<Self::Out>, Self::Error> {
      let var = self.subst.get(&var).copied().unwrap_or(var);
      Ok(Row::Open(var))
    }
  }
}

pub(crate) mod instantiate {
  use base::id::TyVarId;
  use ty::{
    infer::{InArena, ScopedRowK, SimpleRowK, TcUnifierVar, TcVarToUnifierError, TypeK},
    row::{Row, ScopedRow, SimpleRow},
    FallibleTypeFold, ScopedRowVarOf, SimpleRowVarOf, TypeVarOf,
  };

  use ty::{InDb, MkTy, Ty, TypeKind};

  /// Instantiate a type scheme for type checking.
  /// This means replacing all it's TcVars with fresh unifiers and adding any constraints (post
  /// substitution) to the list of constraints that must be true.
  pub(crate) struct Instantiate<'a, 'infer, I> {
    pub(crate) db: &'a dyn crate::Db,
    pub(crate) ctx: &'a I,
    pub(crate) ty_unifiers: Vec<(TyVarId, TcUnifierVar<'infer, TypeK>)>,
    pub(crate) datarow_unifiers: Vec<(TyVarId, TcUnifierVar<'infer, SimpleRowK>)>,
    pub(crate) effrow_unifiers: Vec<(TyVarId, TcUnifierVar<'infer, ScopedRowK>)>,
  }

  impl<'a, 'infer, I> Instantiate<'a, 'infer, I> {
    pub(crate) fn new(db: &'a dyn crate::Db, ctx: &'a I) -> Self {
      Self {
        db,
        ctx,
        ty_unifiers: Default::default(),
        datarow_unifiers: Default::default(),
        effrow_unifiers: Default::default(),
      }
    }
  }
  impl<'a, 'infer, I> FallibleTypeFold<'a> for Instantiate<'a, 'infer, I>
  where
    I: MkTy<InArena<'infer>>,
  {
    type In = InDb;
    type Out = InArena<'infer>;
    type Error = TcVarToUnifierError;

    type AccessTy = &'a (dyn crate::Db + 'a);
    type MkTy = I;

    fn access(&self) -> &Self::AccessTy {
      &self.db
    }

    fn ctx(&self) -> &Self::MkTy {
      self.ctx
    }

    fn try_fold_var(&mut self, var: TypeVarOf<InDb>) -> Result<Ty<InArena<'infer>>, Self::Error> {
      let uni_var = self
        .ty_unifiers
        .iter()
        .find_map(|(key, val)| (key == &var).then_some(val))
        .expect("ty_unifiers did not contain type variable");
      Ok(self.ctx().mk_ty(TypeKind::VarTy(*uni_var)))
    }

    fn try_fold_simple_row_var(
      &mut self,
      var: SimpleRowVarOf<InDb>,
    ) -> Result<SimpleRow<Self::Out>, Self::Error> {
      let uni_var = self
        .datarow_unifiers
        .iter()
        .find_map(|(key, val)| (key == &var).then_some(val))
        .expect("ty_unifiers did not contain data variable");
      Ok(Row::Open(*uni_var))
    }

    fn try_fold_scoped_row_var(
      &mut self,
      var: ScopedRowVarOf<Self::In>,
    ) -> Result<ScopedRow<Self::Out>, Self::Error> {
      let uni_var = self
        .effrow_unifiers
        .iter()
        .find_map(|(key, val)| (key == &var).then_some(val))
        .expect("ty_unifiers did not contain effect variable");
      Ok(Row::Open(*uni_var))
    }
  }
}

pub(crate) mod zonker {
  use base::id::{Id, TyVarId};
  use ena::unify::InPlaceUnificationTable;
  use ty::{
    infer::{InArena, ScopedRowK, SimpleRowK, TcUnifierVar, TypeK, UnifierToTcVarError},
    row::{Row, ScopedRow, SimpleRow},
    FallibleTypeFold, ScopedRowVarOf, SimpleRowVarOf, TypeFoldable, TypeVarOf,
  };

  use ty::{InDb, MkTy, Ty, TypeKind};

  /// Combine our different unifier variables as one enum during zonking
  #[derive(PartialEq, Eq)]
  enum Unifier<'infer> {
    Ty(TcUnifierVar<'infer, TypeK>),
    SimpleRow(TcUnifierVar<'infer, SimpleRowK>),
    ScopedRow(TcUnifierVar<'infer, ScopedRowK>),
  }

  /// Split out our free type variables by their kind once we're done zonking
  #[derive(Default)]
  pub(crate) struct FreeTyVars {
    pub(crate) ty: Vec<TyVarId>,
    pub(crate) data: Vec<TyVarId>,
    pub(crate) eff: Vec<TyVarId>,
  }

  impl From<Zonker<'_, '_>> for FreeTyVars {
    fn from(value: Zonker<'_, '_>) -> Self {
      let mut free_ty_vars = FreeTyVars::default();
      for (i, unifier) in value.free_unifiers.into_iter().enumerate() {
        match unifier {
          Unifier::Ty(_) => free_ty_vars.ty.push(TyVarId::from_raw(i)),
          Unifier::SimpleRow(_) => free_ty_vars.data.push(TyVarId::from_raw(i)),
          Unifier::ScopedRow(_) => free_ty_vars.eff.push(TyVarId::from_raw(i)),
        }
      }
      // Because we walk free_unifiers in order each output vec in free_ty_vars is already in
      // ascending order, no need to resort.
      free_ty_vars
    }
  }

  /// Zonk anything that is TypeFoldable.
  /// This removes all unification variables.
  /// If a unification variables is solved to a type, it is replaced by that type.
  /// If a unification variable has no solution, we replace it by a fresh type variable and record it
  /// as free.
  pub(crate) struct Zonker<'a, 'infer> {
    pub(crate) ctx: &'a dyn crate::Db,
    pub(crate) ty_unifiers: &'a mut InPlaceUnificationTable<TcUnifierVar<'infer, TypeK>>,
    pub(crate) datarow_unifiers: &'a mut InPlaceUnificationTable<TcUnifierVar<'infer, SimpleRowK>>,
    pub(crate) effrow_unifiers: &'a mut InPlaceUnificationTable<TcUnifierVar<'infer, ScopedRowK>>,
    free_unifiers: Vec<Unifier<'infer>>,
  }

  impl<'a, 'infer> Zonker<'a, 'infer> {
    pub(crate) fn new(
      ctx: &'a dyn crate::Db,
      ty_unifiers: &'a mut InPlaceUnificationTable<TcUnifierVar<'infer, TypeK>>,
      datarow_unifiers: &'a mut InPlaceUnificationTable<TcUnifierVar<'infer, SimpleRowK>>,
      effrow_unifiers: &'a mut InPlaceUnificationTable<TcUnifierVar<'infer, ScopedRowK>>,
    ) -> Self {
      Self {
        ctx,
        ty_unifiers,
        datarow_unifiers,
        effrow_unifiers,
        free_unifiers: vec![],
      }
    }

    fn add_ty(&mut self, var: TcUnifierVar<'infer, TypeK>) -> TyVarId {
      // Find the root unification variable and return a type varaible representing that
      // root.
      let root = self.ty_unifiers.find(var);
      let var_indx = self
        .free_unifiers
        .iter()
        .position(|uv| Unifier::Ty(root) == *uv)
        // We have not seen this unification variable before, so create a new one.
        .unwrap_or_else(|| {
          let next_index = self.free_unifiers.len();
          self.free_unifiers.push(Unifier::Ty(root));
          next_index
        });
      TyVarId::from_raw(var_indx)
    }

    fn add_datarow(&mut self, var: TcUnifierVar<'infer, SimpleRowK>) -> TyVarId {
      let root = self.datarow_unifiers.find(var);
      let var_indx = self
        .free_unifiers
        .iter()
        .position(|uv| Unifier::SimpleRow(root) == *uv)
        .unwrap_or_else(|| {
          let next_index = self.free_unifiers.len();
          self.free_unifiers.push(Unifier::SimpleRow(root));
          next_index
        });
      TyVarId::from_raw(var_indx)
    }

    fn add_effrow(&mut self, var: TcUnifierVar<'infer, ScopedRowK>) -> TyVarId {
      let root = self.effrow_unifiers.find(var);
      let var_indx = self
        .free_unifiers
        .iter()
        .position(|uv| Unifier::ScopedRow(root) == *uv)
        .unwrap_or_else(|| {
          let next_index = self.free_unifiers.len();
          self.free_unifiers.push(Unifier::ScopedRow(root));
          next_index
        });
      TyVarId::from_raw(var_indx)
    }
  }

  impl<'a, 'infer> FallibleTypeFold<'infer> for Zonker<'a, 'infer> {
    type In = InArena<'infer>;
    type Out = InDb;
    type Error = UnifierToTcVarError;

    type AccessTy = ();
    type MkTy = dyn ty::Db + 'a;

    fn access(&self) -> &Self::AccessTy {
      &()
    }

    fn ctx(&self) -> &Self::MkTy {
      self.ctx.as_ty_db()
    }

    fn try_fold_var(&mut self, var: TypeVarOf<InArena<'infer>>) -> Result<Ty<InDb>, Self::Error> {
      match self.ty_unifiers.probe_value(var) {
        Some(ty) => ty.try_fold_with(self),
        None => {
          // Our unification variable wasn't solved to a type.
          // Generalize it to a type variable.
          let ty_var = self.add_ty(var);
          Ok(self.ctx().mk_ty(TypeKind::VarTy(ty_var)))
        }
      }
    }

    fn try_fold_simple_row_var(
      &mut self,
      var: SimpleRowVarOf<Self::In>,
    ) -> Result<SimpleRow<Self::Out>, Self::Error> {
      match self.datarow_unifiers.probe_value(var) {
        Some(row) => row.try_fold_with(self).map(Row::Closed),
        None => {
          let row_var = self.add_datarow(var);
          Ok(Row::Open(row_var))
        }
      }
    }

    fn try_fold_scoped_row_var(
      &mut self,
      var: ScopedRowVarOf<Self::In>,
    ) -> Result<ScopedRow<Self::Out>, Self::Error> {
      match self.effrow_unifiers.probe_value(var) {
        Some(row) => row.try_fold_with(self).map(Row::Closed),
        None => {
          let row_var = self.add_effrow(var);
          Ok(Row::Open(row_var))
        }
      }
    }
  }
}
