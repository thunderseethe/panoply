pub(crate) mod occurs_check {
    use aiahr_ty::infer::{InArena, ScopedRowK, SimpleRowK, TcUnifierVar, TypeK, UnifierKind};
    use aiahr_ty::row::{Row, ScopedRow};
    use aiahr_ty::{FallibleEndoTypeFold, TypeVarOf};

    use crate::{AccessTy, MkTy, Ty, TypeKind};

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
            var: aiahr_ty::SimpleRowVarOf<Self::Alloc>,
        ) -> Result<aiahr_ty::row::SimpleRow<Self::Alloc>, Self::Error> {
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
            var: aiahr_ty::ScopedRowVarOf<Self::Alloc>,
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

    use aiahr_ty::{
        infer::{InArena, ScopedRowK, SimpleRowK, TcUnifierVar, TypeK},
        row::Row,
        FallibleEndoTypeFold, SimpleRowVarOf, TypeFoldable, TypeVarOf,
    };
    use ena::unify::InPlaceUnificationTable;

    use crate::{AccessTy, MkTy, Ty, TypeKind};

    /// Normalize a type for unification.
    /// Walks a type and checks any variables it contains against current unifiers. Replacing
    /// unification variables by their value when present.
    pub(crate) struct Normalize<'a, 'inf, I> {
        pub(crate) ctx: &'a I,
        pub(crate) ty_unifiers: &'a mut InPlaceUnificationTable<TcUnifierVar<'inf, TypeK>>,
        pub(crate) datarow_unifiers:
            &'a mut InPlaceUnificationTable<TcUnifierVar<'inf, SimpleRowK>>,
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
        ) -> Result<Row<Self::Alloc>, Self::Error> {
            match self.datarow_unifiers.probe_value(var) {
                Some(row) => row.try_fold_with(self).map(Row::Closed),
                _ => Ok(Row::Open(var)),
            }
        }

        fn try_endofold_scoped_row_var(
            &mut self,
            var: aiahr_ty::ScopedRowVarOf<Self::Alloc>,
        ) -> Result<aiahr_ty::row::ScopedRow<Self::Alloc>, Self::Error> {
            match self.effrow_unifiers.probe_value(var) {
                Some(row) => row.try_fold_with(self).map(Row::Closed),
                _ => Ok(Row::Open(var)),
            }
        }
    }
}

pub(crate) mod instantiate {

    use aiahr_ty::{
        infer::{InArena, ScopedRowK, SimpleRowK, TcUnifierVar, TcVarToUnifierError, TypeK},
        row::Row,
        FallibleTypeFold, ScopedRowVarOf, SimpleRowVarOf, TypeVarOf,
    };

    use crate::{InDb, MkTy, Ty, TypeKind};

    /// Instantiate a type scheme for type checking.
    /// This means replacing all it's TcVars with fresh unifiers and adding any constraints (post
    /// substitution) to the list of constraints that must be true.
    pub(crate) struct Instantiate<'a, 'infer, I> {
        pub(crate) db: &'a dyn crate::Db,
        pub(crate) ctx: &'a I,
        pub(crate) ty_unifiers: Vec<TcUnifierVar<'infer, TypeK>>,
        pub(crate) datarow_unifiers: Vec<TcUnifierVar<'infer, SimpleRowK>>,
        pub(crate) effrow_unifiers: Vec<TcUnifierVar<'infer, ScopedRowK>>,
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

        fn try_fold_var(
            &mut self,
            var: TypeVarOf<InDb>,
        ) -> Result<Ty<InArena<'infer>>, Self::Error> {
            Ok(self.ctx().mk_ty(TypeKind::VarTy(self.ty_unifiers[var.0])))
        }

        fn try_fold_simple_row_var(
            &mut self,
            var: SimpleRowVarOf<InDb>,
        ) -> Result<Row<Self::Out>, Self::Error> {
            Ok(Row::Open(self.datarow_unifiers[var.0]))
        }

        fn try_fold_scoped_row_var(
            &mut self,
            var: ScopedRowVarOf<Self::In>,
        ) -> Result<aiahr_ty::row::ScopedRow<Self::Out>, Self::Error> {
            Ok(Row::Open(self.effrow_unifiers[var.0]))
        }
    }
}

pub(crate) mod zonker {
    use aiahr_core::id::{Id, TyVarId};
    use aiahr_ty::{
        infer::{InArena, ScopedRowK, SimpleRowK, TcUnifierVar, TypeK, UnifierToTcVarError},
        row::Row,
        FallibleTypeFold, ScopedRowVarOf, SimpleRowVarOf, TypeFoldable, TypeVarOf,
    };
    use ena::unify::InPlaceUnificationTable;

    use crate::{InDb, MkTy, Ty, TypeKind};

    /// Zonk anything that is TypeFoldable.
    /// This removes all unification variables.
    /// If a unification variables is solved to a type, it is replaced by that type.
    /// If a unification variable has no solution, we replace it by a fresh type variable and record it
    /// as free.
    pub(crate) struct Zonker<'a, 'infer> {
        pub(crate) ctx: &'a dyn crate::Db,
        pub(crate) free_vars: Vec<TcUnifierVar<'infer, TypeK>>,
        pub(crate) free_data_rows: Vec<TcUnifierVar<'infer, SimpleRowK>>,
        pub(crate) free_eff_rows: Vec<TcUnifierVar<'infer, ScopedRowK>>,
        pub(crate) ty_unifiers: &'a mut InPlaceUnificationTable<TcUnifierVar<'infer, TypeK>>,
        pub(crate) datarow_unifiers:
            &'a mut InPlaceUnificationTable<TcUnifierVar<'infer, SimpleRowK>>,
        pub(crate) effrow_unifiers:
            &'a mut InPlaceUnificationTable<TcUnifierVar<'infer, ScopedRowK>>,
    }

    impl<'a, 'infer> Zonker<'a, 'infer> {
        fn add_ty(&mut self, var: TcUnifierVar<'infer, TypeK>) -> TyVarId {
            // Find the root unification variable and return a type varaible representing that
            // root.
            let root = self.ty_unifiers.find(var);
            let var_indx = self
                .free_vars
                .iter()
                .position(|uv| &root == uv)
                // We have not seen this unification variable before, so create a new one.
                .unwrap_or_else(|| {
                    let next_index = self.free_vars.len();
                    self.free_vars.push(var);
                    next_index
                });
            TyVarId::from_raw(var_indx)
        }

        fn add_datarow(&mut self, var: TcUnifierVar<'infer, SimpleRowK>) -> TyVarId {
            let root = self.datarow_unifiers.find(var);
            let var_indx = self
                .free_data_rows
                .iter()
                .position(|uv| &root == uv)
                .unwrap_or_else(|| {
                    let next_index = self.free_data_rows.len();
                    self.free_data_rows.push(var);
                    next_index
                });
            TyVarId::from_raw(var_indx)
        }

        fn add_effrow(&mut self, var: TcUnifierVar<'infer, ScopedRowK>) -> TyVarId {
            let root = self.effrow_unifiers.find(var);
            let var_indx = self
                .free_eff_rows
                .iter()
                .position(|uv| &root == uv)
                .unwrap_or_else(|| {
                    let next_index = self.free_data_rows.len();
                    self.free_eff_rows.push(var);
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
        type MkTy = dyn aiahr_ty::Db + 'a;

        fn access(&self) -> &Self::AccessTy {
            &()
        }

        fn ctx(&self) -> &Self::MkTy {
            self.ctx.as_ty_db()
        }

        fn try_fold_var(
            &mut self,
            var: TypeVarOf<InArena<'infer>>,
        ) -> Result<Ty<InDb>, Self::Error> {
            match self.ty_unifiers.probe_value(var) {
                Some(ty) => ty.try_fold_with(self),
                _ => {
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
        ) -> Result<Row<Self::Out>, Self::Error> {
            match self.datarow_unifiers.probe_value(var) {
                Some(row) => row.try_fold_with(self).map(Row::Closed),
                _ => {
                    let row_var = self.add_datarow(var);
                    Ok(Row::Open(row_var))
                }
            }
        }

        fn try_fold_scoped_row_var(
            &mut self,
            var: ScopedRowVarOf<Self::In>,
        ) -> Result<aiahr_ty::row::ScopedRow<Self::Out>, Self::Error> {
            match self.effrow_unifiers.probe_value(var) {
                Some(row) => row.try_fold_with(self).map(Row::Closed),
                _ => {
                    let row_var = self.add_effrow(var);
                    Ok(Row::Open(row_var))
                }
            }
        }
    }
}
