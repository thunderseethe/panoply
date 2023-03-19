pub(crate) mod occurs_check {
    use aiahr_core::ty::infer::{InArena, TcUnifierVar};
    use aiahr_core::ty::{FallibleEndoTypeFold, TypeVarOf};

    use crate::{
        //ty::{alloc::TypeVarOf, fold::FallibleEndoTypeFold},
        AccessTy,
        MkTy,
        Ty,
        TypeKind,
    };

    /// Check that a unification variable does not appear within the type the unification variable is
    /// mapped to. This prevents unification from solving to a substitution with a cycle.
    pub(crate) struct OccursCheck<'a, 'inf, I> {
        pub(crate) ctx: &'a I,
        pub(crate) var: TcUnifierVar<'inf>,
    }
    impl<'a, 'inf, I> FallibleEndoTypeFold<'inf> for OccursCheck<'a, 'inf, I>
    where
        I: MkTy<InArena<'inf>> + AccessTy<'inf, InArena<'inf>>,
    {
        type Alloc = InArena<'inf>;
        type Error = TcUnifierVar<'inf>;

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
}

pub(crate) mod normalize {
    use std::convert::Infallible;

    use aiahr_core::ty::infer::{InArena, TcUnifierVar};
    use aiahr_core::ty::TypeFoldable;
    use aiahr_core::ty::{row::Row, FallibleEndoTypeFold, TypeVarOf};
    use ena::unify::InPlaceUnificationTable;

    use crate::{AccessTy, MkTy, Ty, TypeKind};

    /// Normalize a type for unification.
    /// Walks a type and checks any variables it contains against current unifiers. Replacing
    /// unification variables by their value when present.
    pub(crate) struct Normalize<'a, 'inf, I> {
        pub(crate) ctx: &'a I,
        pub(crate) unifiers: &'a mut InPlaceUnificationTable<TcUnifierVar<'inf>>,
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
            match self.unifiers.probe_value(var) {
                Some(ty) => ty.try_fold_with(self),
                _ => Ok(self.endo_ctx().mk_ty(TypeKind::VarTy(var))),
            }
        }

        fn try_endofold_row_var(
            &mut self,
            var: TypeVarOf<Self::Alloc>,
        ) -> Result<Row<Self::Alloc>, Self::Error> {
            //let v = var.try_into()?;
            //Ok(Row::Open(v))
            match self.unifiers.probe_value(var) {
                Some(ty) => {
                    let row = ty
                        .try_to_row()
                        .expect("Kind mismatch: Unified a row variable with a type");
                    row.try_fold_with(self)
                }
                _ => Ok(Row::Open(var)),
            }
        }
    }
}

pub(crate) mod instantiate {
    use aiahr_core::ty::infer::{InArena, TcUnifierVar, TcVarToUnifierError};
    use aiahr_core::ty::{row::Row, FallibleTypeFold, TypeVarOf};

    use crate::{InDb, MkTy, Ty, TypeKind};

    /// Instantiate a type scheme for type checking.
    /// This means replacing all it's TcVars with fresh unifiers and adding any constraints (post
    /// substitution) to the list of constraints that must be true.
    pub(crate) struct Instantiate<'a, 'infer, I> {
        pub(crate) db: &'a dyn crate::Db,
        pub(crate) ctx: &'a I,
        pub(crate) unifiers: Vec<TcUnifierVar<'infer>>,
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
            Ok(self.ctx().mk_ty(TypeKind::VarTy(self.unifiers[var.0])))
        }

        fn try_fold_row_var(
            &mut self,
            var: TypeVarOf<InDb>,
        ) -> Result<Row<InArena<'infer>>, Self::Error> {
            Ok(Row::Open(self.unifiers[var.0]))
        }
    }
}

pub(crate) mod zonker {
    use aiahr_core::{
        id::{Id, TyVarId},
        memory::handle::Handle,
        ty::{
            infer::{InArena, TcUnifierVar, UnifierToTcVarError},
            row::Row,
            FallibleTypeFold, TypeFoldable, TypeVarOf,
        },
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
        pub(crate) unifiers: &'a mut InPlaceUnificationTable<TcUnifierVar<'infer>>,
        pub(crate) free_vars: Vec<TcUnifierVar<'infer>>,
    }

    impl<'a, 'infer> Zonker<'a, 'infer> {
        fn add(&mut self, var: TcUnifierVar<'infer>) -> TyVarId {
            // Find the root unification variable and return a type varaible representing that
            // root.
            let root = self.unifiers.find(var);
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
    }

    impl<'a, 'infer> FallibleTypeFold<'infer> for Zonker<'a, 'infer> {
        type In = InArena<'infer>;
        type Out = InDb;
        type Error = UnifierToTcVarError;

        type AccessTy = ();
        type MkTy = dyn aiahr_core::Db + 'a;

        fn access(&self) -> &Self::AccessTy {
            &()
        }

        fn ctx(&self) -> &Self::MkTy {
            self.ctx.as_core_db()
        }

        fn try_fold_var(
            &mut self,
            var: TypeVarOf<InArena<'infer>>,
        ) -> Result<Ty<InDb>, Self::Error> {
            match self.unifiers.probe_value(var) {
                Some(ty) => ty.try_fold_with(self),
                _ => {
                    // Our unification variable wasn't solved to a type.
                    // Generalize it to a type variable.
                    let ty_var = self.add(var);
                    Ok(self.ctx().mk_ty(TypeKind::VarTy(ty_var)))
                }
            }
        }

        fn try_fold_row_var(
            &mut self,
            var: TypeVarOf<InArena<'infer>>,
        ) -> Result<Row<InDb>, Self::Error> {
            match self.unifiers.probe_value(var) {
                Some(Ty(Handle(TypeKind::RowTy(row)))) => row.try_fold_with(self).map(Row::Closed),
                _ => {
                    let row_var = self.add(var);
                    Ok(Row::Open(row_var))
                }
            }
        }
    }
}
