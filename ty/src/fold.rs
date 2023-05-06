use crate::{
    alloc::{ScopedRowVarOf, SimpleRowVarOf},
    row::{ScopedRow, SimpleRow},
};

use super::{
    alloc::{AccessTy, MkTy, TypeAlloc, TypeVarOf},
    row::Row,
    Ty, TypeKind,
};

/// A trait for things that contain types.
/// This defines how to traverse `Self` to visit each type it contains and fold it.
///
/// Pairs with `FallibleTypeFold` to perform a type fold over arbitrary data containing types.
/// This could be `Ty` itself which would produce a new `Ty`, or it could be something like
/// `ClosedRow` which would produce a new `ClosedRow` by folding each type in the rows values.
pub trait TypeFoldable<'ctx> {
    type Alloc: TypeAlloc + 'ctx;
    type Out<B: TypeAlloc>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, In = Self::Alloc>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::Out>, F::Error>;
}

impl<'ctx, T: TypeFoldable<'ctx>> TypeFoldable<'ctx> for Vec<T> {
    type Alloc = T::Alloc;
    type Out<B: TypeAlloc> = Vec<T::Out<B>>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, In = Self::Alloc>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::Out>, F::Error> {
        self.into_iter().map(|t| t.try_fold_with(fold)).collect()
    }
}

/// Defines a fold over types and sub components of types.
/// This is commonly used to perform substitution.
///
/// Pairs with `TypeFoldable` to perform a type fold over arbitrary data containing types.
pub trait FallibleTypeFold<'access>: Sized {
    type In: TypeAlloc + Clone + 'access;
    type Out: TypeAlloc;
    type Error;

    type AccessTy: ?Sized + AccessTy<'access, Self::In>;
    type MkTy: ?Sized + MkTy<Self::Out>;

    fn access(&self) -> &Self::AccessTy;
    fn ctx(&self) -> &Self::MkTy;

    fn try_fold_ty(&mut self, t: Ty<Self::In>) -> Result<Ty<Self::Out>, Self::Error> {
        self.access().kind(&t).try_default_fold(self)
    }

    fn try_fold_var(&mut self, var: TypeVarOf<Self::In>) -> Result<Ty<Self::Out>, Self::Error>;

    fn try_fold_simple_row_var(
        &mut self,
        var: SimpleRowVarOf<Self::In>,
    ) -> Result<SimpleRow<Self::Out>, Self::Error>;

    fn try_fold_scoped_row_var(
        &mut self,
        var: ScopedRowVarOf<Self::In>,
    ) -> Result<ScopedRow<Self::Out>, Self::Error>;
}

pub trait FallibleEndoTypeFold<'access>: Sized {
    type Alloc: TypeAlloc + Clone + 'access;
    type Error;

    type TyCtx: ?Sized + MkTy<Self::Alloc> + AccessTy<'access, Self::Alloc>;

    fn endo_ctx(&self) -> &Self::TyCtx;

    fn try_endofold_ty(&mut self, ty: Ty<Self::Alloc>) -> Result<Ty<Self::Alloc>, Self::Error> {
        self.endo_ctx().kind(&ty).try_default_fold(self)
    }

    fn try_endofold_var(
        &mut self,
        var: TypeVarOf<Self::Alloc>,
    ) -> Result<Ty<Self::Alloc>, Self::Error> {
        Ok(self.endo_ctx().mk_ty(TypeKind::VarTy(var)))
    }

    fn try_endofold_simple_row_var(
        &mut self,
        var: SimpleRowVarOf<Self::Alloc>,
    ) -> Result<SimpleRow<Self::Alloc>, Self::Error> {
        Ok(Row::Open(var))
    }

    fn try_endofold_scoped_row_var(
        &mut self,
        var: ScopedRowVarOf<Self::Alloc>,
    ) -> Result<ScopedRow<Self::Alloc>, Self::Error> {
        Ok(Row::Open(var))
    }
}

impl<'a, F> FallibleTypeFold<'a> for F
where
    F: FallibleEndoTypeFold<'a>,
{
    type In = F::Alloc;
    type Out = F::Alloc;
    type Error = F::Error;

    type AccessTy = F::TyCtx;
    type MkTy = F::TyCtx;

    fn access(&self) -> &Self::AccessTy {
        self.endo_ctx()
    }

    fn ctx(&self) -> &Self::MkTy {
        self.endo_ctx()
    }

    fn try_fold_var(&mut self, var: TypeVarOf<Self::In>) -> Result<Ty<Self::Out>, Self::Error> {
        self.try_endofold_var(var)
    }

    fn try_fold_ty(&mut self, ty: Ty<Self::In>) -> Result<Ty<Self::Out>, Self::Error> {
        self.try_endofold_ty(ty)
    }

    fn try_fold_simple_row_var(
        &mut self,
        var: SimpleRowVarOf<Self::In>,
    ) -> Result<Row<Self::Out>, Self::Error> {
        self.try_endofold_simple_row_var(var)
    }

    fn try_fold_scoped_row_var(
        &mut self,
        var: ScopedRowVarOf<Self::In>,
    ) -> Result<ScopedRow<Self::Out>, Self::Error> {
        self.try_endofold_scoped_row_var(var)
    }
}

/// Defines the default way to fold over something.
/// This is used by `TypeFoldable` and `FallibleTypeFold` to determine how to fold over something
/// when the trait implementator does not wish to use a custom traversal.
pub(crate) trait DefaultFold<'ctx> {
    type In: TypeAlloc + 'ctx;

    fn try_default_fold<F: FallibleTypeFold<'ctx, In = Self::In>>(
        &self,
        fold: &mut F,
    ) -> Result<Ty<F::Out>, F::Error>;
}
