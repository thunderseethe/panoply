use super::row::Row;
use super::{MkTy, Ty, TypeKind};

/// A trait for things that contain types.
/// This defines how to traverse `Self` to visit each type it contains and fold it.
///
/// Pairs with `FallibleTypeFold` to perform a type fold over arbitrary data containing types.
/// This could be `Ty` itself which would produce a new `Ty`, or it could be something like
/// `ClosedRow` which would produce a new `ClosedRow` by folding each type in the rows values.
pub trait TypeFoldable<'ctx> {
    type TypeVar;
    type Out<TV: 'ctx>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::TypeVar>, F::Error>;
}

impl<'ctx, T: TypeFoldable<'ctx>> TypeFoldable<'ctx> for Vec<T> {
    type TypeVar = T::TypeVar;
    type Out<TV: 'ctx> = Vec<T::Out<TV>>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::TypeVar>, F::Error> {
        self.into_iter()
            .map(|t| t.try_fold_with(fold))
            .collect()
    }
}

/// Defines a fold over types and sub components of types.
/// This is commonly used to perform substitution.
///
/// Pairs with `TypeFoldable` to perform a type fold over arbitrary data containing types.
pub trait FallibleTypeFold<'ctx>: Sized {
    type InTypeVar: Clone;
    type TypeVar: 'ctx + TryFrom<Self::InTypeVar>;
    type Error: From<<Self::TypeVar as TryFrom<Self::InTypeVar>>::Error>;

    fn ctx(&self) -> &dyn MkTy<'ctx, Self::TypeVar>;

    fn try_fold_ty<'a>(
        &mut self,
        t: Ty<'a, Self::InTypeVar>,
    ) -> Result<Ty<'ctx, Self::TypeVar>, Self::Error> {
        t.try_default_fold(self)
    }

    fn try_fold_var(
        &mut self,
        var: Self::InTypeVar,
    ) -> Result<Ty<'ctx, Self::TypeVar>, Self::Error> {
        let v = var.try_into()?;
        Ok(self.ctx().mk_ty(TypeKind::VarTy(v)))
    }

    fn try_fold_row_var(
        &mut self,
        var: Self::InTypeVar,
    ) -> Result<Row<'ctx, Self::TypeVar>, Self::Error> {
        let v = var.try_into()?;
        Ok(Row::Open(v))
    }
}

/// Defines the default way to fold over something.
/// This is used by `TypeFoldable` and `FallibleTypeFold` to determine how to fold over something
/// when the trait implementator does not wish to use a custom traversal.
pub(crate) trait DefaultFold {
    type TypeVar;
    type Out<'a, TV: 'a>;

    fn try_default_fold<'ctx, F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<'ctx, F::TypeVar>, F::Error>;
}
