use std::fmt::{self, Debug};
use std::ops::Deref;

use aiahr_core::define_ids;
use aiahr_core::memory::handle::RefHandle;

use ena::unify::{EqUnifyValue, UnifyKey};

define_ids!(
/// A type variable.
/// These are explicity referred to by the AST and can persist through type checking.
/// They may not be modified by the type checking process, often referred to as untouchabale.
#[derive(Default, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub TcVar;
);

/// A unifier variable.
/// These are produced during the type checking process and MUST NOT persist outside the type
/// checker. They may not appear in the AST once type checking is completed and are removed by
/// zonking.
/// Conversely to the untouchable TcVar, these are "touchable" and will be modified by the type
/// checker.
#[derive(Default, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct TcUnifierVar<'ctx> {
    id: u32,
    _marker: std::marker::PhantomData<&'ctx ()>,
}
impl<'ctx> UnifyKey for TcUnifierVar<'ctx> {
    type Value = Option<Ty<'ctx, TcUnifierVar<'ctx>>>;

    fn index(&self) -> u32 {
        self.id
    }

    fn from_index(id: u32) -> Self {
        Self {
            id,
            _marker: std::marker::PhantomData,
        }
    }

    fn tag() -> &'static str {
        "TcUnifierVar"
    }
}

/// A trait for allocators that can make types and related data types.
pub trait MkTy<'ctx, TV> {
    fn mk_ty(&self, kind: TypeKind<'ctx, TV>) -> Ty<'ctx, TV>;
    fn mk_label(&self, label: &str) -> RowLabel<'ctx>;
    fn mk_row(&self, labels: &[RowLabel<'ctx>], values: &[Ty<'ctx, TV>]) -> ClosedRow<'ctx, TV>;
}

/// During inference our type variables are all unification variables.
/// This is an alias to make inference types easy to talk about.
pub type InferTy<'ctx> = Ty<'ctx, TcUnifierVar<'ctx>>;

/// A monomorphic type.
///
/// This is just a wrapper around an interned reference to the `TypeKind` which contains the actual
/// data.
#[derive(PartialEq, Eq, Hash)]
pub struct Ty<'ctx, TV>(pub RefHandle<'ctx, TypeKind<'ctx, TV>>);

impl<'ctx, TV> Clone for Ty<'ctx, TV> {
    fn clone(&self) -> Self {
        Ty(self.0)
    }
}
impl<'ctx, TV> Copy for Ty<'ctx, TV> {}

impl<'ctx, TV> Deref for Ty<'ctx, TV> {
    type Target = <RefHandle<'ctx, TypeKind<'ctx, TV>> as Deref>::Target;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}
impl<'ctx, TV: fmt::Debug> fmt::Debug for Ty<'ctx, TV> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Ty").field(&self.0 .0).finish()
    }
}

impl<'ctx> EqUnifyValue for Ty<'ctx, TcUnifierVar<'ctx>> {}

impl<'ty, TV> Into<Ty<'ty, TV>> for RefHandle<'ty, TypeKind<'ty, TV>> {
    fn into(self) -> Ty<'ty, TV> {
        Ty(self)
    }
}

/// A label of a row field
pub type RowLabel<'ctx> = RefHandle<'ctx, str>;

/// A closed row is a map of labels to values where all labels are known.
/// Counterpart to an open row where the set of labels is polymorphic
#[derive(PartialEq, Eq, Hash)]
pub struct ClosedRow<'ctx, TV> {
    pub labels: RefHandle<'ctx, [RowLabel<'ctx>]>,
    pub values: RefHandle<'ctx, [Ty<'ctx, TV>]>,
}
impl<'ctx, TV> Clone for ClosedRow<'ctx, TV> {
    fn clone(&self) -> Self {
        ClosedRow { labels: self.labels, values: self.values }
    }
}
impl<'ctx, TV> Copy for ClosedRow<'ctx, TV> {}

impl<'ctx, TV: Debug> Debug for ClosedRow<'ctx, TV> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(self.labels.iter().zip(self.values.iter()))
            .finish()
    }
}

impl<'ctx, TV: Clone> TypeFoldable<'ctx> for ClosedRow<'_, TV> {
    type TypeVar = TV;
    type Out<T: 'ctx> = ClosedRow<'ctx, T>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::TypeVar>, F::Error> {
        let labels = self
            .labels
            .iter()
            .map(|lbl| fold.ctx().mk_label(lbl))
            .collect::<Vec<_>>();
        let values = self
            .values
            .iter()
            .map(|ty| ty.try_fold_with(fold))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(fold.ctx().mk_row(labels.as_slice(), values.as_slice()))
    }
}

/// Data for `Ty`.
/// `TypeKind` is interned to produce a `Ty`.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum TypeKind<'ctx, TV> {
    /// Marker that signifies an operation produced an error. This exists so that we can try to
    /// gracefully recover from a type checking error and produce a list of errors at the end of type checking
    ErrorTy,
    /// Type of integers.
    //TODO: This should be part of some BaseTy or LiteralTy as opposed to a member of Type directly
    IntTy,
    /// A type variable, during type checking this may be either a unifier or a proper type variable
    VarTy(TV),
    /// A row type, this is specifically a closed row. Open rows are represented as VarTy
    RowTy(ClosedRow<'ctx, TV>),
    /// A function type
    FunTy(Ty<'ctx, TV>, Ty<'ctx, TV>),
}

/// Defines the default way to fold over something.
/// This is used by `TypeFoldable` and `FallibleTypeFold` to determine how to fold over something
/// when the trait implementator does not wish to use a custom traversal.
///
/// For example, the default fold over type is:
/// ```
/// fn try_default_fold<'ctx, F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
///     self,
///     fold: &mut F,
/// ) -> Result<Self::Out<'ctx, F::TypeVar>, F::Error> {
///     match self.deref() {
///         VarTy(ref var) => fold.try_fold_var(var),
///         IntTy => Ok(fold.ctx().mk_ty(TypeKind::IntTy)),
///         ErrorTy => Ok(fold.ctx().mk_ty(TypeKind::ErrorTy)),
///         FunTy(arg, ret) => {
///             let arg_ = arg.try_fold_with(fold)?;
///             let ret_ = ret.try_fold_with(fold)?;
///             Ok(fold.ctx().mk_ty(TypeKind::FunTy(arg_, ret_)))
///         }
///         RowTy(row) => {
///             let row_ = row.try_fold_with(fold)?;
///             Ok(fold.ctx().mk_ty(TypeKind::RowTy(row_)))
///         }
///     }
/// }
/// ```
/// So by default we visit each type and return it as is without modification.
trait DefaultFold {
    type TypeVar;
    type Out<'a, TV: 'a>;

    fn try_default_fold<'ctx, F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<'ctx, F::TypeVar>, F::Error>;
}

impl<'ty, TV: Clone> DefaultFold for Ty<'ty, TV> {
    type TypeVar = TV;
    type Out<'a, T: 'a> = Ty<'a, T>;

    fn try_default_fold<'ctx, F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<'ctx, F::TypeVar>, F::Error> {
        match self.deref() {
            TypeKind::VarTy(ref var) => fold.try_fold_var(var.clone()),
            TypeKind::IntTy => Ok(fold.ctx().mk_ty(TypeKind::IntTy)),
            TypeKind::ErrorTy => Ok(fold.ctx().mk_ty(TypeKind::ErrorTy)),
            TypeKind::FunTy(arg, ret) => {
                let arg_ = arg.try_fold_with(fold)?;
                let ret_ = ret.try_fold_with(fold)?;
                Ok(fold.ctx().mk_ty(TypeKind::FunTy(arg_, ret_)))
            }
            TypeKind::RowTy(row) => {
                let row_ = row.try_fold_with(fold)?;
                Ok(fold.ctx().mk_ty(TypeKind::RowTy(row_)))
            }
        }
    }
}

/// Defines a fold over types and sub components of types.
/// This is commonly used to perform substitution.
///
/// Pairs with `TypeFoldable` to perform a type fold over arbitrary data containing types.
pub trait FallibleTypeFold<'ctx>: Sized {
    type Error;
    type TypeVar: 'ctx;
    type InTypeVar: Clone;

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
    ) -> Result<Ty<'ctx, Self::TypeVar>, Self::Error>;
}

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

impl<'ctx, 'ty, TV: Clone> TypeFoldable<'ctx> for Ty<'ty, TV> {
    type TypeVar = TV;
    type Out<T: 'ctx> = Ty<'ctx, T>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, InTypeVar = TV>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::TypeVar>, F::Error> {
        // If fold method is not specified we default to visiting every type via the `DefaultFold`
        // method.
        self.try_default_fold(fold)
    }
}
