use std::fmt;
use std::ops::Deref;

use aiahr_core::define_ids;
use aiahr_core::id::VarId;
use aiahr_core::memory::handle::RefHandle;

use ena::unify::{UnifyKey, EqUnifyValue};

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
/// chcker.
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
        Self { id, _marker: std::marker::PhantomData }
    }

    fn tag() -> &'static str {
        "TcUnifierVar"
    }
}

/// A trait for allocators that can make types out of type kinds.
pub trait MkTy<'ctx, TV> {
    fn mk_ty(&self, kind: TypeKind<'ctx, TV>) -> Ty<'ctx, TV>;
}

/// During inference our type variables are all unification variables.
/// This is an alias to make those types easy to talk about.
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
        f.debug_tuple("Ty").field(&self.0.0).finish()
    }
}

impl<'ctx> EqUnifyValue for Ty<'ctx, TcUnifierVar<'ctx>> {}

impl<'ty, TV> Into<Ty<'ty, TV>> for RefHandle<'ty, TypeKind<'ty, TV>> {
    fn into(self) -> Ty<'ty, TV> {
        Ty(self)
    }
}

/// Kinds of types
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
    /// A function type
    FunTy(Ty<'ctx, TV>, Ty<'ctx, TV>),
}

trait DefaultFold {
    type TV;

    fn try_default_fold<'ast, 'ty, F: FallibleTypeFold<'ty, InTypeVar = Self::TV>>(self, fold: &mut F) -> Result<Ty<'ty, F::TypeVar>, F::Error>;
}

impl<'ty, TV: Clone> DefaultFold for Ty<'ty, TV> {
    type TV = TV;
    fn try_default_fold<'ast, 'ctx, F: FallibleTypeFold<'ctx, InTypeVar = Self::TV>>(self, fold: &mut F) -> Result<Ty<'ctx, F::TypeVar>, F::Error> {
        match self.deref() {
            TypeKind::VarTy(ref var) => fold.try_fold_var(var.clone()),
            TypeKind::IntTy => Ok(fold.ctx().mk_ty(TypeKind::IntTy)),
            TypeKind::ErrorTy => Ok(fold.ctx().mk_ty(TypeKind::ErrorTy)),
            TypeKind::FunTy(arg, ret) => {
                let arg_ = arg.try_fold_with(fold)?;
                let ret_ = ret.try_fold_with(fold)?;
                Ok(fold.ctx().mk_ty(TypeKind::FunTy(arg_, ret_)))
            },
        }
    }
}

/// A variable marked with it's type
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct TypedVarId<'ty, TV> {
    pub var: VarId,
    pub ty: Ty<'ty, TV>,
}

pub trait FallibleTypeFold<'ctx>: Sized {
    type Error;
    type TypeVar;
    type InTypeVar: Clone;

    fn ctx(&self) -> &dyn MkTy<'ctx, Self::TypeVar>;

    fn try_fold_ty<'a>(&mut self, t: Ty<'a, Self::InTypeVar>) -> Result<Ty<'ctx, Self::TypeVar>, Self::Error> {
        t.try_default_fold(self)
    }

    fn try_fold_var(&mut self, var: Self::InTypeVar) -> Result<Ty<'ctx, Self::TypeVar>, Self::Error>;
}

pub trait TypeFoldable<'ty>: Sized {
    type TypeVar;

    fn try_fold_with<'ast, F: FallibleTypeFold<'ty, InTypeVar=Self::TypeVar>>(self, fold: &mut F) -> Result<Ty<'ty, F::TypeVar>, F::Error>;
}

impl<'ctx, 'ty, TV: Clone> TypeFoldable<'ctx> for Ty<'ty, TV> {
    type TypeVar = TV;

    fn try_fold_with<'ast, F: FallibleTypeFold<'ctx, InTypeVar=TV>>(self, fold: &mut F) -> Result<Ty<'ctx, F::TypeVar>, F::Error> {
        self.try_default_fold(fold)
    }
}
