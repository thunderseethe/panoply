use std::fmt::{self, Debug};
use std::ops::Deref;

use aiahr_core::define_ids;
use aiahr_core::memory::handle::RefHandle;

use ena::unify::{EqUnifyValue, UnifyKey};

pub mod row;
use row::*;

pub mod fold;
use fold::*;

use crate::Candidate;

define_ids!(
/// A type variable.
/// These are explicity referred to by the AST and can persist through type checking.
/// They may not be modified by the type checking process, often referred to as untouchabale.
#[derive(Default, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub TcVar;
);

#[derive(Debug, PartialEq, Eq)]
pub struct UnifierToTcVarError {
    index: u32,
}
#[derive(Debug, PartialEq, Eq)]
pub struct TcVarToUnifierError {
    index: u32,
}

impl<'infer> TryFrom<TcUnifierVar<'infer>> for TcVar {
    type Error = UnifierToTcVarError;

    fn try_from(value: TcUnifierVar<'infer>) -> Result<Self, Self::Error> {
        Err(UnifierToTcVarError {
            index: value.index(),
        })
    }
}


impl<'infer> TryFrom<TcVar> for TcUnifierVar<'infer> {
    type Error = TcVarToUnifierError;

    fn try_from(value: TcVar) -> Result<Self, Self::Error> {
        Err(TcVarToUnifierError { index: value.0 as u32 })
    }
}

/// A unifier variable.
/// These are produced during the type checking process and MUST NOT persist outside the type
/// checker. They may not appear in the AST once type checking is completed and are removed by
/// zonking.
/// Conversely to the untouchable TcVar, these are "touchable" and will be modified by the type
/// checker.
#[derive(Default, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct TcUnifierVar<'ctx> {
    id: u32,
    _marker: std::marker::PhantomData<&'ctx ()>,
}
impl<'ctx> Debug for TcUnifierVar<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("TcUnifierVar").field(&self.id).finish()
    }
}
impl<'ctx> UnifyKey for TcUnifierVar<'ctx> {
    type Value = Option<Candidate<'ctx, TcUnifierVar<'ctx>>>;

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
    fn mk_row(&self, fields: &[RowLabel<'ctx>], values: &[Ty<'ctx, TV>]) -> ClosedRow<'ctx, TV>;

    fn empty_row(&self) -> ClosedRow<'ctx, TV> {
        self.mk_row(&[], &[])
    }
    fn empty_row_ty(&self) -> Ty<'ctx, TV> {
        self.mk_ty(TypeKind::RowTy(self.empty_row()))
    }
    fn single_row(&self, label: &str, value: Ty<'ctx, TV>) -> ClosedRow<'ctx, TV> {
        let field = self.mk_label(label);
        self.mk_row(&[field], &[value])
    }
    fn single_row_ty(&self, label: &str, value: Ty<'ctx, TV>) -> Ty<'ctx, TV> {
        self.mk_ty(TypeKind::RowTy(self.single_row(label, value)))
    }

    fn construct_row(&self, mut row: Vec<(RowLabel<'ctx>, Ty<'ctx, TV>)>) -> ClosedRow<'ctx, TV> {
        row.sort_by(|a, b| str::cmp(&a.0, &b.0));

        let fields = row.iter().map(|(k, _)| k).cloned().collect::<Vec<_>>();
        let values = row.iter().map(|(_, v)| v).cloned().collect::<Vec<_>>();
        
        self.mk_row(&fields, &values)
    }
}

/// A monomorphic type.
///
/// This is just a wrapper around an interned reference to the `TypeKind` which contains the actual
/// data.
#[derive(Hash)]
pub struct Ty<'ctx, TV>(pub RefHandle<'ctx, TypeKind<'ctx, TV>>);
impl<'ctx, TV> PartialEq for Ty<'ctx, TV> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<'ctx, TV> Eq for Ty<'ctx, TV> {}

impl<'ctx, TV: Clone> Ty<'ctx, TV> {
    /// Convert a type to a row. If type is not representable as a row return type as an error.
    pub(crate) fn try_to_row(&self) -> Result<Row<'ctx, TV>, Ty<'ctx, TV>> {
        match self.deref() {
            TypeKind::RowTy(row) => Ok(Row::Closed(*row)),
            TypeKind::VarTy(var) => Ok(Row::Open(var.clone())),
            _ => Err(*self),
        }
    }
}

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
        self.0 .0.fmt(f)
    }
}

/// During inference our type variables are all unification variables.
/// This is an alias to make inference types easy to talk about.
pub type InferTy<'ctx> = Ty<'ctx, TcUnifierVar<'ctx>>;

impl<'ctx> EqUnifyValue for Ty<'ctx, TcUnifierVar<'ctx>> {}

impl<'ty, TV> Into<Ty<'ty, TV>> for RefHandle<'ty, TypeKind<'ty, TV>> {
    fn into(self) -> Ty<'ty, TV> {
        Ty(self)
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
    /// A product type. This is purely a wrapper type to coerce a row type to be a product.
    ProdTy(Row<'ctx, TV>),
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
            TypeKind::ProdTy(row) => {
                let row_ = row.clone().try_fold_with(fold)?;
                Ok(fold.ctx().mk_ty(TypeKind::ProdTy(row_)))
            }
        }
    }
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
