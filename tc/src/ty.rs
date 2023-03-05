use std::convert::Infallible;
use std::fmt::{self, Debug};
use std::ops::Deref;

use aiahr_core::id::TyVarId;
use aiahr_core::ident::Ident;
use aiahr_core::memory::handle::RefHandle;

use ena::unify::{EqUnifyValue, UnifyKey};

pub mod row;
use pretty::{docs, DocAllocator, DocBuilder};
use row::*;

pub mod fold;
use fold::*;

#[derive(Debug, PartialEq, Eq)]
pub struct UnifierToTcVarError {
    index: u32,
}
#[derive(Debug, PartialEq, Eq)]
pub struct TcVarToUnifierError {
    index: u32,
}

impl<'infer> TryFrom<TcUnifierVar<'infer>> for TyVarId {
    type Error = UnifierToTcVarError;

    fn try_from(value: TcUnifierVar<'infer>) -> Result<Self, Self::Error> {
        Err(UnifierToTcVarError {
            index: value.index(),
        })
    }
}

impl<'infer> TryFrom<TyVarId> for TcUnifierVar<'infer> {
    type Error = TcVarToUnifierError;

    fn try_from(value: TyVarId) -> Result<Self, Self::Error> {
        Err(TcVarToUnifierError {
            index: value.0 as u32,
        })
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
impl<'ctx> From<Infallible> for TcUnifierVar<'ctx> {
    fn from(_: Infallible) -> Self {
        unreachable!()
    }
}

impl<'a, 'ctx, A, D> pretty::Pretty<'a, D, A> for TcUnifierVar<'ctx>
where
    A: 'a,
    D: ?Sized + DocAllocator<'a, A>,
{
    fn pretty(self, a: &'a D) -> pretty::DocBuilder<'a, D, A> {
        "tv".pretty(a).append(a.as_string(self.id).angles()).group()
    }
}

impl<'ctx> UnifyKey for TcUnifierVar<'ctx> {
    type Value = Option<InferTy<'ctx>>;

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
    fn single_row(&self, label: Ident, value: Ty<'ctx, TV>) -> ClosedRow<'ctx, TV> {
        self.mk_row(&[label], &[value])
    }
    fn single_row_ty(&self, label: Ident, value: Ty<'ctx, TV>) -> Ty<'ctx, TV> {
        self.mk_ty(TypeKind::RowTy(self.single_row(label, value)))
    }

    fn construct_row(&self, mut row: Vec<(RowLabel<'ctx>, Ty<'ctx, TV>)>) -> ClosedRow<'ctx, TV> {
        row.sort_by(|a, b| Ident::cmp(&a.0, &b.0));

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

impl<'ctx, TV: Clone> Ty<'ctx, TV> {
    pub fn try_as_prod_row(self) -> Result<Row<'ctx, TV>, Ty<'ctx, TV>> {
        match self.deref() {
            TypeKind::ProdTy(Row::Closed(row)) | TypeKind::RowTy(row) => Ok(Row::Closed(*row)),
            TypeKind::ProdTy(Row::Open(var)) | TypeKind::VarTy(var) => Ok(Row::Open(var.clone())),
            _ => Err(self),
        }
    }

    pub fn try_as_sum_row(self) -> Result<Row<'ctx, TV>, Ty<'ctx, TV>> {
        match self.deref() {
            TypeKind::SumTy(Row::Closed(row)) | TypeKind::RowTy(row) => Ok(Row::Closed(*row)),
            TypeKind::SumTy(Row::Open(var)) | TypeKind::VarTy(var) => Ok(Row::Open(var.clone())),
            _ => Err(self),
        }
    }

    pub fn try_as_fn_ty(self) -> Result<(Ty<'ctx, TV>, Ty<'ctx, TV>), Ty<'ctx, TV>> {
        match self.deref() {
            TypeKind::FunTy(arg, ret) => Ok((*arg, *ret)),
            _ => Err(self),
        }
    }
}
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
impl<'ctx, TV> Ty<'ctx, TV> {
    pub fn pretty<'a, D>(&self, a: &'a D, db: &dyn crate::Db) -> DocBuilder<'a, D>
    where
        D: ?Sized + DocAllocator<'a>,
        D::Doc: pretty::Pretty<'a, D> + Clone,
        TV: pretty::Pretty<'a, D> + Clone,
    {
        self.0 .0.pretty(a, db)
    }
}

/// During inference our type variables are all unification variables.
/// This is an alias to make inference types easy to talk about.
pub type InferTy<'ctx> = Ty<'ctx, TcUnifierVar<'ctx>>;

impl<'ctx> EqUnifyValue for Ty<'ctx, TcUnifierVar<'ctx>> {}

impl<'ty, TV> From<RefHandle<'ty, TypeKind<'ty, TV>>> for Ty<'ty, TV> {
    fn from(handle: RefHandle<'ty, TypeKind<'ty, TV>>) -> Self {
        Ty(handle)
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
    /// A sum type. This is purely a wrapper type to coerce a row type to be a sum.
    SumTy(Row<'ctx, TV>),
}

impl<'ctx, TV> TypeKind<'ctx, TV> {
    fn pretty<'a, D>(&self, a: &'a D, db: &dyn crate::Db) -> DocBuilder<'a, D>
    where
        D: ?Sized + DocAllocator<'a>,
        D::Doc: pretty::Pretty<'a, D> + Clone,
        TV: pretty::Pretty<'a, D> + Clone,
    {
        match self {
            TypeKind::ErrorTy => a.as_string("Error"),
            TypeKind::IntTy => a.as_string("Int"),
            TypeKind::VarTy(tv) => tv.clone().pretty(a),
            TypeKind::RowTy(closed_row) => closed_row.pretty(a, db).nest(2).parens().group(),
            TypeKind::FunTy(arg, ret) => arg
                .pretty(a, db)
                .append(docs![a, a.softline(), "->", a.softline(), ret.pretty(a, db)].nest(2)),
            TypeKind::ProdTy(row) => row
                .pretty(a, db)
                .enclose(a.softline(), a.softline())
                .braces()
                .group(),
            TypeKind::SumTy(row) => row
                .pretty(a, db)
                .enclose(a.softline(), a.softline())
                .angles()
                .group(),
        }
    }
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
            TypeKind::SumTy(row) => {
                let row_ = row.clone().try_fold_with(fold)?;
                Ok(fold.ctx().mk_ty(TypeKind::SumTy(row_)))
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

#[cfg(test)]
mod tests {
    use bumpalo::Bump;
    use ena::unify::UnifyKey;

    use crate::tests::TestDatabase;
    use crate::TypeKind::*;
    use crate::{MkTy, Row, TyCtx};

    use super::TcUnifierVar;

    #[test]
    fn test_ty_pretty_printing() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let ctx: TyCtx<'_, TcUnifierVar<'_>> = TyCtx::new(&db, &arena);

        let int = ctx.mk_ty(IntTy);
        let row = ctx.mk_row(
            &[ctx.mk_label("x"), ctx.mk_label("y"), ctx.mk_label("z")],
            &[int, int, int],
        );

        let ty = ctx.mk_ty(FunTy(
            ctx.mk_ty(ProdTy(Row::Closed(row))),
            ctx.mk_ty(VarTy(TcUnifierVar::from_index(0))),
        ));
        let arena: pretty::Arena<'_, ()> = pretty::Arena::new();
        let mut out = String::new();
        (&ty)
            .pretty(&arena, &db)
            .into_doc()
            .render_fmt(32, &mut out)
            .unwrap();
        assert_eq!(
            out,
            r#"{ x |> Int, y |> Int, z |> Int }
  -> tv<0>"#
        );
        let mut out = String::new();
        (&ty)
            .pretty(&arena, &db)
            .into_doc()
            .render_fmt(10, &mut out)
            .unwrap();
        assert_eq!(
            out,
            r#"{ x |> Int
, y |> Int
, z |> Int
} -> tv<0>"#
        );
    }
}
