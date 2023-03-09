use std::convert::Infallible;
use std::fmt::{self, Debug};
use std::hash::Hash;
use std::ops::Deref;

use aiahr_core::ident::Ident;

use ena::unify::{EqUnifyValue, UnifyKey};

pub mod row;
use pretty::{docs, DocAllocator, DocBuilder};
use row::*;

pub mod fold;
use fold::*;
use salsa::DebugWithDb;

use crate::{InArena, InDb, TyCtx};

#[derive(Debug, PartialEq, Eq)]
pub struct UnifierToTcVarError {
    index: u32,
}
#[derive(Debug, PartialEq, Eq)]
pub struct TcVarToUnifierError {
    index: u32,
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
pub trait MkTy<A: TypeAlloc<TypeKind<A>>> {
    fn mk_ty(&self, kind: TypeKind<A>) -> Ty<A>;
    fn mk_label(&self, label: &str) -> RowLabel;
    fn mk_row(&self, fields: &[RowLabel], values: &[Ty<A>]) -> ClosedRow<A>;

    fn empty_row(&self) -> ClosedRow<A> {
        self.mk_row(&[], &[])
    }
    fn empty_row_ty(&self) -> Ty<A> {
        self.mk_ty(TypeKind::RowTy(self.empty_row()))
    }
    fn single_row(&self, label: Ident, value: Ty<A>) -> ClosedRow<A> {
        self.mk_row(&[label], &[value])
    }
    fn single_row_ty(&self, label: Ident, value: Ty<A>) -> Ty<A> {
        self.mk_ty(TypeKind::RowTy(self.single_row(label, value)))
    }

    fn construct_row(&self, mut row: Vec<(RowLabel, Ty<A>)>) -> ClosedRow<A> {
        row.sort_by(|a, b| Ident::cmp(&a.0, &b.0));

        let mut fields = Vec::with_capacity(row.len());
        let mut values = Vec::with_capacity(row.len());
        for (k, v) in row {
            fields.push(k);
            values.push(v);
        }

        self.mk_row(&fields, &values)
    }
}

pub trait AccessTy<'a, A: TypeAlloc<TypeKind<A>>> {
    fn kind(&self, ty: &Ty<A>) -> &'a TypeKind<A>;
    fn row_fields(&self, row: &A::RowFields) -> &'a [RowLabel];
    fn row_values(&self, row: &A::RowValues) -> &'a [Ty<A>];
}
impl<'ctx> AccessTy<'ctx, InArena<'ctx>> for TyCtx<'ctx> {
    fn kind(&self, ty: &Ty<InArena<'ctx>>) -> &'ctx TypeKind<InArena<'ctx>> {
        let handle = ty.0;
        handle.0
    }

    fn row_fields(&self, fields: &<InArena<'ctx> as TypeAlloc>::RowFields) -> &'ctx [RowLabel] {
        fields.0
    }

    fn row_values(
        &self,
        values: &<InArena<'ctx> as TypeAlloc>::RowValues,
    ) -> &'ctx [Ty<InArena<'ctx>>] {
        values.0
    }
}
// Technically with arena alloc it's all refs so we don't need any context to access data.
impl<'ctx> AccessTy<'ctx, InArena<'ctx>> for () {
    fn kind(&self, ty: &Ty<InArena<'ctx>>) -> &'ctx TypeKind<InArena<'ctx>> {
        (ty.0).0
    }

    fn row_fields(&self, fields: &<InArena<'ctx> as TypeAlloc>::RowFields) -> &'ctx [RowLabel] {
        fields.0
    }

    fn row_values(
        &self,
        values: &<InArena<'ctx> as TypeAlloc>::RowValues,
    ) -> &'ctx [Ty<InArena<'ctx>>] {
        values.0
    }
}
impl<'db, DB> AccessTy<'db, InDb> for &'db DB
where
    DB: crate::Db,
{
    fn kind(&self, ty: &Ty<InDb>) -> &'db TypeKind<InDb> {
        ty.0.kind(*self)
    }

    fn row_fields(&self, row: &<InDb as TypeAlloc>::RowFields) -> &'db [RowLabel] {
        row.fields(*self).as_slice()
    }

    fn row_values(&self, row: &<InDb as TypeAlloc>::RowValues) -> &'db [Ty<InDb>] {
        row.values(*self).as_slice()
    }
}
impl<'db> AccessTy<'db, InDb> for &'db (dyn crate::Db + '_) {
    fn kind(&self, ty: &Ty<InDb>) -> &'db TypeKind<InDb> {
        ty.0.kind(*self)
    }

    fn row_fields(&self, row: &<InDb as TypeAlloc>::RowFields) -> &'db [RowLabel] {
        row.fields(*self)
    }

    fn row_values(&self, row: &<InDb as TypeAlloc>::RowValues) -> &'db [Ty<InDb>] {
        row.values(*self)
    }
}
/// Allow our type structs (Ty, ClosedRow, etc.) to be generic in how they are allocated
///
/// We have two allocation strategies we'd like to support. During unification we will allocate
/// everything in an arena that is dropped at the end of unification. This is to prevent any
/// unifiers from escaping unification which is an ICE.
///
/// Once unification completes we zonk all our types, generalizing them back to use type variables
/// where allowed. Once this is done we want to store them in the salsa database so that they are
/// incrementally recomputed.
///
/// We aim to achieve this by parameterizing our structs by this allocation trait, and then we can
/// implement unification only against these structs instantiated with our unification allocator.
pub trait TypeAlloc<TK = TypeKind<Self>> {
    type TypeData: Clone + PartialEq + Eq + PartialOrd + Ord + Hash + Debug;
    type RowFields: Clone + PartialEq + Eq + PartialOrd + Ord + Hash + Debug;
    type RowValues: Clone + PartialEq + Eq + PartialOrd + Ord + Hash + Debug;
    type TypeVar: Clone + PartialEq + Eq + PartialOrd + Ord + Hash + Debug;
}

pub(crate) type AllocVar<A> = <A as TypeAlloc>::TypeVar;

/// A monomorphic type.
///
/// This is just a wrapper around an interned reference to the `TypeKind` which contains the actual
/// data.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ty<A: TypeAlloc>(pub A::TypeData);

impl<A: TypeAlloc> Copy for Ty<A>
where
    A: Clone,
    A::TypeData: Copy,
{
}

impl<A: TypeAlloc> Debug for Ty<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
impl<Db> DebugWithDb<Db> for Ty<InDb>
where
    Db: crate::Db,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Db, _include_all_fields: bool) -> fmt::Result {
        self.0.kind(db).debug(db).fmt(f)
    }
}
impl DebugWithDb<dyn crate::Db + '_> for Ty<InDb> {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        db: &dyn crate::Db,
        _include_all_fields: bool,
    ) -> fmt::Result {
        self.0.kind(db).debug(db).fmt(f)
    }
}

impl<'ctx> Ty<InArena<'ctx>> {
    pub fn try_as_prod_row(self) -> Result<Row<InArena<'ctx>>, Self> {
        match self.deref() {
            TypeKind::ProdTy(Row::Closed(row)) | TypeKind::RowTy(row) => Ok(Row::Closed(*row)),
            TypeKind::ProdTy(Row::Open(var)) | TypeKind::VarTy(var) => Ok(Row::Open(var.clone())),
            _ => Err(self),
        }
    }

    pub fn try_as_sum_row(self) -> Result<Row<InArena<'ctx>>, Self> {
        match self.deref() {
            TypeKind::SumTy(Row::Closed(row)) | TypeKind::RowTy(row) => Ok(Row::Closed(*row)),
            TypeKind::SumTy(Row::Open(var)) | TypeKind::VarTy(var) => Ok(Row::Open(var.clone())),
            _ => Err(self),
        }
    }

    pub fn try_as_fn_ty(self) -> Result<(Self, Self), Self> {
        match self.deref() {
            TypeKind::FunTy(arg, ret) => Ok((*arg, *ret)),
            _ => Err(self),
        }
    }
}
impl Ty<InDb> {
    pub fn try_as_prod_row<'a>(self, db: &impl AccessTy<'a, InDb>) -> Result<Row<InDb>, Self> {
        match db.kind(&self) {
            TypeKind::ProdTy(Row::Closed(row)) | TypeKind::RowTy(row) => Ok(Row::Closed(*row)),
            TypeKind::ProdTy(Row::Open(var)) | TypeKind::VarTy(var) => Ok(Row::Open(var.clone())),
            _ => Err(self),
        }
    }

    pub fn try_as_sum_row<'a>(self, db: &impl AccessTy<'a, InDb>) -> Result<Row<InDb>, Self> {
        match db.kind(&self) {
            TypeKind::SumTy(Row::Closed(row)) | TypeKind::RowTy(row) => Ok(Row::Closed(*row)),
            TypeKind::SumTy(Row::Open(var)) | TypeKind::VarTy(var) => Ok(Row::Open(var.clone())),
            _ => Err(self),
        }
    }

    pub fn try_as_fn_ty<'a>(self, db: &impl AccessTy<'a, InDb>) -> Result<(Self, Self), Self> {
        match db.kind(&self) {
            TypeKind::FunTy(arg, ret) => Ok((*arg, *ret)),
            _ => Err(self),
        }
    }
}

impl<'ctx> Ty<InArena<'ctx>> {
    /// Convert a type to a row. If type is not representable as a row return type as an error.
    pub(crate) fn try_to_row(&self) -> Result<Row<InArena<'ctx>>, Self> {
        match self.deref() {
            TypeKind::RowTy(row) => Ok(Row::Closed(*row)),
            TypeKind::VarTy(var) => Ok(Row::Open(var.clone())),
            _ => Err(*self),
        }
    }
}

impl<A: TypeAlloc> Deref for Ty<A>
where
    A::TypeData: Deref,
{
    type Target = <A::TypeData as Deref>::Target;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}
impl<'ctx> Ty<InArena<'ctx>> {
    pub fn pretty<'a, D>(&self, a: &'a D, db: &dyn crate::Db) -> DocBuilder<'a, D>
    where
        D: ?Sized + DocAllocator<'a>,
        D::Doc: pretty::Pretty<'a, D> + Clone,
    {
        self.0.deref().pretty(a, db)
    }
}

/// During inference our type variables are all unification variables.
/// This is an alias to make inference types easy to talk about.
pub type InferTy<'ctx> = Ty<InArena<'ctx>>;

impl<'ctx> EqUnifyValue for Ty<InArena<'ctx>> {}

/// Data for `Ty`.
/// `TypeKind` is interned to produce a `Ty`.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeKind<A: TypeAlloc> {
    /// Marker that signifies an operation produced an error. This exists so that we can try to
    /// gracefully recover from a type checking error and produce a list of errors at the end of type checking
    ErrorTy,
    /// Type of integers.
    //TODO: This should be part of some BaseTy or LiteralTy as opposed to a member of Type directly
    IntTy,
    /// A type variable, during type checking this may be either a unifier or a proper type variable
    VarTy(A::TypeVar),
    /// A row type, this is specifically a closed row. Open rows are represented as VarTy
    RowTy(ClosedRow<A>),
    /// A function type
    FunTy(Ty<A>, Ty<A>),
    /// A product type. This is purely a wrapper type to coerce a row type to be a product.
    ProdTy(Row<A>),
    /// A sum type. This is purely a wrapper type to coerce a row type to be a sum.
    SumTy(Row<A>),
}
impl<A: TypeAlloc> Copy for TypeKind<A>
where
    A: Clone,
    A::TypeVar: Copy,
    ClosedRow<A>: Copy,
    Row<A>: Copy,
    Ty<A>: Copy,
{
}
impl<'ctx> Debug for TypeKind<InArena<'ctx>> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeKind::ErrorTy => f.debug_tuple("ErrorTy").finish(),
            TypeKind::IntTy => f.debug_tuple("IntTy").finish(),
            TypeKind::VarTy(var) => f.debug_tuple("VarTy").field(var).finish(),
            TypeKind::RowTy(row) => f.debug_tuple("RowTy").field(row).finish(),
            TypeKind::FunTy(arg, ret) => f.debug_tuple("FunTy").field(arg).field(ret).finish(),
            TypeKind::ProdTy(row) => f.debug_tuple("ProdTy").field(row).finish(),
            TypeKind::SumTy(row) => f.debug_tuple("SumTy").field(row).finish(),
        }
    }
}
impl Debug for TypeKind<InDb> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Use DebugWithDb for TypeKind<InDb>")
    }
}
impl<Db> DebugWithDb<Db> for TypeKind<InDb>
where
    Db: crate::Db,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Db, _include_all_fields: bool) -> fmt::Result {
        match self {
            TypeKind::ErrorTy => f.debug_tuple("ErrorTy").finish(),
            TypeKind::IntTy => f.debug_tuple("IntTy").finish(),
            TypeKind::VarTy(var) => f.debug_tuple("VarTy").field(var).finish(),
            TypeKind::RowTy(row) => f.debug_tuple("RowTy").field(&row.debug(db)).finish(),
            TypeKind::FunTy(arg, ret) => f
                .debug_tuple("FunTy")
                .field(&arg.debug(db))
                .field(&ret.debug(db))
                .finish(),
            TypeKind::ProdTy(row) => f.debug_tuple("ProdTy").field(&row.debug(db)).finish(),
            TypeKind::SumTy(row) => f.debug_tuple("SumTy").field(&row.debug(db)).finish(),
        }
    }
}
impl DebugWithDb<dyn crate::Db + '_> for TypeKind<InDb> {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        db: &dyn crate::Db,
        _include_all_fields: bool,
    ) -> fmt::Result {
        match self {
            TypeKind::ErrorTy => f.debug_tuple("ErrorTy").finish(),
            TypeKind::IntTy => f.debug_tuple("IntTy").finish(),
            TypeKind::VarTy(var) => f.debug_tuple("VarTy").field(var).finish(),
            TypeKind::RowTy(row) => f.debug_tuple("RowTy").field(&row.debug(db)).finish(),
            TypeKind::FunTy(arg, ret) => f
                .debug_tuple("FunTy")
                .field(&arg.debug(db))
                .field(&ret.debug(db))
                .finish(),
            TypeKind::ProdTy(row) => f.debug_tuple("ProdTy").field(&row.debug(db)).finish(),
            TypeKind::SumTy(row) => f.debug_tuple("SumTy").field(&row.debug(db)).finish(),
        }
    }
}

impl<'ctx> TypeKind<InArena<'ctx>> {
    fn pretty<'a, D>(&self, a: &'a D, db: &dyn crate::Db) -> DocBuilder<'a, D>
    where
        D: ?Sized + DocAllocator<'a>,
        D::Doc: pretty::Pretty<'a, D> + Clone,
    {
        match self {
            TypeKind::ErrorTy => a.as_string("Error"),
            TypeKind::IntTy => a.as_string("Int"),
            TypeKind::VarTy(tv) => pretty::Pretty::pretty(tv.clone(), a),
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

impl<'ctx, A: TypeAlloc + 'ctx> DefaultFold<'ctx> for TypeKind<A>
where
    A: Clone,
{
    type In = A;

    fn try_default_fold<F: FallibleTypeFold<'ctx, In = Self::In>>(
        &self,
        fold: &mut F,
    ) -> Result<Ty<F::Out>, F::Error> {
        Ok(match &self {
            TypeKind::VarTy(ref var) => fold.try_fold_var(var.clone())?,
            TypeKind::IntTy => fold.ctx().mk_ty(TypeKind::IntTy),
            TypeKind::ErrorTy => fold.ctx().mk_ty(TypeKind::ErrorTy),
            TypeKind::FunTy(ref arg, ref ret) => {
                let arg_ = arg.clone().try_fold_with(fold)?;
                let ret_ = ret.clone().try_fold_with(fold)?;
                fold.ctx().mk_ty(TypeKind::<F::Out>::FunTy(arg_, ret_))
            }
            TypeKind::RowTy(ref row) => {
                let row_: ClosedRow<F::Out> = row.clone().try_fold_with(fold)?;
                fold.ctx().mk_ty(TypeKind::<F::Out>::RowTy(row_))
            }
            TypeKind::ProdTy(ref row) => {
                let row_: Row<F::Out> = row.clone().try_fold_with(fold)?;
                fold.ctx().mk_ty(TypeKind::ProdTy(row_))
            }
            TypeKind::SumTy(ref row) => {
                let row_: Row<F::Out> = row.clone().try_fold_with(fold)?;
                fold.ctx().mk_ty(TypeKind::SumTy(row_))
            }
        })
    }
}

impl<'ctx, A: TypeAlloc + Clone + 'ctx> TypeFoldable<'ctx> for Ty<A>
where
    A: Clone,
{
    type Alloc = A;
    type Out<B: TypeAlloc> = Ty<B>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, In = Self::Alloc>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::Out>, F::Error> {
        fold.access().kind(&self).try_default_fold(fold)
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
        let ctx: TyCtx<'_> = TyCtx::new(&db, &arena);

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
