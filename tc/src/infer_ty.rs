//! Implementation details that are specific to type inference.
//! These are kept in their own module because they should not be used outside of the type checking module.
use std::{convert::Infallible, fmt, ops::Deref};

use ena::unify::{EqUnifyValue, UnifyKey, UnifyValue};
use pretty::{docs, DocAllocator, DocBuilder};

use crate::{
    diagnostic::TypeCheckError,
    ty::row::{OrderedRowXorRow, RowInternals, RowLabel, UnsolvedRowEquation},
    ClosedRow, Evidence, Row, Ty, TypeKind,
};

use self::arena::InArena;

/// A unifier variable.
/// These are produced during the type checking process and MUST NOT persist outside the type
/// checker. They may not appear in the AST once type checking is completed and are removed by
/// zonking.
/// Conversely to the untouchable TcVar, these are "touchable" and will be modified by the type
/// checker.
#[derive(Default, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub(crate) struct TcUnifierVar<'ctx> {
    id: u32,
    _marker: std::marker::PhantomData<&'ctx ()>,
}
impl<'ctx> fmt::Debug for TcUnifierVar<'ctx> {
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

/// During inference our type variables are all unification variables.
/// This is an alias to make inference types easy to talk about.
pub(crate) type InferTy<'ctx> = Ty<InArena<'ctx>>;

impl<'ctx> EqUnifyValue for Ty<InArena<'ctx>> {}

pub(crate) mod arena {
    use aiahr_core::memory::{
        handle::RefHandle,
        intern::{Interner, InternerByRef, SyncInterner},
    };
    use bumpalo::Bump;

    use crate::{
        ty::{
            alloc::{AccessTy, IteratorSorted, MkTy},
            row::RowLabel,
        },
        ClosedRow, Ty, TypeAlloc, TypeKind,
    };

    use super::TcUnifierVar;

    pub(crate) struct TyCtx<'ctx> {
        tys: SyncInterner<'ctx, TypeKind<InArena<'ctx>>, Bump>,
        row_fields: SyncInterner<'ctx, [RowLabel], Bump>,
        row_values: SyncInterner<'ctx, [Ty<InArena<'ctx>>], Bump>,
        db: &'ctx dyn crate::Db,
    }

    /// Allocate our type structs in an Arena.
    #[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
    pub(crate) struct InArena<'ctx>(std::marker::PhantomData<&'ctx ()>);
    impl Copy for InArena<'_>
    where
        <Self as TypeAlloc>::TypeData: Copy,
        <Self as TypeAlloc>::TypeVar: Copy,
        <Self as TypeAlloc>::RowFields: Copy,
        <Self as TypeAlloc>::RowValues: Copy,
    {
    }

    impl<'ctx> TypeAlloc<TypeKind<Self>> for InArena<'ctx> {
        type TypeData = RefHandle<'ctx, TypeKind<Self>>;

        type RowFields = RefHandle<'ctx, [RowLabel]>;

        type RowValues = RefHandle<'ctx, [Ty<Self>]>;

        type TypeVar = TcUnifierVar<'ctx>;
    }

    impl<'ctx> TyCtx<'ctx> {
        pub fn new(db: &'ctx dyn crate::Db, arena: &'ctx Bump) -> Self {
            Self {
                tys: SyncInterner::new(arena),
                row_fields: SyncInterner::new(arena),
                row_values: SyncInterner::new(arena),
                db,
            }
        }
    }

    impl<'ctx> MkTy<InArena<'ctx>> for TyCtx<'ctx>
    where
        TypeKind<InArena<'ctx>>: Copy,
    {
        fn mk_ty(&self, kind: TypeKind<InArena<'ctx>>) -> Ty<InArena<'ctx>> {
            Ty(self.tys.intern(kind))
        }

        fn mk_label(&self, label: &str) -> RowLabel {
            self.db.ident_str(label)
        }

        fn mk_row(
            &self,
            fields: &[RowLabel],
            values: &[Ty<InArena<'ctx>>],
        ) -> ClosedRow<InArena<'ctx>> {
            debug_assert!(
                fields.len() == values.len(),
                "Expected row fields and valuse to be the same length"
            );
            debug_assert!(
                fields.iter().considered_sorted(),
                "Expected row fields to be sorted"
            );
            ClosedRow {
                fields: self.row_fields.intern_by_ref(fields),
                values: self.row_values.intern_by_ref(values),
            }
        }
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
            TypeKind::VarTy(tv) => pretty::Pretty::pretty(*tv, a),
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

impl<'ctx> fmt::Debug for TypeKind<InArena<'ctx>> {
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

impl<'ctx> Ty<InArena<'ctx>> {
    pub fn try_as_prod_row(self) -> Result<Row<InArena<'ctx>>, Self> {
        match self.deref() {
            TypeKind::ProdTy(Row::Closed(row)) | TypeKind::RowTy(row) => Ok(Row::Closed(*row)),
            TypeKind::ProdTy(Row::Open(var)) | TypeKind::VarTy(var) => Ok(Row::Open(*var)),
            _ => Err(self),
        }
    }

    pub fn try_as_sum_row(self) -> Result<Row<InArena<'ctx>>, Self> {
        match self.deref() {
            TypeKind::SumTy(Row::Closed(row)) | TypeKind::RowTy(row) => Ok(Row::Closed(*row)),
            TypeKind::SumTy(Row::Open(var)) | TypeKind::VarTy(var) => Ok(Row::Open(*var)),
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

impl<'ctx> Ty<InArena<'ctx>> {
    /// Convert a type to a row. If type is not representable as a row return type as an error.
    pub(crate) fn try_to_row(&self) -> Result<Row<InArena<'ctx>>, Self> {
        match self.deref() {
            TypeKind::RowTy(row) => Ok(Row::Closed(*row)),
            TypeKind::VarTy(var) => Ok(Row::Open(*var)),
            _ => Err(*self),
        }
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

impl<'ctx> ClosedRow<InArena<'ctx>> {
    pub(crate) fn pretty<'a, D>(&self, a: &'a D, db: &dyn crate::Db) -> DocBuilder<'a, D>
    where
        D: ?Sized + DocAllocator<'a>,
        D::Doc: pretty::Pretty<'a, D> + Clone,
    {
        let docs = self
            .fields
            .iter()
            .zip(self.values.iter())
            .map(|(field, value)| {
                docs![
                    a,
                    a.as_string(field.text(db.as_core_db())),
                    a.space(),
                    "|>",
                    a.softline(),
                    value.pretty(a, db)
                ]
                .group()
            });
        a.intersperse(
            docs,
            a.concat([a.softline_(), a.as_string(","), a.space()])
                .into_doc(),
        )
    }
}
impl<'ctx> EqUnifyValue for ClosedRow<InArena<'ctx>> {}
pub(crate) type InferRow<'ctx> = Row<InArena<'ctx>>;
impl<'ctx> From<OrderedRowXorRow<InArena<'ctx>>> for (Row<InArena<'ctx>>, Row<InArena<'ctx>>) {
    fn from(val: OrderedRowXorRow<InArena<'ctx>>) -> Self {
        match val {
            OrderedRowXorRow::ClosedOpen(row, var) => (Row::Closed(row), Row::Open(var)),
            OrderedRowXorRow::OpenOpen { min, max } => (Row::Open(min), Row::Open(max)),
        }
    }
}
impl<'ctx> TryFrom<(Row<InArena<'ctx>>, Row<InArena<'ctx>>)> for OrderedRowXorRow<InArena<'ctx>> {
    type Error = (ClosedRow<InArena<'ctx>>, ClosedRow<InArena<'ctx>>);

    fn try_from(value: (Row<InArena<'ctx>>, Row<InArena<'ctx>>)) -> Result<Self, Self::Error> {
        match value {
            (Row::Open(l), Row::Open(r)) => Ok(Self::with_open_open(l, r)),
            (Row::Open(tv), Row::Closed(row)) | (Row::Closed(row), Row::Open(tv)) => {
                Ok(Self::ClosedOpen(row, tv))
            }
            (Row::Closed(l), Row::Closed(r)) => Err((l, r)),
        }
    }
}
impl<'ctx> From<UnsolvedRowEquation<InArena<'ctx>>> for Evidence<InArena<'ctx>> {
    fn from(eq: UnsolvedRowEquation<InArena<'ctx>>) -> Self {
        match eq {
            UnsolvedRowEquation::ClosedGoal(cand) => Evidence::Row {
                left: Row::Open(cand.min),
                right: Row::Open(cand.max),
                goal: Row::Closed(cand.goal),
            },
            UnsolvedRowEquation::OpenGoal(cand) => match cand.orxr {
                OrderedRowXorRow::ClosedOpen(closed, open) => Evidence::Row {
                    left: Row::Closed(closed),
                    right: Row::Open(open),
                    goal: Row::Open(cand.goal),
                },
                OrderedRowXorRow::OpenOpen { min, max } => Evidence::Row {
                    left: Row::Open(min),
                    right: Row::Open(max),
                    goal: Row::Open(cand.goal),
                },
            },
        }
    }
}

impl<'ctx> Row<InArena<'ctx>> {
    pub(crate) fn pretty<'a, D>(
        &self,
        allocator: &'a D,
        db: &dyn crate::Db,
    ) -> pretty::DocBuilder<'a, D>
    where
        D: ?Sized + DocAllocator<'a>,
        D::Doc: pretty::Pretty<'a, D> + Clone,
    {
        match self {
            Row::Open(tv) => pretty::Pretty::pretty(tv.clone(), allocator),
            Row::Closed(row) => row.pretty(allocator, db),
        }
    }
}

impl<'ctx> UnifyValue for InferRow<'ctx> {
    type Error = TypeCheckError<'ctx>;

    fn unify_values(left: &Self, right: &Self) -> Result<Self, Self::Error> {
        match (left, right) {
            (Row::Open(left_var), Row::Open(right_var)) => {
                Ok(Row::Open(std::cmp::min(*left_var, *right_var)))
            }
            // Prefer the more solved row if possible
            (Row::Open(_), Row::Closed(_)) => Ok(*right),
            (Row::Closed(_), Row::Open(_)) => Ok(*left),
            (Row::Closed(left_row), Row::Closed(right_row)) => (left_row == right_row)
                .then_some(*left)
                .ok_or(TypeCheckError::RowsNotEqual(*left, *right)),
        }
    }
}

impl<'ctx> ClosedRow<InArena<'ctx>> {
    /// Create a new row that contains all self fields that are not present in sub.
    pub fn difference(self, sub: Self) -> (Box<[RowLabel]>, Box<[Ty<InArena<'ctx>>]>) {
        let out_row = self
            .fields
            .iter()
            .zip(self.values.iter())
            .filter(|(field, _)| sub.fields.binary_search_by(|lbl| lbl.cmp(field)).is_err());

        let (mut fields, mut values) = (Vec::new(), Vec::new());
        for (field, value) in out_row {
            fields.push(*field);
            values.push(*value);
        }
        (fields.into_boxed_slice(), values.into_boxed_slice())
    }

    /// Combine two disjoint rows into a new row.
    /// This maintains the row invariants in the resulting row.
    /// If called on two overlapping rows an error is thrown.
    pub fn disjoint_union(
        self,
        right: Self,
    ) -> Result<RowInternals<InArena<'ctx>>, TypeCheckError<'ctx>> {
        self._disjoint_union(right, &(), |left, right, lbl| {
            TypeCheckError::RowsNotDisjoint(left, right, *lbl)
        })
    }
}
