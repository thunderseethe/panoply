//! Implementation details that are specific to type inference.
//! These are kept in their own module because they should not be used outside of the type checking module.
use std::{convert::Infallible, fmt, ops::Deref};

use ena::unify::{EqUnifyValue, UnifyKey, UnifyValue};
use pretty::DocAllocator;

use crate::ty::{
    row::{ClosedRow, Row, RowInternals, RowLabel},
    Ty, TypeKind,
};

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
pub type InferTy<'ctx> = Ty<InArena<'ctx>>;

impl<'ctx> EqUnifyValue for Ty<InArena<'ctx>> {}

pub(crate) mod arena {
    use crate::ty::{
        row::{ClosedRow, RowLabel},
        AccessTy, MkTy, Ty, TypeAlloc, TypeKind,
    };
    use crate::{
        memory::{
            handle::RefHandle,
            intern::{Interner, InternerByRef, SyncInterner},
        },
        ty::alloc::IteratorSorted,
    };
    use bumpalo::Bump;

    use super::TcUnifierVar;

    pub struct TyCtx<'ctx> {
        tys: SyncInterner<'ctx, TypeKind<InArena<'ctx>>, Bump>,
        row_fields: SyncInterner<'ctx, [RowLabel], Bump>,
        row_values: SyncInterner<'ctx, [Ty<InArena<'ctx>>], Bump>,
        db: &'ctx dyn crate::Db,
    }

    /// Allocate our type structs in an Arena.
    #[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
    pub struct InArena<'ctx>(std::marker::PhantomData<&'ctx ()>);
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
pub use arena::{InArena, TyCtx};

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

    pub fn try_to_row(&self) -> Result<Row<InArena<'ctx>>, Self> {
        match self.deref() {
            TypeKind::RowTy(row) => Ok(Row::Closed(*row)),
            TypeKind::VarTy(var) => Ok(Row::Open(*var)),
            _ => Err(*self),
        }
    }
}

impl<'ctx> EqUnifyValue for ClosedRow<InArena<'ctx>> {}

pub type InferRow<'ctx> = Row<InArena<'ctx>>;

impl<'ctx> UnifyValue for InferRow<'ctx> {
    type Error = (Self, Self);

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
                .ok_or((*left, *right)),
        }
    }
}

pub struct RowsNotDisjoint<'ctx> {
    /// Left row that was expected to be disjoint
    pub left: ClosedRow<InArena<'ctx>>,
    /// Right row that was expected to be disjoint
    pub right: ClosedRow<InArena<'ctx>>,
    /// The label left and right both contain
    pub label: RowLabel,
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
    ) -> Result<RowInternals<InArena<'ctx>>, RowsNotDisjoint<'ctx>> {
        self._disjoint_union(right, &(), |left, right, lbl| RowsNotDisjoint {
            left,
            right,
            label: *lbl,
        })
    }
}

#[cfg(test)]
mod tests {
    use pretty::{DocAllocator, Pretty};

    use crate::id::TyVarId;

    use crate::ty::TypeKind::*;
    use crate::ty::{row::Row, MkTy};

    #[derive(Default)]
    #[salsa::db(crate::Jar)]
    struct TestDatabase {
        storage: salsa::Storage<Self>,
    }
    impl salsa::Database for TestDatabase {}

    impl<'a, D> Pretty<'a, D> for TyVarId
    where
        D: DocAllocator<'a>,
    {
        fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
            alloc
                .text("ty_var")
                .append(alloc.as_string(self.0).angles())
        }
    }

    #[test]
    fn test_ty_pretty_printing() {
        //let arena = Bump::new();
        let db = TestDatabase::default();
        //let ctx: TyCtx<'_> = TyCtx::new(&db, &arena);

        let int = db.mk_ty(IntTy);
        let row = db.mk_row(
            &[db.mk_label("x"), db.mk_label("y"), db.mk_label("z")],
            &[int, int, int],
        );

        let ty = db.mk_ty(FunTy(
            db.mk_ty(ProdTy(Row::Closed(row))),
            db.mk_ty(VarTy(TyVarId(0))),
        ));
        let arena: pretty::Arena<'_, ()> = pretty::Arena::new();
        let mut out = String::new();
        ty.pretty(&arena, &db, &&db)
            .into_doc()
            .render_fmt(32, &mut out)
            .unwrap();
        assert_eq!(
            out,
            r#"{ x |> Int, y |> Int, z |> Int }
  -> ty_var<0>"#
        );
        let mut out = String::new();
        ty.pretty(&arena, &db, &&db)
            .into_doc()
            .render_fmt(10, &mut out)
            .unwrap();
        assert_eq!(
            out,
            r#"{ x |> Int
, y |> Int
, z |> Int
} ->
  ty_var<0>"#
        );
    }
}
