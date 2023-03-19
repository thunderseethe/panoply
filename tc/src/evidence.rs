use salsa::DebugWithDb;

use crate::{
    infer_ty::arena::InArena,
    ty::{
        alloc::{db::InDb, TypeAlloc},
        fold::{FallibleTypeFold, TypeFoldable},
    },
    Row,
};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Evidence<A: TypeAlloc> {
    Row {
        left: Row<A>,
        right: Row<A>,
        goal: Row<A>,
    },
}
impl<A: TypeAlloc> Copy for Evidence<A>
where
    A: Clone,
    Row<A>: Copy,
{
}
impl<'ctx> std::fmt::Debug for Evidence<InArena<'ctx>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Evidence::Row { left, right, goal } => f
                .debug_struct("Evidence::Row")
                .field("left", &left)
                .field("right", &right)
                .field("goal", &goal)
                .finish(),
        }
    }
}
impl std::fmt::Debug for Evidence<InDb> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Use DebugWithDb for Evidence<InDb>.")
    }
}
impl<Db> DebugWithDb<Db> for Evidence<InDb>
where
    Db: crate::Db,
{
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &Db,
        _include_all_fields: bool,
    ) -> std::fmt::Result {
        match self {
            Evidence::Row { left, right, goal } => f
                .debug_struct("Evidence::Row")
                .field("left", &left.debug(db))
                .field("right", &right.debug(db))
                .field("goal", &goal.debug(db))
                .finish(),
        }
    }
}

impl<'ctx, A: TypeAlloc + Clone + 'ctx> TypeFoldable<'ctx> for Evidence<A> {
    type Alloc = A;
    type Out<B: TypeAlloc> = Evidence<B>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, In = Self::Alloc>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::Out>, F::Error> {
        match self {
            Evidence::Row { left, right, goal } => Ok(Evidence::Row {
                left: left.try_fold_with(fold)?,
                right: right.try_fold_with(fold)?,
                goal: goal.try_fold_with(fold)?,
            }),
        }
    }
}
