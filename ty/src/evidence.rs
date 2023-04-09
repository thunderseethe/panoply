use super::{row::Row, FallibleTypeFold, InDb, TypeAlloc, TypeFoldable};
use salsa::DebugWithDb;

/// Evidence proving a piece of type level information.
/// This allows type schemes to express constraints on it's type variables that type checking must
/// prove true for the the scheme to type check.
/// This will be things like row combinations or type class constraints.
/// Once we drop down to the IR level where everything is explicitly typed each piece of required
/// evidence will appear as a parameter to it's term, and passing the parameter provides a witness
/// taht the evidence is true.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Evidence<A: TypeAlloc = InDb> {
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
