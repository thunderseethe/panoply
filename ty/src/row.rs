use aiahr_core::ident::Ident;
use pretty::{docs, DocAllocator};
use salsa::DebugWithDb;

use std::cmp::Ordering;
use std::convert::Infallible;
use std::fmt::{self, Debug};
use std::hash::Hash;
use std::iter::Peekable;
use std::ops::ControlFlow;
use std::slice::Iter;

use crate::PrettyType;

use super::{alloc::MkTy, AccessTy, FallibleTypeFold, InDb, Ty, TypeAlloc, TypeFoldable};

/// A label of a row field
pub type RowLabel = Ident;

mod closed_row;
pub use closed_row::{NewRow, RowInternals, RowOps, ScopedClosedRow, SimpleClosedRow};

mod seal_row_sema {
    pub trait SealRowSema {}
}
use seal_row_sema::SealRowSema;

pub trait RowSema: SealRowSema {
    type Open<A: TypeAlloc>: PartialEq + Eq + PartialOrd + Ord + Hash + Clone + Debug;
    type Closed<A: TypeAlloc>: PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + Clone
        + NewRow<A>
        + RowOps<A>;
}

/// Scoped Row Semantics
///
/// Scoped rows allow for multiple instances of the same label within a row.
/// The insertion order of these labels is tracked and meaningful, when we remove an instance of a
/// label we can remove it from the left or the right. When we projectR the value for a label we
/// only take the rightmost value in the row. The rightmost value "shadows" previous entries in a
/// scoped row.
///
/// Because of this behavior scoped rows model lexical scope very naturally. When we introduce a
/// new value we shadow all previous entries, when that entry "goes out of scope" all our previous
/// are still there and the next most recent becomes available. Similar to variable shadowing in
/// lexical scope.
///
/// This behavior makes them a great solution for holding our effect handlers. When we introduce a
/// new `State` handle we would like it to shadow all previous `State` handlers, but only until
/// that handle leaves scope at which point we would like to pick back up with our next innermost
/// `State` handler.
///
/// The semantics of scoped rows differ from simple rows in two key areas:
///  * Row combination is done with union, not disjoint union, and cannot fail
///  * Row combination and difference are not commutative
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
pub struct Scoped;
impl SealRowSema for Scoped {}
impl RowSema for Scoped {
    type Open<A: TypeAlloc> = A::ScopedRowVar;
    type Closed<A: TypeAlloc> = ScopedClosedRow<A>;
}

/// Simple Row Semantics
///
/// Simple rows map unique labels to types, they do not allow more than once instance of a label
/// within a row.
///
/// Because one field is unambiguously mapped to one type, simple rows are ideal for storing data.
/// Their semantics of simple rows differ from scoped rows in two key areas:
///   * Row combination is done with disjoint union, not union, and can fail
///   * Row combination is commutative, and row difference can ignore which sub row (left or right)
///   is being removed.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
pub struct Simple;
impl SealRowSema for Simple {}
impl RowSema for Simple {
    type Open<A: TypeAlloc> = A::SimpleRowVar;
    type Closed<A: TypeAlloc> = SimpleClosedRow<A>;
}

/// A row is a mapping from labels to types, it can be open or closed.
/// An open row is a unknown mapping from fields to types. Like a  type variable allows us to be type polymorphic, open rows allows us to be row polymorphic.
/// A closed row is a known mapping from fields to types. It cannot be polymorphic and is
/// immutable. To extend a closed row you must combine it with another row producing the extension
/// as the result.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub enum Row<Sema: RowSema, A: TypeAlloc = InDb> {
    /// An open row is a polymorphic set of data. Used to allow generic row programming.
    Open(Sema::Open<A>),
    /// A closed row is a concrete mapping from fields to values.
    Closed(Sema::Closed<A>),
}
pub type SimpleRow<A = InDb> = Row<Simple, A>;
pub type ScopedRow<A = InDb> = Row<Scoped, A>;

impl<A: TypeAlloc, Sema: RowSema + Clone> Copy for Row<Sema, A>
where
    A: Clone,
    Sema::Open<A>: Copy,
    Sema::Closed<A>: Copy,
{
}
impl<Db, Sema: RowSema> DebugWithDb<Db> for Row<Sema, InDb>
where
    Db: ?Sized + crate::Db,
    Sema::Open<InDb>: Debug,
    Sema::Closed<InDb>: DebugWithDb<Db>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Db, include_all_fields: bool) -> fmt::Result {
        match self {
            Row::Open(var) => f.debug_tuple("Open").field(var).finish(),
            Row::Closed(row) => f
                .debug_tuple("Closed")
                .field(&row.debug_with(db, include_all_fields))
                .finish(),
        }
    }
}

impl<Db, A: TypeAlloc, Sema: RowSema, Ann> PrettyType<Db, A, Ann> for Row<Sema, A>
where
    Db: ?Sized + crate::Db,
    Sema::Open<A>: Debug,
    Sema::Closed<A>: PrettyType<Db, A, Ann>,
{
    fn pretty<'a, 'b, D>(
        &self,
        allocator: &'a D,
        db: &Db,
        acc: &impl AccessTy<'b, A>,
    ) -> pretty::DocBuilder<'a, D, Ann>
    where
        D: ?Sized + DocAllocator<'a, Ann>,
        D::Doc: pretty::Pretty<'a, D, Ann> + Clone,
        <A as TypeAlloc>::TypeVar: pretty::Pretty<'a, D, Ann>,
        A: 'b,
        Ann: 'a,
    {
        match self {
            Row::Open(tv) => allocator.text(format!("{:?}", tv)),
            Row::Closed(row) => row.pretty(allocator, db, acc),
        }
    }
}

impl<'ctx, A> TypeFoldable<'ctx> for Row<Simple, A>
where
    A: TypeAlloc + Clone + 'ctx,
{
    type Alloc = A;
    type Out<B: TypeAlloc> = Row<Simple, B>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, In = Self::Alloc>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::Out>, F::Error> {
        match self {
            Row::Open(var) => fold.try_fold_simple_row_var(var),
            Row::Closed(crow) => crow.try_fold_with(fold).map(Row::Closed),
        }
    }
}

impl<'ctx, A> TypeFoldable<'ctx> for Row<Scoped, A>
where
    A: TypeAlloc + Clone + 'ctx,
{
    type Alloc = A;
    type Out<B: TypeAlloc> = Row<Scoped, B>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, In = Self::Alloc>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::Out>, F::Error> {
        match self {
            Row::Open(var) => fold.try_fold_scoped_row_var(var),
            Row::Closed(crow) => crow.try_fold_with(fold).map(Row::Closed),
        }
    }
}

pub struct RowsNotDisjoint<'a, V> {
    /// Left row that was expected to be disjoint
    pub left: (&'a [RowLabel], &'a [V]),
    /// Right row that was expected to be disjoint
    pub right: (&'a [RowLabel], &'a [V]),
    /// The label left and right both contain
    pub label: RowLabel,
}
