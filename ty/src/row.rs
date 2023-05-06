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

/// A closed row is a map of labels to types where all labels are known.
/// Counterpart to an open row where the set of labels is polymorphic
///
/// Because our closed row is an interned map, some important invariants are maintained
/// by the construction of ClosedRow:
/// 1. fields and values are the same length
/// 2. The field at index i is the key for the type at index i in values
/// 3. fields is sorted lexographically
#[derive(Debug)]
pub(crate) struct ClosedRow<A: TypeAlloc = InDb> {
    pub fields: A::RowFields,
    pub values: A::RowValues,
}
impl<A: TypeAlloc> PartialEq for ClosedRow<A>
where
    A::RowFields: PartialEq,
    A::RowValues: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.fields == other.fields && self.values == other.values
    }
}
impl<A: TypeAlloc> Eq for ClosedRow<A>
where
    A::RowFields: Eq,
    A::RowValues: Eq,
{
}
impl<A: TypeAlloc> PartialOrd for ClosedRow<A>
where
    A::RowFields: PartialOrd,
    A::RowValues: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.fields
            .partial_cmp(&other.fields)
            .and_then(|field_cmp| {
                self.values
                    .partial_cmp(&other.values)
                    .map(|val_cmp| field_cmp.then(val_cmp))
            })
    }
}
impl<A: TypeAlloc> Ord for ClosedRow<A>
where
    A::RowFields: Ord,
    A::RowValues: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        self.fields
            .cmp(&other.fields)
            .then(self.values.cmp(&other.values))
    }
}
impl<A: TypeAlloc> Clone for ClosedRow<A>
where
    A::RowFields: Clone,
    A::RowValues: Clone,
{
    fn clone(&self) -> Self {
        Self {
            fields: self.fields.clone(),
            values: self.values.clone(),
        }
    }
}
impl<A: TypeAlloc + Clone> Copy for ClosedRow<A>
where
    A::RowFields: Copy,
    A::RowValues: Copy,
{
}
impl<A: TypeAlloc> Hash for ClosedRow<A>
where
    A::RowFields: Hash,
    A::RowValues: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.fields.hash(state);
        self.values.hash(state);
    }
}
impl<A: TypeAlloc> ClosedRow<A> {
    fn is_empty<'a>(&self, acc: &impl AccessTy<'a, A>) -> bool {
        acc.row_fields(&self.fields).is_empty()
    }

    #[allow(clippy::len_without_is_empty)]
    fn len<'a>(&self, acc: &impl AccessTy<'a, A>) -> usize {
        // Because fields.len() must equal values.len() it doesn't matter which we use here
        acc.row_fields(&self.fields).len()
    }
}

impl<A, Db, Ann> PrettyType<Db, A, Ann> for ClosedRow<A>
where
    A: TypeAlloc,
    Db: ?Sized + crate::Db,
{
    fn pretty<'a, 'b, D>(
        &self,
        a: &'a D,
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
        let docs = acc
            .row_fields(&self.fields)
            .iter()
            .zip(acc.row_values(&self.values).iter())
            .map(|(field, value)| {
                docs![
                    a,
                    a.as_string(field.text(db.as_core_db())),
                    a.space(),
                    "|>",
                    a.softline(),
                    value.pretty(a, db, acc)
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

struct HandleOverlapState<'a, A: TypeAlloc> {
    fields: &'a mut Vec<Ident>,
    values: &'a mut Vec<Ty<A>>,
    overlap_label: Ident,
    left_fields: Vec<Ident>,
    left_values: Vec<Ty<A>>,
    right_fields: Vec<Ident>,
    right_values: Vec<Ty<A>>,
}

/// Internal representation of a row.
/// Sometimes we need this to pass values that will become a row
/// around before we're able to intern them into a row.
pub type RowInternals<A> = (Box<[RowLabel]>, Box<[Ty<A>]>);

impl<A: TypeAlloc> ClosedRow<A>
where
    Ty<A>: Copy,
{
    fn _merge_rows<'a, B, F>(
        &self,
        right: &Self,
        acc: &impl AccessTy<'a, A>,
        mut handle_overlap: F,
    ) -> ControlFlow<B, RowInternals<A>>
    where
        A: 'a,
        F: FnMut(HandleOverlapState<'_, A>) -> ControlFlow<B>,
    {
        let goal_len = self.len(acc) + right.len(acc);
        let mut left_fields = acc.row_fields(&self.fields).iter().peekable();
        let mut left_values = acc.row_values(&self.values).iter();
        let mut right_fields = acc.row_fields(&right.fields).iter().peekable();
        let mut right_values = acc.row_values(&right.values).iter();

        let (mut fields, mut values): (Vec<RowLabel>, Vec<Ty<A>>) =
            (Vec::with_capacity(goal_len), Vec::with_capacity(goal_len));

        loop {
            match (left_fields.peek(), right_fields.peek()) {
                (Some(left_lbl), Some(right_lbl)) => {
                    // This ensures we don't use Handle::ord on accident
                    match left_lbl.cmp(right_lbl) {
                        // Push left
                        Ordering::Less => {
                            fields.push(*left_fields.next().unwrap());
                            values.push(*left_values.next().unwrap());
                        }
                        // When two rows are equal we may have a sequence of jkkkkkj
                        Ordering::Equal => {
                            let func =
                                |lbl: Ident,
                                 fields: &mut Peekable<Iter<Ident>>,
                                 values: &mut Iter<Ty<A>>| {
                                    let mut overlap_fields = vec![*fields.next().unwrap()];
                                    let mut overlap_values = vec![*values.next().unwrap()];
                                    while let Some(peek) = fields.peek() {
                                        if **peek != lbl {
                                            break;
                                        }
                                        overlap_fields.push(*fields.next().unwrap());
                                        overlap_values.push(*values.next().unwrap());
                                    }
                                    (overlap_fields, overlap_values)
                                };
                            let left_lbl = **left_lbl;
                            let (left_overlap_fields, left_overlap_values) =
                                func(left_lbl, &mut left_fields, &mut left_values);
                            let (right_overlap_fields, right_overlap_values) =
                                func(**right_lbl, &mut right_fields, &mut right_values);
                            handle_overlap(HandleOverlapState {
                                fields: &mut fields,
                                values: &mut values,
                                overlap_label: left_lbl,
                                left_fields: left_overlap_fields,
                                left_values: left_overlap_values,
                                right_fields: right_overlap_fields,
                                right_values: right_overlap_values,
                            })?
                        }
                        // Push right
                        Ordering::Greater => {
                            fields.push(*right_fields.next().unwrap());
                            values.push(*right_values.next().unwrap());
                        }
                    }
                }
                // Right row bigger than left
                (None, Some(_)) => {
                    fields.extend(right_fields);
                    values.extend(right_values.cloned());
                    break;
                }
                // Left row bigger than right
                (Some(_), None) => {
                    fields.extend(left_fields);
                    values.extend(left_values.cloned());
                    break;
                }
                (None, None) => break,
            }
        }

        fields.shrink_to_fit();
        values.shrink_to_fit();

        ControlFlow::Continue((fields.into_boxed_slice(), values.into_boxed_slice()))
    }
}

impl<Db> DebugWithDb<Db> for ClosedRow<InDb>
where
    Db: ?Sized + crate::Db,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Db, _include_all_fields: bool) -> fmt::Result {
        f.debug_map()
            .entries(
                self.fields
                    .fields(db.as_ty_db())
                    .iter()
                    .map(|handle| handle.text(db.as_core_db()))
                    .zip(self.values.values(db.as_ty_db()).iter()),
            )
            .finish()
    }
}
impl<'ctx, A: TypeAlloc + Clone + 'ctx> TypeFoldable<'ctx> for ClosedRow<A> {
    type Alloc = A;
    type Out<B: TypeAlloc> = RowInternals<B>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, In = Self::Alloc>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::Out>, F::Error> {
        let values = fold
            .access()
            .row_values(&self.values)
            .iter()
            .map(|ty| ty.clone().try_fold_with(fold))
            .collect::<Result<Vec<_>, _>>()?;
        Ok((
            fold.access()
                .row_fields(&self.fields)
                .to_vec()
                .into_boxed_slice(),
            values.into_boxed_slice(),
        ))
    }
}

// This is a visibility trick.
// We need `NewRow` to be public because it appears in a public type and we use it in tc.
// However we do not want other people to be able to implement it.
// So we give it a supertrait that is marked public but lives in a private module
mod new_row {
    use super::{ScopedClosedRow, SimpleClosedRow};
    use crate::TypeAlloc;

    pub trait SealNewRow {}
    impl<A: TypeAlloc> SealNewRow for ScopedClosedRow<A> {}
    impl<A: TypeAlloc> SealNewRow for SimpleClosedRow<A> {}
}

pub trait NewRow<A: TypeAlloc>: new_row::SealNewRow {
    fn new(fields: A::RowFields, values: A::RowValues) -> Self;
}

pub trait RowOps<'a, A: TypeAlloc> {
    fn is_empty(&self, acc: &impl AccessTy<'a, A>) -> bool;
    fn len(&self, acc: &impl AccessTy<'a, A>) -> usize;
    fn fields(&self, acc: &impl AccessTy<'a, A>) -> &'a [RowLabel];
    fn values(&self, acc: &impl AccessTy<'a, A>) -> &'a [Ty<A>];
}

#[derive(Debug)]
pub struct SimpleClosedRow<A: TypeAlloc = InDb>(pub(crate) ClosedRow<A>);

impl<A: TypeAlloc> PartialEq for SimpleClosedRow<A>
where
    ClosedRow<A>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<A: TypeAlloc> Eq for SimpleClosedRow<A> where ClosedRow<A>: Eq {}
impl<A: TypeAlloc> PartialOrd for SimpleClosedRow<A>
where
    ClosedRow<A>: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}
impl<A: TypeAlloc> Ord for SimpleClosedRow<A>
where
    ClosedRow<A>: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
    }
}
impl<A: TypeAlloc> Clone for SimpleClosedRow<A>
where
    ClosedRow<A>: Clone,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<A: TypeAlloc> Hash for SimpleClosedRow<A>
where
    ClosedRow<A>: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}
impl<A: TypeAlloc> NewRow<A> for SimpleClosedRow<A> {
    fn new(fields: <A as TypeAlloc>::RowFields, values: <A as TypeAlloc>::RowValues) -> Self {
        Self(ClosedRow { fields, values })
    }
}

impl<'a, A: TypeAlloc> RowOps<'a, A> for SimpleClosedRow<A> {
    fn is_empty(&self, acc: &impl AccessTy<'a, A>) -> bool {
        self.0.is_empty(acc)
    }

    fn fields(&self, acc: &impl AccessTy<'a, A>) -> &'a [RowLabel] {
        acc.row_fields(&self.0.fields)
    }

    fn values(&self, acc: &impl AccessTy<'a, A>) -> &'a [Ty<A>] {
        acc.row_values(&self.0.values)
    }

    fn len(&self, acc: &impl AccessTy<'a, A>) -> usize {
        self.0.len(acc)
    }
}

impl<A: TypeAlloc> SimpleClosedRow<A> {
    pub fn _disjoint_union<'a, E>(
        &self,
        right: &Self,
        acc: &impl AccessTy<'a, A>,
        mk_err: impl Fn(&Self, &Self, &RowLabel) -> E,
    ) -> Result<RowInternals<A>, E>
    where
        A: 'a,
        Ty<A>: Copy,
        Self: Clone,
    {
        let control_flow = self.0._merge_rows(&right.0, acc, |overlap| {
            ControlFlow::Break(mk_err(self, right, &overlap.overlap_label))
        });
        match control_flow {
            ControlFlow::Continue(row_internals) => Ok(row_internals),
            ControlFlow::Break(err) => Err(err),
        }
    }
}
impl SimpleClosedRow<InDb> {
    /// Invariant: These rows have already been typed checked so we cannot fail at union.
    pub fn disjoint_union<'a>(
        &self,
        acc: &impl AccessTy<'a, InDb>,
        right: &Self,
    ) -> RowInternals<InDb> {
        self._disjoint_union::<Infallible>(right, acc, |_, _, _| unreachable!())
            .unwrap()
    }
}

impl<A: TypeAlloc + Copy> Copy for SimpleClosedRow<A> where ClosedRow<A>: Copy {}
impl<Db> DebugWithDb<Db> for SimpleClosedRow<InDb>
where
    Db: ?Sized + crate::Db,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Db, include_all_fields: bool) -> fmt::Result {
        DebugWithDb::fmt(&self.0, f, db, include_all_fields)
    }
}
impl<A, Db, Ann> PrettyType<Db, A, Ann> for SimpleClosedRow<A>
where
    A: TypeAlloc,
    Db: ?Sized + crate::Db,
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
        PrettyType::pretty(&self.0, allocator, db, acc)
    }
}
impl<'ctx, A: TypeAlloc + Clone + 'ctx> TypeFoldable<'ctx> for SimpleClosedRow<A> {
    type Alloc = A;
    type Out<B: TypeAlloc> = SimpleClosedRow<B>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, In = Self::Alloc>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::Out>, F::Error> {
        let (fields, values) = self.0.try_fold_with(fold)?;
        Ok(fold.ctx().mk_row(&fields, &values))
    }
}

#[derive(Debug)]
pub struct ScopedClosedRow<A: TypeAlloc>(pub(crate) ClosedRow<A>);

impl<A: TypeAlloc> PartialEq for ScopedClosedRow<A>
where
    ClosedRow<A>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<A: TypeAlloc> Eq for ScopedClosedRow<A> where ClosedRow<A>: Eq {}
impl<A: TypeAlloc> PartialOrd for ScopedClosedRow<A>
where
    ClosedRow<A>: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}
impl<A: TypeAlloc> Ord for ScopedClosedRow<A>
where
    ClosedRow<A>: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
    }
}
impl<A: TypeAlloc> Clone for ScopedClosedRow<A>
where
    ClosedRow<A>: Clone,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<A: TypeAlloc> Hash for ScopedClosedRow<A>
where
    ClosedRow<A>: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<A: TypeAlloc> NewRow<A> for ScopedClosedRow<A> {
    fn new(fields: <A as TypeAlloc>::RowFields, values: <A as TypeAlloc>::RowValues) -> Self {
        Self(ClosedRow { fields, values })
    }
}

impl<'a, A: TypeAlloc> RowOps<'a, A> for ScopedClosedRow<A> {
    fn is_empty(&self, acc: &impl AccessTy<'a, A>) -> bool {
        self.0.is_empty(acc)
    }

    fn len(&self, acc: &impl AccessTy<'a, A>) -> usize {
        self.0.len(acc)
    }

    fn fields(&self, acc: &impl AccessTy<'a, A>) -> &'a [RowLabel] {
        acc.row_fields(&self.0.fields)
    }

    fn values(&self, acc: &impl AccessTy<'a, A>) -> &'a [Ty<A>] {
        acc.row_values(&self.0.values)
    }
}

impl<A: TypeAlloc> ScopedClosedRow<A> {
    /// Union together two rows, whenever rows contain overlapping keys both will be included in
    /// the output in `self, right` order.
    pub fn _union<'a>(&self, right: &Self, acc: &impl AccessTy<'a, A>) -> RowInternals<A>
    where
        Ty<A>: Copy,
        A: 'a,
    {
        let control_flow: ControlFlow<Infallible, RowInternals<A>> =
            self.0._merge_rows(&right.0, acc, |overlap| {
                overlap.fields.extend(overlap.left_fields);
                overlap.fields.extend(overlap.right_fields);
                overlap.values.extend(overlap.left_values);
                overlap.values.extend(overlap.right_values);
                ControlFlow::Continue(())
            });
        match control_flow {
            ControlFlow::Continue(row_internals) => row_internals,
            ControlFlow::Break(_) => unreachable!(),
        }
    }
}
impl<A: TypeAlloc + Copy> Copy for ScopedClosedRow<A> where ClosedRow<A>: Copy {}
impl<A, Db, Ann> PrettyType<Db, A, Ann> for ScopedClosedRow<A>
where
    A: TypeAlloc,
    Db: ?Sized + crate::Db,
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
        PrettyType::pretty(&self.0, allocator, db, acc)
    }
}
impl<Db> DebugWithDb<Db> for ScopedClosedRow<InDb>
where
    Db: ?Sized + crate::Db,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Db, include_all_fields: bool) -> fmt::Result {
        DebugWithDb::fmt(&self.0, f, db, include_all_fields)
    }
}
impl<'ctx, A: TypeAlloc + Clone + 'ctx> TypeFoldable<'ctx> for ScopedClosedRow<A> {
    type Alloc = A;
    type Out<B: TypeAlloc> = ScopedClosedRow<B>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, In = Self::Alloc>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::Out>, F::Error> {
        let (fields, values) = self.0.try_fold_with(fold)?;
        Ok(fold.ctx().mk_row(&fields, &values))
    }
}

mod seal_row_sema {
    pub trait SealRowSema {}
}
use seal_row_sema::SealRowSema;

pub trait RowSema: SealRowSema {
    type Open<A: TypeAlloc>: PartialEq + Eq + PartialOrd + Ord + Hash + Clone + Debug;
    type Closed<A: TypeAlloc>: PartialEq + Eq + PartialOrd + Ord + Hash + Clone;
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
pub struct Scoped;
impl SealRowSema for Scoped {}
impl RowSema for Scoped {
    type Open<A: TypeAlloc> = A::ScopedRowVar;
    type Closed<A: TypeAlloc> = ScopedClosedRow<A>;
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
pub struct Simple;
impl SealRowSema for Simple {}
impl RowSema for Simple {
    type Open<A: TypeAlloc> = A::SimpleRowVar;
    type Closed<A: TypeAlloc> = SimpleClosedRow<A>;
}

/// A row is our representaion of data, it maps fields to values.
/// Rows come in two flavors: Open and Closed.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub enum Row<A: TypeAlloc = InDb, Sema: RowSema = Simple> {
    /// An open row is a polymorphic set of data. Used to allow generic row programming.
    Open(Sema::Open<A>),
    /// A closed row is a concrete mapping from fields to values.
    Closed(Sema::Closed<A>),
}
pub type SimpleRow<A> = Row<A, Simple>;
pub type ScopedRow<A> = Row<A, Scoped>;

impl<A: TypeAlloc, Sema: RowSema + Clone> Copy for Row<A, Sema>
where
    A: Clone,
    Sema::Open<A>: Copy,
    Sema::Closed<A>: Copy,
{
}
impl<Db, Sema: RowSema> DebugWithDb<Db> for Row<InDb, Sema>
where
    Db: ?Sized + crate::Db,
    Sema::Open<InDb>: Debug,
    Sema::Closed<InDb>: DebugWithDb<Db>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Db, _include_all_fields: bool) -> fmt::Result {
        match self {
            Row::Open(var) => f.debug_tuple("Open").field(var).finish(),
            Row::Closed(row) => f.debug_tuple("Closed").field(&row.debug(db)).finish(),
        }
    }
}

impl<Db, A: TypeAlloc, Sema: RowSema, Ann> PrettyType<Db, A, Ann> for Row<A, Sema>
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

impl<'ctx, A> TypeFoldable<'ctx> for Row<A>
where
    A: TypeAlloc + Clone + 'ctx,
{
    type Alloc = A;
    type Out<B: TypeAlloc> = Row<B>;

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

impl<'ctx, A> TypeFoldable<'ctx> for Row<A, Scoped>
where
    A: TypeAlloc + Clone + 'ctx,
{
    type Alloc = A;
    type Out<B: TypeAlloc> = Row<B, Scoped>;

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
