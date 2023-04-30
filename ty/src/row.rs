use aiahr_core::ident::Ident;
use pretty::{docs, DocAllocator, Pretty};
use salsa::DebugWithDb;

use std::cmp::Ordering;
use std::convert::Infallible;
use std::fmt::{self, Debug};
use std::hash::Hash;
use std::iter::Peekable;
use std::ops::ControlFlow;
use std::slice::Iter;

use crate::PrettyType;

use super::{alloc::MkTy, AccessTy, FallibleTypeFold, InDb, Ty, TypeAlloc, TypeFoldable, TypeKind};

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
#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub(crate) struct ClosedRow<A: TypeAlloc = InDb> {
    pub fields: A::RowFields,
    pub values: A::RowValues,
}
impl<A: TypeAlloc + Copy> Copy for ClosedRow<A>
where
    A::RowFields: Copy,
    A::RowValues: Copy,
{
}
impl<A: TypeAlloc> ClosedRow<A> {
    fn is_empty<'a>(&self, acc: &(impl ?Sized + AccessTy<'a, A>)) -> bool {
        acc.row_fields(&self.fields).is_empty()
    }

    #[allow(clippy::len_without_is_empty)]
    fn len<'a>(&self, acc: &(impl ?Sized + AccessTy<'a, A>)) -> usize {
        // Because fields.len() must equal values.len() it doesn't matter which we use here
        acc.row_fields(&self.fields).len()
    }
}

impl<A, Db> PrettyType<Db, A> for ClosedRow<A>
where
    A: TypeAlloc,
    Db: ?Sized + crate::Db,
{
    fn pretty<'a, 'b, D, Ann>(
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
        acc: &dyn AccessTy<'a, A>,
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

    /// Union together two rows, whenever rows contain overlapping keys both will be included in
    /// the output in `self, right` order.
    pub fn _union<'a>(&self, right: &Self, acc: &dyn AccessTy<'a, A>) -> RowInternals<A>
    where
        A: 'a,
    {
        let control_flow: ControlFlow<Infallible, RowInternals<A>> =
            self._merge_rows(right, acc, |overlap| {
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

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub struct SimpleClosedRow<A: TypeAlloc = InDb>(pub(crate) ClosedRow<A>);

impl<A: TypeAlloc> SimpleClosedRow<A> {
    pub(crate) fn new(fields: A::RowFields, values: A::RowValues) -> Self {
        Self(ClosedRow { fields, values })
    }

    pub fn is_empty<'a>(&self, acc: &(impl ?Sized + AccessTy<'a, A>)) -> bool {
        self.0.is_empty(acc)
    }

    pub fn fields<'a>(&self, acc: &(impl ?Sized + AccessTy<'a, A>)) -> &'a [RowLabel] {
        acc.row_fields(&self.0.fields)
    }

    pub fn values<'a>(&self, acc: &(impl ?Sized + AccessTy<'a, A>)) -> &'a [Ty<A>] {
        acc.row_values(&self.0.values)
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len<'a>(&self, acc: &(impl ?Sized + AccessTy<'a, A>)) -> usize {
        self.0.len(acc)
    }

    pub fn _disjoint_union<'a, E>(
        &self,
        right: &Self,
        acc: &dyn AccessTy<'a, A>,
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
    pub fn disjoint_union(&self, acc: &dyn AccessTy<InDb>, right: &Self) -> RowInternals<InDb> {
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
impl<A, Db> PrettyType<Db, A> for SimpleClosedRow<A>
where
    A: TypeAlloc,
    Db: ?Sized + crate::Db,
{
    fn pretty<'a, 'b, D, Ann>(
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
        Ok(fold.ctx().mk_simple_row(&fields, &values))
    }
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub struct ScopedClosedRow<A: TypeAlloc>(ClosedRow<A>);
impl<A: TypeAlloc + Copy> Copy for ScopedClosedRow<A> where ClosedRow<A>: Copy {}
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
        Ok(ScopedClosedRow(todo!()))
    }
}

/// A row is our representaion of data, it maps fields to values.
/// Rows come in two flavors: Open and Closed.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub enum Row<A: TypeAlloc = InDb, Closed = SimpleClosedRow<A>> {
    /// An open row is a polymorphic set of data. Used to allow generic row programming.
    Open(A::TypeVar),
    /// A closed row is a concrete mapping from fields to values.
    Closed(Closed),
}
impl<A: TypeAlloc, Closed> Copy for Row<A, Closed>
where
    A: Clone,
    A::TypeVar: Copy,
    Closed: Copy,
{
}
impl<Db, Closed> DebugWithDb<Db> for Row<InDb, Closed>
where
    Db: ?Sized + crate::Db,
    Closed: DebugWithDb<Db>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Db, _include_all_fields: bool) -> fmt::Result {
        match self {
            Row::Open(var) => f.debug_tuple("Open").field(var).finish(),
            Row::Closed(row) => f.debug_tuple("Closed").field(&row.debug(db)).finish(),
        }
    }
}

impl<Db, A: TypeAlloc, Closed> PrettyType<Db, A> for Row<A, Closed>
where
    Db: ?Sized + crate::Db,
    Closed: PrettyType<Db, A>,
{
    fn pretty<'a, 'b, D, Ann>(
        &self,
        allocator: &'a D,
        db: &Db,
        acc: &impl AccessTy<'b, A>,
    ) -> pretty::DocBuilder<'a, D, Ann>
    where
        D: ?Sized + DocAllocator<'a, Ann>,
        D::Doc: pretty::Pretty<'a, D, Ann> + Clone,
        A::TypeVar: Pretty<'a, D, Ann>,
        A: 'b,
        Ann: 'a,
    {
        match self {
            Row::Open(tv) => pretty::Pretty::pretty(tv.clone(), allocator),
            Row::Closed(row) => row.pretty(allocator, db, acc),
        }
    }
}

impl<A: TypeAlloc> Row<A> {
    pub fn to_ty<I: MkTy<A>>(self, ctx: &I) -> Ty<A> {
        match self {
            Row::Open(var) => ctx.mk_ty(TypeKind::VarTy(var)),
            Row::Closed(row) => ctx.mk_ty(TypeKind::RowTy(row)),
        }
    }
}

impl<'ctx, A: TypeAlloc + Clone + 'ctx> TypeFoldable<'ctx> for Row<A> {
    type Alloc = A;
    type Out<B: TypeAlloc> = Row<B>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, In = Self::Alloc>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::Out>, F::Error> {
        match self {
            Row::Open(var) => fold.try_fold_row_var(var),
            Row::Closed(crow) => crow.try_fold_with(fold).map(Row::Closed),
        }
    }
}
