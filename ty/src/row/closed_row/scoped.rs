use super::*;
pub struct ScopedClosedRow<A: TypeAlloc = InDb>(pub(crate) ClosedRow<A>);

impl<A: TypeAlloc> Debug for ScopedClosedRow<A>
where
    A::RowFields: Debug,
    A::RowValues: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
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
impl<A: TypeAlloc> Copy for ScopedClosedRow<A> where ClosedRow<A>: Copy {}
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

impl<A: TypeAlloc> RowOps<A> for ScopedClosedRow<A> {
    fn is_empty<'a>(&self, acc: &impl AccessTy<'a, A>) -> bool {
        self.0.is_empty(acc)
    }

    fn len<'a>(&self, acc: &impl AccessTy<'a, A>) -> usize {
        self.0.len(acc)
    }

    fn fields<'a>(&self, acc: &impl AccessTy<'a, A>) -> &'a [RowLabel] {
        acc.row_fields(&self.0.fields)
    }

    fn values<'a>(&self, acc: &impl AccessTy<'a, A>) -> &'a [Ty<A>] {
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
