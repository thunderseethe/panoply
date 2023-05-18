use super::*;
pub struct SimpleClosedRow<A: TypeAlloc = InDb>(pub(crate) ClosedRow<A>);

impl<A: TypeAlloc> Debug for SimpleClosedRow<A>
where
    A::RowFields: Debug,
    A::RowValues: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
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

impl<A: TypeAlloc> RowOps<A> for SimpleClosedRow<A> {
    fn is_empty<'a>(&self, acc: &impl AccessTy<'a, A>) -> bool {
        self.0.is_empty(acc)
    }

    fn fields<'a>(&self, acc: &impl AccessTy<'a, A>) -> &'a [RowLabel] {
        acc.row_fields(&self.0.fields)
    }

    fn values<'a>(&self, acc: &impl AccessTy<'a, A>) -> &'a [Ty<A>] {
        acc.row_values(&self.0.values)
    }

    fn len<'a>(&self, acc: &impl AccessTy<'a, A>) -> usize {
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
