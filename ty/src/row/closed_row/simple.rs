use crate::row::RowsNotDisjoint;
use crate::RowFields;

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

impl<A: TypeAlloc + Copy> Copy for SimpleClosedRow<A> where ClosedRow<A>: Copy {}
impl<Db> DebugWithDb<Db> for SimpleClosedRow<InDb>
where
    Db: ?Sized + crate::Db,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Db, include_all_fields: bool) -> fmt::Result {
        DebugWithDb::fmt(&self.0, f, db, include_all_fields)
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

impl<A: TypeAlloc> SimpleClosedRow<A> {
    pub fn merge_rowlikes<'a, V: Copy>(
        left: (&'a [RowLabel], &'a [V]),
        right: (&'a [RowLabel], &'a [V]),
    ) -> Result<RowLike<V>, RowsNotDisjoint<'a, V>> {
        let res = merge_rowlikes(left, right, |overlap| {
            ControlFlow::Break(RowsNotDisjoint {
                left,
                right,
                label: overlap.overlap_label,
            })
        });

        match res {
            ControlFlow::Continue(ok) => Ok(ok),
            ControlFlow::Break(err) => Err(err),
        }
    }

    pub fn difference_rowlikes<V: Copy>(
        (goal_fields, goal_values): (&[RowLabel], &[V]),
        sub_fields: &[RowLabel],
    ) -> (Box<[RowLabel]>, Box<[V]>) {
        let out_row = goal_fields
            .iter()
            .zip(goal_values.iter())
            .filter(|(field, _)| sub_fields.binary_search_by(|lbl| lbl.cmp(field)).is_err());

        let (mut fields, mut values) = (Vec::new(), Vec::new());
        for (field, value) in out_row {
            fields.push(*field);
            values.push(*value);
        }

        (fields.into_boxed_slice(), values.into_boxed_slice())
    }
}

impl SimpleClosedRow<InDb> {
    pub fn raw_fields(&self) -> RowFields {
        self.0.fields
    }
}
