use crate::{RowFields, RowValues};

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
        Some(self.cmp(other))
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

impl<A: TypeAlloc> ScopedClosedRow<A> {
    pub fn merge_rowlikes<V: Copy>(
        left: (&[RowLabel], &[V]),
        right: (&[RowLabel], &[V]),
    ) -> (Box<[RowLabel]>, Box<[V]>) {
        let res = merge_rowlikes(left, right, |overlap| {
            overlap.fields.extend(overlap.left_fields);
            overlap.fields.extend(overlap.right_fields);
            overlap.values.extend(overlap.left_values);
            overlap.values.extend(overlap.right_values);
            ControlFlow::<Infallible>::Continue(())
        });

        match res {
            ControlFlow::Continue(row_like_internals) => row_like_internals,
            ControlFlow::Break(_) => unreachable!(),
        }
    }

    pub(crate) fn difference_rowlikes<'a, V: Copy>(
        (goal_fields, goal_values): (&'a [RowLabel], &'a [V]),
        sub_fields: &[RowLabel],
        mut handle_overlap: impl for<'g> FnMut(
            &'g [(&'a RowLabel, &'a V)],
            usize,
        ) -> &'g [(&'a RowLabel, &'a V)],
    ) -> (Box<[RowLabel]>, Box<[V]>) {
        //TODO: Avoid allocating vecs here.
        //Maybe not worth since we'll have to allocate vecs per group anyways? and this lets us do
        //all our work in slices.
        let goal_vec = goal_fields
            .iter()
            .zip(goal_values.iter())
            .collect::<Vec<_>>();
        let mut goal_groups = GroupBy::new(&goal_vec, |a, b| a.0 == b.0);
        let sub_groups = GroupBy::new(sub_fields, |a, b| a == b);

        let (mut fields, mut values) = (vec![], vec![]);

        for sub_group in sub_groups {
            for goal_group in goal_groups.by_ref() {
                match goal_group[0].0.cmp(&sub_group[0]) {
                    Ordering::Less => {
                        fields.extend(goal_group.iter().map(|(field, _)| **field));
                        values.extend(goal_group.iter().map(|(_, value)| **value));
                    }
                    Ordering::Equal => {
                        let right_group = handle_overlap(goal_group, sub_group.len());
                        fields.extend(right_group.iter().map(|(field, _)| **field));
                        values.extend(right_group.iter().map(|(_, value)| **value));
                        break;
                    }
                    Ordering::Greater => {
                        unreachable!(
                            "ICE: Sub scoped row contained field(s) goal scoped row did not"
                        )
                    }
                }
            }
        }

        // Append any remaining goal groups, they cannot appear in the sub row.
        for goal_group in goal_groups {
            fields.extend(goal_group.iter().map(|(field, _)| **field));
            values.extend(goal_group.iter().map(|(_, value)| **value));
        }

        fields.shrink_to_fit();
        values.shrink_to_fit();

        (fields.into_boxed_slice(), values.into_boxed_slice())
    }

    pub fn diff_left_rowlikes<V: Copy>(
        goal: (&[RowLabel], &[V]),
        left_fields: &[RowLabel],
    ) -> (Box<[RowLabel]>, Box<[V]>) {
        Self::difference_rowlikes(goal, left_fields, |goal, left_count| &goal[left_count..])
    }

    pub fn diff_right_rowlikes<V: Copy>(
        goal: (&[RowLabel], &[V]),
        right_fields: &[RowLabel],
    ) -> (Box<[RowLabel]>, Box<[V]>) {
        Self::difference_rowlikes(goal, right_fields, |goal, right_count| {
            &goal[0..(goal.len() - right_count)]
        })
    }
}

impl ScopedClosedRow<InDb> {
    pub fn raw_fields(&self) -> RowFields {
        self.0.fields
    }

    pub fn raw_values(&self) -> RowValues {
        self.0.values
    }
}

// TODO: Replace with std::slice::group_by once stable.
struct GroupBy<'a, T: 'a, P> {
    slice: &'a [T],
    predicate: P,
}

impl<'a, T: 'a, P> GroupBy<'a, T, P>
where
    P: FnMut(&'a T, &'a T) -> bool,
{
    fn new(slice: &'a [T], predicate: P) -> Self {
        Self { slice, predicate }
    }
}
impl<'a, T, P> Iterator for GroupBy<'a, T, P>
where
    P: FnMut(&'a T, &'a T) -> bool,
{
    type Item = &'a [T];

    fn next(&mut self) -> Option<Self::Item> {
        if self.slice.is_empty() {
            None
        } else {
            let mut len = 1;
            let mut iter = self.slice.windows(2);
            while let Some([l, r]) = iter.next() {
                if (self.predicate)(l, r) {
                    len += 1
                } else {
                    break;
                }
            }
            let (head, tail) = self.slice.split_at(len);
            self.slice = tail;
            Some(head)
        }
    }
}
