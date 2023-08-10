use super::*;

mod simple;
pub use simple::SimpleClosedRow;

mod scoped;
pub use scoped::ScopedClosedRow;

/// ClosedRow is an implementation detail of SimpleClosedRow and ScopedClosedRow.
/// Because both of these rows are mappings from fields to types, that differ only in
/// semantics, it provides great code reuse to share one storage struct for both rows and rely
/// on types to distinguish how a stored row should be interpreted.
///
/// ClosedRow is an interned map, you can think of it abstractly as a `Vec<(RowLabel, Ty<A>)`.
/// However for performance reasons it is not stored this way. Instead we intern the list of labels
/// and list of types as two separate entities and store their interned representation in
/// Crepresentation in ClosedRow. This allows for very efficient comparison of closed rows, we simply
/// compare their interned fields to determine if two closed rows could be unified or not.
///
/// There are some invariants upheld by our row construction logic that are not directly encoded in
/// it's representation:
///     * fields and values have the same length.
///     * each valid index into field will be a valid index into values
///     * fields is sorted
pub(crate) struct ClosedRow<A: TypeAlloc = InDb> {
    pub fields: A::RowFields,
    pub values: A::RowValues,
}
impl<A: TypeAlloc> Debug for ClosedRow<A>
where
    A::RowFields: Debug,
    A::RowValues: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ClosedRow")
            .field("fields", &self.fields)
            .field("values", &self.values)
            .finish()
    }
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

/// Holds information needed to resolve an overlap during row merging.
pub(crate) struct HandleOverlapState<'a, V> {
    pub(crate) fields: &'a mut Vec<Ident>,
    pub(crate) values: &'a mut Vec<V>,
    pub(crate) overlap_label: Ident,
    pub(crate) left_fields: Vec<Ident>,
    pub(crate) left_values: Vec<V>,
    pub(crate) right_fields: Vec<Ident>,
    pub(crate) right_values: Vec<V>,
}

/// A thing that is like a row, it is a sorted series of fields and series of values. But is not
/// precisely a row, a mapping from fields to types.
pub type RowLike<V> = (Box<[RowLabel]>, Box<[V]>);

/// Internal representation of a row.
/// Sometimes we need this to pass values that will become a row
/// around before we're able to intern them into a row.
pub type RowInternals<A> = RowLike<Ty<A>>;

/// Merge two row like things, this is merge sorting left and right rows into one big
/// row.
/// `handler_overlap` is called to determine what to do when both rows contain the same label
pub(crate) fn merge_rowlikes<B, F, V>(
    (left_fields, left_values): (&[RowLabel], &[V]),
    (right_fields, right_values): (&[RowLabel], &[V]),
    mut handle_overlap: F,
) -> ControlFlow<B, RowLike<V>>
where
    V: Copy,
    F: FnMut(HandleOverlapState<'_, V>) -> ControlFlow<B>,
{
    let goal_len = left_fields.len() + right_fields.len();
    let mut left_fields = left_fields.iter().peekable();
    let mut left_values = left_values.iter();
    let mut right_fields = right_fields.iter().peekable();
    let mut right_values = right_values.iter();
    let (mut fields, mut values): (Vec<RowLabel>, Vec<V>) =
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
                        let func = |lbl: Ident,
                                    fields: &mut Peekable<Iter<Ident>>,
                                    values: &mut Iter<V>| {
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

impl<Db> DebugWithDb<Db> for ClosedRow<InDb>
where
    Db: ?Sized + crate::Db,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Db, include_all_fields: bool) -> fmt::Result {
        f.debug_map()
            .entries(
                self.fields
                    .fields(db.as_ty_db())
                    .iter()
                    .map(|handle| handle.text(db.as_core_db()))
                    .zip(
                        self.values
                            .values(db.as_ty_db())
                            .iter()
                            .map(|ty| ty.debug_with(db, include_all_fields)),
                    ),
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
    use super::{scoped::ScopedClosedRow, simple::SimpleClosedRow};
    use crate::TypeAlloc;

    pub trait SealNewRow {}
    impl<A: TypeAlloc> SealNewRow for ScopedClosedRow<A> {}
    impl<A: TypeAlloc> SealNewRow for SimpleClosedRow<A> {}
}

/// Create a new closed row from it's fields and values
pub trait NewRow<A: TypeAlloc>: new_row::SealNewRow {
    fn new(fields: A::RowFields, values: A::RowValues) -> Self;
}

/// Common row operations that make sense regardless of which row semantics we are using.
pub trait RowOps<A: TypeAlloc> {
    fn is_empty<'a>(&self, acc: &impl AccessTy<'a, A>) -> bool;
    fn len<'a>(&self, acc: &impl AccessTy<'a, A>) -> usize;
    fn fields<'a>(&self, acc: &impl AccessTy<'a, A>) -> &'a [RowLabel];
    fn values<'a>(&self, acc: &impl AccessTy<'a, A>) -> &'a [Ty<A>];
}
