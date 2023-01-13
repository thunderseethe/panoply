use std::convert::Infallible;
use std::fmt::{self, Debug};
use std::ops::Deref;

use aiahr_core::define_ids;
use aiahr_core::memory::handle::RefHandle;

use ena::unify::{EqUnifyValue, UnifyKey, UnifyValue};

use crate::TypeCheckError;

define_ids!(
/// A type variable.
/// These are explicity referred to by the AST and can persist through type checking.
/// They may not be modified by the type checking process, often referred to as untouchabale.
#[derive(Default, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub TcVar;
);

#[derive(Debug, PartialEq, Eq)]
pub struct UnifierToTcVarError {
    index: u32,
}

impl<'infer> TryFrom<TcUnifierVar<'infer>> for TcVar {
    type Error = UnifierToTcVarError;

    fn try_from(value: TcUnifierVar<'infer>) -> Result<Self, Self::Error> {
        Err(UnifierToTcVarError {
            index: value.index(),
        })
    }
}

/// A unifier variable.
/// These are produced during the type checking process and MUST NOT persist outside the type
/// checker. They may not appear in the AST once type checking is completed and are removed by
/// zonking.
/// Conversely to the untouchable TcVar, these are "touchable" and will be modified by the type
/// checker.
#[derive(Default, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct TcUnifierVar<'ctx> {
    id: u32,
    _marker: std::marker::PhantomData<&'ctx ()>,
}
impl<'ctx> Debug for TcUnifierVar<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("TcUnifierVar").field(&self.id).finish()
    }
}
impl<'ctx> UnifyKey for TcUnifierVar<'ctx> {
    type Value = Option<UnifyVal<'ctx, TcUnifierVar<'ctx>>>;

    fn index(&self) -> u32 {
        self.id
    }

    fn from_index(id: u32) -> Self {
        Self {
            id,
            _marker: std::marker::PhantomData,
        }
    }

    fn tag() -> &'static str {
        "TcUnifierVar"
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum PartialRow<'ctx, TV> {
    /// This row represents the goal of a RowCombine constraint
    OpenGoal {
        left: Row<'ctx, TV>,
        right: Row<'ctx, TV>,
    },
    /// This row represents the left subrow of a RowCombine constraint
    OpenLeft {
        goal: Row<'ctx, TV>,
        right: Row<'ctx, TV>,
    },
    /// This row represents the right subrow of a RowCombine constraint
    OpenRight {
        goal: Row<'ctx, TV>,
        left: Row<'ctx, TV>,
    },
}
impl<'ctx, TV: Clone> TypeFoldable<'ctx> for PartialRow<'_, TV> {
    type TypeVar = TV;
    type Out<T: 'ctx> = PartialRow<'ctx, T>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::TypeVar>, F::Error> {
        match self {
            PartialRow::OpenGoal { left, right } => {
                let left = left.try_fold_with(fold)?;
                let right = right.try_fold_with(fold)?;
                Ok(PartialRow::OpenGoal { left, right })
            }
            PartialRow::OpenLeft { goal, right } => {
                let goal = goal.try_fold_with(fold)?;
                let right = right.try_fold_with(fold)?;
                Ok(PartialRow::OpenLeft { goal, right })
            }
            PartialRow::OpenRight { goal, left } => {
                let goal = goal.try_fold_with(fold)?;
                let left = left.try_fold_with(fold)?;
                Ok(PartialRow::OpenRight { goal, left })
            }
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug, Default)]
pub struct RowSet<'ctx, TV> {
    rows: Vec<PartialRow<'ctx, TV>>,
}
impl<'ctx, TV> RowSet<'ctx, TV> {
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = &'a PartialRow<'ctx, TV>> {
        self.rows.iter()
    }
}
impl<'ctx, TV> From<PartialRow<'ctx, TV>> for RowSet<'ctx, TV> {
    fn from(partial: PartialRow<'ctx, TV>) -> Self {
        RowSet {
            rows: vec![partial],
        }
    }
}
impl<'ctx, TV> IntoIterator for RowSet<'ctx, TV> {
    type Item = PartialRow<'ctx, TV>;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.rows.into_iter()
    }
}
impl<'ctx, TV: Clone> TypeFoldable<'ctx> for RowSet<'_, TV> {
    type TypeVar = TV;
    type Out<T: 'ctx> = RowSet<'ctx, T>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::TypeVar>, F::Error> {
        let rows: Vec<PartialRow<'ctx, F::TypeVar>> = self
            .rows
            .into_iter()
            .map(|row| row.try_fold_with(fold))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(RowSet { rows })
    }
}
impl<'ctx, TV: Clone + Debug + PartialEq> UnifyValue for RowSet<'ctx, TV> {
    type Error = Infallible;

    fn unify_values(left: &Self, right: &Self) -> Result<Self, Self::Error> {
        let mut rows = Vec::with_capacity(left.rows.len() + right.rows.len());
        rows.extend_from_slice(left.rows.as_slice());
        rows.extend_from_slice(right.rows.as_slice());
        rows.dedup_by(|a, b| a.eq(&b));
        Ok(Self { rows })
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum UnifyVal<'ctx, TV> {
    /// Our unifier represents a type
    Ty(Ty<'ctx, TV>),
    /// Our unifier represents a row
    Row(RowSet<'ctx, TV>),
}
pub type InferUnifyVal<'infer> = UnifyVal<'infer, TcUnifierVar<'infer>>;
//impl<'ctx, TV: Eq + Clone + Debug> EqUnifyValue for UnifyVal<'ctx, TV> {}
impl<'ctx> UnifyValue for UnifyVal<'ctx, TcUnifierVar<'ctx>> {
    type Error = (Self, Self);

    fn unify_values(left: &Self, right: &Self) -> Result<Self, Self::Error> {
        match (left, right) {
            (UnifyVal::Ty(left), UnifyVal::Ty(right)) => Ty::unify_values(left, right)
                .map(UnifyVal::Ty)
                .map_err(|(a, b)| (UnifyVal::Ty(a), UnifyVal::Ty(b))),
            (UnifyVal::Ty(ty), UnifyVal::Row(_)) | (UnifyVal::Row(_), UnifyVal::Ty(ty)) => {
                match ty.deref() {
                    // A solved row replaces a row set when unified
                    TypeKind::RowTy(_) => Ok(UnifyVal::Ty(*ty)),
                    _ => Err((left.clone(), right.clone())),
                }
            }
            (UnifyVal::Row(left), UnifyVal::Row(right)) => RowSet::unify_values(left, right)
                .map(UnifyVal::Row)
                .map_err(|_| unreachable!()),
        }
    }
}

impl<'ctx, TV: Clone> TypeFoldable<'ctx> for UnifyVal<'ctx, TV> {
    type TypeVar = TV;
    type Out<T: 'ctx> = UnifyVal<'ctx, T>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::TypeVar>, F::Error> {
        match self {
            UnifyVal::Ty(ty) => ty.try_fold_with(fold).map(UnifyVal::Ty),
            UnifyVal::Row(row) => row.try_fold_with(fold).map(UnifyVal::Row),
        }
    }
}
impl<'ctx, TV> From<Ty<'ctx, TV>> for UnifyVal<'ctx, TV> {
    fn from(ty: Ty<'ctx, TV>) -> Self {
        UnifyVal::Ty(ty)
    }
}
impl<'ctx, TV> From<RowSet<'ctx, TV>> for UnifyVal<'ctx, TV> {
    fn from(row: RowSet<'ctx, TV>) -> Self {
        UnifyVal::Row(row)
    }
}

/// A trait for allocators that can make types and related data types.
pub trait MkTy<'ctx, TV> {
    fn mk_ty(&self, kind: TypeKind<'ctx, TV>) -> Ty<'ctx, TV>;
    fn mk_label(&self, label: &str) -> RowLabel<'ctx>;
    fn mk_row(&self, fields: &[RowLabel<'ctx>], values: &[Ty<'ctx, TV>]) -> ClosedRow<'ctx, TV>;

    fn single_row(&self, label: &str, value: Ty<'ctx, TV>) -> ClosedRow<'ctx, TV> {
        let field = self.mk_label(label);
        self.mk_row(&[field], &[value])
    }

    fn single_row_ty(&self, label: &str, value: Ty<'ctx, TV>) -> Ty<'ctx, TV> {
        self.mk_ty(TypeKind::RowTy(self.single_row(label, value)))
    }
}

/// During inference our type variables are all unification variables.
/// This is an alias to make inference types easy to talk about.
pub type InferTy<'ctx> = Ty<'ctx, TcUnifierVar<'ctx>>;

/// A monomorphic type.
///
/// This is just a wrapper around an interned reference to the `TypeKind` which contains the actual
/// data.
#[derive(Hash)]
pub struct Ty<'ctx, TV>(pub RefHandle<'ctx, TypeKind<'ctx, TV>>);
impl<'ctx, TV> PartialEq for Ty<'ctx, TV> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<'ctx, TV> Eq for Ty<'ctx, TV> {}

impl<'ctx, TV: Clone> Ty<'ctx, TV> {
    /// Convert a type to a row. If type is not representable as a row return type as an error.
    pub(crate) fn try_to_row(&self) -> Result<Row<'ctx, TV>, Ty<'ctx, TV>> {
        match self.deref() {
            TypeKind::RowTy(row) => Ok(Row::Closed(*row)),
            TypeKind::VarTy(var) => Ok(Row::Open(var.clone())),
            _ => Err(*self),
        }
    }
}

impl<'ctx, TV> Clone for Ty<'ctx, TV> {
    fn clone(&self) -> Self {
        Ty(self.0)
    }
}
impl<'ctx, TV> Copy for Ty<'ctx, TV> {}

impl<'ctx, TV> Deref for Ty<'ctx, TV> {
    type Target = <RefHandle<'ctx, TypeKind<'ctx, TV>> as Deref>::Target;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}
impl<'ctx, TV: fmt::Debug> fmt::Debug for Ty<'ctx, TV> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0 .0.fmt(f)
    }
}

impl<'ctx> EqUnifyValue for Ty<'ctx, TcUnifierVar<'ctx>> {}

impl<'ty, TV> Into<Ty<'ty, TV>> for RefHandle<'ty, TypeKind<'ty, TV>> {
    fn into(self) -> Ty<'ty, TV> {
        Ty(self)
    }
}

/// A label of a row field
pub type RowLabel<'ctx> = RefHandle<'ctx, str>;

/// A closed row is a map of labels to values where all labels are known.
/// Counterpart to an open row where the set of labels is polymorphic
///
/// Because our closed row is basically an interned map, some important invariants are maintained
/// by the construction of ClosedRow:
/// 1. fields and values are the same length
/// 2. The field at index i is the key for the type at index i in values
/// 3. fields is sorted lexographically
#[derive(PartialEq, Eq, Hash)]
pub struct ClosedRow<'ctx, TV> {
    pub fields: RefHandle<'ctx, [RowLabel<'ctx>]>,
    pub values: RefHandle<'ctx, [Ty<'ctx, TV>]>,
}

impl<'ctx, TV> ClosedRow<'ctx, TV> {
    pub fn len(&self) -> usize {
        // Because fields.len() must equal values.len() it doesn't matter which we use here
        self.fields.len()
    }

    /// Return true if `self` is a sub row of `row`, false otherwise.
    /// A row is a sub row if all it's fields are within the super row, and all values for those
    /// fields equal the values in the super row.
    pub fn is_sub_row(&self, row: &ClosedRow<'ctx, TV>) -> bool {
        self.fields
            .iter()
            .zip(self.values.iter())
            .all(|(field, value)| {
                row.fields
                    .as_ref()
                    .binary_search(field)
                    .map(|indx| value == &row.values[indx])
                    .unwrap_or(false)
            })
    }
}
impl<'ctx> ClosedRow<'ctx, TcUnifierVar<'ctx>> {
    /// Create a new row that contains all self fields that are not present in sub.
    pub fn difference(self, sub: Self) -> (Box<[RowLabel<'ctx>]>, Box<[InferTy<'ctx>]>) {
        let out_row = self
            .fields
            .iter()
            .zip(self.values.iter())
            .filter(|(field, _)| sub.fields.binary_search(field).is_err());

        let (mut fields, mut values) = (Vec::new(), Vec::new());
        for (field, value) in out_row {
            fields.push(*field);
            values.push(*value);
        }
        (fields.into_boxed_slice(), values.into_boxed_slice())
    }

    /// Combine two disjoint rows into a new row.
    /// This maintains the row invariants in the resulting row.
    /// If called on two overlapping rows an error is thrown.
    pub fn disjoint_union(
        self,
        right: Self,
    ) -> Result<(Box<[RowLabel<'ctx>]>, Box<[InferTy<'ctx>]>), TypeCheckError<'ctx>> {
        use std::cmp::Ordering::*;

        let goal_len = self.len() + right.len();
        let mut left_fields = self.fields.iter().peekable();
        let mut left_values = self.values.iter();
        let mut right_fields = right.fields.iter().peekable();
        let mut right_values = right.values.iter();

        let (mut fields, mut values): (Vec<RowLabel<'ctx>>, Vec<InferTy<'ctx>>) =
            (Vec::with_capacity(goal_len), Vec::with_capacity(goal_len));
        // Because we know our rows are each individually sorted, we can optimistically merge them here
        loop {
            match (left_fields.peek(), right_fields.peek()) {
                (Some(left_lbl), Some(right_lbl)) => {
                    // This ensures we don't use Handle::ord on accident
                    match str::cmp(left_lbl, right_lbl) {
                        // Push left
                        Less => {
                            fields.push(*left_fields.next().unwrap());
                            values.push(*left_values.next().unwrap());
                        }
                        // Because these are disjoint rows overlapping labels are an error
                        Equal => return Err(TypeCheckError::RowsNotDisjoint(self, right)),
                        // Push right
                        Greater => {
                            fields.push(*right_fields.next().unwrap());
                            values.push(*right_values.next().unwrap());
                        }
                    }
                }
                // Right row bigger than left
                (None, Some(_)) => {
                    fields.extend(right_fields);
                    values.extend(right_values);
                    break;
                }
                // Left row bigger than right
                (Some(_), None) => {
                    fields.extend(left_fields);
                    values.extend(left_values);
                    break;
                }
                (None, None) => break,
            }
        }

        fields.shrink_to_fit();
        values.shrink_to_fit();

        Ok((fields.into_boxed_slice(), values.into_boxed_slice()))
    }
}

impl<'ctx, TV> Clone for ClosedRow<'ctx, TV> {
    fn clone(&self) -> Self {
        ClosedRow {
            fields: self.fields,
            values: self.values,
        }
    }
}
impl<'ctx, TV> Copy for ClosedRow<'ctx, TV> {}

impl<'ctx, TV: Debug> Debug for ClosedRow<'ctx, TV> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(
                self.fields
                    .iter()
                    .map(|handle| &handle.0)
                    .zip(self.values.iter()),
            )
            .finish()
    }
}

impl<'ctx, TV: Clone> TypeFoldable<'ctx> for ClosedRow<'_, TV> {
    type TypeVar = TV;
    type Out<T: 'ctx> = ClosedRow<'ctx, T>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::TypeVar>, F::Error> {
        let labels = self
            .fields
            .iter()
            .map(|lbl| fold.ctx().mk_label(lbl))
            .collect::<Vec<_>>();
        let values = self
            .values
            .iter()
            .map(|ty| ty.try_fold_with(fold))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(fold.ctx().mk_row(labels.as_slice(), values.as_slice()))
    }
}

/// A row is our representaion of data, it maps fields to values.
/// Rows come in two flavors: Open and Closed.
#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
pub enum Row<'ctx, TV> {
    /// An open row is a polymorphic set of data. Used to allow generic row programming.
    Open(TV),
    /// A closed row is a concrete mapping from fields to values.
    Closed(ClosedRow<'ctx, TV>),
}

impl<'ctx, TV> From<TV> for Row<'ctx, TV> {
    fn from(var: TV) -> Self {
        Row::Open(var)
    }
}
impl<'ctx, TV> Row<'ctx, TV> {
    pub fn to_ty<I: MkTy<'ctx, TV>>(self, ctx: &I) -> Ty<'ctx, TV> {
        match self {
            Row::Open(var) => ctx.mk_ty(TypeKind::VarTy(var)),
            Row::Closed(row) => ctx.mk_ty(TypeKind::RowTy(row)),
        }
    }

    pub(crate) fn expect_closed(self, msg: &str) -> ClosedRow<'ctx, TV> {
        match self {
            Row::Closed(row) => row,
            Row::Open(_) => panic!("{msg}"),
        }
    }
}

pub type InferRow<'infer> = Row<'infer, TcUnifierVar<'infer>>;
impl<'ctx> UnifyValue for InferRow<'ctx> {
    type Error = TypeCheckError<'ctx>;

    fn unify_values(left: &Self, right: &Self) -> Result<Self, Self::Error> {
        match (left, right) {
            (Row::Open(left_var), Row::Open(right_var)) => {
                Ok(Row::Open(std::cmp::min(*left_var, *right_var)))
            }
            // Prefer the more solved row if possible
            (Row::Open(_), Row::Closed(_)) => Ok(right.clone()),
            (Row::Closed(_), Row::Open(_)) => Ok(left.clone()),
            (Row::Closed(left_row), Row::Closed(right_row)) => (left_row == right_row)
                .then(|| left.clone())
                .ok_or_else(|| TypeCheckError::RowsNotEqual(*left, *right)),
        }
    }
}
impl<'ctx, TV: Clone> TypeFoldable<'ctx> for Row<'_, TV> {
    type TypeVar = TV;
    type Out<T: 'ctx> = Row<'ctx, T>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::TypeVar>, F::Error> {
        match self {
            Row::Open(var) => fold.try_fold_row_var(var),
            Row::Closed(crow) => Ok(Row::Closed(crow.try_fold_with(fold)?)),
        }
    }
}

/// Data for `Ty`.
/// `TypeKind` is interned to produce a `Ty`.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum TypeKind<'ctx, TV> {
    /// Marker that signifies an operation produced an error. This exists so that we can try to
    /// gracefully recover from a type checking error and produce a list of errors at the end of type checking
    ErrorTy,
    /// Type of integers.
    //TODO: This should be part of some BaseTy or LiteralTy as opposed to a member of Type directly
    IntTy,
    /// A type variable, during type checking this may be either a unifier or a proper type variable
    VarTy(TV),
    /// A row type, this is specifically a closed row. Open rows are represented as VarTy
    RowTy(ClosedRow<'ctx, TV>),
    /// A function type
    FunTy(Ty<'ctx, TV>, Ty<'ctx, TV>),
    /// A product type. This is purely a wrapper type to coerce a row type to be a product.
    ProdTy(Row<'ctx, TV>),
}

/// Defines the default way to fold over something.
/// This is used by `TypeFoldable` and `FallibleTypeFold` to determine how to fold over something
/// when the trait implementator does not wish to use a custom traversal.
///
/// For example, the default fold over type is:
/// ```
/// fn try_default_fold<'ctx, F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
///     self,
///     fold: &mut F,
/// ) -> Result<Self::Out<'ctx, F::TypeVar>, F::Error> {
///     match self.deref() {
///         VarTy(ref var) => fold.try_fold_var(var),
///         IntTy => Ok(fold.ctx().mk_ty(TypeKind::IntTy)),
///         ErrorTy => Ok(fold.ctx().mk_ty(TypeKind::ErrorTy)),
///         FunTy(arg, ret) => {
///             let arg_ = arg.try_fold_with(fold)?;
///             let ret_ = ret.try_fold_with(fold)?;
///             Ok(fold.ctx().mk_ty(TypeKind::FunTy(arg_, ret_)))
///         }
///         RowTy(row) => {
///             let row_ = row.try_fold_with(fold)?;
///             Ok(fold.ctx().mk_ty(TypeKind::RowTy(row_)))
///         }
///     }
/// }
/// ```
/// So by default we visit each type and return it as is without modification.
trait DefaultFold {
    type TypeVar;
    type Out<'a, TV: 'a>;

    fn try_default_fold<'ctx, F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<'ctx, F::TypeVar>, F::Error>;
}

impl<'ty, TV: Clone> DefaultFold for Ty<'ty, TV> {
    type TypeVar = TV;
    type Out<'a, T: 'a> = Ty<'a, T>;

    fn try_default_fold<'ctx, F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<'ctx, F::TypeVar>, F::Error> {
        match self.deref() {
            TypeKind::VarTy(ref var) => fold.try_fold_var(var.clone()),
            TypeKind::IntTy => Ok(fold.ctx().mk_ty(TypeKind::IntTy)),
            TypeKind::ErrorTy => Ok(fold.ctx().mk_ty(TypeKind::ErrorTy)),
            TypeKind::FunTy(arg, ret) => {
                let arg_ = arg.try_fold_with(fold)?;
                let ret_ = ret.try_fold_with(fold)?;
                Ok(fold.ctx().mk_ty(TypeKind::FunTy(arg_, ret_)))
            }
            TypeKind::RowTy(row) => {
                let row_ = row.try_fold_with(fold)?;
                Ok(fold.ctx().mk_ty(TypeKind::RowTy(row_)))
            }
            TypeKind::ProdTy(row) => {
                let row_ = row.clone().try_fold_with(fold)?;
                Ok(fold.ctx().mk_ty(TypeKind::ProdTy(row_)))
            }
        }
    }
}

/// Defines a fold over types and sub components of types.
/// This is commonly used to perform substitution.
///
/// Pairs with `TypeFoldable` to perform a type fold over arbitrary data containing types.
pub trait FallibleTypeFold<'ctx>: Sized {
    type InTypeVar: Clone;
    type TypeVar: 'ctx + TryFrom<Self::InTypeVar>;
    type Error: From<<Self::TypeVar as TryFrom<Self::InTypeVar>>::Error>;

    fn ctx(&self) -> &dyn MkTy<'ctx, Self::TypeVar>;

    fn try_fold_ty<'a>(
        &mut self,
        t: Ty<'a, Self::InTypeVar>,
    ) -> Result<Ty<'ctx, Self::TypeVar>, Self::Error> {
        t.try_default_fold(self)
    }

    fn try_fold_var(
        &mut self,
        var: Self::InTypeVar,
    ) -> Result<Ty<'ctx, Self::TypeVar>, Self::Error> {
        let v = var.try_into()?;
        Ok(self.ctx().mk_ty(TypeKind::VarTy(v)))
    }

    fn try_fold_row_var(
        &mut self,
        var: Self::InTypeVar,
    ) -> Result<Row<'ctx, Self::TypeVar>, Self::Error> {
        let v = var.try_into()?;
        Ok(Row::Open(v))
    }
}

/// A trait for things that contain types.
/// This defines how to traverse `Self` to visit each type it contains and fold it.
///
/// Pairs with `FallibleTypeFold` to perform a type fold over arbitrary data containing types.
/// This could be `Ty` itself which would produce a new `Ty`, or it could be something like
/// `ClosedRow` which would produce a new `ClosedRow` by folding each type in the rows values.
pub trait TypeFoldable<'ctx> {
    type TypeVar;
    type Out<TV: 'ctx>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, InTypeVar = Self::TypeVar>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::TypeVar>, F::Error>;
}

impl<'ctx, 'ty, TV: Clone> TypeFoldable<'ctx> for Ty<'ty, TV> {
    type TypeVar = TV;
    type Out<T: 'ctx> = Ty<'ctx, T>;

    fn try_fold_with<F: FallibleTypeFold<'ctx, InTypeVar = TV>>(
        self,
        fold: &mut F,
    ) -> Result<Self::Out<F::TypeVar>, F::Error> {
        // If fold method is not specified we default to visiting every type via the `DefaultFold`
        // method.
        self.try_default_fold(fold)
    }
}
