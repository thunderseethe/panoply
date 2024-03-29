use std::{
  iter::{Enumerate, Map},
  marker::PhantomData,
  mem::transmute,
  ops::{Deref, DerefMut, Index, IndexMut},
  slice::Iter,
};

use pretty::{DocAllocator, Pretty};

use crate::{ident::Ident, modules::Module, pretty::PrettyWithCtx};

/// An ID type that wraps a `usize`. Used to assign unique IDs to objects based on their index in an
/// array.
pub trait Id {
  fn from_raw(raw: usize) -> Self;
  fn raw(&self) -> usize;
}

/// An array of `I` IDs for `T` values. Similar to a `[T]`, but indexed by `I`s.
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Ids<I, T> {
  _phantom: PhantomData<I>,
  slice: [T],
}

impl<I, T> Clone for Box<Ids<I, T>>
where
  T: Clone,
{
  fn clone(&self) -> Self {
    Ids::from_boxed_raw(self.slice.to_vec().into_boxed_slice())
  }
}

impl<I, T> Ids<I, T> {
  fn from_raw(raw: &[T]) -> &Ids<I, T> {
    // Safe because of #[repr(transparent)].
    unsafe { transmute(raw) }
  }

  fn from_raw_mut(raw: &mut [T]) -> &mut Ids<I, T> {
    // Safe because of #[repr(transparent)].
    unsafe { transmute(raw) }
  }

  fn from_boxed_raw(raw: Box<[T]>) -> Box<Ids<I, T>> {
    // Safe because of #[repr(transparent)].
    unsafe { Box::from_raw(transmute(Box::into_raw(raw))) }
  }

  /// As `[T]::is_empty()`.
  pub fn is_empty(&self) -> bool {
    self.slice.is_empty()
  }

  /// As `[T]::len()`.
  pub fn len(&self) -> usize {
    self.slice.len()
  }

  /// Copies the slice into an [`IdGen`].
  pub fn to_gen(&self) -> IdGen<I, T>
  where
    T: Clone,
  {
    IdGen::from_raw(self.slice.to_vec())
  }

  /// Consumes a boxed slice and converts it to an [`IdGen`].
  pub fn into_gen(self: Box<Ids<I, T>>) -> IdGen<I, T> {
    // Safe because of #[repr(transparent)].
    IdGen::from_raw(unsafe { Box::<[T]>::from_raw(transmute(Box::into_raw(self))) }.into_vec())
  }
}

type IdIter<'a, I, T> = Map<Enumerate<Iter<'a, T>>, fn((usize, &T)) -> (I, &T)>;

impl<I, T> Ids<I, T>
where
  I: Id,
{
  /// As `[T]::get(usize)`, but requires an `I`.
  pub fn get(&self, index: I) -> Option<&T> {
    self.slice.get(index.raw())
  }

  /// As `[T]::get__unchecked(usize)`, but requires an `I`.
  /// # Safety
  ///
  /// Calling this method with an out-of-bounds index is *[undefined behavior]*
  /// even if the resulting reference is not used.
  pub unsafe fn get_unchecked(&self, index: I) -> &T {
    self.slice.get_unchecked(index.raw())
  }

  /// As `[T]::iter().enumerate()`, but the indices are in `I`.
  pub fn iter_enumerate(&self) -> IdIter<'_, I, T> {
    self
      .slice
      .iter()
      .enumerate()
      .map(|(i, x)| (I::from_raw(i), x))
  }
}

impl<I, T> Index<I> for Ids<I, T>
where
  I: Id,
{
  type Output = T;

  fn index(&self, index: I) -> &Self::Output {
    &self.slice[index.raw()]
  }
}

impl<I, T> IndexMut<I> for Ids<I, T>
where
  I: Id,
{
  fn index_mut(&mut self, index: I) -> &mut Self::Output {
    &mut self.slice[index.raw()]
  }
}

/// An ID generator for `T` values indexed by `I`. Wraps a `Vec<T>` internally.
#[derive(Debug)]
pub struct IdGen<I, T> {
  vec: Vec<T>,
  _phantom: PhantomData<I>,
}

impl<I, T> IntoIterator for IdGen<I, T> {
  type Item = T;

  type IntoIter = std::vec::IntoIter<T>;

  fn into_iter(self) -> Self::IntoIter {
    self.vec.into_iter()
  }
}
impl<I, T> FromIterator<T> for IdGen<I, T> {
  fn from_iter<II: IntoIterator<Item = T>>(iter: II) -> Self {
    Self {
      vec: iter.into_iter().collect(),
      _phantom: PhantomData,
    }
  }
}

impl<I, T> IdGen<I, T> {
  fn from_raw(raw: Vec<T>) -> IdGen<I, T> {
    IdGen {
      vec: raw,
      _phantom: PhantomData,
    }
  }

  /// As `Vec<T>::new()`.
  pub fn new() -> IdGen<I, T> {
    IdGen::from_raw(Vec::new())
  }

  /// As `Vec<T>::with_capacity()`.
  pub fn with_capacity(capacity: usize) -> IdGen<I, T> {
    IdGen::from_raw(Vec::with_capacity(capacity))
  }

  /// As `Vec<T>::capacity()`.
  pub fn capacity(&self) -> usize {
    self.vec.capacity()
  }

  /// As `Vec<T>::is_empty()`.
  pub fn is_empty(&self) -> bool {
    self.vec.is_empty()
  }

  /// As `Vec<T>::len()`.
  pub fn len(&self) -> usize {
    self.vec.len()
  }

  /// As `Vec<T>::reserve()`.
  pub fn reserve(&mut self, additional: usize) {
    self.vec.reserve(additional)
  }

  /// As `Vec<T>::reserve_exact()`.
  pub fn reserve_exact(&mut self, additional: usize) {
    self.vec.reserve_exact(additional)
  }

  /// Consumes the `IdGen` and converts it into a `Box<Ids<I, T>>`.
  pub fn into_boxed_ids(self) -> Box<Ids<I, T>> {
    Ids::from_boxed_raw(self.vec.into_boxed_slice())
  }
}

impl<I, T> IdGen<I, T>
where
  I: Id,
{
  /// As `Vec<T>::push()`, but returns the ID of the new element.
  pub fn push(&mut self, value: T) -> I {
    let id = I::from_raw(self.len());
    self.vec.push(value);
    id
  }

  /// Create a new IdGen from an existing one by mapping existing elements into new ones.
  pub fn create_from<U>(&self, mut op: impl FnMut(I, &T) -> U) -> IdGen<I, U> {
    IdGen::from_raw(
      self
        .iter_enumerate()
        .map(|(i, t)| op(i, t))
        .collect::<Vec<_>>(),
    )
  }
}

impl<I, T> Default for IdGen<I, T> {
  fn default() -> Self {
    IdGen {
      vec: Vec::new(),
      _phantom: PhantomData,
    }
  }
}

impl<I> IdGen<I, ()>
where
  I: Id,
{
  /// Generate a new Id and return it.
  /// Convenience method when we aren't storing any values with our Ids.
  pub fn generate(&mut self) -> I {
    self.push(())
  }
}

impl<I, T> Deref for IdGen<I, T> {
  type Target = Ids<I, T>;

  fn deref(&self) -> &Self::Target {
    Ids::from_raw(&self.vec)
  }
}

impl<I, T> DerefMut for IdGen<I, T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    Ids::from_raw_mut(&mut self.vec)
  }
}

#[derive(Debug, PartialEq, Eq)]
pub struct IdSupply<I> {
  next: u32,
  _marker: PhantomData<I>,
}
impl<I> Default for IdSupply<I> {
  fn default() -> Self {
    Self {
      next: 0,
      _marker: PhantomData,
    }
  }
}
impl<I, T> From<&IdGen<I, T>> for IdSupply<I> {
  fn from(value: &IdGen<I, T>) -> Self {
    Self {
      next: value.len().try_into().unwrap(),
      _marker: PhantomData,
    }
  }
}
impl<I: Id> IdSupply<I> {
  pub fn start_from(prev: &Self) -> Self {
    Self {
      next: prev.next,
      ..Default::default()
    }
  }

  pub fn supply_id(&mut self) -> I {
    let id = I::from_raw(self.next as usize);
    self.next += 1;
    id
  }
}

/// An identifier for a name resolved term definition
#[salsa::interned]
pub struct TermName {
  pub name: Ident,
  pub module: Module,
}

impl TermName {
  pub fn name_text<Db: ?Sized + crate::Db>(self, db: &Db) -> &str {
    let core_db = db.as_core_db();
    self.name(core_db).text(core_db)
  }
}

/// An identifier for a name resolved effect definition
#[salsa::interned]
pub struct EffectName {
  pub name: Ident,
  pub module: Module,
}

/// An identifier for a name resolved effect operator definition
#[salsa::interned]
pub struct EffectOpName {
  pub name: Ident,
  pub effect: EffectName,
}

#[macro_export]
macro_rules! define_ids {
    ($($(#[$attr:meta])* $vis:vis $name:ident ;)*) => {
        $($(#[$attr])*
        pub struct $name(pub usize);

        impl $crate::id::Id for $name {
            fn from_raw(raw: usize) -> Self {
                $name(raw)
            }

            fn raw(&self) -> usize {
                self.0
            }
        })*
    };
}

define_ids!(
/// A type variable. Unique within a type scheme.
/// These are explicity referred to by the AST and can persist through type checking (unlike
/// unification variables). They may not be modified by the type checking process, often
/// referred to as untouchabale.k
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub TyVarId;

/// An ID for a local variable. Unique within a module.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub VarId;

/// Uniquely identifies variables in ReducIR. Unique within a module.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub ReducIrVarId;

/// Uniquely identifies an IR type. Unique within a module.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub ReducIrTyVarId;

/// Uniquely identifies variables in MedIR. Unique within a module.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub MedIrVarId;
);

impl<'a, D, A: 'a> Pretty<'a, D, A> for VarId
where
  D: DocAllocator<'a, A>,
{
  fn pretty(self, allocator: &'a D) -> pretty::DocBuilder<'a, D, A> {
    allocator
      .text("var")
      .append(allocator.as_string(self.0).angles())
  }
}

impl<'a, D, A> Pretty<'a, D, A> for TyVarId
where
  A: 'a,
  D: DocAllocator<'a, A>,
{
  fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, A> {
    alloc
      .text("ty_var")
      .append(alloc.as_string(self.0).angles())
  }
}

impl<Ctx> PrettyWithCtx<Ctx> for TyVarId {
  fn pretty<'a>(
    &self,
    _: &Ctx,
    alloc: &'a pretty::RcAllocator,
  ) -> pretty::DocBuilder<'a, pretty::RcAllocator> {
    alloc
      .text("ty_var")
      .append(alloc.as_string(self.0).angles())
  }
}

impl<'a, D, A: 'a> Pretty<'a, D, A> for ReducIrVarId
where
  D: DocAllocator<'a, A>,
{
  fn pretty(self, allocator: &'a D) -> pretty::DocBuilder<'a, D, A> {
    allocator
      .text("ir_var")
      .append(allocator.as_string(self.0).angles())
  }
}
