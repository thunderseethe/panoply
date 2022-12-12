use std::{
    iter::{Enumerate, Map},
    marker::PhantomData,
    mem::transmute,
    ops::{Deref, Index},
    slice::Iter,
};

/// An ID type that wraps a `usize`. Used to assign unique IDs to objects based on their index in an
/// array.
pub trait Id {
    fn from_raw(raw: usize) -> Self;
    fn raw(&self) -> usize;
}

/// An array of `I` IDs for `T` values. Similar to a `[T]`, but indexed by `I`s.
#[derive(Debug)]
#[repr(transparent)]
pub struct Ids<I, T> {
    _phantom: PhantomData<I>,
    slice: [T],
}

impl<I, T> Ids<I, T> {
    fn from_raw(raw: &[T]) -> &Ids<I, T> {
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

impl<I, T> Ids<I, T>
where
    I: Id,
{
    /// As `[T]::get(usize)`, but requires an `I`.
    pub fn get(&self, index: I) -> Option<&T> {
        self.slice.get(index.raw())
    }

    /// As `[T]::get__unchecked(usize)`, but requires an `I`.
    pub unsafe fn get_unchecked(&self, index: I) -> &T {
        self.slice.get_unchecked(index.raw())
    }

    /// As `[T]::iter().enumerate()`, but the indices are in `I`.
    pub fn iter_enumerate(&self) -> Map<Enumerate<Iter<'_, T>>, fn((usize, &T)) -> (I, &T)> {
        self.slice
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

/// An ID generator for `T` values indexed by `I`. Wraps a `Vec<T>` internally.
#[derive(Debug)]
pub struct IdGen<I, T> {
    vec: Vec<T>,
    _phantom: PhantomData<I>,
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
            self.iter_enumerate()
                .map(|(i, t)| op(i, t))
                .collect::<Vec<_>>(),
        )
    }
}

impl<I, T> Deref for IdGen<I, T> {
    type Target = Ids<I, T>;

    fn deref(&self) -> &Self::Target {
        Ids::from_raw(&self.vec)
    }
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
/// An ID for a module. Unique within a compilation session.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub ModuleId;

/// An ID for a top-level item in a module. Unique within a module.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub ItemId;

/// An ID for a local variable. Unique within a module.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub VarId;
);
