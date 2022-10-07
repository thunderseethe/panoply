use std::{
    marker::PhantomData,
    mem::transmute,
    ops::{Deref, Index},
};

pub trait Id {
    fn from_raw(raw: usize) -> Self;
    fn raw(&self) -> usize;
}

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

    pub fn is_empty(&self) -> bool {
        self.slice.is_empty()
    }

    pub fn len(&self) -> usize {
        self.slice.len()
    }

    pub fn to_gen(&self) -> IdGen<I, T>
    where
        T: Clone,
    {
        IdGen::from_raw(self.slice.to_vec())
    }

    pub fn into_gen(self: Box<Ids<I, T>>) -> IdGen<I, T> {
        // Safe because of #[repr(transparent)].
        IdGen::from_raw(unsafe { Box::<[T]>::from_raw(transmute(Box::into_raw(self))) }.into_vec())
    }
}

impl<I, T> Ids<I, T>
where
    I: Id,
{
    pub fn get(&self, index: I) -> Option<&T> {
        self.slice.get(index.raw())
    }

    pub unsafe fn get_unchecked(&self, index: I) -> &T {
        self.slice.get_unchecked(index.raw())
    }
}

impl<I, T> FromIterator<T> for Box<Ids<I, T>> {
    fn from_iter<J: IntoIterator<Item = T>>(iter: J) -> Self {
        Ids::from_boxed_raw(iter.into_iter().collect())
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

    pub fn new() -> IdGen<I, T> {
        IdGen::from_raw(Vec::new())
    }

    pub fn with_capacity(capacity: usize) -> IdGen<I, T> {
        IdGen::from_raw(Vec::with_capacity(capacity))
    }

    pub fn capacity(&self) -> usize {
        self.vec.capacity()
    }

    pub fn into_boxed_ids(self) -> Box<Ids<I, T>> {
        Ids::from_boxed_raw(self.vec.into_boxed_slice())
    }

    pub fn is_empty(&self) -> bool {
        self.vec.is_empty()
    }

    pub fn len(&self) -> usize {
        self.vec.len()
    }

    pub fn reserve(&mut self, additional: usize) {
        self.vec.reserve(additional)
    }

    pub fn reserve_exact(&mut self, additional: usize) {
        self.vec.reserve_exact(additional)
    }
}

impl<I, T> IdGen<I, T>
where
    I: Id,
{
    pub fn push(&mut self, value: T) -> I {
        let id = I::from_raw(self.len());
        self.vec.push(value);
        id
    }
}

impl<I, T> Deref for IdGen<I, T> {
    type Target = Ids<I, T>;

    fn deref(&self) -> &Self::Target {
        Ids::from_raw(&self.vec)
    }
}

impl<I, T> FromIterator<T> for IdGen<I, T> {
    fn from_iter<J: IntoIterator<Item = T>>(iter: J) -> Self {
        IdGen::from_raw(iter.into_iter().collect())
    }
}

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
