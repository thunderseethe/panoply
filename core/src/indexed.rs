use bumpalo::Bump;
use la_arena::{Arena, Idx};

use crate::ident::Ident;
use crate::span::SpanOf;

/// A trait for a type that contains a mutable `Arena<T>` reference.
///
/// Allows types to implement `IndexedAllocate<A>` for a generic A as long as it has the required
/// arena.
pub trait HasArenaMut<T>: HasArenaRef<T> {
    fn arena_mut(&mut self) -> &mut Arena<T>;
}

/// A trait for a type that contains an immutable `Arena<T>` reference.
///
/// Allows types to readback indexed allocated terms without having mutable access.
pub trait HasArenaRef<T> {
    fn arena(&self) -> &Arena<T>;
}

/// A trait for a type that contains a `&'a Bump`
///
/// Allows types to implement `ReferenceAllocated<'a, A>` for a generic A as long as it has the
/// required arena.
pub trait HasRefArena<'a> {
    fn ref_arena(&self) -> &'a Bump;
}

/// Convert a reference arena allocated type into an indexed arena allocated type.
pub trait IndexedAllocate<A> {
    type Out;

    fn alloc(&self, alloc: &mut A) -> Self::Out;
}

impl<A, T: IndexedAllocate<A>> IndexedAllocate<A> for &T {
    type Out = T::Out;

    fn alloc(&self, alloc: &mut A) -> Self::Out {
        T::alloc(*self, alloc)
    }
}

impl<A> IndexedAllocate<A> for Ident {
    type Out = Ident;

    fn alloc(&self, _: &mut A) -> Self::Out {
        *self
    }
}

impl<A, T: IndexedAllocate<A>> IndexedAllocate<A> for Option<T> {
    type Out = Option<T::Out>;

    fn alloc(&self, alloc: &mut A) -> Self::Out {
        self.as_ref().map(|t| t.alloc(alloc))
    }
}

impl<A, V: IndexedAllocate<A>> IndexedAllocate<A> for SpanOf<V> {
    type Out = SpanOf<V::Out>;

    fn alloc(&self, alloc: &mut A) -> Self::Out {
        SpanOf {
            start: self.start,
            value: self.value.alloc(alloc),
            end: self.end,
        }
    }
}

pub trait ReferenceAllocate<'a, A> {
    type Out: 'a;

    fn ref_alloc(&self, alloc: &mut A) -> Self::Out;
}

impl<'a, A, T> ReferenceAllocate<'a, A> for &T
where
    T: ReferenceAllocate<'a, A>,
{
    type Out = T::Out;

    fn ref_alloc(&self, alloc: &mut A) -> Self::Out {
        T::ref_alloc(self, alloc)
    }
}

impl<'a, A, T> ReferenceAllocate<'a, A> for SpanOf<T>
where
    T: ReferenceAllocate<'a, A>,
{
    type Out = SpanOf<T::Out>;

    fn ref_alloc(&self, alloc: &mut A) -> Self::Out {
        SpanOf {
            start: self.start,
            value: self.value.ref_alloc(alloc),
            end: self.end,
        }
    }
}

impl<'a, A> ReferenceAllocate<'a, A> for Ident {
    type Out = Ident;

    fn ref_alloc(&self, _: &mut A) -> Self::Out {
        *self
    }
}
impl<'a, A, T> ReferenceAllocate<'a, A> for Option<T>
where
    T: ReferenceAllocate<'a, A>,
{
    type Out = Option<T::Out>;

    fn ref_alloc(&self, alloc: &mut A) -> Self::Out {
        self.as_ref().map(|t| t.ref_alloc(alloc))
    }
}

pub trait IdxAlloc<T> {
    fn alloc(&mut self, value: T) -> Idx<T>;
}
