use la_arena::Arena;

/// A trait for a type that contains an `Arena<T>`
///
/// Allows types to implement `IndexedAllocate<A>` for a generic A as long as it has the required
/// arena.
pub trait HasArena<T> {
    fn arena(&mut self) -> &mut Arena<T>;
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
