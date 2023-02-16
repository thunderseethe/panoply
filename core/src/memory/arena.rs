use bumpalo::Bump;
use bumpalo_herd::Member;

/// An arena for values of a given type.
pub trait Arena<T> {
    /// Stores the given value, persisting it for the lifetime of this object.
    fn alloc<'a>(&'a self, value: T) -> &'a T {
        &self.alloc_slice_by_iter([value].into_iter())[0]
    }

    /// Stores the given values in a slice, persisting them for the lifetime of this object.
    fn alloc_slice_by_iter<'a, I>(&'a self, iter: I) -> &'a [T]
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: ExactSizeIterator;
}

/// As [`Arena`], but takes values by reference to support unsized types.
pub trait ArenaByRef<T: ?Sized> {
    /// Stores the given value, persisting it for the lifetime of this object.
    fn alloc_by_ref<'a>(&'a self, value: &T) -> &'a T;
}

impl<T: Copy> Arena<T> for Bump {
    fn alloc<'a>(&'a self, value: T) -> &'a T {
        self.alloc(value)
    }

    fn alloc_slice_by_iter<'a, I>(&'a self, iter: I) -> &'a [T]
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: ExactSizeIterator,
    {
        self.alloc_slice_fill_iter(iter)
    }
}

impl<T: Copy> ArenaByRef<[T]> for Bump {
    fn alloc_by_ref<'a>(&'a self, value: &[T]) -> &'a [T] {
        self.alloc_slice_copy(value)
    }
}

impl ArenaByRef<str> for Bump {
    fn alloc_by_ref<'a>(&'a self, value: &str) -> &'a str {
        self.alloc_str(value)
    }
}

impl<'h, T: Copy> Arena<T> for Member<'h> {
    fn alloc<'a>(&'a self, value: T) -> &'a T {
        self.alloc(value)
    }

    fn alloc_slice_by_iter<'a, I>(&'a self, iter: I) -> &'a [T]
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: ExactSizeIterator,
    {
        self.alloc_slice_fill_iter(iter)
    }
}

impl<'h, T: Copy> ArenaByRef<[T]> for Member<'h> {
    fn alloc_by_ref<'a>(&'a self, value: &[T]) -> &'a [T] {
        self.alloc_slice_copy(value)
    }
}

impl<'h> ArenaByRef<str> for Member<'h> {
    fn alloc_by_ref<'a>(&'a self, value: &str) -> &'a str {
        self.alloc_str(value)
    }
}
