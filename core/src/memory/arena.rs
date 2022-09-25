use bumpalo::Bump;
use bumpalo_herd::Herd;

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

/// An arena implementation using [`bumpalo::Bump`]. Only supports [`Copy`] types and [`str`], since
/// `Bump`s don't [`Drop`] their contents.
#[derive(Debug)]
pub struct BumpArena {
    arena: Bump,
}

impl BumpArena {
    pub fn new() -> BumpArena {
        BumpArena { arena: Bump::new() }
    }
}

impl<T: Copy> Arena<T> for BumpArena {
    fn alloc<'a>(&'a self, value: T) -> &'a T {
        self.arena.alloc(value)
    }

    fn alloc_slice_by_iter<'a, I>(&'a self, iter: I) -> &'a [T]
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: ExactSizeIterator,
    {
        self.arena.alloc_slice_fill_iter(iter)
    }
}

impl<T: Copy> ArenaByRef<[T]> for BumpArena {
    fn alloc_by_ref<'a>(&'a self, value: &[T]) -> &'a [T] {
        self.arena.alloc_slice_copy(value)
    }
}

impl ArenaByRef<str> for BumpArena {
    fn alloc_by_ref<'a>(&'a self, value: &str) -> &'a str {
        self.arena.alloc_str(value)
    }
}

/// An arena implementation using [`bumpalo_herd::Herd`]. As with [`BumpArena`], only supports
/// [`Copy`] types and [`str`], since `Bump`s don't [`Drop`] their contents.
#[derive(Debug)]
pub struct HerdArena {
    arenas: Herd,
}

impl HerdArena {
    pub fn new() -> HerdArena {
        HerdArena {
            arenas: Herd::new(),
        }
    }
}

impl<T: Copy> Arena<T> for HerdArena {
    fn alloc<'a>(&'a self, value: T) -> &'a T {
        self.arenas.get().alloc(value)
    }

    fn alloc_slice_by_iter<'a, I>(&'a self, iter: I) -> &'a [T]
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: ExactSizeIterator,
    {
        self.arenas.get().alloc_slice_fill_iter(iter)
    }
}

impl<T: Copy> ArenaByRef<[T]> for HerdArena {
    fn alloc_by_ref<'a>(&'a self, value: &[T]) -> &'a [T] {
        self.arenas.get().alloc_slice_copy(value)
    }
}

impl ArenaByRef<str> for HerdArena {
    fn alloc_by_ref<'a>(&'a self, value: &str) -> &'a str {
        self.arenas.get().alloc_str(value)
    }
}

unsafe impl Sync for HerdArena {}
