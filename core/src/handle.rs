use std::{
    cmp::Ordering,
    fmt::{self, Debug, Formatter},
    hash::{Hash, Hasher},
    ptr,
};

/// A handle to an object of type `T`. `Handle` is a thin wrapper around a reference with slightly
/// different semantics: the identity of a handle is determined by its pointer value, rather than
/// the pointee value.
///
/// In particular, this means that:
///
///     * `Debug` only shows the pointer value
///     * `Eq`, `Ord`, `PartialEq`, and `PartialOrd` compare pointer values
///     * `Hash` is computed using the pointer value
///
/// These are useful semantics for, e.g., representing variable references. The same variable name
/// may be bound multiple times in disjoint expressions, but each reference refers to a specific
/// binding site. If binding sites are represented by `&'i str`s into the original source file, then
/// `Handle<'i, str>` is a good type to use to point each variable reference back to the
/// corresponding binding site.
pub struct Handle<'i, T: ?Sized>(pub &'i T);

impl<'i, T: ?Sized> Handle<'i, T> {
    // Explicitly coerces `self.0` into a pointer.
    fn ptr(&self) -> *const T {
        self.0 as *const T
    }
}

// `derive` doesn't understand that references are `Clone` and `Copy` regardless of whether `T` is,
// so `Clone` and `Copy` are implemented by hand; see
// https://github.com/rust-lang/rust/issues/26925.

impl<'i, T: ?Sized> Clone for Handle<'i, T> {
    fn clone(&self) -> Self {
        Handle(self.0)
    }
}

impl<'i, T: ?Sized> Copy for Handle<'i, T> {}

impl<'i, T: ?Sized> Debug for Handle<'i, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Handle").field(&self.ptr()).finish()
    }
}

impl<'i, T: ?Sized> Eq for Handle<'i, T> {}

impl<'i, T: ?Sized> Hash for Handle<'i, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        ptr::hash(self.0, state)
    }
}

impl<'i, T: ?Sized> Ord for Handle<'i, T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.ptr().cmp(&other.ptr())
    }
}

impl<'i, T: ?Sized> PartialEq for Handle<'i, T> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}

impl<'i, T: ?Sized> PartialOrd for Handle<'i, T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
