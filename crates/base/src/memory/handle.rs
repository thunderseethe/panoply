//! This module provides the notion of a "handle", which is a reference or pointer of some kind
//! whose identity corresponds to its pointer value, rather than the pointed-to data.

use std::{
  cmp::Ordering,
  fmt::{self, Debug, Formatter},
  hash::{Hash, Hasher},
  ops::Deref,
  ptr,
  rc::Rc,
  sync::Arc,
};

trait Ptr: Deref {
  fn ptr(&self) -> *const Self::Target {
    self.deref()
  }
}
impl<P: Deref> Ptr for P {}

/// A handle to an object of type `T`. `Handle` is a thin wrapper around a pointer type with
/// slightly different semantics: the identity of a handle is determined by its pointer value, rather
/// than the pointee value.
///
/// In particular, this means that:
///
/// * `Debug` only shows the pointer value
/// * `Eq`, `Ord`, `PartialEq`, and `PartialOrd` compare pointer values
/// * `Hash` is computed using the pointer value
///
/// These are useful semantics for, e.g., representing variable references. The same variable name
/// may be bound multiple times in disjoint expressions, but each reference refers to a specific
/// binding site. If binding sites are represented by `&'i str`s into the original source file, then
/// `Handle<'i, str>` is a good type to use to point each variable reference back to the
/// corresponding binding site.
#[derive(Clone, Copy)]
pub struct Handle<P>(pub P);

impl<P: Deref> Debug for Handle<P> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    f.debug_tuple("Handle").field(&self.0.ptr()).finish()
  }
}

impl<P: Deref> Eq for Handle<P> {}

impl<P: Deref> Hash for Handle<P> {
  fn hash<H: Hasher>(&self, state: &mut H) {
    ptr::hash(self.0.ptr(), state)
  }
}

impl<P: Deref> Ord for Handle<P> {
  fn cmp(&self, other: &Self) -> Ordering {
    self.0.ptr().cmp(&other.0.ptr())
  }
}

impl<P: Deref> PartialEq for Handle<P> {
  fn eq(&self, other: &Self) -> bool {
    ptr::eq(self.0.ptr(), other.0.ptr())
  }
}

impl<P: Deref> PartialOrd for Handle<P> {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl<P: Deref> Deref for Handle<P> {
  type Target = P::Target;

  fn deref(&self) -> &Self::Target {
    self.0.deref()
  }
}

impl<T: ?Sized, P: AsRef<T> + Deref> AsRef<T> for Handle<P> {
  fn as_ref(&self) -> &T {
    self.0.as_ref()
  }
}

/// See `Handle` for further info.
pub type RefHandle<'a, T> = Handle<&'a T>;

/// See `Handle` for further info.
pub type RcHandle<T> = Handle<Rc<T>>;

/// See `Handle` for further info.
pub type ArcHandle<T> = Handle<Arc<T>>;

/// Matches a handle by pointee value.
#[macro_export]
macro_rules! h {
  ($ptr:pat) => {
    $crate::memory::handle::Handle($ptr)
  };
}
