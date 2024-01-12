use std::{
  fmt::{self, Debug, Formatter},
  hash::{BuildHasherDefault, Hash, Hasher},
  ptr::NonNull,
};

use dashmap::{DashSet, SharedValue};
use rustc_hash::FxHasher;

use crate::memory::{
  arena::{Arena, ArenaByRef},
  handle::{Handle, RefHandle},
};

/// An interner for values of a given type. Ensures that equal values are represented by the same
/// pointer.
pub trait Interner<'a, T: Eq + ?Sized> {
  fn intern(&self, value: T) -> RefHandle<'a, T>;
}

/// As [`Interner`], but where values are provided by reference. Allows interning unsized types.
pub trait InternerByRef<'a, T: Eq + ?Sized> {
  fn intern_by_ref(&self, value: &T) -> RefHandle<'a, T>;
}

// A wrapper for a pointer whose identity is determined by its pointee. The pointee must not be
// mutated.
//
// This is a workaround to avoid lifetime issues with internal references. The unsafe blocks are
// safe because the pointee value never changes.
#[derive(Clone, Copy)]
struct ByPointee<T: ?Sized>(NonNull<T>);

impl<T: Debug + ?Sized> Debug for ByPointee<T> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    f.debug_tuple("ByPointee")
      .field(&unsafe { self.0.as_ref() })
      .finish()
  }
}

impl<T: Eq + ?Sized> Eq for ByPointee<T> {}

impl<T: Hash + ?Sized> Hash for ByPointee<T> {
  fn hash<H: Hasher>(&self, state: &mut H) {
    unsafe { self.0.as_ref() }.hash(state)
  }
}

impl<T: PartialEq + ?Sized> PartialEq for ByPointee<T> {
  fn eq(&self, other: &Self) -> bool {
    *unsafe { self.0.as_ref() } == *unsafe { other.0.as_ref() }
  }
}

/// A [`Sync`] interner with a small memory footprint. Loosely based on matklad's string interner:
/// https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html.
pub struct SyncInterner<'a, T: ?Sized, A> {
  storage: &'a A,
  table: DashSet<ByPointee<T>, BuildHasherDefault<FxHasher>>,
}

impl<'a, T, A> SyncInterner<'a, T, A>
where
  T: Eq + Hash + ?Sized,
{
  pub fn new(storage: &'a A) -> SyncInterner<'a, T, A> {
    SyncInterner {
      storage,
      table: DashSet::with_hasher(BuildHasherDefault::default()),
    }
  }
}

impl<'a, T, A> Debug for SyncInterner<'a, T, A>
where
  T: Debug + Eq + Hash + ?Sized,
  A: Debug,
{
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    f.debug_struct("SyncInterner")
      .field("storage", &self.storage)
      .field("table", &self.table)
      .finish()
  }
}

impl<'a, T, A> Interner<'a, T> for SyncInterner<'a, T, A>
where
  T: Eq + Hash,
  A: Arena<T>,
{
  fn intern(&self, value: T) -> RefHandle<'a, T> {
    let mut shard =
      self.table.shards()[self.table.determine_map(&ByPointee(NonNull::from(&value)))].write();
    if let Some((k, _)) = shard.get_key_value(&ByPointee(NonNull::from(&value))) {
      Handle(unsafe { k.0.as_ref() })
    } else {
      let p = self.storage.alloc(value);
      shard.insert(ByPointee(NonNull::from(p)), SharedValue::new(()));
      Handle(p)
    }
  }
}

impl<'a, T, A> InternerByRef<'a, T> for SyncInterner<'a, T, A>
where
  T: Eq + Hash + ?Sized,
  A: ArenaByRef<T>,
{
  fn intern_by_ref(&self, value: &T) -> RefHandle<'a, T> {
    let mut shard =
      self.table.shards()[self.table.determine_map(&ByPointee(NonNull::from(value)))].write();
    if let Some((k, _)) = shard.get_key_value(&ByPointee(NonNull::from(value))) {
      Handle(unsafe { k.0.as_ref() })
    } else {
      let p = self.storage.alloc_by_ref(value);
      shard.insert(ByPointee(NonNull::from(p)), SharedValue::new(()));
      Handle(p)
    }
  }
}

unsafe impl<'a, T, A> Sync for SyncInterner<'a, T, A>
where
  T: Send + ?Sized,
  A: Sync,
{
}
