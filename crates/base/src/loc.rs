use std::hash::Hash;

/// A location in a source text. Contains redundant data to avoid extra computation.
///
/// All source texts have a "one past the end" location which corresponds to a cursor after the last
/// character.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Loc {
  /// A 0-indexed byte offset into the source text. Must point to the start of a valid unicode
  /// character (i.e., scalar value).
  pub byte: usize,
}

impl Loc {
  /// Returns a location at the beginning of the source text.
  pub fn start() -> Loc {
    Loc { byte: 0 }
  }
}
