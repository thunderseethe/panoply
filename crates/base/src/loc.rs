use std::{
  cmp::Ordering,
  hash::{Hash, Hasher},
};

use crate::file::FileId;

/// A location in a source text. Contains redundant data to avoid extra computation.
///
/// All source texts have a "one past the end" location which corresponds to a cursor after the last
/// character.
///
/// `Loc` implements `Eq`, `Hash`, `PartialEq`, and `PartialOrd`, but those comparisons only use the
/// `module` and `byte` fields.
#[derive(Clone, Copy)]
pub struct Loc {
  /// The module ID of the source text.
  pub file: FileId,

  /// A 0-indexed byte offset into the source text. Must point to the start of a valid unicode
  /// character (i.e., scalar value).
  pub byte: usize,

  /// The 0-indexed line that the byte offset lies on.
  pub line: usize,

  /// The 0-indexed column that the byte offset lies on on the line. Measured in characters
  /// (unicode scalar values) after the start of the line.
  pub col: usize,
}
impl std::fmt::Debug for Loc {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("Loc")
      .field("line", &self.line)
      .field("col", &self.col)
      .finish_non_exhaustive()
  }
}

impl Loc {
  /// Returns a location at the beginning of the source text.
  pub fn start(file: FileId) -> Loc {
    Loc {
      file,
      byte: 0,
      line: 0,
      col: 0,
    }
  }
}

impl Eq for Loc {}

impl Hash for Loc {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.file.hash(state);
    self.byte.hash(state);
  }
}

impl PartialEq for Loc {
  fn eq(&self, other: &Self) -> bool {
    self.file == other.file && self.byte == other.byte
  }
}

impl PartialOrd for Loc {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    (self.file == other.file).then(|| self.byte.cmp(&other.byte))
  }
}
