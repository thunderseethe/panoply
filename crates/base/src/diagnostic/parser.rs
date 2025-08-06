//! This module defines errors from the parsing pass.

use std::ops::Range;

/// A parsing error.
#[derive(Debug, Clone)]
pub enum ParseError {
  /// An unexpected token. `None` tokens indicate EOF.
  WrongToken {
    /// Where the wrong token was found.
    span: Range<usize>,
    /// The token that was found.
    got: String,
    /// The tokens that were expected instead.
    want_any: Vec<String>,
  },
}
