//! This module defines errors from the lexing pass.

use crate::{loc::Loc, span::Span};

use super::{Citation, Diagnostic};

/// A lexing error.
#[derive(Debug, Clone)]
pub enum LexError {
  /// The text at the given location could not be parsed as a token.
  NotAToken(Loc),
}

impl Diagnostic for LexError {
  fn name(&self) -> &'static str {
    match self {
      LexError::NotAToken(..) => "lexer-not-a-token",
    }
  }

  fn principal<M>(&self, _: &M) -> Citation {
    match self {
      LexError::NotAToken(loc) => Citation {
        span: Span::zero(*loc),
        message: "Not a valid token".to_owned(),
      },
    }
  }

  fn additional<M>(&self, _: &M) -> Vec<Citation> {
    Vec::new()
  }
}
