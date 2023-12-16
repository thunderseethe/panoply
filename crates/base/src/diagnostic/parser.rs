//! This module defines errors from the parsing pass.

use crate::modules::Module;
use crate::{display_iter::DisplayIterSeparated, displayer::Displayer, span::Span};

use super::{Citation, Diagnostic};

/// A parsing error.
#[derive(Debug, Clone)]
pub enum ParseError {
    /// An unexpected token. `None` tokens indicate EOF.
    WrongToken {
        /// Where the wrong token was found.
        span: Span,
        /// The token that was found.
        got: String,
        /// The tokens that were expected instead.
        want_any: Vec<String>,
    },
}

impl Diagnostic for ParseError {
    fn name(&self) -> &'static str {
        match self {
            ParseError::WrongToken { .. } => "parser-wrong-token",
        }
    }

    fn principal<M: Displayer<Module>>(&self, _: &M) -> Citation {
        match self {
            ParseError::WrongToken {
                span,
                got,
                want_any,
            } => Citation {
                span: *span,
                message: format!(
                    "Unexpected token '{}'; expected one of {}",
                    got,
                    DisplayIterSeparated::new(want_any.iter(), ", ")
                ),
            },
        }
    }

    fn additional<M: Displayer<Module>>(&self, _: &M) -> Vec<Citation> {
        Vec::new()
    }
}
