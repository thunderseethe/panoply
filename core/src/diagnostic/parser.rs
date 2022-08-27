//! This module defines errors from the parsing pass.

use std::{collections::LinkedList, fmt::Display};

use crate::{display_iter::DisplayIterSeparated, span::Span, token::Token};

use super::{Citation, Diagnostic};

/// A parsing error.
#[derive(Debug, Clone)]
pub enum ParseError<'i> {
    /// An unexpected token. `None` tokens indicate EOF.
    WrongToken {
        /// Where the wrong token was found.
        span: Span,
        /// The token that was found.
        got: Option<Token<'i>>,
        /// The tokens that were expected instead.
        want_any: Vec<Option<Token<'i>>>,
    },
}

struct TokenOrEOFByName<'i>(Option<Token<'i>>);

impl<'i> Display for TokenOrEOFByName<'i> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(t) = self.0 {
            write!(f, "'{}'", t.name())
        } else {
            write!(f, "EOF")
        }
    }
}

impl<'i> Diagnostic for ParseError<'i> {
    fn name(&self) -> &'static str {
        match self {
            ParseError::WrongToken { .. } => "parser-wrong-token",
        }
    }

    fn principal(&self) -> Citation {
        match self {
            ParseError::WrongToken {
                span,
                got,
                want_any,
            } => Citation {
                span: *span,
                message: format!(
                    "Unexpected token '{}'; expected one of {}",
                    TokenOrEOFByName(*got),
                    DisplayIterSeparated::new(want_any.iter().map(|t| TokenOrEOFByName(*t)), ", ")
                ),
            },
        }
    }

    fn additional(&self) -> Vec<Citation> {
        Vec::new()
    }
}

#[derive(Debug)]
pub struct ParseErrors<'i>(LinkedList<ParseError<'i>>);

impl<'i> chumsky::Error<Token<'i>> for ParseErrors<'i> {
    type Span = Span;
    type Label = ();

    fn expected_input_found<Iter: IntoIterator<Item = Option<Token<'i>>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<Token<'i>>,
    ) -> Self {
        ParseErrors(LinkedList::from([ParseError::WrongToken {
            span,
            got: found.map(|s| s),
            want_any: expected.into_iter().collect(),
        }]))
    }

    fn with_label(self, _: Self::Label) -> Self {
        self
    }

    fn merge(mut self, other: Self) -> Self {
        let mut other = other;
        self.0.append(&mut other.0);
        self
    }
}
