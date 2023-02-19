//! This module defines errors from the parsing pass.

use std::{collections::LinkedList, fmt::Display};

use crate::{
    display_iter::DisplayIterSeparated, displayer::Displayer, id::ModuleId, span::Span,
    token::Token,
};

use super::{Citation, Diagnostic};

/// A parsing error.
#[derive(Debug, Clone)]
pub enum ParseError<'s> {
    /// An unexpected token. `None` tokens indicate EOF.
    WrongToken {
        /// Where the wrong token was found.
        span: Span,
        /// The token that was found.
        got: Option<Token<'s>>,
        /// The tokens that were expected instead.
        want_any: Vec<Option<Token<'s>>>,
    },
}

struct TokenOrEOFByName<'s>(Option<Token<'s>>);

impl<'s> Display for TokenOrEOFByName<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(t) = self.0 {
            write!(f, "'{}'", t.name())
        } else {
            write!(f, "EOF")
        }
    }
}

impl<'s> Diagnostic for ParseError<'s> {
    fn name(&self) -> &'static str {
        match self {
            ParseError::WrongToken { .. } => "parser-wrong-token",
        }
    }

    fn principal<M: Displayer<ModuleId>>(&self, _: &M) -> Citation {
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

    fn additional<M: Displayer<ModuleId>>(&self, _: &M) -> Vec<Citation> {
        Vec::new()
    }
}

#[derive(Debug)]
pub struct ParseErrors<'s>(pub LinkedList<ParseError<'s>>);

impl<'s> chumsky::Error<Token<'s>> for ParseErrors<'s> {
    type Span = Span;
    type Label = ();

    fn expected_input_found<Iter: IntoIterator<Item = Option<Token<'s>>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<Token<'s>>,
    ) -> Self {
        ParseErrors(LinkedList::from([ParseError::WrongToken {
            span,
            got: found,
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
