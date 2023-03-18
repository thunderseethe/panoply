//! This module defines errors from the parsing pass.

use std::{collections::LinkedList, fmt::Display};

use crate::{
    display_iter::DisplayIterSeparated, displayer::Displayer, id::ModuleId, span::Span,
    token::Token,
};

use super::{Citation, Diagnostic};

/// A parsing error.
#[derive(Debug, Clone)]
pub enum ParseError {
    /// An unexpected token. `None` tokens indicate EOF.
    WrongToken {
        /// Where the wrong token was found.
        span: Span,
        /// The token that was found.
        got: Option<Token>,
        /// The tokens that were expected instead.
        want_any: Vec<Option<Token>>,
    },
}

struct TokenOrEOFByName(Option<Token>);

impl Display for TokenOrEOFByName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(t) = self.0 {
            write!(f, "'{}'", t.name())
        } else {
            write!(f, "EOF")
        }
    }
}

impl Diagnostic for ParseError {
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
pub struct ParseErrors(pub LinkedList<ParseError>);

impl chumsky::Error<Token> for ParseErrors {
    type Span = Span;
    type Label = ();

    fn expected_input_found<Iter: IntoIterator<Item = Option<Token>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<Token>,
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
