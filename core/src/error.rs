use std::fmt::Debug;

use crate::{
    loc::Loc,
    span::{Span, SpanOf},
    token::Token,
};

// TODO: design a better error type
#[derive(Debug)]
pub enum AiahrcError<'i> {
    NameResolutionError(NameResolutionError<'i>),
    ParseError(ParseError<'i>),
}

#[derive(Debug, Clone)]
pub enum ParseError<'i> {
    // `None` tokens indicate EOF.
    WrongToken {
        loc: Loc,
        got: Option<Token<'i>>,
        want_any: Vec<Option<Token<'i>>>,
    },
}

#[derive(Debug)]
pub struct ParseErrors<'i>(Vec<ParseError<'i>>);

impl<'i> chumsky::Error<Token<'i>> for ParseErrors<'i> {
    type Span = Span;
    type Label = ();

    fn expected_input_found<Iter: IntoIterator<Item = Option<Token<'i>>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<Token<'i>>,
    ) -> Self {
        ParseErrors(vec![ParseError::WrongToken {
            loc: span.start,
            got: found.map(|s| s),
            want_any: expected.into_iter().collect(),
        }])
    }

    fn with_label(self, _: Self::Label) -> Self {
        self
    }

    fn merge(self, other: Self) -> Self {
        let ParseErrors(mut errors) = self;
        let ParseErrors(mut other) = other;
        errors.append(&mut other);
        ParseErrors(errors)
    }
}

#[derive(Debug)]
pub enum NameResolutionError<'i> {
    Duplicate {
        original: SpanOf<&'i str>,
        duplicate: SpanOf<&'i str>,
    },
    NotFound(SpanOf<&'i str>),
}

pub trait Errors<E> {
    fn new() -> Self;
    fn push(&mut self, error: E);
}

impl<E> Errors<E> for Vec<E> {
    fn new() -> Self {
        Vec::new()
    }

    fn push(&mut self, error: E) {
        self.push(error)
    }
}
