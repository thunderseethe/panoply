//! This module defines a unified error type for errors from all stages.

use super::{
    lexer::LexError, nameres::NameResolutionError, parser::ParseError, Citation, Diagnostic,
};

/// Any Aiahr compilation error.
#[derive(Debug)]
pub enum AiahrcError<'s> {
    LexError(LexError),
    NameResolutionError(NameResolutionError<'s>),
    ParseError(ParseError<'s>),
}

impl<'s> From<NameResolutionError<'s>> for AiahrcError<'s> {
    fn from(err: NameResolutionError<'s>) -> Self {
        AiahrcError::NameResolutionError(err)
    }
}

impl<'s> From<ParseError<'s>> for AiahrcError<'s> {
    fn from(err: ParseError<'s>) -> Self {
        AiahrcError::ParseError(err)
    }
}

impl<'s> Diagnostic for AiahrcError<'s> {
    fn name(&self) -> &'static str {
        match self {
            AiahrcError::LexError(err) => err.name(),
            AiahrcError::NameResolutionError(err) => err.name(),
            AiahrcError::ParseError(err) => err.name(),
        }
    }

    fn principal(&self) -> Citation {
        match self {
            AiahrcError::LexError(err) => err.principal(),
            AiahrcError::NameResolutionError(err) => err.principal(),
            AiahrcError::ParseError(err) => err.principal(),
        }
    }

    fn additional(&self) -> Vec<Citation> {
        match self {
            AiahrcError::LexError(err) => err.additional(),
            AiahrcError::NameResolutionError(err) => err.additional(),
            AiahrcError::ParseError(err) => err.additional(),
        }
    }
}
