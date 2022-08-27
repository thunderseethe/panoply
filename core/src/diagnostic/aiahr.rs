//! This module defines a unified error type for errors from all stages.

use super::{nameres::NameResolutionError, parser::ParseError, Citation, Diagnostic};

/// Any Aiahr compilation error.
#[derive(Debug)]
pub enum AiahrcError<'i> {
    NameResolutionError(NameResolutionError<'i>),
    ParseError(ParseError<'i>),
}

impl<'i> From<NameResolutionError<'i>> for AiahrcError<'i> {
    fn from(err: NameResolutionError<'i>) -> Self {
        AiahrcError::NameResolutionError(err)
    }
}

impl<'i> From<ParseError<'i>> for AiahrcError<'i> {
    fn from(err: ParseError<'i>) -> Self {
        AiahrcError::ParseError(err)
    }
}

impl<'i> Diagnostic for AiahrcError<'i> {
    fn name(&self) -> &'static str {
        match self {
            AiahrcError::NameResolutionError(err) => err.name(),
            AiahrcError::ParseError(err) => err.name(),
        }
    }

    fn principal(&self) -> Citation {
        match self {
            AiahrcError::NameResolutionError(err) => err.principal(),
            AiahrcError::ParseError(err) => err.principal(),
        }
    }

    fn additional(&self) -> Vec<Citation> {
        match self {
            AiahrcError::NameResolutionError(err) => err.additional(),
            AiahrcError::ParseError(err) => err.additional(),
        }
    }
}
