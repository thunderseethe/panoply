//! This module defines a unified error type for errors from all stages.

use crate::displayer::Displayer;
use crate::ident::Ident;

use super::{
    lexer::LexError, nameres::NameResolutionError, parser::ParseError, Citation, Diagnostic,
};

/// Any Aiahr compilation error.
#[derive(Debug)]
pub enum AiahrcError<'s> {
    LexError(LexError),
    NameResolutionError(NameResolutionError),
    ParseError(ParseError<'s>),
}

impl<'s> From<LexError> for AiahrcError<'s> {
    fn from(err: LexError) -> Self {
        AiahrcError::LexError(err)
    }
}

impl<'s> From<NameResolutionError> for AiahrcError<'s> {
    fn from(err: NameResolutionError) -> Self {
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

    fn principal<M>(&self, modules: &M) -> Citation
    where
        M: Displayer<crate::id::ModuleId> + Displayer<Ident>,
    {
        match self {
            AiahrcError::LexError(err) => err.principal(modules),
            AiahrcError::NameResolutionError(err) => err.principal(modules),
            AiahrcError::ParseError(err) => err.principal(modules),
        }
    }

    fn additional<M>(&self, modules: &M) -> Vec<Citation>
    where
        M: Displayer<crate::id::ModuleId> + Displayer<Ident>,
    {
        match self {
            AiahrcError::LexError(err) => err.additional(modules),
            AiahrcError::NameResolutionError(err) => err.additional(modules),
            AiahrcError::ParseError(err) => err.additional(modules),
        }
    }
}
