//! This module defines a unified error type for errors from all stages.

use crate::displayer::Displayer;
use crate::ident::Ident;
use crate::modules::Module;

use super::{
    lexer::LexError, nameres::NameResolutionError, parser::ParseError, Citation, Diagnostic,
};

/// Any Aiahr compilation error.
#[derive(Clone, Debug)]
pub enum AiahrcError {
    LexError(LexError),
    NameResolutionError(NameResolutionError),
    ParseError(ParseError),
}

impl From<LexError> for AiahrcError {
    fn from(err: LexError) -> Self {
        AiahrcError::LexError(err)
    }
}

impl From<NameResolutionError> for AiahrcError {
    fn from(err: NameResolutionError) -> Self {
        AiahrcError::NameResolutionError(err)
    }
}

impl From<ParseError> for AiahrcError {
    fn from(err: ParseError) -> Self {
        AiahrcError::ParseError(err)
    }
}

impl Diagnostic for AiahrcError {
    fn name(&self) -> &'static str {
        match self {
            AiahrcError::LexError(err) => err.name(),
            AiahrcError::NameResolutionError(err) => err.name(),
            AiahrcError::ParseError(err) => err.name(),
        }
    }

    fn principal<M>(&self, modules: &M) -> Citation
    where
        M: Displayer<Module> + Displayer<Ident>,
    {
        match self {
            AiahrcError::LexError(err) => err.principal(modules),
            AiahrcError::NameResolutionError(err) => err.principal(modules),
            AiahrcError::ParseError(err) => err.principal(modules),
        }
    }

    fn additional<M>(&self, modules: &M) -> Vec<Citation>
    where
        M: Displayer<Module> + Displayer<Ident>,
    {
        match self {
            AiahrcError::LexError(err) => err.additional(modules),
            AiahrcError::NameResolutionError(err) => err.additional(modules),
            AiahrcError::ParseError(err) => err.additional(modules),
        }
    }
}

#[salsa::accumulator]
pub struct AiahrcErrors(AiahrcError);
