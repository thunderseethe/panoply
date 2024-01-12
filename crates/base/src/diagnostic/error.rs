//! This module defines a unified error type for errors from all stages.

use crate::displayer::Displayer;
use crate::ident::Ident;
use crate::modules::Module;

use super::tc::TypeCheckDiagnostic;
use super::{
  lexer::LexError, nameres::NameResolutionError, parser::ParseError, Citation, Diagnostic,
};

/// Any Panoply compilation error.
#[derive(Clone, Debug)]
pub enum PanoplyError {
  LexError(LexError),
  NameResolutionError(NameResolutionError),
  ParseError(ParseError),
  TypeCheckError(TypeCheckDiagnostic),
}

impl From<LexError> for PanoplyError {
  fn from(err: LexError) -> Self {
    PanoplyError::LexError(err)
  }
}

impl From<NameResolutionError> for PanoplyError {
  fn from(err: NameResolutionError) -> Self {
    PanoplyError::NameResolutionError(err)
  }
}

impl From<ParseError> for PanoplyError {
  fn from(err: ParseError) -> Self {
    PanoplyError::ParseError(err)
  }
}

impl From<TypeCheckDiagnostic> for PanoplyError {
  fn from(err: TypeCheckDiagnostic) -> Self {
    PanoplyError::TypeCheckError(err)
  }
}

impl Diagnostic for PanoplyError {
  fn name(&self) -> &'static str {
    match self {
      PanoplyError::LexError(err) => err.name(),
      PanoplyError::NameResolutionError(err) => err.name(),
      PanoplyError::ParseError(err) => err.name(),
      PanoplyError::TypeCheckError(err) => err.name(),
    }
  }

  fn principal<M>(&self, modules: &M) -> Citation
  where
    M: Displayer<Module> + Displayer<Ident>,
  {
    match self {
      PanoplyError::LexError(err) => err.principal(modules),
      PanoplyError::NameResolutionError(err) => err.principal(modules),
      PanoplyError::ParseError(err) => err.principal(modules),
      PanoplyError::TypeCheckError(err) => err.principal(modules),
    }
  }

  fn additional<M>(&self, modules: &M) -> Vec<Citation>
  where
    M: Displayer<Module> + Displayer<Ident>,
  {
    match self {
      PanoplyError::LexError(err) => err.additional(modules),
      PanoplyError::NameResolutionError(err) => err.additional(modules),
      PanoplyError::ParseError(err) => err.additional(modules),
      PanoplyError::TypeCheckError(err) => err.additional(modules),
    }
  }
}

#[salsa::accumulator]
pub struct PanoplyErrors(PanoplyError);
