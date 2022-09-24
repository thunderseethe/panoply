//! This module defines errors from the name resolution pass.

use crate::{
    id::ModuleId,
    span::{SpanOf, Spanned},
};

use super::{Citation, Diagnostic};

/// A name resolution error.
#[derive(Debug)]
pub enum NameResolutionError<'i> {
    /// A duplicate name in the same layer, where the new name is not allowed to shadow the old one.
    Duplicate {
        /// The original name.
        original: SpanOf<&'i str>,
        /// The duplicate.
        duplicate: SpanOf<&'i str>,
    },
    /// A reference to a name that isn't defined.
    NotFound(SpanOf<&'i str>),
    /// A module used as a term.
    ModuleTerm(SpanOf<ModuleId>),
}

impl<'i> Diagnostic for NameResolutionError<'i> {
    fn name(&self) -> &'static str {
        match self {
            NameResolutionError::Duplicate { .. } => "name-resolution-duplicate-definition",
            NameResolutionError::NotFound(..) => "name-resolution-name-not-found",
            NameResolutionError::ModuleTerm(..) => "name-resolution-module-as-term",
        }
    }

    fn principal(&self) -> Citation {
        match self {
            NameResolutionError::Duplicate { duplicate, .. } => Citation {
                span: duplicate.span(),
                message: format!("Duplicate definition of symbol '{}'", duplicate.value),
            },
            NameResolutionError::NotFound(name) => Citation {
                span: name.span(),
                message: format!("Unknown symbol '{}'", name.value),
            },
            NameResolutionError::ModuleTerm(module) => Citation {
                span: module.span(),
                message: format!("Module \'{:?}\' used as a term", module.value),
            },
        }
    }

    fn additional(&self) -> Vec<Citation> {
        match self {
            NameResolutionError::Duplicate { original, .. } => vec![Citation {
                span: original.span(),
                message: "Original definition here".to_owned(),
            }],
            NameResolutionError::NotFound(..) => Vec::new(),
            NameResolutionError::ModuleTerm(..) => Vec::new(),
        }
    }
}
