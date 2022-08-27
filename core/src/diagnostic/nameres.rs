//! This module defines errors from the name resolution pass.

use crate::span::{SpanOf, Spanned};

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
}

impl<'i> Diagnostic for NameResolutionError<'i> {
    fn name(&self) -> &'static str {
        match self {
            NameResolutionError::Duplicate { .. } => "name-resolution-duplicate-definition",
            NameResolutionError::NotFound(..) => "name-resolution-name-not-found",
        }
    }

    fn principal(&self) -> Citation {
        match self {
            NameResolutionError::Duplicate { duplicate, .. } => Citation {
                span: duplicate.span(),
                message: format!("Duplicate definition of symbol '{}'", duplicate.0),
            },
            NameResolutionError::NotFound(name) => Citation {
                span: name.span(),
                message: format!("Unknown symbol '{}'", name.0),
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
        }
    }
}
