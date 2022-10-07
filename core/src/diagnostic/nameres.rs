//! This module defines errors from the name resolution pass.

use crate::{
    id::ModuleId,
    memory::handle::RefHandle,
    span::{Span, SpanOf, Spanned},
};

use super::{Citation, Diagnostic};

/// A name resolution error.
#[derive(Debug)]
pub enum NameResolutionError<'s> {
    /// A duplicate name in the same layer, where the new name is not allowed to shadow the old one.
    Duplicate {
        /// The duplicated name.
        name: RefHandle<'s, str>,
        /// The original definition site.
        original: Span,
        /// The duplicate definition site.
        duplicate: Span,
    },
    /// A reference to a name that isn't defined.
    NotFound(SpanOf<RefHandle<'s, str>>),
    /// A reference to a name in a module that isn't defined.
    NotFoundIn {
        /// The searched-in module.
        module: ModuleId,
        /// The name that isn't defined in the module.
        name: SpanOf<RefHandle<'s, str>>,
    },
    /// A module used as a term.
    ModuleTerm(SpanOf<ModuleId>),
}

impl<'s> Diagnostic for NameResolutionError<'s> {
    fn name(&self) -> &'static str {
        match self {
            NameResolutionError::Duplicate { .. } => "name-resolution-duplicate-definition",
            NameResolutionError::NotFound(..) => "name-resolution-name-not-found",
            NameResolutionError::NotFoundIn { .. } => "name-resolution-name-not-found-in-module",
            NameResolutionError::ModuleTerm(..) => "name-resolution-module-as-term",
        }
    }

    fn principal(&self) -> Citation {
        match self {
            NameResolutionError::Duplicate {
                name, duplicate, ..
            } => Citation {
                span: *duplicate,
                message: format!("Duplicate definition of symbol '{}'", name.0),
            },
            NameResolutionError::NotFound(name) => Citation {
                span: name.span(),
                message: format!("Unknown symbol '{}'", name.value.0),
            },
            NameResolutionError::NotFoundIn { module, name } => Citation {
                span: name.span(),
                message: format!(
                    "Symbol '{}' not found in module '{:?}'",
                    name.value.0, module
                ),
            },
            NameResolutionError::ModuleTerm(module) => Citation {
                span: module.span(),
                message: format!("Module '{:?}' used as a term", module.value),
            },
        }
    }

    fn additional(&self) -> Vec<Citation> {
        match self {
            NameResolutionError::Duplicate { original, .. } => vec![Citation {
                span: *original,
                message: "Original definition here".to_owned(),
            }],
            NameResolutionError::NotFound(..) => Vec::new(),
            NameResolutionError::NotFoundIn { .. } => Vec::new(),
            NameResolutionError::ModuleTerm(..) => Vec::new(),
        }
    }
}
