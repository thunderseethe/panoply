//! This module defines types and traits for diagnostic messages (i.e., errors and warnings).

pub mod aiahr;
mod english;
pub mod lexer;
pub mod nameres;
pub mod parser;

use crate::{displayer::Displayer, id::ModuleId, span::Span};

/// Kinds of diagnostic messages.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum DiagnosticKind {
    /// An error. Any compilation unit that triggers an error will fail to compile.
    Error,
    /// A warning. Warnings do not cause compilation to fail.
    Warning,
}

/// An annotated citation from source code.
#[derive(Debug, Clone, PartialEq)]
pub struct Citation {
    /// The span of the citation in source code.
    pub span: Span,
    /// A message associated with the citation. Should begin with a capital letter but not end with
    /// punctuation.
    pub message: String, // TODO: use a `Display`-like trait.
}

/// A diagnostic message.
pub trait Diagnostic {
    /// The name of this type of diagnostic message. Must match `[a-z0-9-]+`. Intended for easy SEO.
    fn name(&self) -> &'static str;

    /// The principal source of the diagnostic.
    fn principal<M: Displayer<ModuleId>>(&self, modules: &M) -> Citation;

    /// Additional citations contributing to the diagnostic.
    fn additional<M: Displayer<ModuleId>>(&self, modules: &M) -> Vec<Citation>;
}

/// A sink for diagnostic messages of a given type.
pub trait DiagnosticSink<D>
where
    D: Diagnostic,
{
    /// Reports a new diagnostic.
    fn add(&mut self, diag: D);

    /// Reports all diagnostics from an iterator.
    fn add_all<I>(&mut self, iter: I)
    where
        I: Iterator<Item = D>,
    {
        iter.for_each(|d| self.add(d))
    }
}

impl<D, E> DiagnosticSink<D> for Vec<E>
where
    D: Diagnostic,
    E: From<D>,
{
    fn add(&mut self, diag: D) {
        self.push(E::from(diag))
    }

    fn add_all<I>(&mut self, iter: I)
    where
        I: Iterator<Item = D>,
    {
        self.extend(iter.map(E::from))
    }
}
