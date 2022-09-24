use std::{fmt::Debug, iter::FusedIterator};

use crate::span::{Span, SpanOf, Spanned};

/// A non-empty list of elements, separated by some fixed separator. To allow an empty list, wrap in
/// `Option`.
#[derive(Clone, Copy, Debug)]
pub struct Separated<'a, T> {
    pub first: T,
    pub elems: &'a [(Span, T)],
    /// The optional final separator.
    pub comma: Option<Span>,
}

impl<'a, T> Separated<'a, T> {
    /// An iterator over the non-separator elements.
    pub fn elements<'b>(&'b self) -> Elements<'b, T> {
        Elements {
            cs: self,
            idx: Some(0),
        }
    }
}

impl<'a, T: Spanned> Spanned for Separated<'a, T> {
    fn span(&self) -> Span {
        Span {
            start: self.first.start(),
            end: self
                .comma
                .or(self.elems.last().map(|e| e.0))
                .unwrap_or(self.first.span())
                .end(),
        }
    }
}

/// An iterator over the elements of a `Separated`. Needed because `std::iter::Chain` does not
/// implement `ExactSizeIterator`.
#[derive(Debug)]
pub struct Elements<'a, T> {
    cs: &'a Separated<'a, T>,

    // `Some(0)` for `cs.first`, `Some(i + 1)` for `cs.elems[i].1`, or `None` for EOI.
    idx: Option<usize>,
}

impl<'a, T> Clone for Elements<'a, T> {
    fn clone(&self) -> Self {
        Elements {
            cs: self.cs,
            idx: self.idx,
        }
    }
}
impl<'a, T> ExactSizeIterator for Elements<'a, T> {}
impl<'a, T> FusedIterator for Elements<'a, T> {}
impl<'a, T> Iterator for Elements<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        Some(match self.idx? {
            0 => {
                self.idx = if self.cs.elems.is_empty() {
                    None
                } else {
                    Some(1)
                };
                &self.cs.first
            }
            i => {
                self.idx = if i == self.cs.elems.len() {
                    None
                } else {
                    Some(i + 1)
                };
                &self.cs.elems[i].1
            }
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let size = 1 + self.cs.elems.len();
        (size, Some(size))
    }
}

/// A field with a label in `L`, separator, and target in `T`.
#[derive(Clone, Copy, Debug)]
pub struct Field<L, T> {
    pub label: L,
    pub sep: Span,
    pub target: T,
}

impl<L: Spanned, T: Spanned> Spanned for Field<L, T> {
    fn span(&self) -> Span {
        Span {
            start: self.label.start(),
            end: self.target.end(),
        }
    }
}

/// A field with an identifier label, separator, and target in `T`.
pub type IdField<'i, T> = Field<SpanOf<&'i str>, T>;

/// A product row with values in `T`.
#[derive(Clone, Copy, Debug)]
pub struct ProductRow<'a, 'i, T> {
    pub lbrace: Span,
    pub fields: Option<Separated<'a, IdField<'i, T>>>,
    pub rbrace: Span,
}

impl<'a, 'i, T> Spanned for ProductRow<'a, 'i, T> {
    fn span(&self) -> Span {
        Span {
            start: self.lbrace.start(),
            end: self.rbrace.end(),
        }
    }
}

/// A sum row with value in `T`.
#[derive(Clone, Copy, Debug)]
pub struct SumRow<'i, T> {
    pub langle: Span,
    pub field: IdField<'i, T>,
    pub rangle: Span,
}

impl<'i, T> Spanned for SumRow<'i, T> {
    fn span(&self) -> Span {
        Span {
            start: self.langle.start(),
            end: self.rangle.end(),
        }
    }
}

/// A pattern over terms of type `T`.
#[derive(Clone, Copy, Debug)]
pub enum Pattern<'a, 'i, N> {
    ProductRow(ProductRow<'a, 'i, &'a Pattern<'a, 'i, N>>),
    SumRow(SumRow<'i, &'a Pattern<'a, 'i, N>>),
    Whole(SpanOf<N>),
}

impl<'a, 'i, N> Spanned for Pattern<'a, 'i, N> {
    fn span(&self) -> Span {
        match self {
            Pattern::ProductRow(p) => p.span(),
            Pattern::SumRow(s) => s.span(),
            Pattern::Whole(v) => v.span(),
        }
    }
}

/// An Aiahr term.
#[derive(Clone, Copy, Debug)]
pub enum Term<'a, 'i, N> {
    Binding {
        var: SpanOf<N>,
        eq: Span,
        value: &'a Term<'a, 'i, N>,
        semi: Span,
        expr: &'a Term<'a, 'i, N>,
    },
    Handle {
        with: Span,
        handler: &'a Term<'a, 'i, N>,
        do_: Span,
        expr: &'a Term<'a, 'i, N>,
    },
    Abstraction {
        lbar: Span,
        arg: SpanOf<N>,
        rbar: Span,
        body: &'a Term<'a, 'i, N>,
    },
    Application {
        func: &'a Term<'a, 'i, N>,
        lpar: Span,
        arg: &'a Term<'a, 'i, N>,
        rpar: Span,
    },
    ProductRow(ProductRow<'a, 'i, &'a Term<'a, 'i, N>>),
    SumRow(SumRow<'i, &'a Term<'a, 'i, N>>),
    DotAccess {
        base: &'a Term<'a, 'i, N>,
        dot: Span,
        field: SpanOf<&'i str>,
    },
    Match {
        match_: Span,
        langle: Span,
        cases: Separated<'a, Field<&'a Pattern<'a, 'i, N>, &'a Term<'a, 'i, N>>>,
        rangle: Span,
    },
    SymbolRef(SpanOf<N>),
    Parenthesized {
        lpar: Span,
        term: &'a Term<'a, 'i, N>,
        rpar: Span,
    },
}

impl<'a, 'i, N> Spanned for Term<'a, 'i, N> {
    fn span(&self) -> Span {
        match self {
            Term::Binding { var, expr, .. } => Span {
                start: var.start(),
                end: expr.end(),
            },
            Term::Handle { with, expr, .. } => Span {
                start: with.start(),
                end: expr.end(),
            },
            Term::Abstraction { lbar, body, .. } => Span {
                start: lbar.start(),
                end: body.end(),
            },
            Term::Application { func, rpar, .. } => Span {
                start: func.start(),
                end: rpar.end(),
            },
            Term::ProductRow(p) => p.span(),
            Term::SumRow(s) => s.span(),
            Term::DotAccess { base, field, .. } => Span {
                start: base.start(),
                end: field.end(),
            },
            Term::Match { match_, rangle, .. } => Span {
                start: match_.start(),
                end: rangle.end(),
            },
            Term::SymbolRef(v) => v.span(),
            Term::Parenthesized { lpar, rpar, .. } => Span {
                start: lpar.start(),
                end: rpar.end(),
            },
        }
    }
}

/// A top-level item in an Aiahr source file.
#[derive(Clone, Copy, Debug)]
pub enum Item<'a, 'i, N> {
    Term {
        name: SpanOf<N>,
        eq: Span,
        value: &'a Term<'a, 'i, N>,
    },
}

impl<'a, 'i, N> Item<'a, 'i, N> {
    /// Returns the name of the item.
    pub fn name(&self) -> SpanOf<N>
    where
        N: Clone,
    {
        match self {
            Item::Term { name, .. } => name.clone(),
        }
    }
}

impl<'a, 'i, N> Spanned for Item<'a, 'i, N> {
    fn span(&self) -> Span {
        match self {
            Item::Term { name, value, .. } => Span {
                start: name.start(),
                end: value.end(),
            },
        }
    }
}

// CST pattern macros. Used to construct patterns that ignore spans.

#[macro_export(local_inner_macros)]
macro_rules! separated {
    ($first:pat $(,$elems:pat)* $(,)?) => {
        $crate::cst::Separated {
            first: $first,
            elems: &[$((.., $elems)),*],
            ..
        }
    };
}

#[macro_export]
macro_rules! field {
    ($label:pat, $target:pat) => {
        $crate::cst::Field {
            label: $label,
            target: $target,
            ..
        }
    };
}

#[macro_export]
macro_rules! id_field {
    ($label:pat, $target:pat) => {
        $crate::field!($crate::span_of!($label), $target)
    };
}

#[macro_export(local_inner_macros)]
macro_rules! prod {
    ($($fields:pat),+ $(,)?) => {
        $crate::cst::ProductRow {
            fields: Some($crate::separated!($($fields),+)),
            ..
        }
    };
    () => {
        $crate::cst::ProductRow {
            fields: None,
            ..
        }
    }
}

#[macro_export(local_inner_macros)]
macro_rules! sum {
    ($field:pat) => {
        $crate::cst::SumRow { field: $field, .. }
    };
}

#[macro_export]
macro_rules! pat_prod {
    ($($fields:pat),* $(,)?) => {
        &$crate::cst::Pattern::ProductRow($crate::prod!($($fields,)+))
    };
}

#[macro_export]
macro_rules! pat_sum {
    ($field:pat) => {
        &$crate::cst::Pattern::SumRow($crate::sum!($field))
    };
}

#[macro_export]
macro_rules! pat_var {
    ($var:pat) => {
        &$crate::cst::Pattern::Whole($crate::span_of!($var))
    };
}

#[macro_export]
macro_rules! term_local {
    ($var:pat, $value:pat, $expr:pat) => {
        &$crate::cst::Term::Binding {
            var: $crate::span_of!($var),
            value: $value,
            expr: $expr,
            ..
        }
    };
}

#[macro_export]
macro_rules! term_with {
    ($handler:pat, $expr:pat) => {
        &$crate::cst::Term::Handle {
            handler: $handler,
            expr: $expr,
            ..
        }
    };
}

#[macro_export]
macro_rules! term_abs {
    ($arg:pat, $body:pat) => {
        &$crate::cst::Term::Abstraction {
            arg: $crate::span_of!($arg),
            body: $body,
            ..
        }
    };
}

#[macro_export]
macro_rules! term_app {
    ($func:pat, $arg:pat) => {
        &$crate::cst::Term::Application {
            func: $func,
            arg: $arg,
            ..
        }
    };
}

#[macro_export]
macro_rules! term_prod {
    ($($fields:pat),* $(,)?) => {
        &$crate::cst::Term::ProductRow($crate::prod!($($fields,)*))
    };
    () => {
        &$crate::cst::Term::ProductRow($crate::prod!())
    };
}

#[macro_export]
macro_rules! term_sum {
    ($field:pat) => {
        &$crate::cst::Term::SumRow($crate::sum!($field))
    };
}

#[macro_export]
macro_rules! term_dot {
    ($base:pat, $field:pat) => {
        &$crate::cst::Term::DotAccess {
            base: $base,
            field: $crate::span_of!($field),
            ..
        }
    };
}

#[macro_export]
macro_rules! term_match {
    ($($cases:pat),+ $(,)?) => {
        &$crate::cst::Term::Match { cases: $crate::separated!($($cases),+), .. }
    };
}

#[macro_export]
macro_rules! term_sym {
    ($var:pat) => {
        &$crate::cst::Term::SymbolRef($crate::span_of!($var))
    };
}

#[macro_export]
macro_rules! term_paren {
    ($term:pat) => {
        &$crate::cst::Term::Parenthesized { term: $term, .. }
    };
}

#[macro_export]
macro_rules! item_term {
    ($name:pat, $value:pat) => {
        $crate::cst::Item::Term {
            name: $crate::span_of!($name),
            value: $value,
            ..
        }
    };
}
