use std::{fmt::Debug, iter::FusedIterator};

use crate::{
    memory::handle::RefHandle,
    span::{Span, SpanOf, Spanned},
};

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
pub type IdField<'s, T> = Field<SpanOf<RefHandle<'s, str>>, T>;

/// A product row with values in `T`.
#[derive(Clone, Copy, Debug)]
pub struct ProductRow<'a, 's, T> {
    pub lbrace: Span,
    pub fields: Option<Separated<'a, IdField<'s, T>>>,
    pub rbrace: Span,
}

impl<'a, 's, T> Spanned for ProductRow<'a, 's, T> {
    fn span(&self) -> Span {
        Span {
            start: self.lbrace.start(),
            end: self.rbrace.end(),
        }
    }
}

/// A sum row with value in `T`.
#[derive(Clone, Copy, Debug)]
pub struct SumRow<'s, T> {
    pub langle: Span,
    pub field: IdField<'s, T>,
    pub rangle: Span,
}

impl<'s, T> Spanned for SumRow<'s, T> {
    fn span(&self) -> Span {
        Span {
            start: self.langle.start(),
            end: self.rangle.end(),
        }
    }
}

/// A pattern over terms of type `T`.
#[derive(Clone, Copy, Debug)]
pub enum Pattern<'a, 's> {
    ProductRow(ProductRow<'a, 's, &'a Pattern<'a, 's>>),
    SumRow(SumRow<'s, &'a Pattern<'a, 's>>),
    Whole(SpanOf<RefHandle<'s, str>>),
}

impl<'a, 's> Spanned for Pattern<'a, 's> {
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
pub enum Term<'a, 's> {
    Binding {
        var: SpanOf<RefHandle<'s, str>>,
        eq: Span,
        value: &'a Term<'a, 's>,
        semi: Span,
        expr: &'a Term<'a, 's>,
    },
    Handle {
        with: Span,
        handler: &'a Term<'a, 's>,
        do_: Span,
        expr: &'a Term<'a, 's>,
    },
    Abstraction {
        lbar: Span,
        arg: SpanOf<RefHandle<'s, str>>,
        rbar: Span,
        body: &'a Term<'a, 's>,
    },
    Application {
        func: &'a Term<'a, 's>,
        lpar: Span,
        arg: &'a Term<'a, 's>,
        rpar: Span,
    },
    ProductRow(ProductRow<'a, 's, &'a Term<'a, 's>>),
    SumRow(SumRow<'s, &'a Term<'a, 's>>),
    DotAccess {
        base: &'a Term<'a, 's>,
        dot: Span,
        field: SpanOf<RefHandle<'s, str>>,
    },
    Match {
        match_: Span,
        langle: Span,
        cases: Separated<'a, Field<&'a Pattern<'a, 's>, &'a Term<'a, 's>>>,
        rangle: Span,
    },
    SymbolRef(SpanOf<RefHandle<'s, str>>),
    Parenthesized {
        lpar: Span,
        term: &'a Term<'a, 's>,
        rpar: Span,
    },
}

impl<'a, 's> Spanned for Term<'a, 's> {
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
pub enum Item<'a, 's> {
    Term {
        name: SpanOf<RefHandle<'s, str>>,
        eq: Span,
        value: &'a Term<'a, 's>,
    },
}

impl<'a, 's> Item<'a, 's> {
    /// Returns the name of the item.
    pub fn name(&self) -> SpanOf<RefHandle<'s, str>> {
        match self {
            Item::Term { name, .. } => *name,
        }
    }
}

impl<'a, 's> Spanned for Item<'a, 's> {
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
        $crate::field!($crate::span_of!($crate::h!($label)), $target)
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
        &$crate::cst::Pattern::Whole($crate::span_of!($crate::h!($var)))
    };
}

#[macro_export]
macro_rules! term_local {
    ($var:pat, $value:pat, $expr:pat) => {
        &$crate::cst::Term::Binding {
            var: $crate::span_of!($crate::h!((var))),
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
            arg: $crate::span_of!($crate::h!($arg)),
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
            field: $crate::span_of!($crate::h!($field)),
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
        &$crate::cst::Term::SymbolRef($crate::span_of!($crate::h!($var)))
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
            name: $crate::span_of!($crate::h!($name)),
            value: $value,
            ..
        }
    };
}
