use crate::span::{Span, SpanOf, Spanned};
use std::fmt::Debug;

/// A non-empty comma-separated list. To allow an empty list, wrap in `Option`.
#[derive(Clone, Copy, Debug)]
pub struct CommaSep<'a, T> {
    pub first: T,
    pub elems: &'a [(Span, T)],
    /// The optional final comma.
    pub comma: Option<Span>,
}

impl<'a, T: Spanned> Spanned for CommaSep<'a, T> {
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

/// A field with a label, separator, and target in `T`.
#[derive(Clone, Copy, Debug)]
pub struct Field<'i, T> {
    pub label: SpanOf<&'i str>,
    pub sep: Span,
    pub target: T,
}

impl<'i, T: Spanned> Spanned for Field<'i, T> {
    fn span(&self) -> Span {
        Span {
            start: self.label.start(),
            end: self.target.end(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Term<'a, 'i> {
    Binding {
        var: SpanOf<&'i str>,
        eq: Span,
        value: &'a Term<'a, 'i>,
        semi: Span,
        expr: &'a Term<'a, 'i>,
    },
    Abstraction {
        lbar: Span,
        arg: SpanOf<&'i str>,
        rbar: Span,
        body: &'a Term<'a, 'i>,
    },
    Application {
        func: &'a Term<'a, 'i>,
        lpar: Span,
        arg: &'a Term<'a, 'i>,
        rpar: Span,
    },
    ProductRow {
        lbrace: Span,
        fields: Option<CommaSep<'a, Field<'i, &'a Term<'a, 'i>>>>,
        rbrace: Span,
    },
    FieldAccess {
        product: &'a Term<'a, 'i>,
        dot: Span,
        field: SpanOf<&'i str>,
    },
    VariableRef(SpanOf<&'i str>),
    Parenthesized {
        lpar: Span,
        term: &'a Term<'a, 'i>,
        rpar: Span,
    },
}

impl<'a, 'i> Spanned for Term<'a, 'i> {
    fn span(&self) -> Span {
        match self {
            Term::Binding { var, expr, .. } => Span {
                start: var.start(),
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
            Term::ProductRow { lbrace, rbrace, .. } => Span {
                start: lbrace.start(),
                end: rbrace.end(),
            },
            Term::FieldAccess { product, field, .. } => Span {
                start: product.start(),
                end: field.end(),
            },
            Term::VariableRef(v) => v.span(),
            Term::Parenthesized { lpar, rpar, .. } => Span {
                start: lpar.start(),
                end: rpar.end(),
            },
        }
    }
}
