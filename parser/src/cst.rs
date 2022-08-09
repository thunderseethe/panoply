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
    pub fields: Option<CommaSep<'a, IdField<'i, T>>>,
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
pub enum Pattern<'a, 'i> {
    ProductRow(ProductRow<'a, 'i, &'a Pattern<'a, 'i>>),
    SumRow(SumRow<'i, &'a Pattern<'a, 'i>>),
    Whole(SpanOf<&'i str>),
}

impl<'a, 'i> Spanned for Pattern<'a, 'i> {
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
    ProductRow(ProductRow<'a, 'i, &'a Term<'a, 'i>>),
    SumRow(SumRow<'i, &'a Term<'a, 'i>>),
    FieldAccess {
        product: &'a Term<'a, 'i>,
        dot: Span,
        field: SpanOf<&'i str>,
    },
    Match {
        match_: Span,
        cases: CommaSep<'a, Field<&'a Pattern<'a, 'i>, &'a Term<'a, 'i>>>,
        end: Span,
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
            Term::ProductRow(p) => p.span(),
            Term::SumRow(s) => s.span(),
            Term::FieldAccess { product, field, .. } => Span {
                start: product.start(),
                end: field.end(),
            },
            Term::Match { match_, end, .. } => Span {
                start: match_.start(),
                end: end.end(),
            },
            Term::VariableRef(v) => v.span(),
            Term::Parenthesized { lpar, rpar, .. } => Span {
                start: lpar.start(),
                end: rpar.end(),
            },
        }
    }
}

/// A top-level item in an Aiahr source file.
#[derive(Clone, Copy, Debug)]
pub enum Item<'a, 'i> {
    Term {
        name: SpanOf<&'i str>,
        eq: Span,
        value: &'a Term<'a, 'i>,
    },
}
