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
                .unwrap_or_else(|| self.first.span())
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
        Span::join(&self.label, &self.target)
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
        Span::join(&self.lbrace, &self.rbrace)
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
        Span::join(&self.langle, &self.rangle)
    }
}

/// A non-empty row with concrete fields in `C`.
#[derive(Clone, Copy, Debug)]
pub enum Row<'a, 's, C> {
    Concrete(Separated<'a, C>),
    Variable(Separated<'a, SpanOf<RefHandle<'s, str>>>),
    Mixed {
        concrete: Separated<'a, C>,
        vbar: Span,
        variables: Separated<'a, SpanOf<RefHandle<'s, str>>>,
    },
}

impl<'a, 's, C: Spanned> Spanned for Row<'a, 's, C> {
    fn span(&self) -> Span {
        match self {
            Row::Concrete(c) => c.span(),
            Row::Variable(v) => v.span(),
            Row::Mixed {
                concrete,
                variables,
                ..
            } => Span::join(concrete, variables),
        }
    }
}

/// A row of types.
pub type TypeRow<'a, 's> = Row<'a, 's, IdField<'s, &'a Type<'a, 's>>>;

/// An unqualified Aiahr type.
#[derive(Clone, Copy, Debug)]
pub enum Type<'a, 's> {
    Named(SpanOf<RefHandle<'s, str>>),
    Sum {
        langle: Span,
        variants: TypeRow<'a, 's>,
        rangle: Span,
    },
    Product {
        lbrace: Span,
        fields: Option<TypeRow<'a, 's>>,
        rbrace: Span,
    },
    Function {
        domain: &'a Type<'a, 's>,
        arrow: Span,
        codomain: &'a Type<'a, 's>,
    },
    Parenthesized {
        lpar: Span,
        type_: &'a Type<'a, 's>,
        rpar: Span,
    },
}

impl<'a, 's> Spanned for Type<'a, 's> {
    fn span(&self) -> Span {
        match self {
            Type::Named(n) => n.span(),
            Type::Sum { langle, rangle, .. } => Span::join(langle, rangle),
            Type::Product { lbrace, rbrace, .. } => Span::join(lbrace, rbrace),
            Type::Function {
                domain, codomain, ..
            } => Span::join(*domain, *codomain),
            Type::Parenthesized { lpar, rpar, .. } => Span::join(lpar, rpar),
        }
    }
}

/// An atomic row for use in a type constraint.
#[derive(Clone, Copy, Debug)]
pub enum RowAtom<'a, 's> {
    Concrete {
        lpar: Span,
        fields: Separated<'a, IdField<'s, &'a Type<'a, 's>>>,
        rpar: Span,
    },
    Variable(SpanOf<RefHandle<'s, str>>),
}

impl<'a, 's> Spanned for RowAtom<'a, 's> {
    fn span(&self) -> Span {
        match self {
            RowAtom::Concrete { lpar, rpar, .. } => Span::join(lpar, rpar),
            RowAtom::Variable(v) => v.span(),
        }
    }
}

/// A type constraint.
#[derive(Clone, Copy, Debug)]
pub enum Constraint<'a, 's> {
    RowSum {
        lhs: RowAtom<'a, 's>,
        plus: Span,
        rhs: RowAtom<'a, 's>,
        eq: Span,
        goal: RowAtom<'a, 's>,
    },
}

impl<'a, 's> Spanned for Constraint<'a, 's> {
    fn span(&self) -> Span {
        match self {
            Constraint::RowSum { lhs, goal, .. } => Span::join(lhs, goal),
        }
    }
}

/// A quantifier for a polytype.
#[derive(Clone, Copy, Debug)]
pub struct Quantifier<'s> {
    pub forall: Span,
    pub var: SpanOf<RefHandle<'s, str>>,
    pub dot: Span,
}

/// A qualifiers for a type.
#[derive(Clone, Copy, Debug)]
pub struct Qualifiers<'a, 's> {
    pub constraints: Separated<'a, Constraint<'a, 's>>,
    pub arrow: Span,
}

impl<'a, 's> Spanned for Qualifiers<'a, 's> {
    fn span(&self) -> Span {
        Span::join(&self.constraints, &self.arrow)
    }
}

/// A polymorphic Aiahr type.
#[derive(Clone, Copy, Debug)]
pub struct Scheme<'a, 's> {
    pub quantifiers: &'a [Quantifier<'s>],
    pub qualifiers: Option<Qualifiers<'a, 's>>,
    pub type_: &'a Type<'a, 's>,
}

impl<'a, 's> Spanned for Scheme<'a, 's> {
    fn span(&self) -> Span {
        Span {
            start: self
                .qualifiers
                .map(|q| q.span())
                .unwrap_or_else(|| self.type_.span())
                .start(),
            end: self.type_.end(),
        }
    }
}

/// An Aiahr pattern.
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

/// A typing annotation for a variable.
#[derive(Clone, Copy, Debug)]
pub struct Annotation<T> {
    pub colon: Span,
    pub type_: T,
}

/// A monotype annotation.
pub type TypeAnnotation<'a, 's> = Annotation<&'a Type<'a, 's>>;

/// A scheme annotation.
pub type SchemeAnnotation<'a, 's> = Annotation<&'a Scheme<'a, 's>>;

/// An Aiahr term.
#[derive(Clone, Copy, Debug)]
pub enum Term<'a, 's> {
    Binding {
        var: SpanOf<RefHandle<'s, str>>,
        annotation: Option<TypeAnnotation<'a, 's>>,
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
        annotation: Option<TypeAnnotation<'a, 's>>,
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
            Term::Binding { var, expr, .. } => Span::join(var, *expr),
            Term::Handle { with, expr, .. } => Span::join(with, *expr),
            Term::Abstraction { lbar, body, .. } => Span::join(lbar, *body),
            Term::Application { func, rpar, .. } => Span::join(*func, rpar),
            Term::ProductRow(p) => p.span(),
            Term::SumRow(s) => s.span(),
            Term::DotAccess { base, field, .. } => Span::join(*base, field),
            Term::Match { match_, rangle, .. } => Span::join(match_, rangle),
            Term::SymbolRef(v) => v.span(),
            Term::Parenthesized { lpar, rpar, .. } => Span::join(lpar, rpar),
        }
    }
}

/// A top-level item in an Aiahr source file.
#[derive(Clone, Copy, Debug)]
pub enum Item<'a, 's> {
    Term {
        name: SpanOf<RefHandle<'s, str>>,
        annotation: Option<SchemeAnnotation<'a, 's>>,
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
            Item::Term { name, value, .. } => Span::join(name, *value),
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
macro_rules! row_concrete {
    ($($fields:pat),+ $(,)?) => {
        $crate::cst::Row::Concrete($crate::separated!($($fields),+))
    };
}

#[macro_export]
macro_rules! row_variable {
    ($($vars:pat),+ $(,)?) => {
        $crate::cst::Row::Variable($crate::separated!($($crate::span_of!($crate::h!($vars))),+))
    };
}

#[macro_export]
macro_rules! row_mixed {
    (($($fields:pat),+ $(,)?), ($($vars:pat),+ $(,)?)) => {
        $crate::cst::Row::Mixed {
            concrete: $crate::separated!($($fields),+),
            variables: $crate::separated!($($crate::span_of!($crate::h!($vars))),+),
            ..
        }
    };
}

#[macro_export]
macro_rules! type_named {
    ($name:pat) => {
        &$crate::cst::Type::Named($crate::span_of!($crate::h!($name)))
    };
}

#[macro_export]
macro_rules! type_sum {
    ($variants:pat) => {
        &$crate::cst::Type::Sum {
            variants: $variants,
            ..
        }
    };
}

#[macro_export]
macro_rules! type_prod {
    ($fields:pat) => {
        &$crate::cst::Type::Product {
            fields: Some($fields),
            ..
        }
    };
    () => {
        &$crate::cst::Type::Product { fields: None, .. }
    };
}

#[macro_export]
macro_rules! type_func {
    ($dom:pat, $cod:pat) => {
        &$crate::cst::Type::Function {
            domain: $dom,
            codomain: $cod,
            ..
        }
    };
}

#[macro_export]
macro_rules! type_par {
    ($ty:pat) => {
        &$crate::cst::Type::Parenthesized { type_: $ty, .. }
    };
}

#[macro_export]
macro_rules! rwx_concrete {
    ($($fields:pat),+ $(,)?) => {
        $crate::cst::RowAtom::Concrete {
            fields: $crate::separated!($($fields),+),
            ..
        }
    };
}

#[macro_export]
macro_rules! rwx_variable {
    ($var:pat) => {
        $crate::cst::RowAtom::Variable($crate::span_of!($crate::h!($var)))
    };
}

#[macro_export]
macro_rules! ct_rowsum {
    ($lhs:pat, $rhs:pat, $goal:pat) => {
        $crate::cst::Constraint::RowSum {
            lhs: $lhs,
            rhs: $rhs,
            goal: $goal,
            ..
        }
    };
}

#[macro_export]
macro_rules! quant {
    ($($vars:pat),* $(,)?) => { &[$(
        $crate::cst::Quantifier { var: $crate::span_of!($crate::h!($vars)), .. }
    ),*] };
}

#[macro_export]
macro_rules! qual {
    ($($cts:pat),+ $(,)?) => {
        $crate::cst::Qualifiers {
            constraints: $crate::separated!($($cts),+),
            ..
        }
    };
}

#[macro_export]
macro_rules! scheme {
    ($type_:pat) => {
        &$crate::cst::Scheme {
            quantifiers: &[],
            qualifiers: None,
            type_: $type_,
        }
    };
    ($qualifiers:pat, $type_:pat) => {
        &$crate::cst::Scheme {
            quantifiers: &[],
            qualifiers: Some($qualifiers),
            type_: $type_,
        }
    };
    ($quantifiers:pat, None, $type_:pat) => {
        &$crate::cst::Scheme {
            quantifiers: $quantifiers,
            qualifiers: None,
            type_: $type_,
        }
    };
    ($quantifiers:pat, $qualifiers:pat, $type_:pat) => {
        &$crate::cst::Scheme {
            quantifiers: $quantifiers,
            qualifiers: Some($qualifiers),
            type_: $type_,
        }
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
            annotation: None,
            value: $value,
            expr: $expr,
            ..
        }
    };
    ($var:pat, $type_:pat, $value:pat, $expr:pat) => {
        &$crate::cst::Term::Binding {
            var: $crate::span_of!($crate::h!((var))),
            annotation: Some($crate::cst::TypeAnnotation { type_: $type_, .. }),
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
            annotation: None,
            body: $body,
            ..
        }
    };
    ($arg:pat, $type_:pat, $body:pat) => {
        &$crate::cst::Term::Abstraction {
            arg: $crate::span_of!($crate::h!($arg)),
            annotation: Some($crate::cst::TypeAnnotation { type_: $type_, .. }),
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
            annotation: None,
            value: $value,
            ..
        }
    };
    ($name:pat, $type_:pat, $value:pat) => {
        $crate::cst::Item::Term {
            name: $crate::span_of!($crate::h!($name)),
            annotation: Some($crate::cst::SchemeAnnotation { type_: $type_, .. }),
            value: $value,
            ..
        }
    };
}

#[macro_export]
macro_rules! item_effect {
    ($name:pat, $($ops:pat),* $(,)?) => {
        $crate::cst::Item::Effect {
            name: $crate::span_of!($crate::h!($name)),
            ops: &[$($ops),*],
            ..
        }
    };
}
