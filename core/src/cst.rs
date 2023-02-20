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
        self.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a Separated<'a, T> {
    type Item = &'a T;

    type IntoIter = Elements<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        Elements {
            head: Some(&self.first),
            tail: self.elems.iter().map(|(_, t)| t),
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
    head: Option<&'a T>,
    tail: std::iter::Map<std::slice::Iter<'a, (Span, T)>, fn(&'a (Span, T)) -> &'a T>,
}

impl<'a, T> Clone for Elements<'a, T> {
    fn clone(&self) -> Self {
        Elements {
            head: self.head,
            tail: self.tail.clone(),
        }
    }
}
impl<'a, T> ExactSizeIterator for Elements<'a, T> {}
impl<'a, T> FusedIterator for Elements<'a, T> {}
impl<'a, T> Iterator for Elements<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.head.take().or_else(|| self.tail.next())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (lower, upper) = self.tail.size_hint();
        (lower + 1, upper.map(|u| u + 1))
    }
}

pub struct Elements2<'a, T> {
    first: Option<&'a T>,
    tail: std::iter::Map<std::slice::Iter<'a, (Span, T)>, fn(&'a (Span, T)) -> &'a T>,
}
impl<'a, T> ExactSizeIterator for Elements2<'a, T> {}
impl<'a, T> FusedIterator for Elements2<'a, T> {}
impl<'a, T> Iterator for Elements2<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.first.take().or_else(|| self.tail.next())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (lower, upper) = self.tail.size_hint();
        (lower + 1, upper.map(|u| u + 1))
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

impl<'a, 's, T> IntoIterator for &'a ProductRow<'a, 's, T> {
    type Item = &'a IdField<'s, T>;

    type IntoIter = std::iter::FlatMap<
        std::option::Iter<'a, Separated<'a, IdField<'s, T>>>,
        Elements<'a, IdField<'s, T>>,
        fn(&'a Separated<'a, IdField<'s, T>>) -> Elements<'a, IdField<'s, T>>,
    >;

    fn into_iter(self) -> Self::IntoIter {
        self.fields.iter().flat_map(IntoIterator::into_iter)
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

/// A non-empty row with concrete fields in `C` and variables in `V`.
#[derive(Clone, Copy, Debug)]
pub enum Row<'a, V, C> {
    Concrete(Separated<'a, C>),
    Variable(Separated<'a, SpanOf<V>>),
    Mixed {
        concrete: Separated<'a, C>,
        vbar: Span,
        variables: Separated<'a, SpanOf<V>>,
    },
}

impl<'a, V, C: Spanned> Spanned for Row<'a, V, C> {
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
pub type TypeRow<'a, 's, V> = Row<'a, V, IdField<'s, &'a Type<'a, 's, V>>>;

/// An unqualified Aiahr type.
#[derive(Clone, Copy, Debug)]
pub enum Type<'a, 's, V> {
    Named(SpanOf<V>),
    Sum {
        langle: Span,
        variants: TypeRow<'a, 's, V>,
        rangle: Span,
    },
    Product {
        lbrace: Span,
        fields: Option<TypeRow<'a, 's, V>>,
        rbrace: Span,
    },
    Function {
        domain: &'a Type<'a, 's, V>,
        arrow: Span,
        codomain: &'a Type<'a, 's, V>,
    },
    Parenthesized {
        lpar: Span,
        type_: &'a Type<'a, 's, V>,
        rpar: Span,
    },
}

impl<'a, 's, V> Spanned for Type<'a, 's, V> {
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
pub enum RowAtom<'a, 's, V> {
    Concrete {
        lpar: Span,
        fields: Separated<'a, IdField<'s, &'a Type<'a, 's, V>>>,
        rpar: Span,
    },
    Variable(SpanOf<V>),
}

impl<'a, 's, V> Spanned for RowAtom<'a, 's, V> {
    fn span(&self) -> Span {
        match self {
            RowAtom::Concrete { lpar, rpar, .. } => Span::join(lpar, rpar),
            RowAtom::Variable(v) => v.span(),
        }
    }
}

/// A type constraint.
#[derive(Clone, Copy, Debug)]
pub enum Constraint<'a, 's, V> {
    RowSum {
        lhs: RowAtom<'a, 's, V>,
        plus: Span,
        rhs: RowAtom<'a, 's, V>,
        eq: Span,
        goal: RowAtom<'a, 's, V>,
    },
}

impl<'a, 's, V> Spanned for Constraint<'a, 's, V> {
    fn span(&self) -> Span {
        match self {
            Constraint::RowSum { lhs, goal, .. } => Span::join(lhs, goal),
        }
    }
}

/// A quantifier for a polytype.
#[derive(Clone, Copy, Debug)]
pub struct Quantifier<V> {
    pub forall: Span,
    pub var: SpanOf<V>,
    pub dot: Span,
}

impl<V> Spanned for Quantifier<V> {
    fn span(&self) -> Span {
        Span::join(&self.forall, &self.dot)
    }
}

/// A qualifiers for a type.
#[derive(Clone, Copy, Debug)]
pub struct Qualifiers<'a, 's, V> {
    pub constraints: Separated<'a, Constraint<'a, 's, V>>,
    pub arrow: Span,
}

impl<'a, 's, V> Spanned for Qualifiers<'a, 's, V> {
    fn span(&self) -> Span {
        Span::join(&self.constraints, &self.arrow)
    }
}

/// A polymorphic Aiahr type.
#[derive(Clone, Copy, Debug)]
pub struct Scheme<'a, 's, V> {
    pub quantifiers: &'a [Quantifier<V>],
    pub qualifiers: Option<Qualifiers<'a, 's, V>>,
    pub type_: &'a Type<'a, 's, V>,
}

impl<'a, 's, V> Spanned for Scheme<'a, 's, V> {
    fn span(&self) -> Span {
        Span {
            start: self
                .quantifiers
                .first()
                .map(Spanned::span)
                .unwrap_or_else(|| {
                    self.qualifiers
                        .as_ref()
                        .map(|q| q.span())
                        .unwrap_or_else(|| self.type_.span())
                })
                .start(),
            end: self.type_.end(),
        }
    }
}

/// An effect operation.
#[derive(Clone, Copy, Debug)]
pub struct EffectOp<'a, 's> {
    pub name: SpanOf<RefHandle<'s, str>>,
    pub colon: Span,
    pub type_: &'a Type<'a, 's, RefHandle<'s, str>>,
}

impl<'a, 's> Spanned for EffectOp<'a, 's> {
    fn span(&self) -> Span {
        Span::join(&self.name, self.type_)
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
pub type TypeAnnotation<'a, 's, V> = Annotation<&'a Type<'a, 's, V>>;

/// A scheme annotation.
pub type SchemeAnnotation<'a, 's, V> = Annotation<&'a Scheme<'a, 's, V>>;

/// An Aiahr term.
#[derive(Clone, Copy, Debug)]
pub enum Term<'a, 's> {
    Binding {
        var: SpanOf<RefHandle<'s, str>>,
        annotation: Option<TypeAnnotation<'a, 's, RefHandle<'s, str>>>,
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
        annotation: Option<TypeAnnotation<'a, 's, RefHandle<'s, str>>>,
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
    Effect {
        effect: Span,
        name: SpanOf<RefHandle<'s, str>>,
        lbrace: Span,
        ops: &'a [EffectOp<'a, 's>],
        rbrace: Span,
    },
    Term {
        name: SpanOf<RefHandle<'s, str>>,
        annotation: Option<SchemeAnnotation<'a, 's, RefHandle<'s, str>>>,
        eq: Span,
        value: &'a Term<'a, 's>,
    },
}

impl<'a, 's> Item<'a, 's> {
    /// Returns the name of the item.
    pub fn name(&self) -> SpanOf<RefHandle<'s, str>> {
        match self {
            Item::Effect { name, .. } | Item::Term { name, .. } => *name,
        }
    }
}

impl<'a, 's> Spanned for Item<'a, 's> {
    fn span(&self) -> Span {
        match self {
            Item::Effect { effect, rbrace, .. } => Span::join(effect, rbrace),
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
    ($name:literal) => {
        &$crate::cst::Type::Named($crate::span_of!($crate::h!($name)))
    };
    ($name:pat) => {
        &$crate::cst::Type::Named($crate::span_of!($name))
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
    ($($vars:literal),* $(,)?) => { &[$(
        $crate::cst::Quantifier { var: $crate::span_of!($crate::h!($vars)), .. }
    ),*] };
    ($($vars:pat),* $(,)?) => { &[$(
        $crate::cst::Quantifier { var: $crate::span_of!($vars), .. }
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
macro_rules! eff_op {
    ($name:pat, $type_:pat) => {
        $crate::cst::EffectOp {
            name: $crate::span_of!($crate::h!($name)),
            type_: $type_,
            ..
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
macro_rules! item_effect {
    ($name:pat, $($ops:pat),* $(,)?) => {
        $crate::cst::Item::Effect {
            name: $crate::span_of!($crate::h!($name)),
            ops: &[$($ops),*],
            ..
        }
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
