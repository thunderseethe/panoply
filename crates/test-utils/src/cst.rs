use std::iter::FusedIterator;

use base::{
    ident::Ident,
    indexed::{HasArenaRef, HasRefArena, ReferenceAllocate},
    span::{Span, SpanOf, Spanned},
};
use bumpalo::Bump;
use cst::{self, CstIndxAlloc, Field, IdField};
use la_arena::{Arena, Idx};

/// A non-empty list of elements, separated by some fixed separator. To allow an empty list, wrap in
/// `Option`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Separated<'a, T> {
    pub first: T,
    pub elems: &'a [(Span, T)],
    /// The optional final separator.
    pub comma: Option<Span>,
}

impl<'a, T> Separated<'a, T> {
    /// An iterator over the non-separator elements.
    pub fn elements(&self) -> Elements<'_, T> {
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
                .or_else(|| self.elems.last().map(|e| e.0))
                .unwrap_or_else(|| self.first.span())
                .end(),
        }
    }
}
type ElementTail<'a, T> =
    std::iter::Map<std::slice::Iter<'a, (Span, T)>, fn(&'a (Span, T)) -> &'a T>;
/// An iterator over the elements of a `Separated`. Needed because `std::iter::Chain` does not
/// implement `ExactSizeIterator`.
#[derive(Debug)]
pub struct Elements<'a, T> {
    head: Option<&'a T>,
    tail: ElementTail<'a, T>,
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

/// A product row with values in `T`.
#[derive(Clone, Copy, Debug)]
pub struct ProductRow<'a, T> {
    pub lbrace: Span,
    pub fields: Option<Separated<'a, IdField<T>>>,
    pub rbrace: Span,
}

impl<'a, T> Spanned for ProductRow<'a, T> {
    fn span(&self) -> Span {
        Span::join(&self.lbrace, &self.rbrace)
    }
}

impl<'a, T> IntoIterator for &'a ProductRow<'a, T> {
    type Item = &'a IdField<T>;

    type IntoIter = std::iter::FlatMap<
        std::option::Iter<'a, Separated<'a, IdField<T>>>,
        Elements<'a, IdField<T>>,
        fn(&'a Separated<'a, IdField<T>>) -> Elements<'a, IdField<T>>,
    >;

    fn into_iter(self) -> Self::IntoIter {
        self.fields.iter().flat_map(IntoIterator::into_iter)
    }
}

/// A sum row with value in `T`.
#[derive(Clone, Copy, Debug)]
pub struct SumRow<T> {
    pub langle: Span,
    pub field: IdField<T>,
    pub rangle: Span,
}

impl<T> Spanned for SumRow<T> {
    fn span(&self) -> Span {
        Span::join(&self.langle, &self.rangle)
    }
}

/// A non-empty row with concrete fields in `C` and variables in `V`.
#[derive(Clone, Copy, Debug, PartialEq)]
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
pub type TypeRow<'a, V> = Row<'a, V, IdField<&'a Type<'a, V>>>;

/// An unqualified type.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Type<'a, V> {
    Int(Span),
    Named(SpanOf<V>),
    Sum {
        langle: Span,
        variants: TypeRow<'a, V>,
        rangle: Span,
    },
    Product {
        lbrace: Span,
        fields: Option<TypeRow<'a, V>>,
        rbrace: Span,
    },
    Function {
        domain: &'a Type<'a, V>,
        arrow: Span,
        codomain: &'a Type<'a, V>,
    },
    Parenthesized {
        lpar: Span,
        type_: &'a Type<'a, V>,
        rpar: Span,
    },
}

impl<'a, V> Spanned for Type<'a, V> {
    fn span(&self) -> Span {
        match self {
            Type::Int(span) => *span,
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
pub enum RowAtom<'a, V> {
    Concrete {
        lpar: Span,
        fields: Separated<'a, IdField<&'a Type<'a, V>>>,
        rpar: Span,
    },
    Variable(SpanOf<V>),
}

impl<'a, V> Spanned for RowAtom<'a, V> {
    fn span(&self) -> Span {
        match self {
            RowAtom::Concrete { lpar, rpar, .. } => Span::join(lpar, rpar),
            RowAtom::Variable(v) => v.span(),
        }
    }
}

/// A type constraint.
#[derive(Clone, Copy, Debug)]
pub enum Constraint<'a, V> {
    RowSum {
        lhs: RowAtom<'a, V>,
        plus: Span,
        rhs: RowAtom<'a, V>,
        eq: Span,
        goal: RowAtom<'a, V>,
    },
}

impl<'a, V> Spanned for Constraint<'a, V> {
    fn span(&self) -> Span {
        match self {
            Constraint::RowSum { lhs, goal, .. } => Span::join(lhs, goal),
        }
    }
}

/// A qualifiers for a type.
#[derive(Clone, Copy, Debug)]
pub struct Qualifiers<'a, V> {
    pub constraints: Separated<'a, Constraint<'a, V>>,
    pub arrow: Span,
}

impl<'a, V> Spanned for Qualifiers<'a, V> {
    fn span(&self) -> Span {
        Span::join(&self.constraints, &self.arrow)
    }
}

/// A polymorphic type.
#[derive(Clone, Copy, Debug)]
pub struct Scheme<'a, V> {
    pub quantifiers: &'a [cst::Quantifier<V>],
    pub qualifiers: Option<Qualifiers<'a, V>>,
    pub type_: &'a Type<'a, V>,
}

impl<'a, V> Spanned for Scheme<'a, V> {
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
pub struct EffectOp<'a, O, V> {
    pub name: SpanOf<O>,
    pub colon: Span,
    pub type_: &'a Type<'a, V>,
}

impl<'a, O, V> Spanned for EffectOp<'a, O, V> {
    fn span(&self) -> Span {
        Span::join(&self.name, self.type_)
    }
}

/// A pattern.
#[derive(Clone, Copy, Debug)]
pub enum Pattern<'a> {
    ProductRow(ProductRow<'a, &'a Pattern<'a>>),
    SumRow(SumRow<&'a Pattern<'a>>),
    Whole(SpanOf<Ident>),
}

impl<'a> Spanned for Pattern<'a> {
    fn span(&self) -> Span {
        match self {
            Pattern::ProductRow(p) => p.span(),
            Pattern::SumRow(s) => s.span(),
            Pattern::Whole(v) => v.span(),
        }
    }
}

/// A monotype annotation.
pub type TypeAnnotation<'a, V> = cst::Annotation<&'a Type<'a, V>>;

/// A scheme annotation.
pub type SchemeAnnotation<'a, V> = cst::Annotation<&'a Scheme<'a, V>>;

/// A term.
#[derive(Clone, Copy, Debug)]
pub enum Term<'a> {
    Binding {
        var: SpanOf<Ident>,
        annotation: Option<TypeAnnotation<'a, Ident>>,
        eq: Span,
        value: &'a Term<'a>,
        semi: Span,
        expr: &'a Term<'a>,
    },
    Handle {
        with: Span,
        handler: &'a Term<'a>,
        do_: Span,
        expr: &'a Term<'a>,
    },
    Abstraction {
        lbar: Span,
        arg: SpanOf<Ident>,
        annotation: Option<TypeAnnotation<'a, Ident>>,
        rbar: Span,
        body: &'a Term<'a>,
    },
    Application {
        func: &'a Term<'a>,
        lpar: Span,
        arg: &'a Term<'a>,
        rpar: Span,
    },
    ProductRow(ProductRow<'a, &'a Term<'a>>),
    Concat {
        left: &'a Term<'a>,
        concat: Span,
        right: &'a Term<'a>,
    },
    SumRow(SumRow<&'a Term<'a>>),
    DotAccess {
        base: &'a Term<'a>,
        dot: Span,
        field: SpanOf<Ident>,
    },
    Match {
        match_: Span,
        langle: Span,
        cases: Separated<'a, Field<&'a Pattern<'a>, &'a Term<'a>>>,
        rangle: Span,
    },
    SymbolRef(SpanOf<Ident>),
    Parenthesized {
        lpar: Span,
        term: &'a Term<'a>,
        rpar: Span,
    },
    Int(SpanOf<usize>),
}

impl<'a> Spanned for Term<'a> {
    fn span(&self) -> Span {
        match self {
            Term::Binding { var, expr, .. } => Span::join(var, *expr),
            Term::Handle { with, expr, .. } => Span::join(with, *expr),
            Term::Abstraction { lbar, body, .. } => Span::join(lbar, *body),
            Term::Application { func, rpar, .. } => Span::join(*func, rpar),
            Term::ProductRow(p) => p.span(),
            Term::Concat { left, right, .. } => Span::join(left, right),
            Term::SumRow(s) => s.span(),
            Term::DotAccess { base, field, .. } => Span::join(*base, field),
            Term::Match { match_, rangle, .. } => Span::join(match_, rangle),
            Term::SymbolRef(v) => v.span(),
            Term::Parenthesized { lpar, rpar, .. } => Span::join(lpar, rpar),
            Term::Int(span_of_int) => span_of_int.span(),
        }
    }
}

/// A top-level item in an source file.
#[derive(Clone, Copy, Debug)]
pub enum Item<'a> {
    Effect {
        effect: Span,
        name: SpanOf<Ident>,
        lbrace: Span,
        ops: &'a [EffectOp<'a, Ident, Ident>],
        rbrace: Span,
    },
    Term {
        name: SpanOf<Ident>,
        annotation: Option<SchemeAnnotation<'a, Ident>>,
        eq: Span,
        value: &'a Term<'a>,
    },
}

impl<'a> Item<'a> {
    /// Returns the name of the item.
    pub fn name(&self) -> SpanOf<Ident> {
        match self {
            Item::Effect { name, .. } | Item::Term { name, .. } => *name,
        }
    }
}

impl<'a> Spanned for Item<'a> {
    fn span(&self) -> Span {
        match self {
            Item::Effect { effect, rbrace, .. } => Span::join(effect, rbrace),
            Item::Term { name, value, .. } => Span::join(name, *value),
        }
    }
}

/// A parsed module.
#[derive(Clone, Copy, Debug)]
pub struct CstModule<'a> {
    pub items: &'a [Item<'a>],
}

pub struct CstRefAlloc<'a, 'b> {
    pub arena: &'a Bump,
    pub indices: &'b CstIndxAlloc,
}
impl<'a, 'b> CstRefAlloc<'a, 'b> {
    pub fn new(arena: &'a Bump, indices: &'b CstIndxAlloc) -> Self {
        Self { arena, indices }
    }
}
impl<'a> HasRefArena<'a> for CstRefAlloc<'a, '_> {
    fn ref_arena(&self) -> &'a Bump {
        self.arena
    }
}
impl<T> HasArenaRef<T> for CstRefAlloc<'_, '_>
where
    CstIndxAlloc: HasArenaRef<T>,
{
    fn arena(&self) -> &Arena<T> {
        self.indices.arena()
    }
}

impl<'a, 'b, T> ReferenceAllocate<'a, CstRefAlloc<'a, 'b>> for cst::Separated<T>
where
    T: ReferenceAllocate<'a, CstRefAlloc<'a, 'b>>,
{
    type Out = Separated<'a, T::Out>;

    fn ref_alloc(&self, alloc: &mut CstRefAlloc<'a, 'b>) -> Self::Out {
        Separated {
            first: self.first.ref_alloc(alloc),
            elems: alloc.ref_arena().alloc_slice_fill_iter(
                self.elems
                    .iter()
                    .map(|(span, t)| (*span, t.ref_alloc(alloc))),
            ),
            comma: self.comma,
        }
    }
}

impl<'a, 'b, V, C> ReferenceAllocate<'a, CstRefAlloc<'a, 'b>> for cst::Row<V, C>
where
    V: ReferenceAllocate<'a, CstRefAlloc<'a, 'b>>,
    C: ReferenceAllocate<'a, CstRefAlloc<'a, 'b>>,
{
    type Out = Row<'a, V::Out, C::Out>;

    fn ref_alloc(&self, alloc: &mut CstRefAlloc<'a, 'b>) -> Self::Out {
        match self {
            cst::Row::Concrete(concrete) => Row::Concrete(concrete.ref_alloc(alloc)),
            cst::Row::Variable(vars) => Row::Variable(vars.ref_alloc(alloc)),
            cst::Row::Mixed {
                concrete,
                vbar,
                variables,
            } => Row::Mixed {
                concrete: concrete.ref_alloc(alloc),
                vbar: *vbar,
                variables: variables.ref_alloc(alloc),
            },
        }
    }
}

impl<'a> ReferenceAllocate<'a, CstRefAlloc<'a, '_>> for Idx<cst::Type<Ident>> {
    type Out = &'a Type<'a, Ident>;

    fn ref_alloc(&self, alloc: &mut CstRefAlloc<'a, '_>) -> Self::Out {
        let type_ = match alloc.arena()[*self].clone() {
            cst::Type::Int(span) => Type::Int(span),
            cst::Type::Named(var) => Type::Named(var.ref_alloc(alloc)),
            cst::Type::Sum {
                langle,
                variants,
                rangle,
            } => Type::Sum {
                langle,
                variants: variants.ref_alloc(alloc),
                rangle,
            },
            cst::Type::Product {
                lbrace,
                fields,
                rbrace,
            } => Type::Product {
                lbrace,
                fields: fields.ref_alloc(alloc),
                rbrace,
            },
            cst::Type::Function {
                domain,
                arrow,
                codomain,
            } => Type::Function {
                domain: domain.ref_alloc(alloc),
                arrow,
                codomain: codomain.ref_alloc(alloc),
            },
            cst::Type::Parenthesized { lpar, type_, rpar } => Type::Parenthesized {
                lpar,
                type_: type_.ref_alloc(alloc),
                rpar,
            },
        };
        alloc.ref_arena().alloc(type_) as &_
    }
}

impl<'a> ReferenceAllocate<'a, CstRefAlloc<'a, '_>> for cst::EffectOp<Ident, Ident> {
    type Out = EffectOp<'a, Ident, Ident>;

    fn ref_alloc(&self, alloc: &mut CstRefAlloc<'a, '_>) -> Self::Out {
        EffectOp {
            name: self.name.ref_alloc(alloc),
            colon: self.colon,
            type_: self.type_.ref_alloc(alloc),
        }
    }
}
impl<'a> ReferenceAllocate<'a, CstRefAlloc<'a, '_>> for cst::Scheme<Ident> {
    type Out = &'a Scheme<'a, Ident>;

    fn ref_alloc(&self, alloc: &mut CstRefAlloc<'a, '_>) -> Self::Out {
        let scheme = Scheme {
            quantifiers: alloc
                .ref_arena()
                .alloc_slice_fill_iter(self.quantifiers.iter().map(|quant| quant.ref_alloc(alloc)))
                as &_,
            qualifiers: self.qualifiers.ref_alloc(alloc),
            type_: self.type_.ref_alloc(alloc),
        };
        alloc.ref_arena().alloc(scheme) as &_
    }
}
impl<'a> ReferenceAllocate<'a, CstRefAlloc<'a, '_>> for cst::Item {
    type Out = Item<'a>;

    fn ref_alloc(&self, alloc: &mut CstRefAlloc<'a, '_>) -> Self::Out {
        match self {
            cst::Item::Effect(eff) => Item::Effect {
                effect: eff.effect,
                name: eff.name,
                lbrace: eff.lbrace,
                ops: alloc
                    .ref_arena()
                    .alloc_slice_fill_iter(eff.ops.iter().map(|op| op.ref_alloc(alloc))),
                rbrace: eff.rbrace,
            },
            cst::Item::Term(term) => Item::Term {
                name: term.name,
                annotation: term.annotation.ref_alloc(alloc),
                eq: term.eq,
                value: term.value.ref_alloc(alloc),
            },
        }
    }
}

impl<'a, 'b, T: ReferenceAllocate<'a, CstRefAlloc<'a, 'b>>>
    ReferenceAllocate<'a, CstRefAlloc<'a, 'b>> for cst::SumRow<T>
{
    type Out = SumRow<T::Out>;

    fn ref_alloc(&self, alloc: &mut CstRefAlloc<'a, 'b>) -> Self::Out {
        SumRow {
            langle: self.langle,
            field: self.field.ref_alloc(alloc),
            rangle: self.rangle,
        }
    }
}
impl<'a, 'b, T> ReferenceAllocate<'a, CstRefAlloc<'a, 'b>> for cst::ProductRow<T>
where
    T: ReferenceAllocate<'a, CstRefAlloc<'a, 'b>>,
{
    type Out = ProductRow<'a, T::Out>;

    fn ref_alloc(&self, alloc: &mut CstRefAlloc<'a, 'b>) -> Self::Out {
        ProductRow {
            lbrace: self.lbrace,
            fields: self.fields.ref_alloc(alloc),
            rbrace: self.rbrace,
        }
    }
}

impl<'a> ReferenceAllocate<'a, CstRefAlloc<'a, '_>> for Idx<cst::Pattern> {
    type Out = &'a Pattern<'a>;

    fn ref_alloc(&self, alloc: &mut CstRefAlloc<'a, '_>) -> Self::Out {
        let pat = match alloc.arena()[*self].clone() {
            cst::Pattern::ProductRow(prod) => Pattern::ProductRow(prod.ref_alloc(alloc)),
            cst::Pattern::SumRow(sum) => Pattern::SumRow(sum.ref_alloc(alloc)),
            cst::Pattern::Whole(var) => Pattern::Whole(var.ref_alloc(alloc)),
        };
        alloc.ref_arena().alloc(pat) as &_
    }
}
impl<'a> ReferenceAllocate<'a, CstRefAlloc<'a, '_>> for cst::CstModule {
    type Out = CstModule<'a>;

    fn ref_alloc(&self, alloc: &mut CstRefAlloc<'a, '_>) -> Self::Out {
        CstModule {
            items: alloc
                .ref_arena()
                .alloc_slice_fill_iter(self.items.iter().map(|item| item.ref_alloc(alloc))),
        }
    }
}
impl<'a> ReferenceAllocate<'a, CstRefAlloc<'a, '_>> for Idx<cst::Term> {
    type Out = &'a Term<'a>;

    fn ref_alloc(&self, alloc: &mut CstRefAlloc<'a, '_>) -> Self::Out {
        let term = match alloc.arena()[*self].clone() {
            cst::Term::Binding {
                var,
                annotation,
                eq,
                value,
                semi,
                expr,
            } => Term::Binding {
                var,
                annotation: annotation.ref_alloc(alloc),
                eq,
                value: value.ref_alloc(alloc),
                semi,
                expr: expr.ref_alloc(alloc),
            },
            cst::Term::Handle {
                with,
                handler,
                do_,
                expr,
            } => Term::Handle {
                with,
                handler: handler.ref_alloc(alloc),
                do_,
                expr: expr.ref_alloc(alloc),
            },
            cst::Term::Abstraction {
                lbar,
                arg,
                annotation,
                rbar,
                body,
            } => Term::Abstraction {
                lbar,
                arg,
                annotation: annotation.ref_alloc(alloc),
                rbar,
                body: body.ref_alloc(alloc),
            },
            cst::Term::Application {
                func,
                lpar,
                arg,
                rpar,
            } => Term::Application {
                func: func.ref_alloc(alloc),
                lpar,
                arg: arg.ref_alloc(alloc),
                rpar,
            },
            cst::Term::ProductRow(prod) => Term::ProductRow(prod.ref_alloc(alloc)),
            cst::Term::SumRow(sum) => Term::SumRow(sum.ref_alloc(alloc)),
            cst::Term::DotAccess { base, dot, field } => Term::DotAccess {
                base: base.ref_alloc(alloc),
                dot,
                field,
            },
            cst::Term::Match {
                match_,
                langle,
                cases,
                rangle,
            } => Term::Match {
                match_,
                langle,
                cases: cases.ref_alloc(alloc),
                rangle,
            },
            cst::Term::SymbolRef(symbol) => Term::SymbolRef(symbol),
            cst::Term::Parenthesized { lpar, term, rpar } => Term::Parenthesized {
                lpar,
                term: term.ref_alloc(alloc),
                rpar,
            },
            cst::Term::Concat {
                left,
                concat,
                right,
            } => Term::Concat {
                left: left.ref_alloc(alloc),
                concat,
                right: right.ref_alloc(alloc),
            },
            cst::Term::Int(int) => Term::Int(int),
        };
        alloc.ref_arena().alloc(term) as &_
    }
}

impl<'a> ReferenceAllocate<'a, CstRefAlloc<'a, '_>> for cst::Qualifiers<Ident> {
    type Out = Qualifiers<'a, Ident>;

    fn ref_alloc(&self, alloc: &mut CstRefAlloc<'a, '_>) -> Self::Out {
        Qualifiers {
            constraints: self.constraints.ref_alloc(alloc),
            arrow: self.arrow,
        }
    }
}

impl<'a> ReferenceAllocate<'a, CstRefAlloc<'a, '_>> for cst::Constraint<Ident> {
    type Out = Constraint<'a, Ident>;

    fn ref_alloc(&self, alloc: &mut CstRefAlloc<'a, '_>) -> Self::Out {
        match self {
            cst::Constraint::RowSum {
                lhs,
                plus,
                rhs,
                eq,
                goal,
            } => Constraint::RowSum {
                lhs: lhs.ref_alloc(alloc),
                plus: *plus,
                rhs: rhs.ref_alloc(alloc),
                eq: *eq,
                goal: goal.ref_alloc(alloc),
            },
        }
    }
}

impl<'a> ReferenceAllocate<'a, CstRefAlloc<'a, '_>> for cst::RowAtom<Ident> {
    type Out = RowAtom<'a, Ident>;

    fn ref_alloc(&self, alloc: &mut CstRefAlloc<'a, '_>) -> Self::Out {
        match self {
            cst::RowAtom::Concrete { lpar, fields, rpar } => RowAtom::Concrete {
                lpar: *lpar,
                fields: fields.ref_alloc(alloc),
                rpar: *rpar,
            },
            cst::RowAtom::Variable(var) => RowAtom::Variable(var.ref_alloc(alloc)),
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
        ::cst::Field {
            label: $label,
            target: $target,
            ..
        }
    };
}

#[macro_export]
macro_rules! id_field {
    ($label:pat, $target:pat) => {
        $crate::field!(::base::span_of!($label), $target)
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
        $crate::cst::Row::Variable($crate::separated!($(::base::span_of!($vars)),+))
    };
}

#[macro_export]
macro_rules! row_mixed {
    (($($fields:pat),+ $(,)?), ($($vars:pat),+ $(,)?)) => {
        $crate::cst::Row::Mixed {
            concrete: $crate::separated!($($fields),+),
            variables: $crate::separated!($(::base::span_of!($vars)),+),
            ..
        }
    };
}

#[macro_export]
macro_rules! type_named {
    ($name:literal) => {
        &$crate::cst::Type::Named(::base::span_of!($crate::h!($name)))
    };
    ($name:pat) => {
        &$crate::cst::Type::Named(::base::span_of!($name))
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
        &::test_utils::cst::Type::Product {
            fields: Some($fields),
            ..
        }
    };
    () => {
        &::test_utils::cst::Type::Product { fields: None, .. }
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
macro_rules! type_int {
    () => {
        &$crate::cst::Type::Int(_)
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
        $crate::cst::RowAtom::Variable(::base::span_of!($var))
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
        ::cst::Quantifier { var: ::base::span_of!($crate::h!($vars)), .. }
    ),*] };
    ($($vars:pat),* $(,)?) => { &[$(
        ::cst::Quantifier { var: ::base::span_of!($vars), .. }
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
        &$crate::cst::Pattern::Whole(::base::span_of!($var))
    };
}

#[macro_export]
macro_rules! term_local {
    ($var:pat, $value:pat, $expr:pat) => {
        &$crate::cst::Term::Binding {
            var: ::base::span_of!($var),
            annotation: None,
            value: $value,
            expr: $expr,
            ..
        }
    };
    ($var:pat, $type_:pat, $value:pat, $expr:pat) => {
        &$crate::cst::Term::Binding {
            var: ::base::span_of!($var),
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
            arg: ::base::span_of!($arg),
            annotation: None,
            body: $body,
            ..
        }
    };
    ($arg:pat, $type_:pat, $body:pat) => {
        &$crate::cst::Term::Abstraction {
            arg: ::base::span_of!($arg),
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
            field: ::base::span_of!($field),
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
        &$crate::cst::Term::SymbolRef(::base::span_of!($var))
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
            name: ::base::span_of!($name),
            ops: &[$($ops),*],
            ..
        }
    };
}

#[macro_export]
macro_rules! item_term {
    ($name:pat, $value:pat) => {
        $crate::cst::Item::Term {
            name: ::base::span_of!($name),
            annotation: None,
            value: $value,
            ..
        }
    };
    ($name:pat, $type_:pat, $value:pat) => {
        $crate::cst::Item::Term {
            name: ::base::span_of!($name),
            annotation: Some($crate::cst::SchemeAnnotation { type_: $type_, .. }),
            value: $value,
            ..
        }
    };
}
