use std::fmt::Debug;
use std::iter::FusedIterator;
use std::ops::Index;

use la_arena::{Arena, Idx};

use aiahr_core::{
    ident::Ident,
    indexed::{HasArenaMut, HasArenaRef, IdxAlloc, IndexedAllocate, ReferenceAllocate},
    span::{Span, SpanOf, Spanned},
};

pub mod nameres;

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct CstIndxAlloc {
    types: Arena<Type<Ident>>,
    terms: Arena<Term>,
    pats: Arena<Pattern>,
}
impl HasArenaRef<Pattern> for CstIndxAlloc {
    fn arena(&self) -> &Arena<Pattern> {
        &self.pats
    }
}
impl HasArenaMut<Pattern> for CstIndxAlloc {
    fn arena_mut(&mut self) -> &mut Arena<Pattern> {
        &mut self.pats
    }
}
impl HasArenaRef<Type<Ident>> for CstIndxAlloc {
    fn arena(&self) -> &Arena<Type<Ident>> {
        &self.types
    }
}
impl HasArenaMut<Type<Ident>> for CstIndxAlloc {
    fn arena_mut(&mut self) -> &mut Arena<Type<Ident>> {
        &mut self.types
    }
}
impl HasArenaRef<Term> for CstIndxAlloc {
    fn arena(&self) -> &Arena<Term> {
        &self.terms
    }
}
impl HasArenaMut<Term> for CstIndxAlloc {
    fn arena_mut(&mut self) -> &mut Arena<Term> {
        &mut self.terms
    }
}
impl<T> Index<Idx<T>> for CstIndxAlloc
where
    Self: HasArenaRef<T>,
{
    type Output = T;

    fn index(&self, index: Idx<T>) -> &Self::Output {
        &self.arena()[index]
    }
}
impl<T> IdxAlloc<T> for CstIndxAlloc
where
    Self: HasArenaMut<T>,
{
    fn alloc(&mut self, value: T) -> Idx<T> {
        self.arena_mut().alloc(value)
    }
}

/// A typing annotation for a variable.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct Annotation<T> {
    pub colon: Span,
    pub type_: T,
}
impl<'a, A, T> ReferenceAllocate<'a, A> for Annotation<T>
where
    T: ReferenceAllocate<'a, A>,
{
    type Out = Annotation<T::Out>;

    fn ref_alloc(&self, alloc: &mut A) -> Self::Out {
        Annotation {
            colon: self.colon,
            type_: self.type_.ref_alloc(alloc),
        }
    }
}
/// A monotype annotation.
pub type TypeAnnotation<V> = Annotation<Idx<Type<V>>>;

/// A scheme annotation.
pub type SchemeAnnotation<V> = Annotation<Scheme<V>>;

/// A non-empty list of elements, separated by some fixed separator. To allow an empty list, wrap in
/// `Option`.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Separated<T> {
    pub first: T,
    pub elems: Vec<(Span, T)>,
    pub comma: Option<Span>,
}
impl<T: Spanned> Spanned for Separated<T> {
    fn span(&self) -> Span {
        Span::join(
            &self.first,
            &self
                .comma
                .or_else(|| self.elems.last().map(|e| e.0))
                .unwrap_or_else(|| self.first.span()),
        )
    }
}
impl<T> Separated<T> {
    /// An iterator over the non-separator elements.
    pub fn elements(&self) -> Elements<'_, T> {
        self.into_iter()
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
impl<'a, T> IntoIterator for &'a Separated<T> {
    type Item = &'a T;

    type IntoIter = Elements<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        Elements {
            head: Some(&self.first),
            tail: self.elems.iter().map(|(_, t)| t),
        }
    }
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

pub type IdField<T> = Field<SpanOf<Ident>, T>;

/// A product row with values in `T`.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ProductRow<T> {
    pub lbrace: Span,
    pub fields: Option<Separated<IdField<T>>>,
    pub rbrace: Span,
}
impl<T> Spanned for ProductRow<T> {
    fn span(&self) -> Span {
        Span::join(&self.lbrace, &self.rbrace)
    }
}
impl<'a, T> IntoIterator for &'a ProductRow<T> {
    type Item = &'a IdField<T>;

    type IntoIter = std::iter::FlatMap<
        std::option::Iter<'a, Separated<IdField<T>>>,
        Elements<'a, IdField<T>>,
        fn(&'a Separated<IdField<T>>) -> Elements<'a, IdField<T>>,
    >;

    fn into_iter(self) -> Self::IntoIter {
        self.fields.iter().flat_map(IntoIterator::into_iter)
    }
}

/// A sum row with value in `T`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Row<V, C> {
    Concrete(Separated<C>),
    Variable(Separated<SpanOf<V>>),
    Mixed {
        concrete: Separated<C>,
        vbar: Span,
        variables: Separated<SpanOf<V>>,
    },
}

/// A row of types.
pub type TypeRow<V> = Row<V, IdField<Idx<Type<V>>>>;

/// An unqualified Aiahr syntactic type.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type<V> {
    Named(SpanOf<V>),
    Sum {
        langle: Span,
        variants: TypeRow<V>,
        rangle: Span,
    },
    Product {
        lbrace: Span,
        fields: Option<TypeRow<V>>,
        rbrace: Span,
    },
    Function {
        domain: Idx<Self>,
        arrow: Span,
        codomain: Idx<Self>,
    },
    Parenthesized {
        lpar: Span,
        type_: Idx<Self>,
        rpar: Span,
    },
}

/// An atomic row for use in a type constraint.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum RowAtom<V> {
    Concrete {
        lpar: Span,
        fields: Separated<IdField<Idx<Type<V>>>>,
        rpar: Span,
    },
    Variable(SpanOf<V>),
}

/// A type constraint.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Constraint<V> {
    RowSum {
        lhs: RowAtom<V>,
        plus: Span,
        rhs: RowAtom<V>,
        eq: Span,
        goal: RowAtom<V>,
    },
}

/// A qualifiers for a type.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Qualifiers<V> {
    pub constraints: Separated<Constraint<V>>,
    pub arrow: Span,
}

/// A polymorphic Aiahr type.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Scheme<V> {
    pub quantifiers: Vec<Quantifier<V>>,
    pub qualifiers: Option<Qualifiers<V>>,
    pub type_: Idx<Type<V>>,
}

/// An effect operation.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct EffectOp<O, V> {
    pub name: SpanOf<O>,
    pub colon: Span,
    pub type_: Idx<Type<V>>,
}

/// An Aiahr pattern.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Pattern {
    ProductRow(ProductRow<Idx<Self>>),
    SumRow(SumRow<Idx<Self>>),
    Whole(SpanOf<Ident>),
}

/// An Aiahr term.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Term {
    Binding {
        var: SpanOf<Ident>,
        annotation: Option<TypeAnnotation<Ident>>,
        eq: Span,
        value: Idx<Self>,
        semi: Span,
        expr: Idx<Self>,
    },
    Handle {
        with: Span,
        handler: Idx<Self>,
        do_: Span,
        expr: Idx<Self>,
    },
    Abstraction {
        lbar: Span,
        arg: SpanOf<Ident>,
        annotation: Option<TypeAnnotation<Ident>>,
        rbar: Span,
        body: Idx<Self>,
    },
    Application {
        func: Idx<Self>,
        lpar: Span,
        arg: Idx<Self>,
        rpar: Span,
    },
    ProductRow(ProductRow<Idx<Self>>),
    // TODO: Roll this into product row once we figure out better syntax.
    // Consider copying over what we do for prod types where we allow `{ x, y, z }` syntax.
    Concat {
        left: Idx<Self>,
        concat: Span,
        right: Idx<Self>,
    },
    SumRow(SumRow<Idx<Self>>),
    DotAccess {
        base: Idx<Self>,
        dot: Span,
        field: SpanOf<Ident>,
    },
    Match {
        match_: Span,
        langle: Span,
        cases: Separated<Field<Idx<Pattern>, Idx<Self>>>,
        rangle: Span,
    },
    SymbolRef(SpanOf<Ident>),
    Parenthesized {
        lpar: Span,
        term: Idx<Self>,
        rpar: Span,
    },
}
impl Term {
    pub fn spanned<'a>(&'a self, arenas: &'a CstIndxAlloc) -> SpanTerm<'a> {
        SpanTerm { term: self, arenas }
    }
}
pub struct SpanTerm<'a> {
    term: &'a Term,
    arenas: &'a CstIndxAlloc,
}
impl SpanTerm<'_> {
    fn with_term(&self, term: Idx<Term>) -> Self {
        Self {
            term: &self.arenas[term],
            arenas: self.arenas,
        }
    }
}
impl Spanned for SpanTerm<'_> {
    fn span(&self) -> Span {
        match self.term {
            Term::Binding { var, expr, .. } => Span::join(var, &self.with_term(*expr)),
            Term::Handle { with, expr, .. } => Span::join(with, &self.with_term(*expr)),
            Term::Abstraction { lbar, body, .. } => Span::join(lbar, &self.with_term(*body)),
            Term::Application { func, rpar, .. } => Span::join(&self.with_term(*func), rpar),
            Term::ProductRow(p) => p.span(),
            Term::Concat { left, right, .. } => {
                Span::join(&self.with_term(*left), &self.with_term(*right))
            }
            Term::SumRow(s) => s.span(),
            Term::DotAccess { base, field, .. } => Span::join(&self.with_term(*base), field),
            Term::Match { match_, rangle, .. } => Span::join(match_, rangle),
            Term::SymbolRef(v) => v.span(),
            Term::Parenthesized { lpar, rpar, .. } => Span::join(lpar, rpar),
        }
    }
}

/// An effect definition in the CST
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EffectDefn {
    pub effect: Span,
    pub name: SpanOf<Ident>,
    pub lbrace: Span,
    pub ops: Vec<EffectOp<Ident, Ident>>,
    pub rbrace: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TermDefn {
    pub name: SpanOf<Ident>,
    pub annotation: Option<SchemeAnnotation<Ident>>,
    pub eq: Span,
    pub value: Idx<Term>,
}

/// A top-level item in an Aiahr source file.
#[derive(Clone, Debug, Eq, PartialEq)]
// Clippy thinks our second largest variant is 0 bytes which is clearly wrong.
#[allow(clippy::large_enum_variant)]
pub enum Item {
    Effect(EffectDefn),
    Term(TermDefn),
}

/// A parsed module.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct CstModule {
    pub indices: CstIndxAlloc,
    pub items: Vec<Item>,
}

impl<A, L: IndexedAllocate<A>, T: IndexedAllocate<A>> IndexedAllocate<A> for Field<L, T> {
    type Out = Field<L::Out, T::Out>;

    fn alloc(&self, alloc: &mut A) -> Self::Out {
        Field {
            label: self.label.alloc(alloc),
            sep: self.sep,
            target: self.target.alloc(alloc),
        }
    }
}

impl<A, T: IndexedAllocate<A>> IndexedAllocate<A> for Annotation<T> {
    type Out = Annotation<T::Out>;

    fn alloc(&self, alloc: &mut A) -> Self::Out {
        Annotation {
            colon: self.colon,
            type_: self.type_.alloc(alloc),
        }
    }
}

/// A quantifier for a polytype.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
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
impl<'a, A, V: 'a + ReferenceAllocate<'a, A>> ReferenceAllocate<'a, A> for Quantifier<V>
where
    V: ReferenceAllocate<'a, A>,
{
    type Out = Quantifier<V::Out>;

    fn ref_alloc(&self, alloc: &mut A) -> Self::Out {
        Quantifier {
            forall: self.forall,
            var: self.var.ref_alloc(alloc),
            dot: self.dot,
        }
    }
}

/// A field with a label in `L`, separator, and target in `T`.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct Field<L, T> {
    pub label: L,
    pub sep: Span,
    pub target: T,
}
impl<'a, A, L: ReferenceAllocate<'a, A>, T: ReferenceAllocate<'a, A>> ReferenceAllocate<'a, A>
    for Field<L, T>
{
    type Out = Field<L::Out, T::Out>;

    fn ref_alloc(&self, alloc: &mut A) -> Self::Out {
        Field {
            label: self.label.ref_alloc(alloc),
            sep: self.sep,
            target: self.target.ref_alloc(alloc),
        }
    }
}

impl<L: Spanned, T: Spanned> Spanned for Field<L, T> {
    fn span(&self) -> Span {
        Span::join(&self.label, &self.target)
    }
}
