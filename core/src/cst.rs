use std::{fmt::Debug, iter::FusedIterator};

use crate::{
    ident::Ident,
    span::{Span, SpanOf, Spanned},
};

pub mod indexed {
    //! Variant of the same data types as cst but using owned data and indexed arenas instead of
    //! reference arenas. This is to ease the transition to Salsa.

    use bumpalo::Bump;
    use la_arena::{Arena, Idx};

    use crate::ident::Ident;
    use crate::indexed::{HasArena, HasRefArena, IndexedAllocate, ReferenceAllocate};
    use crate::span::{Span, SpanOf};

    #[derive(Default)]
    pub struct CstIndxAlloc {
        types: Arena<Type<Ident>>,
        terms: Arena<Term>,
        pats: Arena<Pattern>,
    }
    impl HasArena<Pattern> for CstIndxAlloc {
        fn arena(&self) -> &Arena<Pattern> {
            &self.pats
        }
        fn arena_mut(&mut self) -> &mut Arena<Pattern> {
            &mut self.pats
        }
    }
    impl HasArena<Type<Ident>> for CstIndxAlloc {
        fn arena(&self) -> &Arena<Type<Ident>> {
            &self.types
        }
        fn arena_mut(&mut self) -> &mut Arena<Type<Ident>> {
            &mut self.types
        }
    }
    impl HasArena<Term> for CstIndxAlloc {
        fn arena(&self) -> &Arena<Term> {
            &self.terms
        }
        fn arena_mut(&mut self) -> &mut Arena<Term> {
            &mut self.terms
        }
    }

    pub struct CstRefAlloc<'a> {
        arena: &'a Bump,
        indices: CstIndxAlloc,
    }
    impl<'a> HasRefArena<'a> for CstRefAlloc<'a> {
        fn ref_arena(&self) -> &'a Bump {
            self.arena
        }
    }
    impl HasArena<Term> for CstRefAlloc<'_> {
        fn arena(&self) -> &Arena<Term> {
            &self.indices.terms
        }

        fn arena_mut(&mut self) -> &mut Arena<Term> {
            &mut self.indices.terms
        }
    }
    impl HasArena<Pattern> for CstRefAlloc<'_> {
        fn arena(&self) -> &Arena<Pattern> {
            &self.indices.pats
        }

        fn arena_mut(&mut self) -> &mut Arena<Pattern> {
            &mut self.indices.pats
        }
    }
    impl HasArena<Type<Ident>> for CstRefAlloc<'_> {
        fn arena(&self) -> &Arena<Type<Ident>> {
            &self.indices.types
        }

        fn arena_mut(&mut self) -> &mut Arena<Type<Ident>> {
            &mut self.indices.types
        }
    }

    /// A non-empty list of elements, separated by some fixed separator. To allow an empty list, wrap in
    /// `Option`.
    #[derive(Clone, Debug)]
    pub struct Separated<T> {
        pub first: T,
        pub elems: Vec<(Span, T)>,
        pub comma: Option<Span>,
    }

    pub type IdField<T> = super::Field<SpanOf<Ident>, T>;

    /// A product row with values in `T`.
    #[derive(Clone, Debug)]
    pub struct ProductRow<T> {
        pub lbrace: Span,
        pub fields: Option<Separated<IdField<T>>>,
        pub rbrace: Span,
    }

    /// A sum row with value in `T`.
    #[derive(Clone, Copy, Debug)]
    pub struct SumRow<T> {
        pub langle: Span,
        pub field: IdField<T>,
        pub rangle: Span,
    }

    /// A non-empty row with concrete fields in `C` and variables in `V`.
    #[derive(Clone, Debug)]
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

    /// An unqualified Aiahr type.
    #[derive(Clone, Debug)]
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
    #[derive(Clone, Debug)]
    pub enum RowAtom<V> {
        Concrete {
            lpar: Span,
            fields: Separated<IdField<Idx<Type<V>>>>,
            rpar: Span,
        },
        Variable(SpanOf<V>),
    }

    /// A type constraint.
    #[derive(Clone, Debug)]
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
    #[derive(Clone, Debug)]
    pub struct Qualifiers<V> {
        pub constraints: Separated<Constraint<V>>,
        pub arrow: Span,
    }

    /// A polymorphic Aiahr type.
    #[derive(Clone, Debug)]
    pub struct Scheme<V> {
        pub quantifiers: Vec<super::Quantifier<V>>,
        pub qualifiers: Option<Qualifiers<V>>,
        pub type_: Idx<Type<V>>,
    }

    /// An effect operation.
    #[derive(Clone, Debug)]
    pub struct EffectOp<O, V> {
        pub name: SpanOf<O>,
        pub colon: Span,
        pub type_: Idx<Type<V>>,
    }

    /// An Aiahr pattern.
    #[derive(Clone, Debug)]
    pub enum Pattern {
        ProductRow(ProductRow<Idx<Self>>),
        SumRow(SumRow<Idx<Self>>),
        Whole(SpanOf<Ident>),
    }

    /// A monotype annotation.
    pub type TypeAnnotation<V> = super::Annotation<Idx<Type<V>>>;

    /// A scheme annotation.
    pub type SchemeAnnotation<V> = super::Annotation<Scheme<V>>;

    /// An Aiahr term.
    #[derive(Clone, Debug)]
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
        SumRow(SumRow<Idx<Self>>),
        DotAccess {
            base: Idx<Self>,
            dot: Span,
            field: SpanOf<Ident>,
        },
        Match {
            match_: Span,
            langle: Span,
            cases: Separated<super::Field<Idx<Pattern>, Idx<Self>>>,
            rangle: Span,
        },
        SymbolRef(SpanOf<Ident>),
        Parenthesized {
            lpar: Span,
            term: Idx<Self>,
            rpar: Span,
        },
    }

    /// A top-level item in an Aiahr source file.
    #[derive(Clone, Debug)]
    pub enum Item {
        Effect {
            effect: Span,
            name: SpanOf<Ident>,
            lbrace: Span,
            ops: Vec<EffectOp<Ident, Ident>>,
            rbrace: Span,
        },
        Term {
            name: SpanOf<Ident>,
            annotation: Option<SchemeAnnotation<Ident>>,
            eq: Span,
            value: Idx<Term>,
        },
    }

    impl<A> IndexedAllocate<A> for Ident {
        type Out = Ident;

        fn alloc(&self, _: &mut A) -> Self::Out {
            *self
        }
    }
    impl<'a, A> ReferenceAllocate<'a, A> for Ident {
        type Out = Ident;

        fn ref_alloc(&self, _: &A) -> Self::Out {
            *self
        }
    }

    impl<A, T: IndexedAllocate<A>> IndexedAllocate<A> for Option<T> {
        type Out = Option<T::Out>;

        fn alloc(&self, alloc: &mut A) -> Self::Out {
            self.as_ref().map(|t| t.alloc(alloc))
        }
    }
    impl<'a, A, T> ReferenceAllocate<'a, A> for Option<T>
    where
        T: ReferenceAllocate<'a, A>,
    {
        type Out = Option<T::Out>;

        fn ref_alloc(&self, alloc: &A) -> Self::Out {
            self.as_ref().map(|t| t.ref_alloc(alloc))
        }
    }

    impl<A, V: IndexedAllocate<A>> IndexedAllocate<A> for SpanOf<V> {
        type Out = SpanOf<V::Out>;

        fn alloc(&self, alloc: &mut A) -> Self::Out {
            SpanOf {
                start: self.start,
                value: self.value.alloc(alloc),
                end: self.end,
            }
        }
    }
    impl<'a, A, V: ReferenceAllocate<'a, A>> ReferenceAllocate<'a, A> for SpanOf<V> {
        type Out = SpanOf<V::Out>;

        fn ref_alloc(&self, alloc: &A) -> Self::Out {
            SpanOf {
                start: self.start,
                value: self.value.ref_alloc(alloc),
                end: self.end,
            }
        }
    }

    impl<A, T: IndexedAllocate<A>> IndexedAllocate<A> for super::Separated<'_, T> {
        type Out = Separated<T::Out>;

        fn alloc(&self, alloc: &mut A) -> Self::Out {
            Separated {
                first: self.first.alloc(alloc),
                elems: self
                    .elems
                    .iter()
                    .map(|(span, elem)| (*span, elem.alloc(alloc)))
                    .collect(),
                comma: self.comma,
            }
        }
    }
    impl<'a, A: HasRefArena<'a>, T: ReferenceAllocate<'a, A>> ReferenceAllocate<'a, A>
        for Separated<T>
    {
        type Out = super::Separated<'a, T::Out>;

        fn ref_alloc(&self, alloc: &A) -> Self::Out {
            super::Separated {
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

    impl<A, L: IndexedAllocate<A>, T: IndexedAllocate<A>> IndexedAllocate<A> for super::Field<L, T> {
        type Out = super::Field<L::Out, T::Out>;

        fn alloc(&self, alloc: &mut A) -> Self::Out {
            super::Field {
                label: self.label.alloc(alloc),
                sep: self.sep,
                target: self.target.alloc(alloc),
            }
        }
    }
    impl<'a, A, L: ReferenceAllocate<'a, A>, T: ReferenceAllocate<'a, A>> ReferenceAllocate<'a, A>
        for super::Field<L, T>
    {
        type Out = super::Field<L::Out, T::Out>;

        fn ref_alloc(&self, alloc: &A) -> Self::Out {
            super::Field {
                label: self.label.ref_alloc(alloc),
                sep: self.sep,
                target: self.target.ref_alloc(alloc),
            }
        }
    }

    impl<A, V: IndexedAllocate<A>, C: IndexedAllocate<A>> IndexedAllocate<A> for super::Row<'_, V, C> {
        type Out = Row<V::Out, C::Out>;

        fn alloc(&self, alloc: &mut A) -> Self::Out {
            match self {
                super::Row::Concrete(concrete) => Row::Concrete(concrete.alloc(alloc)),
                super::Row::Variable(vars) => Row::Variable(vars.alloc(alloc)),
                super::Row::Mixed {
                    concrete,
                    vbar,
                    variables,
                } => Row::Mixed {
                    concrete: concrete.alloc(alloc),
                    vbar: *vbar,
                    variables: variables.alloc(alloc),
                },
            }
        }
    }
    impl<'a, A, V, C> ReferenceAllocate<'a, A> for Row<V, C>
    where
        A: HasRefArena<'a>,
        V: ReferenceAllocate<'a, A>,
        C: ReferenceAllocate<'a, A>,
    {
        type Out = super::Row<'a, V::Out, C::Out>;

        fn ref_alloc(&self, alloc: &A) -> Self::Out {
            match self {
                Row::Concrete(concrete) => super::Row::Concrete(concrete.ref_alloc(alloc)),
                Row::Variable(vars) => super::Row::Variable(vars.ref_alloc(alloc)),
                Row::Mixed {
                    concrete,
                    vbar,
                    variables,
                } => super::Row::Mixed {
                    concrete: concrete.ref_alloc(alloc),
                    vbar: *vbar,
                    variables: variables.ref_alloc(alloc),
                },
            }
        }
    }

    impl<'a, A, V: IndexedAllocate<A>> IndexedAllocate<A> for super::Type<'a, V>
    where
        A: HasArena<Type<V::Out>>,
    {
        type Out = Idx<Type<V::Out>>;

        fn alloc(&self, alloc: &mut A) -> Self::Out {
            let ty = match self {
                super::Type::Named(var) => Type::Named(var.alloc(alloc)),
                super::Type::Sum {
                    langle,
                    variants,
                    rangle,
                } => Type::Sum {
                    langle: *langle,
                    variants: variants.alloc(alloc),
                    rangle: *rangle,
                },
                super::Type::Product {
                    lbrace,
                    fields,
                    rbrace,
                } => Type::Product {
                    lbrace: *lbrace,
                    fields: fields.alloc(alloc),
                    rbrace: *rbrace,
                },
                super::Type::Function {
                    domain,
                    arrow,
                    codomain,
                } => Type::Function {
                    domain: domain.alloc(alloc),
                    arrow: *arrow,
                    codomain: codomain.alloc(alloc),
                },
                super::Type::Parenthesized { lpar, type_, rpar } => Type::Parenthesized {
                    lpar: *lpar,
                    type_: type_.alloc(alloc),
                    rpar: *rpar,
                },
            };
            alloc.arena_mut().alloc(ty)
        }
    }
    impl<'a, A, V: ReferenceAllocate<'a, A>> ReferenceAllocate<'a, A> for Idx<Type<V>>
    where
        A: HasRefArena<'a> + HasArena<Type<V>>,
    {
        type Out = &'a super::Type<'a, V::Out>;

        fn ref_alloc(&self, alloc: &A) -> Self::Out {
            let type_ = match &alloc.arena()[*self] {
                Type::Named(var) => super::Type::Named(var.ref_alloc(alloc)),
                Type::Sum {
                    langle,
                    variants,
                    rangle,
                } => super::Type::Sum {
                    langle: *langle,
                    variants: variants.ref_alloc(alloc),
                    rangle: *rangle,
                },
                Type::Product {
                    lbrace,
                    fields,
                    rbrace,
                } => super::Type::Product {
                    lbrace: *lbrace,
                    fields: fields.ref_alloc(alloc),
                    rbrace: *rbrace,
                },
                Type::Function {
                    domain,
                    arrow,
                    codomain,
                } => super::Type::Function {
                    domain: domain.ref_alloc(alloc),
                    arrow: *arrow,
                    codomain: codomain.ref_alloc(alloc),
                },
                Type::Parenthesized { lpar, type_, rpar } => super::Type::Parenthesized {
                    lpar: *lpar,
                    type_: type_.ref_alloc(alloc),
                    rpar: *rpar,
                },
            };
            alloc.ref_arena().alloc(type_) as &_
        }
    }

    impl<A, O: IndexedAllocate<A>, V: IndexedAllocate<A>> IndexedAllocate<A>
        for super::EffectOp<'_, O, V>
    where
        A: HasArena<Type<V::Out>>,
    {
        type Out = EffectOp<O::Out, V::Out>;

        fn alloc(&self, alloc: &mut A) -> Self::Out {
            EffectOp {
                name: self.name.alloc(alloc),
                colon: self.colon,
                type_: self.type_.alloc(alloc),
            }
        }
    }
    impl<'a, A, O, V> ReferenceAllocate<'a, A> for EffectOp<O, V>
    where
        A: HasRefArena<'a> + HasArena<Type<V>>,
        O: ReferenceAllocate<'a, A>,
        V: ReferenceAllocate<'a, A>,
    {
        type Out = super::EffectOp<'a, O::Out, V::Out>;

        fn ref_alloc(&self, alloc: &A) -> Self::Out {
            super::EffectOp {
                name: self.name.ref_alloc(alloc),
                colon: self.colon,
                type_: self.type_.ref_alloc(alloc),
            }
        }
    }

    impl<A, T: IndexedAllocate<A>> IndexedAllocate<A> for super::Annotation<T> {
        type Out = super::Annotation<T::Out>;

        fn alloc(&self, alloc: &mut A) -> Self::Out {
            super::Annotation {
                colon: self.colon,
                type_: self.type_.alloc(alloc),
            }
        }
    }
    impl<'a, A, T> ReferenceAllocate<'a, A> for super::Annotation<T>
    where
        T: ReferenceAllocate<'a, A>,
    {
        type Out = super::Annotation<T::Out>;

        fn ref_alloc(&self, alloc: &A) -> Self::Out {
            super::Annotation {
                colon: self.colon,
                type_: self.type_.ref_alloc(alloc),
            }
        }
    }

    impl<'a, A, V: IndexedAllocate<A>> IndexedAllocate<A> for super::Scheme<'a, V>
    where
        A: HasArena<Type<V::Out>>,
    {
        type Out = Scheme<V::Out>;

        fn alloc(&self, alloc: &mut A) -> Self::Out {
            Scheme {
                quantifiers: self
                    .quantifiers
                    .iter()
                    .map(|quant| quant.alloc(alloc))
                    .collect(),
                qualifiers: self.qualifiers.alloc(alloc),
                type_: self.type_.alloc(alloc),
            }
        }
    }
    impl<'a, A, V: 'a + ReferenceAllocate<'a, A>> ReferenceAllocate<'a, A> for Scheme<V>
    where
        A: HasRefArena<'a> + HasArena<Type<V>>,
    {
        type Out = &'a super::Scheme<'a, V::Out>;

        fn ref_alloc(&self, alloc: &A) -> Self::Out {
            let scheme = super::Scheme {
                quantifiers: alloc.ref_arena().alloc_slice_fill_iter(
                    self.quantifiers.iter().map(|quant| quant.ref_alloc(alloc)),
                ) as &_,
                qualifiers: self.qualifiers.ref_alloc(alloc),
                type_: self.type_.ref_alloc(alloc),
            };
            alloc.ref_arena().alloc(scheme) as &_
        }
    }

    impl<A, V: IndexedAllocate<A>> IndexedAllocate<A> for super::Quantifier<V> {
        type Out = super::Quantifier<V::Out>;

        fn alloc(&self, alloc: &mut A) -> Self::Out {
            super::Quantifier {
                forall: self.forall,
                var: self.var.alloc(alloc),
                dot: self.dot,
            }
        }
    }
    impl<'a, A, V: 'a + ReferenceAllocate<'a, A>> ReferenceAllocate<'a, A> for super::Quantifier<V>
    where
        V: ReferenceAllocate<'a, A>,
    {
        type Out = super::Quantifier<V::Out>;

        fn ref_alloc(&self, alloc: &A) -> Self::Out {
            super::Quantifier {
                forall: self.forall,
                var: self.var.ref_alloc(alloc),
                dot: self.dot,
            }
        }
    }

    impl<A, V: IndexedAllocate<A>> IndexedAllocate<A> for super::Qualifiers<'_, V>
    where
        A: HasArena<Type<V::Out>>,
    {
        type Out = Qualifiers<V::Out>;

        fn alloc(&self, alloc: &mut A) -> Self::Out {
            Qualifiers {
                constraints: self.constraints.alloc(alloc),
                arrow: self.arrow,
            }
        }
    }
    impl<'a, A, V: ReferenceAllocate<'a, A>> ReferenceAllocate<'a, A> for Qualifiers<V>
    where
        A: HasRefArena<'a> + HasArena<Type<V>>,
    {
        type Out = super::Qualifiers<'a, V::Out>;

        fn ref_alloc(&self, alloc: &A) -> Self::Out {
            super::Qualifiers {
                constraints: self.constraints.ref_alloc(alloc),
                arrow: self.arrow,
            }
        }
    }

    impl<A, V: IndexedAllocate<A>> IndexedAllocate<A> for super::Constraint<'_, V>
    where
        A: HasArena<Type<V::Out>>,
    {
        type Out = Constraint<V::Out>;

        fn alloc(&self, alloc: &mut A) -> Self::Out {
            match self {
                super::Constraint::RowSum {
                    lhs,
                    plus,
                    rhs,
                    eq,
                    goal,
                } => Constraint::RowSum {
                    lhs: lhs.alloc(alloc),
                    plus: *plus,
                    rhs: rhs.alloc(alloc),
                    eq: *eq,
                    goal: goal.alloc(alloc),
                },
            }
        }
    }
    impl<'a, A, V> ReferenceAllocate<'a, A> for Constraint<V>
    where
        A: HasRefArena<'a> + HasArena<Type<V>>,
        V: ReferenceAllocate<'a, A>,
    {
        type Out = super::Constraint<'a, V::Out>;

        fn ref_alloc(&self, alloc: &A) -> Self::Out {
            match self {
                Constraint::RowSum {
                    lhs,
                    plus,
                    rhs,
                    eq,
                    goal,
                } => super::Constraint::RowSum {
                    lhs: lhs.ref_alloc(alloc),
                    plus: *plus,
                    rhs: rhs.ref_alloc(alloc),
                    eq: *eq,
                    goal: goal.ref_alloc(alloc),
                },
            }
        }
    }

    impl<A, V: IndexedAllocate<A>> IndexedAllocate<A> for super::RowAtom<'_, V>
    where
        A: HasArena<Type<V::Out>>,
    {
        type Out = RowAtom<V::Out>;

        fn alloc(&self, alloc: &mut A) -> Self::Out {
            match self {
                super::RowAtom::Concrete { lpar, fields, rpar } => RowAtom::Concrete {
                    lpar: *lpar,
                    fields: fields.alloc(alloc),
                    rpar: *rpar,
                },
                super::RowAtom::Variable(var) => RowAtom::Variable(var.alloc(alloc)),
            }
        }
    }
    impl<'a, A, V> ReferenceAllocate<'a, A> for RowAtom<V>
    where
        A: HasRefArena<'a> + HasArena<Type<V>>,
        V: ReferenceAllocate<'a, A>,
    {
        type Out = super::RowAtom<'a, V::Out>;

        fn ref_alloc(&self, alloc: &A) -> Self::Out {
            match self {
                RowAtom::Concrete { lpar, fields, rpar } => super::RowAtom::Concrete {
                    lpar: *lpar,
                    fields: fields.ref_alloc(alloc),
                    rpar: *rpar,
                },
                RowAtom::Variable(var) => super::RowAtom::Variable(var.ref_alloc(alloc)),
            }
        }
    }

    impl IndexedAllocate<CstIndxAlloc> for super::Term<'_> {
        type Out = Idx<Term>;

        fn alloc(&self, alloc: &mut CstIndxAlloc) -> Self::Out {
            let term = match self {
                super::Term::Binding {
                    var,
                    annotation,
                    eq,
                    value,
                    semi,
                    expr,
                } => Term::Binding {
                    var: var.ref_alloc(alloc),
                    annotation: annotation.alloc(alloc),
                    eq: *eq,
                    value: value.alloc(alloc),
                    semi: *semi,
                    expr: expr.alloc(alloc),
                },
                super::Term::Handle {
                    with,
                    handler,
                    do_,
                    expr,
                } => Term::Handle {
                    with: *with,
                    handler: handler.alloc(alloc),
                    do_: *do_,
                    expr: expr.alloc(alloc),
                },
                super::Term::Abstraction {
                    lbar,
                    arg,
                    annotation,
                    rbar,
                    body,
                } => Term::Abstraction {
                    lbar: *lbar,
                    arg: arg.ref_alloc(alloc),
                    annotation: annotation.alloc(alloc),
                    rbar: *rbar,
                    body: body.alloc(alloc),
                },
                super::Term::Application {
                    func,
                    lpar,
                    arg,
                    rpar,
                } => Term::Application {
                    func: func.alloc(alloc),
                    lpar: *lpar,
                    arg: arg.alloc(alloc),
                    rpar: *rpar,
                },
                super::Term::ProductRow(prod) => Term::ProductRow(prod.alloc(alloc)),
                super::Term::SumRow(sum) => Term::SumRow(sum.alloc(alloc)),
                super::Term::DotAccess { base, dot, field } => Term::DotAccess {
                    base: base.alloc(alloc),
                    dot: *dot,
                    field: field.ref_alloc(alloc),
                },
                super::Term::Match {
                    match_,
                    langle,
                    cases,
                    rangle,
                } => Term::Match {
                    match_: *match_,
                    langle: *langle,
                    cases: cases.alloc(alloc),
                    rangle: *rangle,
                },
                super::Term::SymbolRef(symbol) => Term::SymbolRef(symbol.ref_alloc(alloc)),
                super::Term::Parenthesized { lpar, term, rpar } => Term::Parenthesized {
                    lpar: *lpar,
                    term: term.alloc(alloc),
                    rpar: *rpar,
                },
            };
            alloc.arena_mut().alloc(term)
        }
    }
    impl<'a, A> ReferenceAllocate<'a, A> for Idx<Term>
    where
        A: HasRefArena<'a> + HasArena<Term> + HasArena<Type<Ident>> + HasArena<Pattern>,
    {
        type Out = &'a super::Term<'a>;

        fn ref_alloc(&self, alloc: &A) -> Self::Out {
            let term = match &alloc.arena()[*self] {
                Term::Binding {
                    var,
                    annotation,
                    eq,
                    value,
                    semi,
                    expr,
                } => super::Term::Binding {
                    var: *var,
                    annotation: annotation.ref_alloc(alloc),
                    eq: *eq,
                    value: value.ref_alloc(alloc),
                    semi: *semi,
                    expr: expr.ref_alloc(alloc),
                },
                Term::Handle {
                    with,
                    handler,
                    do_,
                    expr,
                } => super::Term::Handle {
                    with: *with,
                    handler: handler.ref_alloc(alloc),
                    do_: *do_,
                    expr: expr.ref_alloc(alloc),
                },
                Term::Abstraction {
                    lbar,
                    arg,
                    annotation,
                    rbar,
                    body,
                } => super::Term::Abstraction {
                    lbar: *lbar,
                    arg: *arg,
                    annotation: annotation.ref_alloc(alloc),
                    rbar: *rbar,
                    body: body.ref_alloc(alloc),
                },
                Term::Application {
                    func,
                    lpar,
                    arg,
                    rpar,
                } => super::Term::Application {
                    func: func.ref_alloc(alloc),
                    lpar: *lpar,
                    arg: arg.ref_alloc(alloc),
                    rpar: *rpar,
                },
                Term::ProductRow(prod) => super::Term::ProductRow(prod.ref_alloc(alloc)),
                Term::SumRow(sum) => super::Term::SumRow(sum.ref_alloc(alloc)),
                Term::DotAccess { base, dot, field } => super::Term::DotAccess {
                    base: base.ref_alloc(alloc),
                    dot: *dot,
                    field: *field,
                },
                Term::Match {
                    match_,
                    langle,
                    cases,
                    rangle,
                } => super::Term::Match {
                    match_: *match_,
                    langle: *langle,
                    cases: cases.ref_alloc(alloc),
                    rangle: *rangle,
                },
                Term::SymbolRef(symbol) => super::Term::SymbolRef(*symbol),
                Term::Parenthesized { lpar, term, rpar } => super::Term::Parenthesized {
                    lpar: *lpar,
                    term: term.ref_alloc(alloc),
                    rpar: *rpar,
                },
            };
            alloc.ref_arena().alloc(term) as &_
        }
    }

    impl<A, T: IndexedAllocate<A>> IndexedAllocate<A> for super::SumRow<T> {
        type Out = SumRow<T::Out>;

        fn alloc(&self, alloc: &mut A) -> Self::Out {
            SumRow {
                langle: self.langle,
                field: self.field.alloc(alloc),
                rangle: self.rangle,
            }
        }
    }
    impl<'a, A, T: ReferenceAllocate<'a, A>> ReferenceAllocate<'a, A> for SumRow<T> {
        type Out = super::SumRow<T::Out>;

        fn ref_alloc(&self, alloc: &A) -> Self::Out {
            super::SumRow {
                langle: self.langle,
                field: self.field.ref_alloc(alloc),
                rangle: self.rangle,
            }
        }
    }

    impl<A, T: IndexedAllocate<A>> IndexedAllocate<A> for super::ProductRow<'_, T> {
        type Out = ProductRow<T::Out>;

        fn alloc(&self, alloc: &mut A) -> Self::Out {
            ProductRow {
                lbrace: self.lbrace,
                fields: self.fields.alloc(alloc),
                rbrace: self.rbrace,
            }
        }
    }
    impl<'a, A, T> ReferenceAllocate<'a, A> for ProductRow<T>
    where
        A: HasRefArena<'a>,
        T: ReferenceAllocate<'a, A>,
    {
        type Out = super::ProductRow<'a, T::Out>;

        fn ref_alloc(&self, alloc: &A) -> Self::Out {
            super::ProductRow {
                lbrace: self.lbrace,
                fields: self.fields.ref_alloc(alloc),
                rbrace: self.rbrace,
            }
        }
    }

    impl<A> IndexedAllocate<A> for super::Pattern<'_>
    where
        A: HasArena<Pattern>,
    {
        type Out = Idx<Pattern>;

        fn alloc(&self, alloc: &mut A) -> Self::Out {
            let pat = match self {
                super::Pattern::ProductRow(prod) => Pattern::ProductRow(prod.alloc(alloc)),
                super::Pattern::SumRow(sum) => Pattern::SumRow(sum.alloc(alloc)),
                super::Pattern::Whole(var) => Pattern::Whole(var.ref_alloc(alloc)),
            };
            alloc.arena_mut().alloc(pat)
        }
    }

    impl<'a, A> ReferenceAllocate<'a, A> for Idx<Pattern>
    where
        A: HasRefArena<'a> + HasArena<Pattern>,
    {
        type Out = &'a super::Pattern<'a>;

        fn ref_alloc(&self, alloc: &A) -> Self::Out {
            let pat = match &alloc.arena()[*self] {
                Pattern::ProductRow(prod) => super::Pattern::ProductRow(prod.ref_alloc(alloc)),
                Pattern::SumRow(sum) => super::Pattern::SumRow(sum.ref_alloc(alloc)),
                Pattern::Whole(var) => super::Pattern::Whole(var.ref_alloc(alloc)),
            };
            alloc.ref_arena().alloc(pat) as &_
        }
    }

    impl IndexedAllocate<CstIndxAlloc> for super::Item<'_> {
        type Out = Item;

        fn alloc(&self, alloc: &mut CstIndxAlloc) -> Self::Out {
            match self {
                super::Item::Effect {
                    effect,
                    name,
                    lbrace,
                    ops,
                    rbrace,
                } => Item::Effect {
                    effect: *effect,
                    name: name.ref_alloc(alloc),
                    lbrace: *lbrace,
                    ops: ops.iter().map(|op| op.alloc(alloc)).collect(),
                    rbrace: *rbrace,
                },
                super::Item::Term {
                    name,
                    annotation,
                    eq,
                    value,
                } => Item::Term {
                    name: name.ref_alloc(alloc),
                    annotation: annotation.alloc(alloc),
                    eq: *eq,
                    value: value.alloc(alloc),
                },
            }
        }
    }
    impl<'a, A> ReferenceAllocate<'a, A> for Item
    where
        A: HasRefArena<'a> + HasArena<Type<Ident>> + HasArena<Term> + HasArena<Pattern>,
    {
        type Out = super::Item<'a>;

        fn ref_alloc(&self, alloc: &A) -> Self::Out {
            match self {
                Item::Effect {
                    effect,
                    name,
                    lbrace,
                    ops,
                    rbrace,
                } => super::Item::Effect {
                    effect: *effect,
                    name: *name,
                    lbrace: *lbrace,
                    ops: alloc
                        .ref_arena()
                        .alloc_slice_fill_iter(ops.iter().map(|op| op.ref_alloc(alloc))),
                    rbrace: *rbrace,
                },
                Item::Term {
                    name,
                    annotation,
                    eq,
                    value,
                } => super::Item::Term {
                    name: *name,
                    annotation: annotation.ref_alloc(alloc),
                    eq: *eq,
                    value: value.ref_alloc(alloc),
                },
            }
        }
    }
}

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
pub type IdField<T> = Field<SpanOf<Ident>, T>;

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
pub type TypeRow<'a, V> = Row<'a, V, IdField<&'a Type<'a, V>>>;

/// An unqualified Aiahr type.
#[derive(Clone, Copy, Debug)]
pub enum Type<'a, V> {
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
pub struct Qualifiers<'a, V> {
    pub constraints: Separated<'a, Constraint<'a, V>>,
    pub arrow: Span,
}

impl<'a, V> Spanned for Qualifiers<'a, V> {
    fn span(&self) -> Span {
        Span::join(&self.constraints, &self.arrow)
    }
}

/// A polymorphic Aiahr type.
#[derive(Clone, Copy, Debug)]
pub struct Scheme<'a, V> {
    pub quantifiers: &'a [Quantifier<V>],
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

/// An Aiahr pattern.
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

/// A typing annotation for a variable.
#[derive(Clone, Copy, Debug)]
pub struct Annotation<T> {
    pub colon: Span,
    pub type_: T,
}

/// A monotype annotation.
pub type TypeAnnotation<'a, V> = Annotation<&'a Type<'a, V>>;

/// A scheme annotation.
pub type SchemeAnnotation<'a, V> = Annotation<&'a Scheme<'a, V>>;

/// An Aiahr term.
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
}

impl<'a> Spanned for Term<'a> {
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
macro_rules! row_concrete {
    ($($fields:pat),+ $(,)?) => {
        $crate::cst::Row::Concrete($crate::separated!($($fields),+))
    };
}

#[macro_export]
macro_rules! row_variable {
    ($($vars:pat),+ $(,)?) => {
        $crate::cst::Row::Variable($crate::separated!($($crate::span_of!($vars)),+))
    };
}

#[macro_export]
macro_rules! row_mixed {
    (($($fields:pat),+ $(,)?), ($($vars:pat),+ $(,)?)) => {
        $crate::cst::Row::Mixed {
            concrete: $crate::separated!($($fields),+),
            variables: $crate::separated!($($crate::span_of!($vars)),+),
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
        $crate::cst::RowAtom::Variable($crate::span_of!($var))
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
    ($name:literal, $type_:pat) => {
        $crate::cst::EffectOp {
            name: $crate::span_of!($crate::h!($name)),
            type_: $type_,
            ..
        }
    };
    ($name:pat, $type_:pat) => {
        $crate::cst::EffectOp {
            name: $crate::span_of!($name),
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
        &$crate::cst::Pattern::Whole($crate::span_of!($var))
    };
}

#[macro_export]
macro_rules! term_local {
    ($var:pat, $value:pat, $expr:pat) => {
        &$crate::cst::Term::Binding {
            var: $crate::span_of!($var),
            annotation: None,
            value: $value,
            expr: $expr,
            ..
        }
    };
    ($var:pat, $type_:pat, $value:pat, $expr:pat) => {
        &$crate::cst::Term::Binding {
            var: $crate::span_of!($var),
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
            arg: $crate::span_of!($arg),
            annotation: None,
            body: $body,
            ..
        }
    };
    ($arg:pat, $type_:pat, $body:pat) => {
        &$crate::cst::Term::Abstraction {
            arg: $crate::span_of!($arg),
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
macro_rules! item_effect {
    ($name:pat, $($ops:pat),* $(,)?) => {
        $crate::cst::Item::Effect {
            name: $crate::span_of!($name),
            ops: &[$($ops),*],
            ..
        }
    };
}

#[macro_export]
macro_rules! item_term {
    ($name:pat, $value:pat) => {
        $crate::cst::Item::Term {
            name: $crate::span_of!($name),
            annotation: None,
            value: $value,
            ..
        }
    };
    ($name:pat, $type_:pat, $value:pat) => {
        $crate::cst::Item::Term {
            name: $crate::span_of!($name),
            annotation: Some($crate::cst::SchemeAnnotation { type_: $type_, .. }),
            value: $value,
            ..
        }
    };
}
