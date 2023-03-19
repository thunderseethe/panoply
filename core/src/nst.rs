use crate::{
    cst::{EffectOp, Field, ProductRow, SchemeAnnotation, Separated, SumRow, TypeAnnotation},
    id::{EffectId, EffectOpId, ItemId, ModuleId, TyVarId, VarId},
    ident::Ident,
    span::{Span, SpanOf, Spanned},
};

pub mod indexed {
    use bumpalo::Bump;
    use la_arena::{Arena, Idx};

    use crate::cst::indexed::{
        EffectOp, ProductRow, SchemeAnnotation, Separated, SumRow, Type, TypeAnnotation,
    };
    use crate::cst::Field;
    use crate::id::{EffectId, EffectOpId, ItemId, ModuleId, TyVarId, VarId};
    use crate::ident::Ident;
    use crate::indexed::{HasArena, HasRefArena, IndexedAllocate, ReferenceAllocate};
    use crate::span::{Span, SpanOf};

    #[derive(Default)]
    pub struct NstIndxAlloc {
        types: Arena<Type<TyVarId>>,
        terms: Arena<Term>,
        pats: Arena<Pattern>,
    }
    impl HasArena<Type<TyVarId>> for NstIndxAlloc {
        fn arena(&self) -> &Arena<Type<TyVarId>> {
            &self.types
        }
        fn arena_mut(&mut self) -> &mut Arena<Type<TyVarId>> {
            &mut self.types
        }
    }
    impl HasArena<Term> for NstIndxAlloc {
        fn arena(&self) -> &Arena<Term> {
            &self.terms
        }
        fn arena_mut(&mut self) -> &mut Arena<Term> {
            &mut self.terms
        }
    }
    impl HasArena<Pattern> for NstIndxAlloc {
        fn arena(&self) -> &Arena<Pattern> {
            &self.pats
        }
        fn arena_mut(&mut self) -> &mut Arena<Pattern> {
            &mut self.pats
        }
    }

    pub struct NstRefAlloc<'a> {
        /// Allocate the new reference based tree types.
        arena: &'a Bump,
        /// Included to expand indices encountered during conversion
        indices: NstIndxAlloc,
    }
    impl<'a> HasRefArena<'a> for NstRefAlloc<'a> {
        fn ref_arena(&self) -> &'a Bump {
            self.arena
        }
    }
    impl<T> HasArena<T> for NstRefAlloc<'_>
    where
        NstIndxAlloc: HasArena<T>,
    {
        fn arena(&self) -> &Arena<T> {
            self.indices.arena()
        }

        fn arena_mut(&mut self) -> &mut Arena<T> {
            self.indices.arena_mut()
        }
    }

    /// A pattern with names resolved.
    #[derive(Clone, Debug)]
    pub enum Pattern {
        ProductRow(ProductRow<Idx<Self>>),
        SumRow(SumRow<Idx<Self>>),
        Whole(SpanOf<VarId>),
    }

    /// An Aiahr term with names resolved.
    #[derive(Clone, Debug)]
    pub enum Term {
        Binding {
            var: SpanOf<VarId>,
            annotation: Option<TypeAnnotation<TyVarId>>,
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
            arg: SpanOf<VarId>,
            annotation: Option<TypeAnnotation<TyVarId>>,
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
        FieldAccess {
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
        EffectOpRef(SpanOf<(ModuleId, EffectId, EffectOpId)>),
        ItemRef(SpanOf<(ModuleId, ItemId)>),
        VariableRef(SpanOf<VarId>),
        Parenthesized {
            lpar: Span,
            term: Idx<Self>,
            rpar: Span,
        },
    }

    /// A top-level item in an Aiahr source file with names resolved.
    #[derive(Clone, Debug)]
    pub enum Item {
        Effect {
            effect: Span,
            name: SpanOf<EffectId>,
            lbrace: Span,
            ops: Vec<Option<EffectOp<EffectOpId, TyVarId>>>,
            rbrace: Span,
        },
        Term {
            name: SpanOf<ItemId>,
            annotation: Option<SchemeAnnotation<TyVarId>>,
            eq: Span,
            value: Idx<Term>,
        },
    }

    impl<A> IndexedAllocate<A> for EffectOpId {
        type Out = EffectOpId;

        fn alloc(&self, _: &mut A) -> Self::Out {
            *self
        }
    }
    impl<A> ReferenceAllocate<'_, A> for EffectOpId {
        type Out = EffectOpId;

        fn ref_alloc(&self, _: &mut A) -> Self::Out {
            *self
        }
    }

    impl<A> IndexedAllocate<A> for TyVarId {
        type Out = TyVarId;

        fn alloc(&self, _: &mut A) -> Self::Out {
            *self
        }
    }
    impl<A> ReferenceAllocate<'_, A> for TyVarId {
        type Out = TyVarId;

        fn ref_alloc(&self, _: &mut A) -> Self::Out {
            *self
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
                super::Pattern::Whole(var) => Pattern::Whole(*var),
            };
            alloc.arena_mut().alloc(pat)
        }
    }
    impl<'a, A> ReferenceAllocate<'a, A> for Idx<Pattern>
    where
        A: HasRefArena<'a> + HasArena<Pattern>,
    {
        type Out = &'a super::Pattern<'a>;

        fn ref_alloc(&self, alloc: &mut A) -> Self::Out {
            let pat = match alloc.arena()[*self].clone() {
                Pattern::ProductRow(prod) => super::Pattern::ProductRow(prod.ref_alloc(alloc)),
                Pattern::SumRow(sum) => super::Pattern::SumRow(sum.ref_alloc(alloc)),
                Pattern::Whole(var) => super::Pattern::Whole(var),
            };
            alloc.ref_arena().alloc(pat) as &_
        }
    }

    impl IndexedAllocate<NstIndxAlloc> for super::Term<'_> {
        type Out = Idx<Term>;

        fn alloc(&self, alloc: &mut NstIndxAlloc) -> Self::Out {
            let term = match self {
                super::Term::Binding {
                    var,
                    annotation,
                    eq,
                    value,
                    semi,
                    expr,
                } => Term::Binding {
                    var: *var,
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
                    arg: *arg,
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
                super::Term::FieldAccess { base, dot, field } => Term::FieldAccess {
                    base: base.alloc(alloc),
                    dot: *dot,
                    field: field.alloc(alloc),
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
                super::Term::Parenthesized { lpar, term, rpar } => Term::Parenthesized {
                    lpar: *lpar,
                    term: term.alloc(alloc),
                    rpar: *rpar,
                },
                super::Term::EffectOpRef(ids) => Term::EffectOpRef(*ids),
                super::Term::ItemRef(id) => Term::ItemRef(*id),
                super::Term::VariableRef(id) => Term::VariableRef(*id),
            };
            alloc.arena_mut().alloc(term)
        }
    }
    impl<'a, A> ReferenceAllocate<'a, A> for Idx<Term>
    where
        A: HasRefArena<'a> + HasArena<Term> + HasArena<Type<TyVarId>> + HasArena<Pattern>,
    {
        type Out = &'a super::Term<'a>;

        fn ref_alloc(&self, alloc: &mut A) -> Self::Out {
            let term = match alloc.arena()[*self].clone() {
                Term::Binding {
                    var,
                    annotation,
                    eq,
                    value,
                    semi,
                    expr,
                } => super::Term::Binding {
                    var,
                    annotation: annotation.ref_alloc(alloc),
                    eq,
                    value: value.ref_alloc(alloc),
                    semi,
                    expr: expr.ref_alloc(alloc),
                },
                Term::Handle {
                    with,
                    handler,
                    do_,
                    expr,
                } => super::Term::Handle {
                    with,
                    handler: handler.ref_alloc(alloc),
                    do_,
                    expr: expr.ref_alloc(alloc),
                },
                Term::Abstraction {
                    lbar,
                    arg,
                    annotation,
                    rbar,
                    body,
                } => super::Term::Abstraction {
                    lbar,
                    arg,
                    annotation: annotation.ref_alloc(alloc),
                    rbar,
                    body: body.ref_alloc(alloc),
                },
                Term::Application {
                    func,
                    lpar,
                    arg,
                    rpar,
                } => super::Term::Application {
                    func: func.ref_alloc(alloc),
                    lpar,
                    arg: arg.ref_alloc(alloc),
                    rpar,
                },
                Term::ProductRow(prod) => super::Term::ProductRow(prod.ref_alloc(alloc)),
                Term::SumRow(sum) => super::Term::SumRow(sum.ref_alloc(alloc)),
                Term::FieldAccess { base, dot, field } => super::Term::FieldAccess {
                    base: base.ref_alloc(alloc),
                    dot,
                    field: field.ref_alloc(alloc),
                },
                Term::Match {
                    match_,
                    langle,
                    cases,
                    rangle,
                } => super::Term::Match {
                    match_,
                    langle,
                    cases: cases.ref_alloc(alloc),
                    rangle,
                },
                Term::EffectOpRef(ids) => super::Term::EffectOpRef(ids),
                Term::ItemRef(ids) => super::Term::ItemRef(ids),
                Term::VariableRef(var) => super::Term::VariableRef(var),
                Term::Parenthesized { lpar, term, rpar } => super::Term::Parenthesized {
                    lpar,
                    term: term.ref_alloc(alloc),
                    rpar,
                },
            };
            alloc.ref_arena().alloc(term) as &_
        }
    }

    impl IndexedAllocate<NstIndxAlloc> for super::Item<'_> {
        type Out = Item;

        fn alloc(&self, alloc: &mut NstIndxAlloc) -> Self::Out {
            match self {
                super::Item::Effect {
                    effect,
                    name,
                    lbrace,
                    ops,
                    rbrace,
                } => Item::Effect {
                    effect: *effect,
                    name: *name,
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
                    name: *name,
                    annotation: annotation.alloc(alloc),
                    eq: *eq,
                    value: value.alloc(alloc),
                },
            }
        }
    }
}

/// A pattern with names resolved.
#[derive(Clone, Copy, Debug)]
pub enum Pattern<'a> {
    ProductRow(ProductRow<'a, &'a Pattern<'a>>),
    SumRow(SumRow<&'a Pattern<'a>>),
    Whole(SpanOf<VarId>),
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

/// An Aiahr term with names resolved.
#[derive(Clone, Copy, Debug)]
pub enum Term<'a> {
    Binding {
        var: SpanOf<VarId>,
        annotation: Option<TypeAnnotation<'a, TyVarId>>,
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
        arg: SpanOf<VarId>,
        annotation: Option<TypeAnnotation<'a, TyVarId>>,
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
    FieldAccess {
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
    EffectOpRef(SpanOf<(ModuleId, EffectId, EffectOpId)>),
    ItemRef(SpanOf<(ModuleId, ItemId)>),
    VariableRef(SpanOf<VarId>),
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
            Term::FieldAccess { base, field, .. } => Span::join(*base, field),
            Term::Match { match_, rangle, .. } => Span::join(match_, rangle),
            Term::EffectOpRef(o) => o.span(),
            Term::ItemRef(i) => i.span(),
            Term::VariableRef(v) => v.span(),
            Term::Parenthesized { lpar, rpar, .. } => Span::join(lpar, rpar),
        }
    }
}

/// A top-level item in an Aiahr source file with names resolved.
#[derive(Clone, Copy, Debug)]
pub enum Item<'a> {
    Effect {
        effect: Span,
        name: SpanOf<EffectId>,
        lbrace: Span,
        ops: &'a [Option<EffectOp<'a, EffectOpId, TyVarId>>],
        rbrace: Span,
    },
    Term {
        name: SpanOf<ItemId>,
        annotation: Option<SchemeAnnotation<'a, TyVarId>>,
        eq: Span,
        value: &'a Term<'a>,
    },
}

impl<'a> Spanned for Item<'a> {
    fn span(&self) -> Span {
        match self {
            Item::Effect { effect, rbrace, .. } => Span::join(effect, rbrace),
            Item::Term { name, value, .. } => Span::join(name, *value),
        }
    }
}

#[macro_export]
macro_rules! npat_prod {
    ($($fields:pat),* $(,)?) => {
        &$crate::nst::Pattern::ProductRow($crate::prod!($($fields,)+))
    };
}

#[macro_export]
macro_rules! npat_sum {
    ($field:pat) => {
        &$crate::nst::Pattern::SumRow($crate::sum!($field))
    };
}

#[macro_export]
macro_rules! npat_var {
    ($var:pat) => {
        &$crate::nst::Pattern::Whole($crate::span_of!($var))
    };
}

#[macro_export]
macro_rules! nterm_local {
    ($var:pat, $value:pat, $expr:pat) => {
        &$crate::nst::Term::Binding {
            var: $crate::span_of!($var),
            value: $value,
            expr: $expr,
            ..
        }
    };
    ($var:pat, $type_:pat, $value:pat, $expr:pat) => {
        &$crate::nst::Term::Binding {
            var: $crate::span_of!($var),
            annotation: Some($crate::cst::TypeAnnotation { type_: $type_, .. }),
            value: $value,
            expr: $expr,
            ..
        }
    };
}

#[macro_export]
macro_rules! nterm_with {
    ($handler:pat, $expr:pat) => {
        &$crate::nst::Term::Handle {
            handler: $handler,
            expr: $expr,
            ..
        }
    };
}

#[macro_export]
macro_rules! nterm_abs {
    ($arg:pat, $body:pat) => {
        &$crate::nst::Term::Abstraction {
            arg: $crate::span_of!($arg),
            body: $body,
            ..
        }
    };
    ($arg:pat, $type_:pat, $body:pat) => {
        &$crate::nst::Term::Abstraction {
            arg: $crate::span_of!($arg),
            annotation: Some($crate::cst::TypeAnnotation { type_: $type_, .. }),
            body: $body,
            ..
        }
    };
}

#[macro_export]
macro_rules! nterm_app {
    ($func:pat, $arg:pat) => {
        &$crate::nst::Term::Application {
            func: $func,
            arg: $arg,
            ..
        }
    };
}

#[macro_export]
macro_rules! nterm_prod {
    ($($fields:pat),* $(,)?) => {
        &$crate::nst::Term::ProductRow($crate::prod!($($fields,)*))
    };
    () => {
        &$crate::nst::Term::ProductRow($crate::prod!())
    };
}

#[macro_export]
macro_rules! nterm_sum {
    ($field:pat) => {
        &$crate::nst::Term::SumRow($crate::sum!($field))
    };
}

#[macro_export]
macro_rules! nterm_dot {
    ($base:pat, $field:pat) => {
        &$crate::nst::Term::FieldAccess {
            base: $base,
            field: $crate::span_of!($field),
            ..
        }
    };
}

#[macro_export]
macro_rules! nterm_match {
    ($($cases:pat),+ $(,)?) => {
        &$crate::nst::Term::Match { cases: $crate::separated!($($cases),+), .. }
    };
}

#[macro_export]
macro_rules! nterm_toplvl {
    ($mod_:pat, $item:pat) => {
        &$crate::nst::Term::TopLevelRef($crate::span_of!(($mod_, $item)))
    };
}

#[macro_export]
macro_rules! nterm_item {
    ($mod_:pat, $item:pat) => {
        &$crate::nst::Term::ItemRef($crate::span_of!(($mod_, $item)))
    };
}

#[macro_export]
macro_rules! nterm_var {
    ($var:pat) => {
        &$crate::nst::Term::VariableRef($crate::span_of!($var))
    };
}

#[macro_export]
macro_rules! nterm_paren {
    ($term:pat) => {
        &$crate::nst::Term::Parenthesized { term: $term, .. }
    };
}

#[macro_export]
macro_rules! nitem_effect {
    ($name:pat, $($ops:pat),* $(,)?) => {
        $crate::nst::Item::Effect {
            name: $crate::span_of!($name),
            ops: &[$($ops),*],
            ..
        }
    };
}

#[macro_export]
macro_rules! nitem_term {
    ($name:pat, $value:pat) => {
        $crate::nst::Item::Term {
            name: $crate::span_of!($name),
            value: $value,
            ..
        }
    };
    ($name:pat, $type_:pat, $value:pat) => {
        $crate::nst::Item::Term {
            name: $crate::span_of!($name),
            annotation: Some($crate::cst::SchemeAnnotation { type_: $type_, .. }),
            value: $value,
            ..
        }
    };
}
