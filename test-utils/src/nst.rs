use aiahr_core::cst::Field;
use bumpalo::Bump;
use la_arena::{Arena, Idx};

use crate::cst::{EffectOp, ProductRow, SchemeAnnotation, Separated, SumRow, TypeAnnotation};
use aiahr_core::id::{EffectId, EffectOpId, ItemId, ModuleId, TyVarId, VarId};
use aiahr_core::ident::Ident;
use aiahr_core::indexed::{HasArenaRef, HasRefArena, ReferenceAllocate};
use aiahr_core::nst::{self, NstIndxAlloc};
use aiahr_core::span::{Span, SpanOf, Spanned};

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

pub struct NstRefAlloc<'a, 'b> {
    /// Allocate the new reference based tree types.
    arena: &'a Bump,
    /// Included to expand indices encountered during conversion
    indices: &'b NstIndxAlloc,
}

impl<'a, 'b> NstRefAlloc<'a, 'b> {
    pub fn new(arena: &'a Bump, indices: &'b NstIndxAlloc) -> Self {
        Self { arena, indices }
    }
}
impl<'a> HasRefArena<'a> for NstRefAlloc<'a, '_> {
    fn ref_arena(&self) -> &'a Bump {
        self.arena
    }
}
impl<T> HasArenaRef<T> for NstRefAlloc<'_, '_>
where
    NstIndxAlloc: HasArenaRef<T>,
{
    fn arena(&self) -> &Arena<T> {
        self.indices.arena()
    }
}

impl<'a> ReferenceAllocate<'a, NstRefAlloc<'a, '_>> for EffectOpId {
    type Out = EffectOpId;

    fn ref_alloc(&self, _: &mut NstRefAlloc<'a, '_>) -> Self::Out {
        *self
    }
}

impl<'a> ReferenceAllocate<'a, NstRefAlloc<'a, '_>> for TyVarId {
    type Out = TyVarId;

    fn ref_alloc(&self, _: &mut NstRefAlloc<'a, '_>) -> Self::Out {
        *self
    }
}

impl<'a, 'b, T> ReferenceAllocate<'a, NstRefAlloc<'a, 'b>> for nst::Separated<T>
where
    T: ReferenceAllocate<'a, NstRefAlloc<'a, 'b>>,
{
    type Out = Separated<'a, T::Out>;

    fn ref_alloc(&self, alloc: &mut NstRefAlloc<'a, 'b>) -> Self::Out {
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

use crate::cst::Row;

impl<'a, 'b, V, C> ReferenceAllocate<'a, NstRefAlloc<'a, 'b>> for nst::Row<V, C>
where
    V: ReferenceAllocate<'a, NstRefAlloc<'a, 'b>>,
    C: ReferenceAllocate<'a, NstRefAlloc<'a, 'b>>,
{
    type Out = Row<'a, V::Out, C::Out>;

    fn ref_alloc(&self, alloc: &mut NstRefAlloc<'a, 'b>) -> Self::Out {
        match self {
            nst::Row::Concrete(concrete) => Row::Concrete(concrete.ref_alloc(alloc)),
            nst::Row::Variable(vars) => Row::Variable(vars.ref_alloc(alloc)),
            nst::Row::Mixed {
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

impl<'a, 'b, T> ReferenceAllocate<'a, NstRefAlloc<'a, 'b>> for nst::ProductRow<T>
where
    T: ReferenceAllocate<'a, NstRefAlloc<'a, 'b>>,
{
    type Out = ProductRow<'a, T::Out>;

    fn ref_alloc(&self, alloc: &mut NstRefAlloc<'a, 'b>) -> Self::Out {
        ProductRow {
            lbrace: self.lbrace,
            fields: self.fields.ref_alloc(alloc),
            rbrace: self.rbrace,
        }
    }
}

impl<'a, 'b, T: ReferenceAllocate<'a, NstRefAlloc<'a, 'b>>>
    ReferenceAllocate<'a, NstRefAlloc<'a, 'b>> for nst::SumRow<T>
{
    type Out = SumRow<T::Out>;

    fn ref_alloc(&self, alloc: &mut NstRefAlloc<'a, 'b>) -> Self::Out {
        SumRow {
            langle: self.langle,
            field: self.field.ref_alloc(alloc),
            rangle: self.rangle,
        }
    }
}

impl<'a> ReferenceAllocate<'a, NstRefAlloc<'a, '_>> for Idx<nst::Pattern> {
    type Out = &'a Pattern<'a>;

    fn ref_alloc(&self, alloc: &mut NstRefAlloc<'a, '_>) -> Self::Out {
        let pat = match alloc.arena()[*self].clone() {
            nst::Pattern::ProductRow(prod) => Pattern::ProductRow(prod.ref_alloc(alloc)),
            nst::Pattern::SumRow(sum) => Pattern::SumRow(sum.ref_alloc(alloc)),
            nst::Pattern::Whole(var) => Pattern::Whole(var),
        };
        alloc.ref_arena().alloc(pat) as &_
    }
}

impl<'a> ReferenceAllocate<'a, NstRefAlloc<'a, '_>> for Idx<nst::Term> {
    type Out = &'a Term<'a>;

    fn ref_alloc(&self, alloc: &mut NstRefAlloc<'a, '_>) -> Self::Out {
        let term = match alloc.arena()[*self].clone() {
            nst::Term::Binding {
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
            nst::Term::Handle {
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
            nst::Term::Abstraction {
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
            nst::Term::Application {
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
            nst::Term::ProductRow(prod) => Term::ProductRow(prod.ref_alloc(alloc)),
            nst::Term::SumRow(sum) => Term::SumRow(sum.ref_alloc(alloc)),
            nst::Term::FieldAccess { base, dot, field } => Term::FieldAccess {
                base: base.ref_alloc(alloc),
                dot,
                field: field.ref_alloc(alloc),
            },
            nst::Term::Match {
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
            nst::Term::EffectOpRef(ids) => Term::EffectOpRef(ids),
            nst::Term::ItemRef(ids) => Term::ItemRef(ids),
            nst::Term::VariableRef(var) => Term::VariableRef(var),
            nst::Term::Parenthesized { lpar, term, rpar } => Term::Parenthesized {
                lpar,
                term: term.ref_alloc(alloc),
                rpar,
            },
        };
        alloc.ref_arena().alloc(term) as &_
    }
}

use crate::cst::Type;

impl<'a> ReferenceAllocate<'a, NstRefAlloc<'a, '_>> for Idx<nst::Type<TyVarId>> {
    type Out = &'a Type<'a, TyVarId>;

    fn ref_alloc(&self, alloc: &mut NstRefAlloc<'a, '_>) -> Self::Out {
        let type_ = match alloc.arena()[*self].clone() {
            nst::Type::Named(var) => Type::Named(var.ref_alloc(alloc)),
            nst::Type::Sum {
                langle,
                variants,
                rangle,
            } => Type::Sum {
                langle,
                variants: variants.ref_alloc(alloc),
                rangle,
            },
            nst::Type::Product {
                lbrace,
                fields,
                rbrace,
            } => Type::Product {
                lbrace,
                fields: fields.ref_alloc(alloc),
                rbrace,
            },
            nst::Type::Function {
                domain,
                arrow,
                codomain,
            } => Type::Function {
                domain: domain.ref_alloc(alloc),
                arrow,
                codomain: codomain.ref_alloc(alloc),
            },
            nst::Type::Parenthesized { lpar, type_, rpar } => Type::Parenthesized {
                lpar,
                type_: type_.ref_alloc(alloc),
                rpar,
            },
        };
        alloc.ref_arena().alloc(type_) as &_
    }
}

impl<'a> ReferenceAllocate<'a, NstRefAlloc<'a, '_>> for nst::EffectOp<EffectOpId, TyVarId> {
    type Out = EffectOp<'a, EffectOpId, TyVarId>;

    fn ref_alloc(&self, alloc: &mut NstRefAlloc<'a, '_>) -> Self::Out {
        EffectOp {
            name: self.name.ref_alloc(alloc),
            colon: self.colon,
            type_: self.type_.ref_alloc(alloc),
        }
    }
}

impl<'a> ReferenceAllocate<'a, NstRefAlloc<'a, '_>> for nst::RowAtom<TyVarId> {
    type Out = crate::cst::RowAtom<'a, TyVarId>;

    fn ref_alloc(&self, alloc: &mut NstRefAlloc<'a, '_>) -> Self::Out {
        match self {
            nst::RowAtom::Concrete { lpar, fields, rpar } => crate::cst::RowAtom::Concrete {
                lpar: *lpar,
                fields: fields.ref_alloc(alloc),
                rpar: *rpar,
            },
            nst::RowAtom::Variable(var) => crate::cst::RowAtom::Variable(var.ref_alloc(alloc)),
        }
    }
}

impl<'a> ReferenceAllocate<'a, NstRefAlloc<'a, '_>> for nst::Constraint<TyVarId> {
    type Out = crate::cst::Constraint<'a, TyVarId>;

    fn ref_alloc(&self, alloc: &mut NstRefAlloc<'a, '_>) -> Self::Out {
        match self {
            nst::Constraint::RowSum {
                lhs,
                plus,
                rhs,
                eq,
                goal,
            } => crate::cst::Constraint::RowSum {
                lhs: lhs.ref_alloc(alloc),
                plus: *plus,
                rhs: rhs.ref_alloc(alloc),
                eq: *eq,
                goal: goal.ref_alloc(alloc),
            },
        }
    }
}

impl<'a> ReferenceAllocate<'a, NstRefAlloc<'a, '_>> for aiahr_core::cst::Qualifiers<TyVarId> {
    type Out = crate::cst::Qualifiers<'a, TyVarId>;

    fn ref_alloc(&self, alloc: &mut NstRefAlloc<'a, '_>) -> Self::Out {
        crate::cst::Qualifiers {
            constraints: self.constraints.ref_alloc(alloc),
            arrow: self.arrow,
        }
    }
}

impl<'a> ReferenceAllocate<'a, NstRefAlloc<'a, '_>> for nst::Scheme<TyVarId> {
    type Out = &'a crate::cst::Scheme<'a, TyVarId>;

    fn ref_alloc(&self, alloc: &mut NstRefAlloc<'a, '_>) -> Self::Out {
        let scheme = crate::cst::Scheme {
            quantifiers: alloc
                .ref_arena()
                .alloc_slice_fill_iter(self.quantifiers.iter().map(|quant| quant.ref_alloc(alloc))),
            qualifiers: self.qualifiers.as_ref().map(|qual| qual.ref_alloc(alloc)),
            type_: self.type_.ref_alloc(alloc),
        };
        alloc.ref_arena().alloc(scheme)
    }
}

impl<'a> ReferenceAllocate<'a, NstRefAlloc<'a, '_>> for nst::Item {
    type Out = Item<'a>;

    fn ref_alloc(&self, alloc: &mut NstRefAlloc<'a, '_>) -> Self::Out {
        match self {
            nst::Item::Effect {
                effect,
                name,
                lbrace,
                ops,
                rbrace,
            } => Item::Effect {
                effect: *effect,
                name: *name,
                lbrace: *lbrace,
                ops: alloc
                    .ref_arena()
                    .alloc_slice_fill_iter(ops.iter().map(|op| op.ref_alloc(alloc))),
                rbrace: *rbrace,
            },
            nst::Item::Term {
                name,
                annotation,
                eq,
                value,
            } => Item::Term {
                name: *name,
                annotation: annotation.ref_alloc(alloc),
                eq: *eq,
                value: value.ref_alloc(alloc),
            },
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
        &$crate::nst::Pattern::Whole(aiahr_core::span_of!($var))
    };
}

#[macro_export]
macro_rules! nterm_local {
    ($var:pat, $value:pat, $expr:pat) => {
        &$crate::nst::Term::Binding {
            var: aiahr_core::span_of!($var),
            value: $value,
            expr: $expr,
            ..
        }
    };
    ($var:pat, $type_:pat, $value:pat, $expr:pat) => {
        &$crate::nst::Term::Binding {
            var: aiahr_core::span_of!($var),
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
            arg: aiahr_core::span_of!($arg),
            body: $body,
            ..
        }
    };
    ($arg:pat, $type_:pat, $body:pat) => {
        &$crate::nst::Term::Abstraction {
            arg: aiahr_core::span_of!($arg),
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
            field: aiahr_core::span_of!($field),
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
        &$crate::nst::Term::TopLevelRef(aiahr_core::span_of!(($mod_, $item)))
    };
}

#[macro_export]
macro_rules! nterm_item {
    ($mod_:pat, $item:pat) => {
        &$crate::nst::Term::ItemRef(aiahr_core::span_of!(($mod_, $item)))
    };
}

#[macro_export]
macro_rules! nterm_var {
    ($var:pat) => {
        &$crate::nst::Term::VariableRef(aiahr_core::span_of!($var))
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
            name: aiahr_core::span_of!($name),
            ops: &[$($ops),*],
            ..
        }
    };
}

#[macro_export]
macro_rules! nitem_term {
    ($name:pat, $value:pat) => {
        $crate::nst::Item::Term {
            name: aiahr_core::span_of!($name),
            value: $value,
            ..
        }
    };
    ($name:pat, $type_:pat, $value:pat) => {
        $crate::nst::Item::Term {
            name: aiahr_core::span_of!($name),
            annotation: Some($crate::cst::SchemeAnnotation { type_: $type_, .. }),
            value: $value,
            ..
        }
    };
}
