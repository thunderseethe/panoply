use la_arena::{Arena, Idx};
use std::ops::Index;

// We re-export these so it's easier to differntiate reference and indexed during migration
use super::Field;
pub use super::{
    Constraint, EffectOp, ProductRow, Qualifiers, Row, RowAtom, Scheme, SchemeAnnotation,
    Separated, SumRow, Type, TypeAnnotation, TypeRow,
};
use base::{
    id::{EffectName, EffectOpName, Ids, TermName, TyVarId, VarId},
    ident::Ident,
    indexed::{HasArenaMut, HasArenaRef, IdxAlloc, IdxView},
    span::{Span, SpanOf, Spanned},
};

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct NstIndxAlloc {
    types: Arena<Type<TyVarId>>,
    terms: Arena<Term>,
    pats: Arena<Pattern>,
}
impl HasArenaRef<Type<TyVarId>> for NstIndxAlloc {
    fn arena(&self) -> &Arena<Type<TyVarId>> {
        &self.types
    }
}
impl HasArenaMut<Type<TyVarId>> for NstIndxAlloc {
    fn arena_mut(&mut self) -> &mut Arena<Type<TyVarId>> {
        &mut self.types
    }
}
impl HasArenaRef<Term> for NstIndxAlloc {
    fn arena(&self) -> &Arena<Term> {
        &self.terms
    }
}
impl HasArenaMut<Term> for NstIndxAlloc {
    fn arena_mut(&mut self) -> &mut Arena<Term> {
        &mut self.terms
    }
}
impl HasArenaRef<Pattern> for NstIndxAlloc {
    fn arena(&self) -> &Arena<Pattern> {
        &self.pats
    }
}
impl HasArenaMut<Pattern> for NstIndxAlloc {
    fn arena_mut(&mut self) -> &mut Arena<Pattern> {
        &mut self.pats
    }
}
impl<T> IdxAlloc<T> for NstIndxAlloc
where
    Self: HasArenaMut<T>,
{
    fn alloc(&mut self, value: T) -> Idx<T> {
        self.arena_mut().alloc(value)
    }
}
impl<T> IdxView<T> for NstIndxAlloc
where
    Self: HasArenaRef<T>,
{
    fn view(&self, idx: Idx<T>) -> &T {
        &self.arena()[idx]
    }
}
impl<T> Index<Idx<T>> for NstIndxAlloc
where
    Self: IdxView<T>,
{
    type Output = T;

    fn index(&self, index: Idx<T>) -> &Self::Output {
        self.view(index)
    }
}

/// A pattern with names resolved.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Pattern {
    ProductRow(ProductRow<Idx<Self>>),
    SumRow(SumRow<Idx<Self>>),
    Whole(SpanOf<VarId>),
}
impl Spanned for Pattern {
    fn span(&self) -> Span {
        match self {
            Pattern::ProductRow(prod) => prod.span(),
            Pattern::SumRow(sum) => sum.span(),
            Pattern::Whole(var) => var.span(),
        }
    }
}

/// An Aiahr term with names resolved.
#[derive(Clone, Debug, PartialEq, Eq)]
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
    Concat {
        left: Idx<Self>,
        concat: Span,
        right: Idx<Self>,
    },
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
    EffectOpRef(SpanOf<EffectOpName>),
    ItemRef(SpanOf<TermName>),
    VariableRef(SpanOf<VarId>),
    Parenthesized {
        lpar: Span,
        term: Idx<Self>,
        rpar: Span,
    },
    Int(SpanOf<usize>),
}
impl Term {
    pub fn spanned<'a>(&'a self, arenas: &'a NstIndxAlloc) -> SpanTerm<'a> {
        SpanTerm { term: self, arenas }
    }
}
pub struct SpanTerm<'a> {
    term: &'a Term,
    arenas: &'a NstIndxAlloc,
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
            Term::FieldAccess { base, field, .. } => Span::join(&self.with_term(*base), field),
            Term::Match { match_, rangle, .. } => Span::join(match_, rangle),
            Term::EffectOpRef(o) => o.span(),
            Term::ItemRef(i) => i.span(),
            Term::VariableRef(v) => v.span(),
            Term::Parenthesized { lpar, rpar, .. } => Span::join(lpar, rpar),
            Term::Int(span_of_int) => span_of_int.span(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct EffectDefn {
    pub effect: Span,
    pub name: SpanOf<EffectName>,
    pub lbrace: Span,
    pub ops: Vec<Option<EffectOp<EffectOpName, TyVarId>>>,
    pub rbrace: Span,
}
impl Spanned for EffectDefn {
    fn span(&self) -> Span {
        Span::join(&self.effect, &self.rbrace)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct TermDefn {
    pub name: SpanOf<TermName>,
    pub annotation: Option<SchemeAnnotation<TyVarId>>,
    pub eq: Span,
    pub value: Idx<Term>,
}
impl TermDefn {
    pub fn spanned(&self, arenas: &NstIndxAlloc) -> Span {
        Span::join(&self.name.span(), &arenas[self.value].spanned(arenas))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LocalIds {
    pub ty_vars: Box<Ids<TyVarId, SpanOf<Ident>>>,
    pub vars: Box<Ids<VarId, SpanOf<Ident>>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AllocItem<T> {
    pub alloc: NstIndxAlloc,
    pub local_ids: LocalIds,
    pub item: T,
}

pub mod traverse {
    use std::ops::ControlFlow;

    use base::{
        id::{EffectOpName, TermName, TyVarId, VarId},
        ident::Ident,
        indexed::IdxView,
        span::SpanOf,
    };
    use la_arena::Idx;

    use crate::{IdField, Row, Type};

    use super::{Pattern, Term};

    /// A depth-first traversal of our NST. Expectation is this traversal will be used to search
    /// the tree for a particular value, so each traversal method returns a `ControlFlow` to allow
    /// breaking early with the found value.
    ///
    /// We also support a suite of `should_traverse` method that will skip irrelevant nodes. These
    /// are split out as their own methods so they can be override without users having to override
    /// the default `traverse_*` method just to return early.
    ///
    /// By default each traverse method will continue the search until we've touched every tree
    /// node. This can lead to suboptimal default behavior when a traversal knows it won't find
    /// what it's looking for but does not break with a value.
    pub trait DfsTraverseNst: IdxView<Term> + IdxView<Type<TyVarId>> + IdxView<Pattern> {
        type Out;

        fn traverse_int(&self, _: &SpanOf<usize>) -> ControlFlow<Self::Out> {
            ControlFlow::Continue(())
        }

        fn traverse_var(&self, _: &SpanOf<VarId>) -> ControlFlow<Self::Out> {
            ControlFlow::Continue(())
        }
        fn traverse_ty_var(&self, _: &SpanOf<TyVarId>) -> ControlFlow<Self::Out> {
            ControlFlow::Continue(())
        }
        fn traverse_row_label(&self, _: &SpanOf<Ident>) -> ControlFlow<Self::Out> {
            ControlFlow::Continue(())
        }
        fn traverse_effect_op(&self, _: &SpanOf<EffectOpName>) -> ControlFlow<Self::Out> {
            ControlFlow::Continue(())
        }
        fn traverse_term_name(&self, _: &SpanOf<TermName>) -> ControlFlow<Self::Out> {
            ControlFlow::Continue(())
        }

        fn should_traverse_row(&self, _: &Row<TyVarId, IdField<Idx<Type<TyVarId>>>>) -> bool {
            true
        }
        fn traverse_row(
            &self,
            row: &Row<TyVarId, IdField<Idx<Type<TyVarId>>>>,
        ) -> ControlFlow<Self::Out> {
            if !self.should_traverse_row(row) {
                return ControlFlow::Continue(());
            }
            match row {
                Row::Concrete(closed) => {
                    for field in closed.elements() {
                        self.traverse_row_label(&field.label)?;
                        self.traverse_type(field.target)?;
                    }
                    ControlFlow::Continue(())
                }
                Row::Variable(vars) => {
                    for ty_var in vars {
                        self.traverse_ty_var(ty_var)?;
                    }
                    ControlFlow::Continue(())
                }
                Row::Mixed {
                    concrete,
                    variables,
                    ..
                } => {
                    for field in concrete.elements() {
                        self.traverse_row_label(&field.label)?;
                        self.traverse_type(field.target)?;
                    }
                    for ty_var in variables {
                        self.traverse_ty_var(ty_var)?;
                    }
                    ControlFlow::Continue(())
                }
            }
        }

        fn should_traverse_type(&self, _: &Type<TyVarId>) -> bool {
            true
        }
        fn traverse_type(&self, idx: Idx<Type<TyVarId>>) -> ControlFlow<Self::Out> {
            let ty = self.view(idx);
            if !self.should_traverse_type(ty) {
                return ControlFlow::Continue(());
            }
            match ty {
                Type::Named(ty_var) => self.traverse_ty_var(ty_var),
                Type::Sum { variants, .. } => self.traverse_row(variants),
                Type::Product { fields, .. } => match fields {
                    Some(fields) => self.traverse_row(fields),
                    None => ControlFlow::Continue(()),
                },
                Type::Function {
                    domain, codomain, ..
                } => {
                    self.traverse_type(*domain)?;
                    self.traverse_type(*codomain)
                }
                Type::Parenthesized { type_, .. } => self.traverse_type(*type_),
            }
        }

        fn should_traverse_pat(&self, _: &Pattern) -> bool {
            true
        }
        fn traverse_pat(&self, idx: Idx<Pattern>) -> ControlFlow<Self::Out> {
            let pat = self.view(idx);
            if !self.should_traverse_pat(pat) {
                return ControlFlow::Continue(());
            }
            match pat {
                Pattern::ProductRow(prod) => {
                    if let Some(fields) = &prod.fields {
                        for field in fields.elements() {
                            self.traverse_row_label(&field.label)?;
                            self.traverse_pat(field.target)?;
                        }
                    }
                    ControlFlow::Continue(())
                }
                Pattern::SumRow(sum) => {
                    self.traverse_row_label(&sum.field.label)?;
                    self.traverse_pat(sum.field.target)
                }
                Pattern::Whole(var) => self.traverse_var(var),
            }
        }

        fn should_traverse_term(&self, _: &Term) -> bool {
            true
        }
        fn traverse_term(&self, idx: Idx<Term>) -> ControlFlow<Self::Out> {
            let term = self.view(idx);
            if !self.should_traverse_term(term) {
                return ControlFlow::Continue(());
            }
            match term {
                Term::Binding {
                    var,
                    annotation,
                    value,
                    expr,
                    ..
                } => {
                    self.traverse_var(var)?;
                    if let Some(ann) = annotation {
                        self.traverse_type(ann.type_)?;
                    }
                    self.traverse_term(*value)?;
                    self.traverse_term(*expr)
                }
                Term::Handle { handler, expr, .. } => {
                    self.traverse_term(*handler)?;
                    self.traverse_term(*expr)
                }
                Term::Abstraction {
                    arg,
                    annotation,
                    body,
                    ..
                } => {
                    self.traverse_var(arg)?;
                    if let Some(ann) = annotation {
                        self.traverse_type(ann.type_)?;
                    }
                    self.traverse_term(*body)
                }
                Term::Application { func, arg, .. } => {
                    self.traverse_term(*func)?;
                    self.traverse_term(*arg)
                }
                Term::ProductRow(prod) => {
                    if let Some(fields) = &prod.fields {
                        for field in fields.elements() {
                            self.traverse_row_label(&field.label)?;
                            self.traverse_term(field.target)?;
                        }
                    }
                    ControlFlow::Continue(())
                }
                Term::Concat { left, right, .. } => {
                    self.traverse_term(*left)?;
                    self.traverse_term(*right)
                }
                Term::SumRow(sum) => {
                    self.traverse_row_label(&sum.field.label)?;
                    self.traverse_term(sum.field.target)
                }
                Term::FieldAccess { base, field, .. } => {
                    self.traverse_term(*base)?;
                    self.traverse_row_label(field)
                }
                Term::Match { cases, .. } => {
                    for case in cases.elements() {
                        self.traverse_pat(case.label)?;
                        self.traverse_term(case.target)?;
                    }
                    ControlFlow::Continue(())
                }
                Term::EffectOpRef(eff_op) => self.traverse_effect_op(eff_op),
                Term::ItemRef(term) => self.traverse_term_name(term),
                Term::VariableRef(var) => self.traverse_var(var),
                Term::Parenthesized { term, .. } => self.traverse_term(*term),
                Term::Int(int) => self.traverse_int(int),
            }
        }
    }
}
