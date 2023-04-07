use std::ops::Index;

use la_arena::{Arena, Idx};

// We re-export these so it's easier to differntiate reference and indexed during migration
pub use crate::cst::indexed::{
    Constraint, EffectOp, ProductRow, Qualifiers, Row, RowAtom, Scheme, SchemeAnnotation,
    Separated, SumRow, Type, TypeAnnotation, TypeRow,
};
use crate::cst::Field;
use crate::id::{EffectId, EffectOpId, ItemId, ModuleId, TyVarId, VarId};
use crate::ident::Ident;
use crate::indexed::{HasArenaMut, HasArenaRef, IdxAlloc};
use crate::span::{Span, SpanOf, Spanned};

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct NstIndxAlloc {
    types: Arena<Type<TyVarId>>,
    terms: Arena<Term>,
    pats: Arena<Pattern>,
}
impl IdxAlloc<Term> for NstIndxAlloc {
    fn alloc(&mut self, value: Term) -> Idx<Term> {
        self.terms.alloc(value)
    }
}
impl IdxAlloc<Type<TyVarId>> for NstIndxAlloc {
    fn alloc(&mut self, value: Type<TyVarId>) -> Idx<Type<TyVarId>> {
        self.types.alloc(value)
    }
}
impl IdxAlloc<Pattern> for NstIndxAlloc {
    fn alloc(&mut self, value: Pattern) -> Idx<Pattern> {
        self.pats.alloc(value)
    }
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
impl<T> Index<Idx<T>> for NstIndxAlloc
where
    Self: HasArenaRef<T>,
{
    type Output = T;

    fn index(&self, index: Idx<T>) -> &Self::Output {
        &self.arena()[index]
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
            Term::SumRow(s) => s.span(),
            Term::FieldAccess { base, field, .. } => Span::join(&self.with_term(*base), field),
            Term::Match { match_, rangle, .. } => Span::join(match_, rangle),
            Term::EffectOpRef(o) => o.span(),
            Term::ItemRef(i) => i.span(),
            Term::VariableRef(v) => v.span(),
            Term::Parenthesized { lpar, rpar, .. } => Span::join(lpar, rpar),
        }
    }
}

/// A top-level item in an Aiahr source file with names resolved.
#[derive(Clone, Debug, PartialEq, Hash)]
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
impl Eq for Item {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AllocItem {
    pub alloc: NstIndxAlloc,
    pub item: Item,
}

//pub use indexed::{Term, Pattern, Item, NstIndxAlloc, AllocItem  };
/*pub use indexed::{
        Constraint, EffectOp, ProductRow, Qualifiers, Row, RowAtom, Scheme, SchemeAnnotation,
        Separated, SumRow, Type, TypeAnnotation, TypeRow,
};*/
