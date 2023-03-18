use la_arena::Arena;

use crate::cst::indexed::Type;
use crate::id::{TyVarId, VarId};
use crate::ident::Ident;

/// Holds references to arenas we need to convert from cst types to these types.
#[derive(Default)]
pub struct IndexedAllocator {
    cst_types: Arena<Type<Ident>>,
    nst_types: Arena<Type<TyVarId>>,
    cst_terms: Arena<crate::cst::indexed::Term>,
    nst_terms: Arena<crate::nst::indexed::Term>,
    ast_terms: Arena<crate::ast::indexed::Term<VarId>>,
    cst_pats: Arena<crate::cst::indexed::Pattern>,
    nst_pats: Arena<crate::nst::indexed::Pattern>,
}

pub trait HasArena<T> {
    fn arena(&mut self) -> &mut Arena<T>;
}

impl HasArena<Type<Ident>> for IndexedAllocator {
    fn arena(&mut self) -> &mut Arena<Type<Ident>> {
        &mut self.cst_types
    }
}

impl HasArena<Type<TyVarId>> for IndexedAllocator {
    fn arena(&mut self) -> &mut Arena<Type<TyVarId>> {
        &mut self.nst_types
    }
}

impl HasArena<crate::cst::indexed::Term> for IndexedAllocator {
    fn arena(&mut self) -> &mut Arena<crate::cst::indexed::Term> {
        &mut self.cst_terms
    }
}

impl HasArena<crate::nst::indexed::Term> for IndexedAllocator {
    fn arena(&mut self) -> &mut Arena<crate::nst::indexed::Term> {
        &mut self.nst_terms
    }
}

impl HasArena<crate::ast::indexed::Term<VarId>> for IndexedAllocator {
    fn arena(&mut self) -> &mut Arena<crate::ast::indexed::Term<VarId>> {
        &mut self.ast_terms
    }
}

impl HasArena<crate::cst::indexed::Pattern> for IndexedAllocator {
    fn arena(&mut self) -> &mut Arena<crate::cst::indexed::Pattern> {
        &mut self.cst_pats
    }
}

impl HasArena<crate::nst::indexed::Pattern> for IndexedAllocator {
    fn arena(&mut self) -> &mut Arena<crate::nst::indexed::Pattern> {
        &mut self.nst_pats
    }
}

pub trait IndexedAllocate {
    type Out;

    fn alloc<'db>(&self, alloc: &mut IndexedAllocator) -> Self::Out;
}

impl<T: IndexedAllocate> IndexedAllocate for &T {
    type Out = T::Out;

    fn alloc<'db>(&self, alloc: &mut IndexedAllocator) -> Self::Out {
        T::alloc(*self, alloc)
    }
}
