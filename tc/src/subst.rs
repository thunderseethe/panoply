use std::fmt::Debug;
use std::ops::Index;

use aiahr_core::ast::Term;
use aiahr_core::id::{Id, IdGen};
use bumpalo::Bump;

use crate::constraint::Constraint;
use crate::ty::{TcUnifierVar, TcVar, Type, UnifierTypeId};
use crate::{TypedVarId, UnifierType, UnifierVarId};

/// Acts as a dense map from TcUnifierVar to T.
/// Abstracts over the two kinds of substitutions we have. UnifierSubst which may not have types
/// for all unifiers, and GeneralizedSubst which must have types for all unifiers.
#[derive(PartialEq, Eq, Clone)]
pub struct SubstInternal<T> {
    dense_map: Vec<T>,
}
impl<T> Default for SubstInternal<T> {
    fn default() -> Self {
        SubstInternal { dense_map: vec![] }
    }
}
impl<T: Debug> Debug for SubstInternal<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map()
            .entries(
                self.dense_map
                    .iter()
                    .enumerate()
                    .map(|(raw, ty)| (TcUnifierVar::from_raw(raw), ty)),
            )
            .finish()
    }
}
impl<T> Index<TcUnifierVar> for SubstInternal<T> {
    type Output = T;

    fn index(&self, index: TcUnifierVar) -> &Self::Output {
        &self.dense_map[index.0]
    }
}

/// An internal substituion, MAY have types for all unifiers
pub type UnifierSubst<'ty> = SubstInternal<Option<&'ty UnifierType<'ty>>>;

/// A completed substitution, MUST have types for all unifiers.
/// This is the substitution returned from `solve_constraints`.
pub type GeneralizedSubst<'ty> = SubstInternal<&'ty UnifierType<'ty>>;

pub trait InsertType<'ty> {
    /// Insert a type into a `substitution` (or anything).
    fn insert(&mut self, id: TcUnifierVar, ty: &'ty UnifierType<'ty>);
}
impl<'ty> InsertType<'ty> for UnifierSubst<'ty> {
    fn insert(&mut self, id: TcUnifierVar, ty: &'ty UnifierType<'ty>) {
        let indx = id.0;
        if indx >= self.dense_map.len() {
            self.dense_map
                .extend(std::iter::repeat(None).take(indx - self.dense_map.len() + 1))
        }
        self.dense_map[indx].replace(ty);
    }
}
impl<'ty> InsertType<'ty> for GeneralizedSubst<'ty> {
    // For generalized we just panic if you try to set a unifier that doesn't exist yet
    fn insert(&mut self, id: TcUnifierVar, ty: &'ty UnifierType<'ty>) {
        self.dense_map[id.0] = ty;
    }
}

impl<'ty> UnifierSubst<'ty> {
    pub(crate) fn fresh_unifier(&mut self) -> TcUnifierVar {
        let raw = self.dense_map.len();
        self.dense_map.push(None);
        TcUnifierVar::from_raw(raw)
    }

    pub(crate) fn fresh_unifier_with(&mut self, val: &'ty UnifierType<'ty>) -> TcUnifierVar {
        let raw = self.dense_map.len();
        self.dense_map.push(Some(val));
        TcUnifierVar::from_raw(raw)
    }

    pub fn get(&self, unifier_var: TcUnifierVar) -> Option<&'ty UnifierType<'ty>> {
        self.dense_map.get(unifier_var.0).and_then(|ty| ty.clone())
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = (TcUnifierVar, &'ty UnifierType<'ty>)> + 'a {
        self.raw_iter()
            .filter_map(|(id, opt_ty)| opt_ty.map(|ty| (id, ty)))
    }
    pub fn raw_iter<'a>(
        &'a self,
    ) -> impl Iterator<Item = (TcUnifierVar, Option<&'ty UnifierType<'ty>>)> + 'a {
        self.dense_map
            .iter()
            .enumerate()
            .map(|(i, opt)| (TcUnifierVar::from_raw(i), opt.clone()))
    }

    pub fn raw_iter_mut<'a>(
        &'a mut self,
    ) -> impl Iterator<Item = (TcUnifierVar, &mut Option<&'ty UnifierType<'ty>>)> {
        self.dense_map
            .iter_mut()
            .enumerate()
            .map(|(id, opt_ty)| (TcUnifierVar::from_raw(id), opt_ty))
    }
}

impl<T> FromIterator<T> for SubstInternal<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self {
            dense_map: iter.into_iter().collect(),
        }
    }
}
impl<'ty> FromIterator<&'ty UnifierType<'ty>> for UnifierSubst<'ty> {
    fn from_iter<T: IntoIterator<Item = &'ty UnifierType<'ty>>>(iter: T) -> Self {
        Self::from_iter(iter.into_iter().map(Some))
    }
}

/// A trait for applying a substitution to a thing. Often this will be a type, but it could also be an Ast or Constraint.
pub trait Substitutable<'ty, Subst> {
    /// A type to hold allocators apply needs to create any modified nodes
    type Alloc;
    /// The output type after our substitution is applied
    type Output;

    /// Produce a new Output where all type vars that appear in self have been replaced by their
    /// corresponding values in subst.
    fn apply(&self, alloc: Self::Alloc, subst: &Subst) -> Self::Output;
}

/// Replace all occurrences of type variables in self by their corresponding type in subst.
impl<'ty> Substitutable<'ty, UnifierSubst<'ty>> for UnifierType<'ty> {
    type Alloc = &'ty Bump;
    type Output = &'ty UnifierType<'ty>;

    fn apply(&self, ty_arena: Self::Alloc, subst: &UnifierSubst<'ty>) -> Self::Output {
        match self {
            Type::IntTy | Type::ErrorTy => ty_arena.alloc(*self),
            Type::VarTy(UnifierTypeId::Unifier(uni)) => {
                subst.get(*uni).unwrap_or_else(|| ty_arena.alloc(*self))
            }
            Type::VarTy(_) => ty_arena.alloc(*self),
            Type::FunTy(arg, ret) => ty_arena.alloc(Type::FunTy(
                arg.apply(ty_arena, subst),
                ret.apply(ty_arena, subst),
            )),
        }
    }
}
impl<'ty> Substitutable<'ty, IdGen<TcVar, &'ty UnifierType<'ty>>> for UnifierType<'ty> {
    type Alloc = &'ty Bump;
    type Output = &'ty UnifierType<'ty>;

    fn apply(
        &self,
        ty_arena: Self::Alloc,
        subst: &IdGen<TcVar, &'ty UnifierType<'ty>>,
    ) -> Self::Output {
        match self {
            Type::IntTy | Type::ErrorTy => ty_arena.alloc(*self),
            Type::VarTy(UnifierTypeId::Var(var)) => subst
                .get(*var)
                .map(|ty| *ty)
                .unwrap_or_else(|| ty_arena.alloc(Type::VarTy(UnifierTypeId::Var(*var)))),
            Type::VarTy(var) => ty_arena.alloc(Type::VarTy(*var)),
            Type::FunTy(arg, ret) => ty_arena.alloc(Type::FunTy(
                arg.apply(ty_arena, subst),
                ret.apply(ty_arena, subst),
            )),
        }
    }
}

impl<'ty> Substitutable<'ty, IdGen<TcVar, &'ty UnifierType<'ty>>> for Type<'ty, TcVar> {
    type Alloc = &'ty Bump;
    type Output = &'ty UnifierType<'ty>;

    fn apply(
        &self,
        ty_arena: Self::Alloc,
        subst: &IdGen<TcVar, &'ty UnifierType<'ty>>,
    ) -> Self::Output {
        self.and_then(ty_arena, |tv| {
            subst
                .get(*tv)
                .map(|ty| *ty)
                .unwrap_or_else(|| ty_arena.alloc(Type::VarTy(UnifierTypeId::Var(*tv))))
        })
    }
}

impl<'ty> Substitutable<'ty, IdGen<TcVar, &'ty UnifierType<'ty>>> for Constraint<'ty> {
    type Alloc = &'ty Bump;
    type Output = Constraint<'ty>;

    fn apply(
        &self,
        alloc: Self::Alloc,
        subst: &IdGen<TcVar, &'ty UnifierType<'ty>>,
    ) -> Self::Output {
        match self {
            Constraint::Eq(left, right) => {
                Constraint::Eq(left.apply(alloc, subst), right.apply(alloc, subst))
            }
        }
    }
}

/// Replace all occurrences of type variables in each type in the ast by their corresponding type
/// in subst.
impl<'ast: 'ty, 'ty> Substitutable<'ty, GeneralizedSubst<'ty>>
    for &'ast Term<'ast, UnifierVarId<'ty>>
{
    type Alloc = (&'ast Bump, &'ty Bump);
    type Output = &'ast Term<'ast, TypedVarId<'ty, TcVar>>;

    fn apply(
        &self,
        alloc @ (ast_arena, ty_arena): Self::Alloc,
        subst: &GeneralizedSubst<'ty>,
    ) -> Self::Output {
        // TODO: optimize this to reuse existing allocation if we don't apply a substitution to a
        // term. At the moment this reallocs the entire term which is inefficient
        match self {
            Term::Abstraction {
                arg: TypedVarId { var, ty },
                body,
            } => ast_arena.alloc(Term::Abstraction {
                arg: TypedVarId {
                    var: *var,
                    ty: ty.zonk(ty_arena, subst),
                },
                body: body.apply(alloc, subst),
            }),
            Term::Application { func, arg } => ast_arena.alloc(Term::Application {
                func: func.apply(alloc, subst),
                arg: arg.apply(alloc, subst),
            }),
            Term::Variable(TypedVarId { var, ty }) => ast_arena.alloc(Term::Variable(TypedVarId {
                var: *var,
                ty: ty.zonk(ty_arena, subst),
            })),
            Term::Item(item) => ast_arena.alloc(Term::Item(*item)),
        }
    }
}
