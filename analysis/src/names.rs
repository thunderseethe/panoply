use std::fmt::Debug;

use aiahr_core::{
    id::{EffectId, IdGen, Ids, ModuleId, TyVarId, VarId},
    ident::Ident,
    span::{SpanOf, Spanned},
};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    base::BaseNames,
    effect::EffectNames,
    name::{BaseName, Name},
    ops::{GensOps, IdOps, InsertResult, MatchesOps},
};

#[derive(Debug)]
struct Gens {
    ty_vars: IdGen<TyVarId, SpanOf<Ident>>,
    vars: IdGen<VarId, SpanOf<Ident>>,
}

impl Gens {
    fn into_ids(self) -> LocalIds {
        LocalIds {
            ty_vars: self.ty_vars.into_boxed_ids(),
            vars: self.vars.into_boxed_ids(),
        }
    }
}

impl IdOps<TyVarId> for Gens {
    fn get(&self, id: TyVarId) -> SpanOf<Ident> {
        self.ty_vars[id]
    }
}

impl IdOps<VarId> for Gens {
    fn get(&self, id: VarId) -> SpanOf<Ident> {
        self.vars[id]
    }
}

impl GensOps<TyVarId> for Gens {
    fn push(&mut self, name: SpanOf<Ident>) -> TyVarId {
        self.ty_vars.push(name)
    }
}

impl GensOps<VarId> for Gens {
    fn push(&mut self, name: SpanOf<Ident>) -> VarId {
        self.vars.push(name)
    }
}

#[derive(Debug, Default)]
struct Matches {
    ty_var: Option<TyVarId>,
    var: Option<VarId>,
}

impl Matches {
    fn iter(&self) -> impl '_ + Iterator<Item = Name> {
        self.ty_var
            .into_iter()
            .map(Name::TyVar)
            .chain(self.var.into_iter().map(Name::Var))
    }
}

impl MatchesOps<TyVarId> for Matches {
    fn new(id: TyVarId) -> Self {
        Matches {
            ty_var: Some(id),
            ..Default::default()
        }
    }

    fn get_mut(&mut self) -> &mut Option<TyVarId> {
        &mut self.ty_var
    }
}

impl MatchesOps<VarId> for Matches {
    fn new(id: VarId) -> Self {
        Matches {
            var: Some(id),
            ..Default::default()
        }
    }

    fn get_mut(&mut self) -> &mut Option<VarId> {
        &mut self.var
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct LocalIds {
    pub ty_vars: Box<Ids<TyVarId, SpanOf<Ident>>>,
    pub vars: Box<Ids<VarId, SpanOf<Ident>>>,
}

/// The names visible from a given context in a module. Supports shadowing.
#[derive(Debug)]
pub struct Names<'b, 'a> {
    base: &'b BaseNames<'b, 'a>,
    gens: Gens,
    names: FxHashMap<Ident, Vec<Matches>>,
    scopes: Vec<FxHashSet<Ident>>,
}

impl<'b, 'a> Names<'b, 'a> {
    /// Constructs a new `Names` with the given base.
    pub fn new(base: &'b BaseNames<'b, 'a>) -> Names<'b, 'a> {
        Names {
            base,
            gens: Gens {
                ty_vars: IdGen::new(),
                vars: IdGen::new(),
            },
            names: FxHashMap::default(),
            scopes: vec![FxHashSet::default()],
        }
    }

    pub fn into_ids(self) -> (IdGen<VarId, SpanOf<Ident>>, IdGen<TyVarId, SpanOf<Ident>>) {
        (self.gens.vars, self.gens.ty_vars)
    }

    /// Executes the given function on a subscope of the current object.
    pub fn subscope<R, F>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Names<'b, 'a>) -> R,
    {
        self.scopes.push(FxHashSet::default());
        let ret = f(self);
        self.scopes.pop().unwrap().into_iter().for_each(|name| {
            // TODO: Are there meaningful performance improvements if we clean
            // up empty vectors?
            self.names.get_mut(&name).unwrap().pop().unwrap();
        });
        ret
    }

    /// Gets the effect corresponding to the given ID.
    pub fn get_effect(&self, module: ModuleId, effect: EffectId) -> &EffectNames {
        self.base.get_effect(module, effect)
    }

    fn insert<I>(&mut self, name: SpanOf<Ident>) -> InsertResult<I>
    where
        I: Copy,
        Gens: GensOps<I>,
        Matches: MatchesOps<I>,
    {
        let id = self.gens.push(name);
        let scope = self.scopes.last_mut().unwrap();
        if scope.contains(&name.value) {
            let ms = self.names.get_mut(&name.value).unwrap().last_mut().unwrap();
            if let Some(old) = ms.get_mut() {
                InsertResult::err(id, self.gens.get(*old).span().of(*old))
            } else {
                *ms.get_mut() = Some(id);
                InsertResult::ok(id)
            }
        } else {
            scope.insert(name.value);
            self.names
                .entry(name.value)
                .or_default()
                .push(Matches::new(id));
            InsertResult::ok(id)
        }
    }

    /// Inserts a variable into the current scope, if it doesn't already exist.
    ///
    /// If the variable is a duplicate in the current scope, returns an error
    /// with the original name. Otherwise, returns the ID of the new variable.
    pub fn insert_ty_var(&mut self, name: SpanOf<Ident>) -> InsertResult<TyVarId> {
        self.insert(name)
    }

    /// Inserts a variable into the current scope, if it doesn't already exist.
    ///
    /// If the variable is a duplicate in the current scope, returns an error
    /// with the original name. Otherwise, returns the ID of the new variable.
    pub fn insert_var(&mut self, name: SpanOf<Ident>) -> InsertResult<VarId> {
        self.insert(name)
    }

    /// Finds the correct ID associated with the given string.
    pub fn find(&self, name: Ident) -> impl '_ + Iterator<Item = SpanOf<Name>> {
        self.names
            .get(&name)
            .into_iter()
            .flat_map(|stack| stack.iter().rev().flat_map(Matches::iter))
            .map(|n| self.get(n).span().of(n))
            .chain(self.base.find(name).map(|sn| sn.map(Name::from)))
    }

    /// Finds the correct ID associated with the given string in the given module.
    pub fn find_in(
        &self,
        module: ModuleId,
        name: Ident,
    ) -> impl '_ + Iterator<Item = SpanOf<BaseName>> {
        self.base.find_in(module, name)
    }

    /// Converts into variable IDs.
    pub fn into_local_ids(self) -> LocalIds {
        self.gens.into_ids()
    }
}

impl<'b, 'a, I> IdOps<I> for Names<'b, 'a>
where
    Name: From<I>,
{
    fn get(&self, id: I) -> SpanOf<Ident> {
        match Name::from(id) {
            Name::Module(m) => self.base.get(m),
            Name::Effect(m, e) => self.base.get((m, e)),
            Name::EffectOp(m, e, o) => self.base.get((m, e, o)),
            Name::Item(m, i) => self.base.get((m, i)),
            Name::TyVar(t) => self.gens.ty_vars[t],
            Name::Var(v) => self.gens.vars[v],
        }
    }
}
