use std::fmt::Debug;

use aiahr_core::{
    id::{EffectId, IdGen, Ids, ModuleId, TyVarId, VarId},
    memory::handle::RefHandle,
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
struct Gens<'s> {
    ty_vars: IdGen<TyVarId, SpanOf<RefHandle<'s, str>>>,
    vars: IdGen<VarId, SpanOf<RefHandle<'s, str>>>,
}

impl<'s> Gens<'s> {
    fn into_ids(self) -> LocalIds<'s> {
        LocalIds {
            ty_vars: self.ty_vars.into_boxed_ids(),
            vars: self.vars.into_boxed_ids(),
        }
    }
}

impl<'s> IdOps<'s, TyVarId> for Gens<'s> {
    fn get(&self, id: TyVarId) -> SpanOf<RefHandle<'s, str>> {
        self.ty_vars[id]
    }
}

impl<'s> IdOps<'s, VarId> for Gens<'s> {
    fn get(&self, id: VarId) -> SpanOf<RefHandle<'s, str>> {
        self.vars[id]
    }
}

impl<'s> GensOps<'s, TyVarId> for Gens<'s> {
    fn push(&mut self, name: SpanOf<RefHandle<'s, str>>) -> TyVarId {
        self.ty_vars.push(name)
    }
}

impl<'s> GensOps<'s, VarId> for Gens<'s> {
    fn push(&mut self, name: SpanOf<RefHandle<'s, str>>) -> VarId {
        self.vars.push(name)
    }
}

#[derive(Debug, Default)]
struct Matches {
    ty_var: Option<TyVarId>,
    var: Option<VarId>,
}

impl Matches {
    fn iter<'a>(&'a self) -> impl 'a + Iterator<Item = Name> {
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

#[derive(Debug)]
pub struct LocalIds<'s> {
    pub ty_vars: Box<Ids<TyVarId, SpanOf<RefHandle<'s, str>>>>,
    pub vars: Box<Ids<VarId, SpanOf<RefHandle<'s, str>>>>,
}

/// The names visible from a given context in a module. Supports shadowing.
#[derive(Debug)]
pub struct Names<'b, 'a, 's> {
    base: &'b BaseNames<'b, 'a, 's>,
    gens: Gens<'s>,
    names: FxHashMap<RefHandle<'s, str>, Vec<Matches>>,
    scopes: Vec<FxHashSet<RefHandle<'s, str>>>,
}

impl<'b, 'a, 's> Names<'b, 'a, 's> {
    /// Constructs a new `Names` with the given base.
    pub fn new(base: &'b BaseNames<'b, 'a, 's>) -> Names<'b, 'a, 's> {
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

    /// Executes the given function on a subscope of the current object.
    pub fn subscope<R, F>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Names<'b, 'a, 's>) -> R,
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
    pub fn get_effect(&self, module: ModuleId, effect: EffectId) -> &EffectNames<'s> {
        self.base.get_effect(module, effect)
    }

    fn insert<I>(&mut self, name: SpanOf<RefHandle<'s, str>>) -> InsertResult<I>
    where
        I: Copy,
        Gens<'s>: GensOps<'s, I>,
        Matches: MatchesOps<I>,
    {
        let id = self.gens.push(name);
        let scope = self.scopes.last_mut().unwrap();
        if scope.contains(&name.value) {
            let ms = self.names.get_mut(&name.value).unwrap().last_mut().unwrap();
            if let Some(old) = ms.get_mut() {
                InsertResult::err(id, self.gens.get(old.clone()).span().of(*old))
            } else {
                *ms.get_mut() = Some(id);
                InsertResult::ok(id)
            }
        } else {
            scope.insert(name.value);
            self.names
                .entry(name.value)
                .or_insert(Vec::new())
                .push(Matches::new(id));
            InsertResult::ok(id)
        }
    }

    /// Inserts a variable into the current scope, if it doesn't already exist.
    ///
    /// If the variable is a duplicate in the current scope, returns an error
    /// with the original name. Otherwise, returns the ID of the new variable.
    pub fn insert_ty_var(&mut self, name: SpanOf<RefHandle<'s, str>>) -> InsertResult<TyVarId> {
        self.insert(name)
    }

    /// Inserts a variable into the current scope, if it doesn't already exist.
    ///
    /// If the variable is a duplicate in the current scope, returns an error
    /// with the original name. Otherwise, returns the ID of the new variable.
    pub fn insert_var(&mut self, name: SpanOf<RefHandle<'s, str>>) -> InsertResult<VarId> {
        self.insert(name)
    }

    /// Finds the correct ID associated with the given string.
    pub fn find<'c>(&'c self, name: RefHandle<'s, str>) -> impl 'c + Iterator<Item = SpanOf<Name>> {
        self.names
            .get(&name)
            .into_iter()
            .flat_map(|stack| stack.iter().rev().flat_map(Matches::iter))
            .map(|n| self.get(n).span().of(n))
            .chain(self.base.find(name).map(|sn| sn.map(Name::from)))
    }

    /// Finds the correct ID associated with the given string in the given module.
    pub fn find_in<'c>(
        &'c self,
        module: ModuleId,
        name: RefHandle<'s, str>,
    ) -> impl 'c + Iterator<Item = SpanOf<BaseName>> {
        self.base.find_in(module, name)
    }

    /// Converts into variable IDs.
    pub fn into_local_ids(self) -> LocalIds<'s> {
        self.gens.into_ids()
    }
}

impl<'b, 'a, 's, I> IdOps<'s, I> for Names<'b, 'a, 's>
where
    Name: From<I>,
{
    fn get(&self, id: I) -> SpanOf<RefHandle<'s, str>> {
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
