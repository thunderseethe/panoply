//! This module defines various sorts of name types for use in different contexts.
//!
//! Generally, if one type logically represents a subset of the possible objects as another, then
//! there will be a `From` implementation on the larger type from the smaller one.

use aiahr_core::{
    diagnostic::nameres::NameKind,
    id::{ItemId, ModuleId, TyVarId, VarId},
};

/// Names whose kind can be determined.
pub trait NameKinded {
    fn kind(&self) -> NameKind;
}

/// A name that may appear as a top-level definition within a module.
#[derive(Clone, Copy, Debug)]
pub enum ModuleName {
    Item(ItemId),
}

impl ModuleName {
    /// Converts the name to one scoped to the given module.
    pub fn based_in(&self, m: ModuleId) -> BaseName {
        match self {
            ModuleName::Item(i) => BaseName::Item(m, *i),
        }
    }
}

impl From<ItemId> for ModuleName {
    fn from(i: ItemId) -> Self {
        ModuleName::Item(i)
    }
}

impl NameKinded for ModuleName {
    fn kind(&self) -> NameKind {
        match self {
            ModuleName::Item(_) => NameKind::Item,
        }
    }
}

/// A name visible at any scope in a module.
#[derive(Clone, Copy, Debug)]
pub enum BaseName {
    Module(ModuleId),
    Item(ModuleId, ItemId),
}

impl From<ModuleId> for BaseName {
    fn from(m: ModuleId) -> Self {
        BaseName::Module(m)
    }
}

impl From<(ModuleId, ItemId)> for BaseName {
    fn from((m, i): (ModuleId, ItemId)) -> Self {
        BaseName::Item(m, i)
    }
}

impl NameKinded for BaseName {
    fn kind(&self) -> NameKind {
        match self {
            BaseName::Module(_) => NameKind::Module,
            BaseName::Item(_, _) => NameKind::Item,
        }
    }
}

/// A name only visible in a local scope.
#[derive(Clone, Copy, Debug)]
pub enum LocalName {
    TyVar(TyVarId),
    Var(VarId),
}

impl From<TyVarId> for LocalName {
    fn from(t: TyVarId) -> Self {
        LocalName::TyVar(t)
    }
}

impl From<VarId> for LocalName {
    fn from(v: VarId) -> Self {
        LocalName::Var(v)
    }
}

impl NameKinded for LocalName {
    fn kind(&self) -> NameKind {
        match self {
            LocalName::TyVar(_) => NameKind::TyVar,
            LocalName::Var(_) => NameKind::Var,
        }
    }
}

/// Any kind of name in Aiahr.
#[derive(Clone, Copy, Debug)]
pub enum Name {
    Module(ModuleId),
    Item(ModuleId, ItemId),
    TyVar(TyVarId),
    Var(VarId),
}

impl From<ModuleId> for Name {
    fn from(m: ModuleId) -> Self {
        Name::Module(m)
    }
}

impl From<(ModuleId, ItemId)> for Name {
    fn from((m, i): (ModuleId, ItemId)) -> Self {
        Name::Item(m, i)
    }
}

impl From<TyVarId> for Name {
    fn from(t: TyVarId) -> Self {
        Name::TyVar(t)
    }
}

impl From<VarId> for Name {
    fn from(v: VarId) -> Self {
        Name::Var(v)
    }
}

impl From<BaseName> for Name {
    fn from(base: BaseName) -> Self {
        match base {
            BaseName::Module(m) => Name::Module(m),
            BaseName::Item(m, i) => Name::Item(m, i),
        }
    }
}

impl NameKinded for Name {
    fn kind(&self) -> NameKind {
        match self {
            Name::Module(_) => NameKind::Module,
            Name::Item(_, _) => NameKind::Item,
            Name::TyVar(_) => NameKind::TyVar,
            Name::Var(_) => NameKind::Var,
        }
    }
}
