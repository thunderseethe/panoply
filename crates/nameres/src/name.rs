//! This module defines various sorts of name types for use in different contexts.
//!
//! Generally, if one type logically represents a subset of the possible objects as another, then
//! there will be a `From` implementation on the larger type from the smaller one.

use ::base::{
  diagnostic::nameres::NameKind,
  id::{EffectName, EffectOpName, TermName, TyVarId, VarId},
  modules::Module,
};

/// Names whose kind can be determined.
pub trait NameKinded {
  fn kind(&self) -> NameKind;
}

/// A name that may appear as a top-level definition within a module.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ModuleName {
  Effect(EffectName),
  Item(TermName),
}

impl From<ModuleName> for BaseName {
  fn from(value: ModuleName) -> Self {
    match value {
      ModuleName::Effect(e) => BaseName::Effect(e),
      ModuleName::Item(t) => BaseName::Item(t),
    }
  }
}

impl From<EffectName> for ModuleName {
  fn from(e: EffectName) -> Self {
    ModuleName::Effect(e)
  }
}

impl From<TermName> for ModuleName {
  fn from(i: TermName) -> Self {
    ModuleName::Item(i)
  }
}

impl NameKinded for ModuleName {
  fn kind(&self) -> NameKind {
    match self {
      ModuleName::Effect(_) => NameKind::Effect,
      ModuleName::Item(_) => NameKind::Item,
    }
  }
}

/// A name visible at any scope in a module.
#[derive(Clone, Copy, Debug)]
pub enum BaseName {
  Module(Module),
  Effect(EffectName),
  EffectOp(EffectOpName),
  Item(TermName),
}

impl From<Module> for BaseName {
  fn from(m: Module) -> Self {
    BaseName::Module(m)
  }
}

impl From<EffectName> for BaseName {
  fn from(eff_name: EffectName) -> Self {
    BaseName::Effect(eff_name)
  }
}

impl From<EffectOpName> for BaseName {
  fn from(op_name: EffectOpName) -> Self {
    BaseName::EffectOp(op_name)
  }
}

impl From<TermName> for BaseName {
  fn from(term: TermName) -> Self {
    BaseName::Item(term)
  }
}

impl NameKinded for BaseName {
  fn kind(&self) -> NameKind {
    match self {
      BaseName::Module(_) => NameKind::Module,
      BaseName::Effect(_) => NameKind::Effect,
      BaseName::EffectOp(_) => NameKind::EffectOp,
      BaseName::Item(_) => NameKind::Item,
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

/// Every kind of name.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Name {
  Module(Module),
  Effect(EffectName),
  EffectOp(EffectOpName),
  Item(TermName),
  TyVar(TyVarId),
  Var(VarId),
}

impl From<Module> for Name {
  fn from(m: Module) -> Self {
    Name::Module(m)
  }
}

impl From<EffectName> for Name {
  fn from(eff_name: EffectName) -> Self {
    Name::Effect(eff_name)
  }
}

impl From<EffectOpName> for Name {
  fn from(eff_op_name: EffectOpName) -> Self {
    Name::EffectOp(eff_op_name)
  }
}

impl From<TermName> for Name {
  fn from(term: TermName) -> Self {
    Name::Item(term)
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
      BaseName::Effect(e) => Name::Effect(e),
      BaseName::EffectOp(o) => Name::EffectOp(o),
      BaseName::Item(t) => Name::Item(t),
    }
  }
}

impl NameKinded for Name {
  fn kind(&self) -> NameKind {
    match self {
      Name::Module(_) => NameKind::Module,
      Name::Effect(_) => NameKind::Effect,
      Name::EffectOp(_) => NameKind::EffectOp,
      Name::Item(_) => NameKind::Item,
      Name::TyVar(_) => NameKind::TyVar,
      Name::Var(_) => NameKind::Var,
    }
  }
}
