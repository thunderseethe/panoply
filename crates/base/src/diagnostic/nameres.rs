//! This module defines errors from the name resolution pass.

use bitflags::bitflags;
use std::{array, iter::Flatten, ops::Range, option};

use crate::{
  loc::Loc,
  span::{Span, SpanOf, Spanned},
};

use super::{Citation, Diagnostic, english::EnglishIterExt};

/// A kind of name.
#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum NameKind {
  Module = 0b1,
  Effect = 0b10,
  EffectOp = 0b100,
  Item = 0b1000,
  TyVar = 0b10000,
  Var = 0b100000,
  Type = 0b1000000,
}

impl NameKind {
  /// The English name for a name kind, with an indefinite article (i.e., "a" or "an").
  pub fn indefinite_noun(&self) -> &'static str {
    match self {
      NameKind::Module => "a module",
      NameKind::Effect => "an effect",
      NameKind::EffectOp => "an effect operation",
      NameKind::Item => "a top-level item",
      NameKind::TyVar => "a type variable",
      NameKind::Var => "a local variable",
      NameKind::Type => "a top-level type",
    }
  }
}

bitflags! {
    /// A bitset of name kinds.
    #[derive(Copy, Clone, Debug)]
    pub struct NameKinds: u8 {
        const MODULE = NameKind::Module as u8;
        const EFFECT = NameKind::Effect as u8;
        const EFFECT_OP = NameKind::EffectOp as u8;
        const ITEM = NameKind::Item as u8;
        const TY_VAR = NameKind::TyVar as u8;
        const VAR = NameKind::Var as u8;
        const TYPE = NameKind::Type as u8;
    }
}

impl NameKinds {
  fn get(&self, kind: NameKind) -> Option<NameKind> {
    self.contains(NameKinds::from(kind)).then_some(kind)
  }

  pub fn enum_iter(&self) -> Flatten<array::IntoIter<option::IntoIter<NameKind>, 4>> {
    [
      self.get(NameKind::Item).into_iter(),
      self.get(NameKind::Module).into_iter(),
      self.get(NameKind::TyVar).into_iter(),
      self.get(NameKind::Var).into_iter(),
    ]
    .into_iter()
    .flatten()
  }
}

impl From<NameKind> for NameKinds {
  fn from(kind: NameKind) -> Self {
    match kind {
      NameKind::Module => NameKinds::MODULE,
      NameKind::Effect => NameKinds::EFFECT,
      NameKind::EffectOp => NameKinds::EFFECT_OP,
      NameKind::Item => NameKinds::ITEM,
      NameKind::TyVar => NameKinds::TY_VAR,
      NameKind::Var => NameKinds::VAR,
      NameKind::Type => NameKinds::TYPE,
    }
  }
}

/// A reason that a reference wasn't resolved to a candidate name.
#[derive(Clone, Copy, Debug)]
pub enum RejectionReason {
  /// The reference context and candidate name have incompatible kinds.
  WrongKind {
    /// The kind of the candidate name.
    actual: NameKind,
    /// The kinds that would be valid at the reference site.
    expected: NameKinds,
  },
}

impl RejectionReason {
  // An explanation for why the candidate was rejected, suitable for use as a dependent clause.
  pub fn dependent_clause(&self) -> String {
    match self {
      RejectionReason::WrongKind { actual, expected } => {
        format!(
          "it refers to {}, while the context above requires {}",
          actual.indefinite_noun(),
          expected
            .enum_iter()
            .map(|kind| kind.indefinite_noun())
            .english_list("or")
        )
      }
    }
  }
}

/// A suggestion for what name the user might have intended to refer to.
#[derive(Clone, Debug)]
pub struct Suggestion {
  /// The suggested name.
  pub name: SpanOf<String>,
  /// Why the name wasn't matched.
  pub why_not: RejectionReason,
}

/// A name resolution error.
#[derive(Clone, Debug)]
pub enum NameResolutionError {
  /// A duplicate name in the same layer, where the new name is not allowed to shadow the old one.
  Duplicate {
    /// The duplicated name.
    name: String,
    /// The kind of both names.
    kind: NameKind,
    /// The original definition site.
    original: Range<u32>,
    /// The duplicate definition site.
    duplicate: Range<u32>,
  },
  /// A reference to a name that isn't defined.
  NotFound {
    /// The name that isn't defined.
    name: String,
    /// The span of the occureence of the name.
    span: Range<u32>,
    /// The module that was expected to contain the given name as a member.
    context_module: Option<String>,
    /// Possible names that the user could have intended.
    suggestions: Vec<Suggestion>,
  },
  /// A compound name has the wrong kind for the context in which it is used.
  WrongKind {
    /// The location of the compound name.
    expr: Range<u32>,
    /// The kind of the compound name.
    actual: NameKind,
    /// The kinds that would be valid at the usage site.
    expected: NameKinds,
  },
}

fn fix_up(range: &Range<u32>) -> Span {
  Span {
    start: Loc {
      byte: range.start as usize,
    },
    end: Loc {
      byte: range.end as usize,
    },
  }
}

impl Diagnostic for NameResolutionError {
  fn name(&self) -> &'static str {
    match self {
      NameResolutionError::Duplicate { .. } => "name-resolution-duplicate-definition",
      NameResolutionError::NotFound { .. } => "name-resolution-name-not-found",
      NameResolutionError::WrongKind { .. } => "name-resolution-wrong-kind",
    }
  }

  fn principal(&self) -> Citation {
    match self {
      NameResolutionError::Duplicate {
        name, duplicate, ..
      } => Citation {
        span: fix_up(duplicate),
        message: format!("Duplicate definition of symbol '{}'", name),
      },
      NameResolutionError::NotFound {
        name,
        span,
        context_module,
        ..
      } => Citation {
        span: fix_up(span),
        message: match context_module {
          Some(m) => format!("Symbol '{}' not found in module {}", name, m),
          None => format!("Symbol '{}' not found", name),
        },
      },
      NameResolutionError::WrongKind {
        expr,
        actual,
        expected,
      } => Citation {
        span: fix_up(expr),
        message: format!(
          "Expression refers to {}, while it is used as if it was {}",
          actual.indefinite_noun(),
          expected
            .enum_iter()
            .map(|kind| kind.indefinite_noun())
            .english_list("or")
        ),
      },
    }
  }

  fn additional(&self) -> Vec<Citation> {
    match self {
      NameResolutionError::Duplicate { original, .. } => vec![Citation {
        span: fix_up(original),
        message: "Original definition here".to_owned(),
      }],
      NameResolutionError::NotFound { suggestions, .. } => suggestions
        .iter()
        .map(|sugg| Citation {
          span: sugg.name.span(),
          message: format!(
            "Did you mean '{}'? (rejected because {})",
            &sugg.name.value,
            sugg.why_not.dependent_clause()
          ),
        })
        .collect(),
      _ => Vec::new(),
    }
  }
}
