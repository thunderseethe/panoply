use reducir::{ReducIr, ReducIrKind, ReducIrLocal, ReducIrTermName, ReducIrVar};
use rustc_hash::FxHashMap;

/// Occurrence information about a binder
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Occurrence {
  /// The binder does not occur at all.
  Dead,
  /// The binder occurs exactly once, and that occurrence is not inside a lambda. Inlining is always safe.
  Once,
  /// The binder occurs exactly once, but inside a lambda. Inlining will not duplicate code, but it might duplicate work.
  OnceInAbs,
  /// The binder occurs at most once in each of several distinct case branches, and none of these occurrences is inside a lambda.
  ManyBranch,
  /// The binder may occur many times, including inside lambdas.
  Many,
}
impl Occurrence {
  fn in_abs(self) -> Self {
    match self {
      Occurrence::Once => Occurrence::OnceInAbs,
      occ => occ,
    }
  }
}

#[derive(Default)]
pub(crate) struct Occurrences {
  binders: FxHashMap<ReducIrLocal, Occurrence>,
  items: FxHashMap<ReducIrTermName, Occurrence>,
}
impl Occurrences {
  fn with_binder(var: ReducIrVar) -> Self {
    let mut binders = FxHashMap::default();
    binders.insert(var.var, Occurrence::Once);
    Self {
      binders,
      ..Default::default()
    }
  }

  fn with_item(name: ReducIrTermName) -> Self {
    let mut items = FxHashMap::default();
    items.insert(name, Occurrence::Once);
    Self {
      items,
      ..Default::default()
    }
  }

  fn mark_in_abs(self, bound_vars: &[ReducIrVar]) -> Self {
    let mut binders = self
      .binders
      .into_iter()
      .map(|(bind, occ)| (bind, occ.in_abs()))
      .collect::<FxHashMap<_, _>>();

    for var in bound_vars {
      binders
        .entry(var.var)
        .and_modify(|occ| {
          // Bounds vars aren't actually OnceInAbs since they aren't free in the abs.
          if let Occurrence::OnceInAbs = occ {
            *occ = Occurrence::Once;
          }
        })
        // Any bound var that doesn't have occurrence info yet is dead.
        .or_insert(Occurrence::Dead);
    }

    Self {
      binders,
      items: self
        .items
        .into_iter()
        .map(|(name, occ)| (name, occ.in_abs()))
        .collect(),
    }
  }

  fn merge_internal(mut self, other: Self, marker: Occurrence) -> Self {
    for (bind, occ) in other.binders.into_iter() {
      self
        .binders
        .entry(bind)
        .and_modify(|occ| {
          if *occ < marker {
            *occ = marker;
          }
        })
        .or_insert(occ);
    }
    for (name, occ) in other.items.into_iter() {
      self
        .items
        .entry(name)
        .and_modify(|occ| {
          if *occ < marker {
            *occ = marker;
          }
        })
        .or_insert(occ);
    }
    self
  }

  fn merge_branch(self, other: Self) -> Self {
    self.merge_internal(other, Occurrence::ManyBranch)
  }

  fn merge(self, other: Self) -> Self {
    self.merge_internal(other, Occurrence::Many)
  }
}

impl std::ops::Index<&ReducIrLocal> for Occurrences {
  type Output = Occurrence;

  fn index(&self, index: &ReducIrLocal) -> &Self::Output {
    self.binders.get(index).unwrap_or(&Occurrence::Dead)
  }
}
impl std::ops::Index<ReducIrVar> for Occurrences {
  type Output = Occurrence;

  fn index(&self, index: ReducIrVar) -> &Self::Output {
    self.binders.get(&index.var).unwrap_or(&Occurrence::Dead)
  }
}
impl std::ops::Index<&ReducIrTermName> for Occurrences {
  type Output = Occurrence;

  fn index(&self, index: &ReducIrTermName) -> &Self::Output {
    self.items.get(index).unwrap_or(&Occurrence::Dead)
  }
}

pub(crate) fn occurence_analysis(ir: &ReducIr) -> Occurrences {
  match ir.kind() {
    ReducIrKind::Int(_) => Occurrences::default(),
    ReducIrKind::Var(var) => Occurrences::with_binder(*var),
    ReducIrKind::Item(name, _) => Occurrences::with_item(*name),
    ReducIrKind::Abs(vars, body) => occurence_analysis(body).mark_in_abs(vars),
    ReducIrKind::App(head, spine) => spine
      .iter()
      .map(occurence_analysis)
      .fold(occurence_analysis(head), |a, b| a.merge(b)),
    ReducIrKind::Locals(binds, body) => binds
      .iter()
      .map(|bind| &bind.defn)
      .map(occurence_analysis)
      .fold(occurence_analysis(body), |a, b| a.merge(b)),
    ReducIrKind::Struct(elems) => elems
      .iter()
      .map(occurence_analysis)
      .reduce(Occurrences::merge)
      .unwrap_or_else(Occurrences::default),
    ReducIrKind::Case(_, discr, branches) => branches
      .iter()
      .map(occurence_analysis)
      .reduce(Occurrences::merge_branch)
      .unwrap_or_else(Occurrences::default)
      .merge(occurence_analysis(discr)),
    ReducIrKind::TyAbs(_, body)
    | ReducIrKind::TyApp(body, _)
    | ReducIrKind::FieldProj(_, body)
    | ReducIrKind::Tag(_, _, body) => occurence_analysis(body),
    ReducIrKind::X(_) => unreachable!(),
  }
}

#[cfg(test)]
mod tests {
  use base::id::{IdSupply, TermName};
  use reducir::ty::ReducIrTy;
  use reducir::{ReducIr, ReducIrLocal, ReducIrVar};
  use salsa::AsId;

  use crate::occurrence::Occurrence;

  use super::occurence_analysis;

  fn var_supply() -> impl FnMut() -> ReducIrVar {
    let mut var_supply = IdSupply::default();
    move || ReducIrVar {
      var: ReducIrLocal {
        top_level: reducir::ReducIrTermName::Term(TermName::from_id(salsa::Id::from_u32(0))),
        id: var_supply.supply_id(),
      },
      ty: ReducIrTy::from_id(salsa::Id::from_u32(0)),
    }
  }

  #[test]
  fn test_var_occurs_once() {
    let mut gen_var = var_supply();
    let var = gen_var();
    let occs = occurence_analysis(&ReducIr::var(var));
    assert_eq!(occs[var], Occurrence::Once);
  }

  #[test]
  fn test_free_var_in_abs_occurs_once_in_abs() {
    let mut gen_var = var_supply();

    let var = gen_var();
    let unused = gen_var();
    let occs = occurence_analysis(&ReducIr::abss([unused], ReducIr::var(var)));
    assert_eq!(occs[var], Occurrence::OnceInAbs);
  }

  #[test]
  fn test_bound_var_occurs_once_in_body_is_once() {
    let mut gen_var = var_supply();

    let var = gen_var();
    let occs = occurence_analysis(&ReducIr::abss([var], ReducIr::var(var)));
    assert_eq!(occs[var], Occurrence::Once);
  }

  #[test]
  fn test_unused_var_is_dead_occurs() {
    let mut gen_var = var_supply();

    let var = gen_var();
    let unused = gen_var();
    let occs = occurence_analysis(&ReducIr::abss([var, unused], ReducIr::var(var)));
    assert_eq!(occs[unused], Occurrence::Dead);
  }

  #[test]
  fn test_var_occurs_multiple_times() {
    let mut gen_var = var_supply();

    let var = gen_var();
    let occs = occurence_analysis(&ReducIr::app(ReducIr::var(var), [ReducIr::var(var)]));
    assert_eq!(occs[var], Occurrence::Many);
  }

  #[test]
  fn test_var_occurs_only_in_case_is_many_branch() {
    let mut gen_var = var_supply();

    let var = gen_var();
    let discr = gen_var();
    let occs = occurence_analysis(&ReducIr::case(
      ReducIrTy::from_id(salsa::Id::from_u32(0)),
      ReducIr::var(discr),
      [ReducIr::var(var), ReducIr::var(var)],
    ));
    assert_eq!(occs[var], Occurrence::ManyBranch);
  }

  #[test]
  fn test_var_in_discr_and_branch_is_many() {
    let mut gen_var = var_supply();

    let var = gen_var();
    let occs = occurence_analysis(&ReducIr::case(
      ReducIrTy::from_id(salsa::Id::from_u32(0)),
      ReducIr::var(var),
      [ReducIr::var(var), ReducIr::var(var)],
    ));
    assert_eq!(occs[var], Occurrence::Many);
  }

  #[test]
  fn test_many_occurences_isnt_reset_by_case() {
    let mut gen_var = var_supply();

    let var = gen_var();
    let discr = gen_var();
    let occs = occurence_analysis(&ReducIr::case(
      ReducIrTy::from_id(salsa::Id::from_u32(0)),
      ReducIr::var(discr),
      [
        ReducIr::app(ReducIr::var(var), [ReducIr::var(var)]),
        ReducIr::var(var),
      ],
    ));
    assert_eq!(occs[var], Occurrence::Many);
  }
}
