use std::collections::hash_map::Entry;

use bumpalo::Bump;
use rustc_hash::FxHashMap;

use crate::{TypeCheckError, InferCtx};


/// Solve constraints generated from Ast during first phase of type checking.
/// At a high level the algorithm is:
///
/// Loop until no interactions:
///   1. Canonicalize input constraints
///   2. Find interactions between canonical constraints
///   3. Interact constraints, producing new constraints
///   4. Canonicalize freshly generated constraints
///
/// At the end of this process we'll have a set of canonical constraints that can be turned into a
/// substitution "solving" any open type variables (and crucially ALL unifier variables) for type
/// checking.
/*pub(crate) struct ConstraintSolver<'ctx> {
    ctx: &'ctx InferCtx<'ctx>,
    unifiers: InPlaceUnificationTable<TcUnifierVar<'ctx>>,
    constraint: Vec<Constraint<InferTy<'ctx>>>,
}
impl<'ctx> ConstraintSolver<'ctx> {
    pub(crate) fn new(ctx: &'ctx InferCtx<'ctx>, ) -> Self {
        Self {
            ctx,
        }
    }

    /// Solve a set of input constraints.
    /// If solving succeeds a substitution from type variables to their types is returned.
    /// If solving failes a list of residual constraints and errors are returned.
    pub(crate) fn solve<Ty>(
        self,
        constraints: impl Iterator<Item = Constraint<Ty>>,
    ) -> (
        UnifierSubst<'ty>,
        Vec<Constraint<Ty>>,
        Vec<TypeCheckError<'ty>>,
    ) {
        // Canonicalize our input set of constraints
        let (mut canon, mut residual) = canonical_constraints(constraints);
        let mut interactions = vec![];

        // Find any interactions in that initial set of constraints.
        // This will remove any canon constraints that form an intraction from the bag
        find_interactions(&mut canon, &mut interactions);
        // If we found no interactions we're done typechecking
        while !interactions.is_empty() {
            // Otherwise perform interactions
            for interact in interactions.drain(..) {
                // Since we remove canon constraints from the bag each interaction will re insert
                // one of the original canon constraints, and the result of the interaction.
                // We put the new constraints back into canonical form and then add them back to
                // the bag before checking for more interactions and looping
                let (new_canon, new_residual) = match interact {
                    Interaction::InteractEq { tvar, left, right } => canonical_constraints(
                        [
                            Constraint::Eq(self.ty_arena.alloc(TypeKind::VarTy(tvar)), left),
                            Constraint::Eq(left, right),
                        ]
                        .into_iter(),
                    ),
                    Interaction::InteractOccurs {
                        tvar,
                        needle,
                        haystack:
                            CanonicalConstraint::CanonEq {
                                tvar: hay_tvar,
                                ty: hay_ty,
                            },
                    } => canonical_constraints(
                        [
                            Constraint::Eq(
                                self.ty_arena.alloc(TypeKind::VarTy(UnifierTypeId::Unifier(tvar))),
                                needle,
                            ),
                            Constraint::Eq(
                                self.ty_arena.alloc(TypeKind::VarTy(hay_tvar)),
                                hay_ty.apply_oneshot(self.ty_arena, tvar, needle),
                            ),
                        ]
                        .into_iter(),
                    ),
                };
                merge_left(&mut canon, new_canon);
                residual.extend(new_residual);
            }
            find_interactions(&mut canon, &mut interactions);
        }

        // TODO: move this up and collect errors during interactions/canonicalization.
        let mut errors = vec![];
        match self.flatten(canon) {
            Ok(subst) => (subst, residual, errors),
            Err(err) => {
                errors.push(err);
                (UnifierSubst::default(), residual, errors)
            }
        }
    }

    /// Flatten out a canonical bag into a substitution.
    /// This walks each key of the bag checking that only one type remains. If the remaining type
    /// is a variable we resolve that variable to it's type and then map both variables to the
    /// resolved type. This means the final substitution should map type variables -> concrete
    /// types with no type variable indirections between them.
    fn flatten(
        &self,
        mut canon: CanonConstraintBag<&'ty InferUnifierType<'ty>>,
    ) -> Result<UnifierSubst<'ty>, TypeCheckError<'ty>> {
        fn resolve_var<'ty>(
            error_ty: &'ty UnifierType<'ty>,
            canon: &mut CanonConstraintBag<&'ty InferUnifierType<'ty>>,
            uv: TcUnifierVar,
        ) -> Result<Option<&'ty UnifierType<'ty>>, TypeCheckError<'ty>> {
            match canon.get(&(uv.into())) {
                Some(tys) => {
                    if tys.is_empty() {
                        return Ok(None);
                    }
                    if tys.len() > 1 {
                        return Err(TypeCheckError::UnresolvedTypes(uv, tys.clone()));
                    }
                    match tys[0] {
                        // If our var is mapped to another var chase down that variable,
                        // and save the result for this variable as well.
                        TypeKind::VarTy(UnifierTypeId::Unifier(var)) => {
                            let opt_ty = resolve_var(error_ty, canon, *var)?;
                            if let Some(ty) = opt_ty {
                                canon.insert(uv.into(), vec![ty]);
                            }
                            Ok(opt_ty)
                        }
                        ty => Ok(Some(ty)),
                    }
                }
                None => Ok(None),
            }
        }
        let mut subst = UnifierSubst::default();
        let keys = canon
            .keys()
            .filter_map(|tv| match tv {
                UnifierTypeId::Unifier(uni) => Some(uni),
                UnifierTypeId::Var(_) => None,
            })
            .cloned()
            .collect::<Vec<_>>();

        for key in keys {
            if let Some(ty) = resolve_var(self.error_ty, &mut canon, key)? {
                subst.insert(key, ty);
            }
        }
        Ok(subst)
    }
}*/

/// A constraint produced during initail type checking.
/// It will be solved in the second half of type checking to produce a map from Unifier variables
/// to their types.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Constraint<Ty> {
    /// Two types must be equal for our term to type check
    Eq(Ty, Ty),
}

/*/// A canonical constraint
/// A constraint that is in a standard form: type var ~ type
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum CanonicalConstraint<Ty> {
    CanonEq {
        tvar: TcUnifierVar,
        ty: Ty,
    },
}

/// Result of canoncilization
enum CanonResult<Ty> {
    /// A canonical constraint
    Canon(CanonicalConstraint<Ty>),
    /// More work is needed to canonicalize
    Work(Vec<Constraint<Ty>>),
    /// Could not canoncicalize, mark as residual
    Residue(Constraint<Ty>),
}

impl<Ty> From<CanonicalConstraint<Ty>> for CanonResult<Ty> {
    fn from(canon: CanonicalConstraint<Ty>) -> Self {
        Self::Canon(canon)
    }
}
impl<Ty> From<Constraint<Ty>> for CanonResult<Ty> {
    fn from(ct: Constraint<Ty>) -> Self {
        Self::Residue(ct)
    }
}

impl<'ty> Constraint<&'ty InferUnifierType<'ty>> {
    /// Take a step towards canonicalizing a constraint:
    /// * Decompose compound types into a list of composite type constraints
    /// * Turn a VarTy ~ Type constraint into a canonical constraint
    /// * Incompatible types become residual constraints
    fn canonical(self) -> CanonResult<&'ty InferUnifierType<'ty>> {
        use Constraint::*;
        use crate::ty::TypeKind::*;
        match self {
            // If two IntTy meet this is tautologically true and we can remove the constraint
            // entirely
            Eq(IntTy, IntTy) => CanonResult::Work(vec![]),
            Eq(FunTy(a_arg_ty, a_ret_ty), FunTy(b_arg_ty, b_ret_ty)) => {
                CanonResult::Work(vec![Eq(a_arg_ty, b_arg_ty), Eq(a_ret_ty, b_ret_ty)])
            }
            Eq(ErrorTy, _) | Eq(_, ErrorTy) | Eq(IntTy, FunTy(_, _)) | Eq(FunTy(_, _), IntTy) => {
                self.into()
            }
            // Special case this logic to handler variable ordering
            Eq(a_ty @ VarTy(a), b_ty @ VarTy(b)) => CanonicalConstraint::CanonEq {
                tvar: std::cmp::min(*a, *b),
                ty: std::cmp::max(a_ty, b_ty),
            }
            .into(),
            Eq(VarTy(var), ty) | Eq(ty, VarTy(var)) => {
                // Occurrence check
                if ty.type_vars().all(|tv| tv != var) {
                    CanonicalConstraint::CanonEq { tvar: *var, ty }.into()
                } else {
                    self.into()
                }
            }
        }
    }
}*/

/// A multiset of canoncial constraints
//pub(super) type CanonConstraintBag<Ty> = FxHashMap<TcUnifierVar, Vec<Ty>>;

/// In place merge the right canon bag into the left canon bag
/*pub(super) fn merge_left<Ty>(canon: &mut CanonConstraintBag<Ty>, new_canon: CanonConstraintBag<Ty>) {
    for (tvar, tys) in new_canon.into_iter() {
        canon.entry(tvar).or_default().extend(tys);
    }
}*/

/// Canonical constraints
/*pub(super) fn canonical_constraints<Ty>(
    cts: impl Iterator<Item = Constraint<Ty>>,
) -> (CanonConstraintBag<Ty>, Vec<Constraint<Ty>>) {
    fn aux<Ty>(
        canon: &mut CanonConstraintBag<Ty>,
        residue: &mut Vec<Constraint<Ty>>,
        cts: impl Iterator<Item = Constraint<Ty>>,
    ) {
        for ct in cts {
            match ct.canonical() {
                CanonResult::Canon(CanonicalConstraint::CanonEq { tvar, ty }) => {
                    canon.entry(tvar).or_default().push(ty)
                }
                CanonResult::Residue(ct) => residue.push(ct),
                CanonResult::Work(cts) => aux(canon, residue, cts.into_iter()),
            }
        }
    }
    let mut result = (FxHashMap::default(), vec![]);
    aux(&mut result.0, &mut result.1, cts);
    result
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Interaction<Ty> {
    InteractEq {
        tvar: TcUnifierVar,
        left: Ty,
        right: Ty,
    },
    InteractOccurs {
        tvar: TcUnifierVar,
        needle: Ty,
        haystack: CanonicalConstraint<Ty>,
    },
}

/// Search through a canonical constraint bag for interactions
/// canon_bag is mutated so it does not contain any constraints that appear in interactions.
pub(super) fn find_interactions<Ty>(
    canon_bag: &mut CanonConstraintBag<Ty>,
    interactions: &mut Vec<Interaction<Ty>>,
) {
    //let mut interactions = vec![];
    // We need to mutate canon_bag so don't hold keys
    let keys = canon_bag.keys().cloned().collect::<Vec<_>>();
    'outer_keys: for key in keys {
        // We may have a single type remaining (because we consume pairs of types)
        // If we do check for any occurrences within that type.
        // We split this out over two segments like this to avoid lifetime issues.
        let tailing_canon = canon_bag.get_mut(&key).and_then(|tys| {
            while tys.len() >= 2 {
                let left = tys.pop().unwrap();
                let right = tys.pop().unwrap();

                interactions.push(Interaction::InteractEq {
                    tvar: key,
                    left,
                    right,
                });
            }
            tys.pop().map(|ty| (key, ty))
        });

        if let Some((tvar, ty)) = tailing_canon {
            for uv in ty.unifier_vars() {
                if let Entry::Occupied(mut occu) = canon_bag.entry(uv.into()) {
                    // We only want to produce at most one occurence for a given tv
                    // Other candidate occurences will get worked out in later iterations since all
                    // types under a tv must be equal to each other.
                    match occu.get_mut().pop() {
                        Some(needle) => {
                            interactions.push(Interaction::InteractOccurs {
                                tvar: *uv,
                                needle,
                                haystack: CanonicalConstraint::CanonEq { tvar, ty },
                            });
                            // If we produce an interaction continue the outer loop
                            continue 'outer_keys;
                        }
                        None => {
                            occu.remove_entry();
                        }
                    }
                }
            }

            // Our loop did not produce an interaction so reinstate the canon ct
            canon_bag.entry(tvar).or_default().push(ty);
        }
    }
}*/

#[cfg(test)]
mod tests {
    use super::*;

    /*macro_rules! unifier_var_ty {
        ($e:expr) => {
            Type::VarTy(TcUnifierVar($e))
        };
    }

    *macro_rules! bag { () => {{ FxHashMap::default() }}; ($($key:expr => [$($ty:expr),*]),*) => {{
            let mut bag = FxHashMap::default();
            $(bag.insert($key, vec![$($ty),*]);)*
            bag
        }};
    }

    #[test]
    fn test_tc_interactions_eq() {
        let tv = TcUnifierVar(0);
        let tv0 = unifier_var_ty!(1);
        let fun = TypeKind::FunTy(&unifier_var_ty!(2), &unifier_var_ty!(3));
        let mut bag = bag!(tv => [&tv0, &fun]);

        let mut ints = vec![];
        find_interactions(&mut bag, &mut ints);

        assert_eq!(
            ints,
            vec![Interaction::InteractEq {
                tvar: tv,
                left: &fun,
                right: &tv0
            }]
        );
        assert_eq!(bag, bag!(tv => []));
    }

    #[test]
    fn test_tc_interactions_occur() {
        let arn = Bump::new();
        let a = UnifierTypeId::Unifier(TcUnifierVar(0));
        let b = UnifierTypeId::Unifier(TcUnifierVar(1));
        let fun: &UnifierType<'_> = arn.alloc(TypeKind::FunTy(
            arn.alloc(unifier_var_ty!(3)),
            arn.alloc(TypeKind::FunTy(arn.alloc(unifier_var_ty!(2)), arn.alloc(TypeKind::VarTy(b)))),
        ));
        let mut bag = bag!(
            a => [fun],
            b => [arn.alloc(unifier_var_ty!(4))]
        );

        let mut ints = vec![];
        find_interactions(&mut bag, &mut ints);

        assert_eq!(
            ints,
            vec![Interaction::InteractOccurs {
                tvar: TcUnifierVar(1),
                needle: &unifier_var_ty!(4),
                haystack: CanonicalConstraint::CanonEq {
                    tvar: a,
                    ty: &TypeKind::FunTy(&unifier_var_ty!(3), &TypeKind::FunTy(&unifier_var_ty!(2), &TypeKind::VarTy(b)))
                }
            }]
        );
        assert_eq!(bag, bag!(a => [], b => []));
    }

    #[test]
    fn test_tc_interactions_multi_eq_with_one_leftover() {
        let tv = UnifierTypeId::Unifier(TcUnifierVar(0));
        let mut bag = bag!(tv => [&unifier_var_ty!(1), &unifier_var_ty!(2), &unifier_var_ty!(3), &unifier_var_ty!(4), &unifier_var_ty!(5)]);

        let mut ints = vec![];
        find_interactions(&mut bag, &mut ints);

        assert_eq!(
            ints,
            vec![
                Interaction::InteractEq {
                    tvar: tv,
                    left: &unifier_var_ty!(5),
                    right: &unifier_var_ty!(4)
                },
                Interaction::InteractEq {
                    tvar: tv,
                    left: &unifier_var_ty!(3),
                    right: &unifier_var_ty!(2)
                },
            ]
        );
        assert_eq!(bag, bag!(tv => [&unifier_var_ty!(1)]));
    }

    #[test]
    fn test_tc_solve_propagates() {
        let arena = Bump::new();
        let cs = ConstraintSolver::new(&arena);

        let a = arena.alloc(unifier_var_ty!(0));
        let b = arena.alloc(unifier_var_ty!(1));
        let c = arena.alloc(unifier_var_ty!(2));
        let int: &UnifierType<'_> = arena.alloc(TypeKind::IntTy);

        let (subst, _residual, errors) = cs.solve(
            vec![
                Constraint::Eq(a, b),
                Constraint::Eq(b, c),
                Constraint::Eq(c, int),
            ]
            .into_iter(),
        );

        assert_eq!(errors, vec![]);
        assert_eq!(subst, UnifierSubst::from_iter([int, int, int,]));
    }

    #[test]
    fn test_tc_solve_occur_check_fails() {
        let arena = Bump::new();
        let cs = ConstraintSolver::new(&arena);

        let a = arena.alloc(unifier_var_ty!(0));
        let b = arena.alloc(unifier_var_ty!(1));
        let c = arena.alloc(unifier_var_ty!(2));
        let fun = arena.alloc(TypeKind::FunTy(c, b));

        let (_subst, residual, _errors) =
            cs.solve(vec![Constraint::Eq(c, a), Constraint::Eq(a, fun)].into_iter());

        assert_eq!(residual, vec![Constraint::Eq(fun, c)]);
    }

    #[test]
    fn test_tc_solve_decompose() {
        let arena = Bump::new();
        let cs = ConstraintSolver::new(&arena);

        let a = arena.alloc(unifier_var_ty!(0));
        let b = arena.alloc(unifier_var_ty!(1));
        let c = arena.alloc(unifier_var_ty!(2));
        let fun = arena.alloc(TypeKind::FunTy(a, b));
        let int: &'_ UnifierType<'_> = arena.alloc(TypeKind::IntTy);
        let int_fun = arena.alloc(TypeKind::FunTy(int, int));

        let (subst, residual, _errors) =
            cs.solve(vec![Constraint::Eq(c, fun), Constraint::Eq(c, int_fun)].into_iter());

        assert_eq!(residual, vec![]);
        assert_eq!(subst, UnifierSubst::from_iter([int, int, int_fun,]));
    }*/
}
