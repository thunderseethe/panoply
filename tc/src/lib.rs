use std::{collections::hash_map::Entry, fmt::Debug, ops::Index};

use aiahr_core::{
    ast::{Ast, Term, Term::*},
    define_ids,
    id::{Id, IdGen, VarId},
};
use bumpalo::Bump;
use rustc_hash::FxHashMap;

define_ids!(
/// A type variable.
/// These are explicity referred to by the AST and can persist through type checking.
/// They may not be modified by the type checking process, often referred to as untouchabale.
#[derive(Default, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub TcVar;

/// A unifier variable.
/// These are produced during the type checking process and MUST NOT persist outside the type
/// checker. They may not appear in the AST once type checking is completed and are removed by
/// zonking.
/// Conversely to the untouchable TcVar, these are "touchable" and will be modified by the type
/// chcker.
#[derive(Default, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub TcUnifierVar;
);

impl AsRef<TcUnifierVar> for TcUnifierVar {
    fn as_ref(&self) -> &TcUnifierVar {
        &self
    }
}

trait InsertType<'ty> {
    fn insert(&mut self, id: TcUnifierVar, ty: &'ty UnifierType<'ty>);
}

/// Acts as a dense map from TcUnifierVar to T.
/// Abstracts over the two kinds of substitutions we have. UnifierSubst which may not have types
/// for all unifiers, and GeneralizedSubst which must have types for all unifiers.
#[derive(PartialEq, Eq, Clone)]
struct SubstInternal<T> {
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
type UnifierSubst<'ty> = SubstInternal<Option<&'ty UnifierType<'ty>>>;

/// A completed substitution, MUST have types for all unifiers.
/// This is the substitution returned from `solve_constraints`.
type GeneralizedSubst<'ty> = SubstInternal<&'ty UnifierType<'ty>>;

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
    fn fresh_unifier(&mut self) -> TcUnifierVar {
        let raw = self.dense_map.len();
        self.dense_map.push(None);
        TcUnifierVar::from_raw(raw)
    }

    fn get(&self, unifier_var: TcUnifierVar) -> Option<&'ty UnifierType<'ty>> {
        self.dense_map.get(unifier_var.0).and_then(|ty| ty.clone())
    }

    fn iter<'a>(&'a self) -> impl Iterator<Item = (TcUnifierVar, &'ty UnifierType<'ty>)> + 'a {
        self.raw_iter()
            .filter_map(|(id, opt_ty)| opt_ty.map(|ty| (id, ty)))
    }
    fn raw_iter<'a>(
        &'a self,
    ) -> impl Iterator<Item = (TcUnifierVar, Option<&'ty UnifierType<'ty>>)> + 'a {
        self.dense_map
            .iter()
            .enumerate()
            .map(|(i, opt)| (TcUnifierVar::from_raw(i), opt.clone()))
    }

    fn iter_mut<'a>(
        &'a mut self,
    ) -> impl Iterator<Item = (TcUnifierVar, &mut &'ty UnifierType<'ty>)> {
        self.dense_map
            .iter_mut()
            .enumerate()
            .filter_map(|(i, opt_ty)| opt_ty.as_mut().map(|ty| (TcUnifierVar::from_raw(i), ty)))
    }
    fn raw_iter_mut<'a>(
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

/// During type checking we will refer to both type and unifier variables.
/// However once type checking completes we must remove all unifiers variables, so we use
/// UnifierTypeId to track which type variables are touch and untouchable.
/// Once type checking completes we transform from `Type<'_, UnifierTypeId>` to `Type<'_, TcVar>`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum UnifierTypeId {
    Var(TcVar),
    Unifier(TcUnifierVar),
}
impl From<&TcUnifierVar> for UnifierTypeId {
    fn from(uv: &TcUnifierVar) -> Self {
        UnifierTypeId::Unifier(*uv)
    }
}
impl From<TcUnifierVar> for UnifierTypeId {
    fn from(uv: TcUnifierVar) -> Self {
        UnifierTypeId::Unifier(uv)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub enum Type<'ty, TV> {
    /// Marker that signifies an operation produced an error. This exists so that we can try to
    /// gracefully recover from a type checking error and produce a list of errors at the end of type checking
    ErrorTy,
    /// Type of integers.
    //TODO: This should be part of some BaseTy or LiteralTy as opposed to a member of Type directly
    IntTy,
    /// A type variable, during type checking this may be either a unifier or a proper type variable
    VarTy(TV),
    /// A function type
    FunTy(&'ty Type<'ty, TV>, &'ty Type<'ty, TV>),
}

impl<'tv, TV> Type<'tv, TV> {
    fn type_vars(&self) -> impl Iterator<Item = &TV> {
        TypeVarsIter { stack: vec![self] }
    }
}

impl<'ty> UnifierType<'ty> {
    fn unifier_vars(&self) -> impl Iterator<Item = &TcUnifierVar> {
        self.type_vars().filter_map(|tv| match tv {
            UnifierTypeId::Unifier(uni) => Some(uni),
            UnifierTypeId::Var(_) => None,
        })
    }

    /// Zonk a type removing all unifier vars and replacing them with their concrete type from the substitution.
    /// This function will panic if a unifier is encountered but does not have an entry in subst.
    ///
    /// Substitution and zonking are very similar but subtly different operations.
    /// Zonking a type _removes_ all unifiers destructively leaving the type with only references to TcVar.
    /// Whereas a substitution does not change the variables present in a type.
    fn zonk<'new_ty>(
        &self,
        ty_arena: &'new_ty Bump,
        subst: &GeneralizedSubst<'ty>,
    ) -> &'new_ty Type<'new_ty, TcVar> {
        // Because we're changing types we basically have to re-alloc every node, so the signature
        // provides the ability to do that in a fresh arena if desired
        // TODO: Add a unit type and do this. We use IntTy for this purpose for now
        match self {
            ErrorTy => ty_arena.alloc(ErrorTy),
            IntTy => ty_arena.alloc(IntTy),
            VarTy(UnifierTypeId::Unifier(uni)) => subst[*uni].zonk(ty_arena, subst),
            VarTy(UnifierTypeId::Var(var)) => ty_arena.alloc(VarTy(*var)),
            FunTy(arg, ret) => {
                ty_arena.alloc(FunTy(arg.zonk(ty_arena, subst), ret.zonk(ty_arena, subst)))
            }
        }
    }
}

struct TypeVarsIter<'tv, TV> {
    stack: Vec<&'tv Type<'tv, TV>>,
}
impl<'tv, TV> Iterator for TypeVarsIter<'tv, TV> {
    type Item = &'tv TV;

    fn next(&mut self) -> Option<Self::Item> {
        self.stack.pop().and_then(|ty| match ty {
            ErrorTy => self.next(),
            IntTy => self.next(),
            VarTy(tv) => Some(tv),
            FunTy(arg_ty, ret_ty) => {
                self.stack.extend_from_slice(&[arg_ty, ret_ty]);
                self.next()
            }
        })
    }
}

// Open up type into this scope for convenience
use Type::*;

type UnifierType<'ty> = Type<'ty, UnifierTypeId>;
type UnifierVarId<'ty> = TypedVarId<'ty, UnifierTypeId>;

/// A variable marked with it's type
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct TypedVarId<'ty, TV> {
    var: VarId,
    ty: &'ty Type<'ty, TV>,
}

/// A constraint produced during initail type checking.
/// It will be solved in the second half of type checking to produce a map from Unifier variables
/// to their types.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Constraint<'ty> {
    /// Two types must be equal for our term to type check
    Eq(&'ty UnifierType<'ty>, &'ty UnifierType<'ty>),
}

/// A canonical constraint
/// A constraint that is in a standard form: type var ~ type
#[derive(Debug, PartialEq, Eq)]
enum CanonicalConstraint<'ty> {
    CanonEq {
        tvar: UnifierTypeId,
        ty: &'ty UnifierType<'ty>,
    },
}

/// Result of canoncilization
enum CanonResult<'ty> {
    /// A canonical constraint
    Canon(CanonicalConstraint<'ty>),
    /// More work is needed to canonicalize
    Work(Vec<Constraint<'ty>>),
    /// Could not canoncicalize, mark as residual
    Residue(Constraint<'ty>),
}

impl<'ty> From<CanonicalConstraint<'ty>> for CanonResult<'ty> {
    fn from(canon: CanonicalConstraint<'ty>) -> Self {
        Self::Canon(canon)
    }
}
impl<'ty> From<Constraint<'ty>> for CanonResult<'ty> {
    fn from(ct: Constraint<'ty>) -> Self {
        Self::Residue(ct)
    }
}

impl<'ty> Constraint<'ty> {
    /// Take a step towards canonicalizing a constraint:
    /// * Decompose compound types into a list of composite type constraints
    /// * Turn a VarTy ~ Type constraint into a canonical constraint
    /// * Incompatible types become residual constraints
    fn canonical(self) -> CanonResult<'ty> {
        use Constraint::*;
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
}

/// A multiset of canoncial constraints
type CanonConstraintBag<'ty> = FxHashMap<UnifierTypeId, Vec<&'ty UnifierType<'ty>>>;

/// In place merge the right canon bag into the left canon bag
fn merge_left<'ty>(canon: &mut CanonConstraintBag<'ty>, new_canon: CanonConstraintBag<'ty>) {
    for (tvar, tys) in new_canon.into_iter() {
        canon.entry(tvar).or_default().extend(tys);
    }
}

/// Canonical constraints
fn canonical_constraints<'ty>(
    cts: impl Iterator<Item = Constraint<'ty>>,
) -> (CanonConstraintBag<'ty>, Vec<Constraint<'ty>>) {
    fn aux<'ty>(
        canon: &mut CanonConstraintBag<'ty>,
        residue: &mut Vec<Constraint<'ty>>,
        cts: impl Iterator<Item = Constraint<'ty>>,
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
enum Interaction<'ty> {
    InteractEq {
        tvar: UnifierTypeId,
        left: &'ty UnifierType<'ty>,
        right: &'ty UnifierType<'ty>,
    },
    InteractOccurs {
        tvar: TcUnifierVar,
        needle: &'ty UnifierType<'ty>,
        haystack: CanonicalConstraint<'ty>,
    },
}

/// Search through a canonical constraint bag for interactions
/// canon_bag is mutated so it does not contain any constraints that appear in interactions.
fn find_interactions<'ty>(
    canon_bag: &mut CanonConstraintBag<'ty>,
    interactions: &mut Vec<Interaction<'ty>>,
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
}

/// Errors that may be produced during the type checking process
#[derive(Debug, PartialEq, Eq)]
pub enum TypeCheckError<'ty> {
    /// A variable we expected to exist with a type, did not
    VarNotDefined(VarId),
    // TODO: Consider whether we should expose the unifier var stuff in errors here or not?
    UnsolvedTyVariable(UnifierTypeId),
    UnresolvedTypes(TcUnifierVar, Vec<&'ty UnifierType<'ty>>),
    // TODO: Don't leak constraints in the public API here
    UnsolvedConstraints(Vec<Constraint<'ty>>),
}

/// A trait for applying a substitution to a thing. Often this will be a type, but it might also be an Ast.
trait Substitutable<'ty> {
    /// A type to hold allocators apply needs to create any modified nodes
    type Alloc;
    /// The output type after our substitution is applied
    type Output;
    type Subst: Default + InsertType<'ty>;

    /// A variant of apply that only performs one substitution.
    /// Split out as a separate method to allow for more performant implementations.
    fn apply_oneshot(
        &self,
        ty_arena: Self::Alloc,
        key: TcUnifierVar,
        val: &'ty UnifierType<'ty>,
    ) -> Self::Output {
        let mut unifier_subst = Self::Subst::default();
        unifier_subst.insert(key, val);
        self.apply(ty_arena, &unifier_subst)
    }

    /// Produce a new Output where all type vars that appear in self have been replaced by their
    /// corresponding values in subst.
    fn apply(&self, alloc: Self::Alloc, subst: &Self::Subst) -> Self::Output;
}

/// Replace all occurrences of type variables in self by their corresponding type in subst.
impl<'ty> Substitutable<'ty> for &'ty UnifierType<'ty> {
    type Alloc = &'ty Bump;
    type Output = &'ty UnifierType<'ty>;
    type Subst = UnifierSubst<'ty>;

    fn apply_oneshot(
        &self,
        ty_arena: Self::Alloc,
        key: TcUnifierVar,
        val: &'ty UnifierType<'ty>,
    ) -> Self::Output {
        match self {
            ErrorTy | IntTy => self,
            VarTy(UnifierTypeId::Unifier(uni)) if uni == &key => val,
            VarTy(_) => self,
            FunTy(arg, ret) => ty_arena.alloc(FunTy(
                arg.apply_oneshot(ty_arena, key, val),
                ret.apply_oneshot(ty_arena, key, val),
            )),
        }
    }

    fn apply(&self, ty_arena: Self::Alloc, subst: &UnifierSubst<'ty>) -> Self::Output {
        match self {
            ErrorTy | IntTy => self,
            VarTy(UnifierTypeId::Unifier(uni)) => subst.get(*uni).unwrap_or(self),
            VarTy(_) => self,
            FunTy(arg, ret) => ty_arena.alloc(FunTy(
                arg.apply(ty_arena, subst),
                ret.apply(ty_arena, subst),
            )),
        }
    }
}

/// Replace all occurrences of type variables in each type in the ast by their corresponding type
/// in subst.
impl<'ast, 'ty> Substitutable<'ty> for &'ast Term<'ast, UnifierVarId<'ty>> {
    type Alloc = (&'ast Bump, &'ty Bump);
    type Output = &'ast Term<'ast, TypedVarId<'ty, TcVar>>;
    type Subst = GeneralizedSubst<'ty>;

    fn apply(
        &self,
        alloc @ (ast_arena, ty_arena): Self::Alloc,
        subst: &GeneralizedSubst<'ty>,
    ) -> Self::Output {
        // TODO: optimize this to reuse existing allocation if we don't apply a substitution to a
        // term. At the moment this reallocs the entire term which is inefficient
        match self {
            Abstraction {
                arg: TypedVarId { var, ty },
                body,
            } => ast_arena.alloc(Abstraction {
                arg: TypedVarId {
                    var: *var,
                    ty: ty.zonk(ty_arena, subst),
                },
                body: body.apply(alloc, subst),
            }),
            Application { func, arg } => ast_arena.alloc(Application {
                func: func.apply(alloc, subst),
                arg: arg.apply(alloc, subst),
            }),
            Variable(TypedVarId { var, ty }) => ast_arena.alloc(Variable(TypedVarId {
                var: *var,
                ty: ty.zonk(ty_arena, subst),
            })),
        }
    }
}

/// # A bidirectional damnas-milner type checker.
///
/// Type checking a term happens in two main phases: Generating constraints, and solving constraints.
///
/// ## Constraint Generation
/// Constraint generation is done by two mutually recursive methods, infer and check.
///
/// * `infer` - generates a type based on the input term and will internally call check to verify
/// subterms.
/// * `check` - checks a term against an expected type, recording an error if types cannot match.
/// If no specific case applies, check will call infer and check the expected type is equal to the
/// inferred type.
///
/// ## Constraint Solving
/// Once constraint generation is completed we have a typed ast that contains unifiers. We move on
/// to solving the generated constraints. This is done by `ConstraintSolver` which runs a straight
/// forward loop to find active pairs and reduce them in the cosntraint graph.
///
/// The output of this solution will be a substitution from type variables (include unifier vars)
/// to their concrete types. Chains of type variables { a := b, b := c, c := int } are flattened to
/// { a := int, b := int, c : = int }.
///
/// We check for any unsolved unifier variables, any unifiers that are free are given fresh type
/// variables. This will generalize our results when we zonk away all our unifier variables.
///
/// ### Zonking (for lack of a better term)
/// The substitution produced by constraint solving is used for one final step in producing our type
/// checked term. We must ensure all unifier variables are removed from the typed term and
/// resulting type. They cannot leak outside the type checker.
///
/// This process is referred to as zonking, I won't defend that name. Nor will the people I've
/// deftly swiped it from. But I will use it to refer to the process that finds unifier variables
/// in our output and resolves them to the concrete types they are mapped to in our substitution.
struct TypeChecker<'ast, 'ty> {
    /// The arena to allocate the resulting typed ast
    ast_arena: &'ast Bump,
    ty_arena: &'ty Bump,
    // convenience allocation so we don't have to realloc ErrorTy everytime we need it
    error_ty: &'ty UnifierType<'ty>,
    /// Store types for local variables.
    local_env: FxHashMap<VarId, &'ty UnifierType<'ty>>,
    /// List of unifiers and their current mapped types, if any.
    unifiers: UnifierSubst<'ty>, //Vec<Option<&'ty UnifierType<'ty>>>,
    /// List of errors produced during type checking
    errors: Vec<TypeCheckError<'ty>>,
    /// Constraints generated during intial stage of type checking,
    /// to be solved later.
    constraints: Vec<Constraint<'ty>>,
    /// Supply of fresh type variables for when we generalize our term.
    type_var_supply: IdGen<TcVar, ()>,
}

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
struct ConstraintSolver<'ty> {
    ty_arena: &'ty Bump,
    error_ty: &'ty UnifierType<'ty>,
}
impl<'ty> ConstraintSolver<'ty> {
    fn new(ty_arena: &'ty Bump) -> Self {
        Self {
            ty_arena,
            error_ty: ty_arena.alloc(ErrorTy),
        }
    }

    /// Solve a set of input constraints.
    /// If solving succeeds a substitution from type variables to their types is returned.
    /// If solving failes a list of residual constraints and errors are returned.
    fn solve(
        self,
        constraints: impl Iterator<Item = Constraint<'ty>>,
    ) -> (
        UnifierSubst<'ty>,
        Vec<Constraint<'ty>>,
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
                            Constraint::Eq(self.ty_arena.alloc(VarTy(tvar)), left),
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
                                self.ty_arena.alloc(VarTy(UnifierTypeId::Unifier(tvar))),
                                needle,
                            ),
                            Constraint::Eq(
                                self.ty_arena.alloc(VarTy(hay_tvar)),
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
        mut canon: CanonConstraintBag<'ty>,
    ) -> Result<UnifierSubst<'ty>, TypeCheckError<'ty>> {
        fn resolve_var<'ty>(
            error_ty: &'ty UnifierType<'ty>,
            canon: &mut CanonConstraintBag<'ty>,
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
                        VarTy(UnifierTypeId::Unifier(var)) => {
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
}

impl<'ast, 'ty> TypeChecker<'ast, 'ty> {
    fn new(ast_arena: &'ast Bump, ty_arena: &'ty Bump) -> Self {
        Self {
            ast_arena,
            ty_arena,
            error_ty: ty_arena.alloc(ErrorTy),
            local_env: FxHashMap::default(),
            errors: vec![],
            unifiers: UnifierSubst::default(),
            constraints: vec![],
            type_var_supply: IdGen::new(),
        }
    }

    /// Allocate a unifier variable as a variable type
    fn as_type_var(&self, unifier: TcUnifierVar) -> &'ty UnifierType<'ty> {
        self.ty_arena
            .alloc(Type::VarTy(UnifierTypeId::Unifier(unifier)))
    }

    // This helper exists to get around liftimes
    fn fresh_unifier_var(&mut self) -> &'ty UnifierType<'ty> {
        // If we try to combine this as
        // `self.as_type_var(self.fresh_unifier())`
        // rust think's we're holding an immutable and mutable borrow at the same time so
        // we need the temp variable.
        let uv = self.unifiers.fresh_unifier();
        self.as_type_var(uv)
    }

    /// Record two types must be equal
    fn equate_ty(&mut self, ty: &'ty UnifierType<'ty>, other_ty: &'ty UnifierType<'ty>) {
        match (ty, other_ty) {
            // Unifier optimization
            // If we're going to infer and equate a type to unifier, we can instead set that
            // unifier equal to the inferred type directly.
            (VarTy(UnifierTypeId::Unifier(unifier_var)), other_ty) => {
                if let Some(ty) = self.unifiers.get(*unifier_var) {
                    self.constraints.push(Constraint::Eq(ty, other_ty));
                } else {
                    self.unifiers.insert(*unifier_var, other_ty);
                }
            }
            (ty, VarTy(UnifierTypeId::Unifier(unifier_var))) => {
                if let Some(other_ty) = self.unifiers.get(*unifier_var) {
                    self.constraints.push(Constraint::Eq(ty, other_ty));
                } else {
                    self.unifiers.insert(*unifier_var, ty);
                }
            }
            (ty, other_ty) => {
                self.constraints.push(Constraint::Eq(other_ty, ty));
            }
        }
    }

    /// Check a term against a given type.
    /// This method pairs with infer to form a bidirectional type checker
    fn check(
        &mut self,
        term: &Term<'_, VarId>,
        expected_ty: &'ty UnifierType<'ty>,
    ) -> &'ast Term<'ast, UnifierVarId<'ty>> {
        use Type::*;
        match (term, expected_ty) {
            (Abstraction { arg, body }, FunTy(arg_ty, body_ty)) => {
                // Check an abstraction against a function type by checking the body checks against
                // the function return type with the function argument type in scope.
                self.local_env.insert(*arg, arg_ty);
                let body = self.check(body, body_ty);
                self.local_env.remove(arg);

                self.ast_arena.alloc(Abstraction {
                    arg: TypedVarId {
                        var: *arg,
                        ty: arg_ty,
                    },
                    body,
                })
            }
            // Bucket case for when we need to check a rule against a type but no case applies
            (term, expected_ty) => {
                // Infer a type for our term and check that the expected type is equal to the
                // inferred type.
                let (typed_term, inferred_ty) = self.infer(term);
                self.equate_ty(expected_ty, inferred_ty);
                typed_term
            }
        }
    }

    /// Infer a type for a term
    /// This method pairs with check to form a bidirectional type checker
    fn infer(
        &mut self,
        term: &Term<'_, VarId>,
    ) -> (
        &'ast Term<'ast, UnifierVarId<'ty>>,
        &'ty Type<'ty, UnifierTypeId>,
    ) {
        match term {
            // Abstraction inference  is done by creating two new unifiers <arg> and <body>
            // The abstraction body is checked against these fresh type variables
            // The resulting type of the inference is function type <arg> -> <body>
            Abstraction { arg, body } => {
                let arg_ty = self.fresh_unifier_var();
                let body_ty = self.fresh_unifier_var();

                self.local_env.insert(*arg, arg_ty);
                let body = self.check(body, body_ty);
                self.local_env.remove(arg);

                let typed_term = self.ast_arena.alloc(Abstraction {
                    arg: TypedVarId {
                        var: *arg,
                        ty: arg_ty,
                    },
                    body,
                });
                let ty = self.ty_arena.alloc(Type::FunTy(arg_ty, body_ty));
                (typed_term, ty)
            }
            // Application inference starts by inferring types for the func of the application.
            // We equate this inferred type to a function type, generating fresh unifiers if
            // needed.
            // We then check the arg of the application against the arg type of the function type
            // The resulting type of this application is the function result type.
            Application { func, arg } => {
                let (typed_func, fun_ty) = self.infer(func);
                // Optimization: eagerly use FunTy if available. Otherwise dispatch fresh unifiers
                // for arg and ret type.
                let (arg_ty, ret_ty) = match fun_ty {
                    FunTy(arg_ty, ret_ty) => (*arg_ty, *ret_ty),
                    ty => {
                        let arg_ty = self.fresh_unifier_var();
                        let ret_ty = self.fresh_unifier_var();
                        self.equate_ty(ty, self.ty_arena.alloc(FunTy(arg_ty, ret_ty)));
                        (arg_ty, ret_ty)
                    }
                };

                let typed_arg = self.check(arg, arg_ty);

                let typed_app = self.ast_arena.alloc(Application {
                    func: typed_func,
                    arg: typed_arg,
                });
                (typed_app, ret_ty)
            }
            // If the variable is in environemnt return it's type, otherwise return an error.
            Variable(var) => {
                if let Some(ty) = self.local_env.get(var) {
                    (
                        self.ast_arena.alloc(Variable(TypedVarId { var: *var, ty })),
                        ty,
                    )
                } else {
                    self.errors.push(TypeCheckError::VarNotDefined(*var));
                    (
                        self.ast_arena.alloc(Variable(TypedVarId {
                            var: *var,
                            ty: self.error_ty,
                        })),
                        self.error_ty,
                    )
                }
            }
        }
    }

    fn solve_constraints(&mut self) -> GeneralizedSubst<'ty> {
        let solver: ConstraintSolver<'ty> = ConstraintSolver::new(self.ty_arena);

        // TODO: We need to represent our unifiers as input constraints
        // We currently naively create constraints for unifiers, solve them, and then copy them
        // back into the unifiers.
        // But if possible it'd be better to recognize them as unifiers and update them in place as
        // we solve. However this would complicate interaction finding/solving.
        let unifier_constrs = self.unifiers.iter().map(|(unifier, ty)| {
            let unifier_ty = self.ty_arena.alloc(VarTy(unifier.into()));
            Constraint::Eq(unifier_ty, ty)
        });
        let constrs = self
            .constraints
            .drain(..)
            .chain(unifier_constrs)
            .collect::<Vec<_>>();
        let (subst, residual, errors) = solver.solve(constrs.into_iter());

        // Update unifiers that got solved during the process, generalize any unifiers that are free by introducing fresh type variables
        for (uv, opt_ty) in self.unifiers.raw_iter_mut() {
            *opt_ty = Some(
                opt_ty
                    .map(|ty| ty.apply(self.ty_arena, &subst))
                    .or(subst.get(uv))
                    .unwrap_or_else(|| {
                        let ty_var = self.type_var_supply.push(());
                        self.ty_arena.alloc(VarTy(UnifierTypeId::Var(ty_var)))
                    }),
            )
        }
        self.errors.extend(errors);
        if !residual.is_empty() {
            self.errors
                .push(TypeCheckError::UnsolvedConstraints(residual));
        }

        // During generalization we store all unifiers in self.unifiers. So we can construct a full
        // generalized subst by assigning a type to each of them
        GeneralizedSubst::from_iter(self.unifiers.raw_iter().map(|(uv, opt_ty)| {
            opt_ty
                .map(|ty| ty.apply(self.ty_arena, &subst))
                .or(subst.get(uv))
                .unwrap_or_else(|| {
                    let ty_var = self.type_var_supply.push(());
                    self.ty_arena.alloc(VarTy(UnifierTypeId::Var(ty_var)))
                })
        }))
    }
}

/// Type check a term.
/// This is a convenience wrapper to instantiate a type checker and run it's pipeline against a
/// term.
/// Returns the typed ast and any errors encountered.
pub fn tc_term<'ast, 'ty>(
    ast_arena: &'ast Bump,
    ty_arena: &'ty Bump,
    ast: Ast<'_, VarId>,
) -> (
    Ast<'ast, TypedVarId<'ty, TcVar>>,
    &'ty Type<'ty, TcVar>,
    Vec<TypeCheckError<'ty>>,
) {
    let mut tc = TypeChecker::new(ast_arena, ty_arena);

    let (typed_term, ty) = tc.infer(ast.root());
    let subst = tc.solve_constraints();

    println!("{:?}", subst);

    let (typed_term, ty) = (
        typed_term.apply((ast_arena, ty_arena), &subst),
        ty.zonk(ty_arena, &subst),
    );
    // TODO: Use subst to zonk our r|esulting typed_term and type
    // TODO: Figure out how to do Spans
    (Ast::new(FxHashMap::default(), typed_term), ty, tc.errors)
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! unifier_var_ty {
        ($e:expr) => {
            Type::VarTy(UnifierTypeId::Unifier(TcUnifierVar($e)))
        };
    }

    macro_rules! bag {
        () => {{ FxHashMap::default() }};
        ($($key:expr => [$($ty:expr),*]),*) => {{
            let mut bag = FxHashMap::default();
            $(bag.insert($key, vec![$($ty),*]);)*
            bag
        }};
    }

    #[test]
    fn test_tc_abs() {
        let arena = Bump::new();
        let untyped_ast = Ast::new(
            FxHashMap::default(),
            arena.alloc(Abstraction {
                arg: VarId(0),
                body: arena.alloc(Abstraction {
                    arg: VarId(1),
                    body: arena.alloc(Variable(VarId(0))),
                }),
            }),
        );

        let (typed_ast, ty, errors) = tc_term(&arena, &arena, untyped_ast);

        assert_eq!(errors, vec![], "no type checking errors");
        assert_eq!(
            ty,
            &FunTy(&VarTy(TcVar(0)), &FunTy(&VarTy(TcVar(1)), &VarTy(TcVar(0)))),
            "inferred types match"
        );
        assert_eq!(
            typed_ast.root(),
            &Abstraction {
                arg: TypedVarId {
                    var: VarId(0),
                    ty: &VarTy(TcVar(0))
                },
                body: &Abstraction {
                    arg: TypedVarId {
                        var: VarId(1),
                        ty: &VarTy(TcVar(1))
                    },
                    body: &Variable(TypedVarId {
                        var: VarId(0),
                        ty: &VarTy(TcVar(0)),
                    })
                }
            },
            "typed asts match"
        )
    }

    #[test]
    fn test_tc_interactions_eq() {
        let tv = UnifierTypeId::Unifier(TcUnifierVar(0));
        let tv0 = unifier_var_ty!(1);
        let fun = &FunTy(&unifier_var_ty!(2), &unifier_var_ty!(3));
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
        let fun: &UnifierType<'_> = arn.alloc(FunTy(
            arn.alloc(unifier_var_ty!(3)),
            arn.alloc(FunTy(arn.alloc(unifier_var_ty!(2)), arn.alloc(VarTy(b)))),
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
                    ty: &FunTy(&unifier_var_ty!(3), &FunTy(&unifier_var_ty!(2), &VarTy(b)))
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
        let int: &UnifierType<'_> = arena.alloc(IntTy);

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
        let fun = arena.alloc(FunTy(c, b));

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
        let fun = arena.alloc(FunTy(a, b));
        let int: &'_ UnifierType<'_> = arena.alloc(IntTy);
        let int_fun = arena.alloc(FunTy(int, int));

        let (subst, residual, _errors) =
            cs.solve(vec![Constraint::Eq(c, fun), Constraint::Eq(c, int_fun)].into_iter());

        assert_eq!(residual, vec![]);
        assert_eq!(subst, UnifierSubst::from_iter([int, int, int_fun,]));
    }
}
