use std::fmt::Debug;

use aiahr_core::{
    ast::{Ast, Term, Term::*},
    id::{IdGen, ItemId, ModuleId, VarId},
};
use bumpalo::Bump;
use rustc_hash::FxHashMap;

mod constraint;
mod ty;
mod subst;

use constraint::{Constraint, ConstraintSolver};
use ty::{*, Type::*};
use subst::{GeneralizedSubst, UnifierSubst, Substitutable, InsertType};


/// A type scheme. This is a type with all it's free type variables bound and it's required
/// constraints explicitly listed
struct Scheme<'ty, TV> {
    vars: IdGen<TV, ()>,
    constraints: Vec<Constraint<'ty>>,
    ty: &'ty Type<'ty, TV>,
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

/// Errors that may be produced during type checking
#[derive(Debug, PartialEq, Eq)]
pub enum TypeCheckError<'ty> {
    /// A variable we expected to exist with a type, did not
    VarNotDefined(VarId),
    // TODO: Consider whether we should expose the unifier var stuff in errors here or not?
    UnsolvedTyVariable(UnifierTypeId),
    UnresolvedTypes(TcUnifierVar, Vec<&'ty UnifierType<'ty>>),
    // TODO: Don't leak constraints in the public API here
    UnsolvedConstraints(Vec<Constraint<'ty>>),
    ItemNotDefined((ModuleId, ItemId)),
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
    /// Store type schemes for global variables.
    global_env: FxHashMap<(ModuleId, ItemId), Scheme<'ty, TcVar>>,
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

impl<'ast, 'ty> TypeChecker<'ast, 'ty> {
    fn new(ast_arena: &'ast Bump, ty_arena: &'ty Bump) -> Self {
        Self {
            ast_arena,
            ty_arena,
            error_ty: ty_arena.alloc(ErrorTy),
            local_env: FxHashMap::default(),
            global_env: FxHashMap::default(),
            errors: vec![],
            unifiers: UnifierSubst::default(),
            constraints: vec![],
            type_var_supply: IdGen::new(),
        }
    }

    #[cfg(test)]
    fn with_global_env(
        ast_arena: &'ast Bump,
        ty_arena: &'ty Bump,
        global_env: FxHashMap<(ModuleId, ItemId), Scheme<'ty, TcVar>>,
    ) -> Self {
        Self {
            global_env,
            ..Self::new(ast_arena, ty_arena)
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
            Item(item) => {
                match self.global_env.get(item) {
                    None => {
                        self.errors.push(TypeCheckError::ItemNotDefined(*item));
                        (self.ast_arena.alloc(Item(*item)), self.error_ty)
                    }
                    Some(scheme) => {
                        // Wrap this up as an instantiate method eventually
                        let var_to_unifier = scheme.vars.create_from(|ty_var, _| {
                            let unifier = self.unifiers.fresh_unifier_with(
                                self.ty_arena.alloc(VarTy(UnifierTypeId::Var(ty_var))),
                            );
                            self.ty_arena.alloc(VarTy(UnifierTypeId::Unifier(unifier))) as &_
                        });

                        let ty = scheme.ty.apply(self.ty_arena, &var_to_unifier);
                        self.constraints.extend(
                            scheme
                                .constraints
                                .iter()
                                .map(|constr| constr.apply(self.ty_arena, &var_to_unifier)),
                        );
                        (self.ast_arena.alloc(Item(*item)), ty)
                    }
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
pub fn tc_term<'ast: 'ty, 'ty>(
    ast_arena: &'ast Bump,
    ty_arena: &'ty Bump,
    ast: Ast<'_, VarId>,
) -> (
    Ast<'ast, TypedVarId<'ty, TcVar>>,
    &'ty Type<'ty, TcVar>,
    Vec<TypeCheckError<'ty>>,
) {
    tc_term_with(TypeChecker::new(ast_arena, ty_arena), ast)
}

fn tc_term_with<'ast: 'ty, 'ty>(
    mut tc: TypeChecker<'ast, 'ty>,
    ast: Ast<'_, VarId>,
) -> (
    Ast<'ast, TypedVarId<'ty, TcVar>>,
    &'ty Type<'ty, TcVar>,
    Vec<TypeCheckError<'ty>>,
) {
    let (typed_term, ty) = tc.infer(ast.root());
    let subst = tc.solve_constraints();

    println!("{:?}", subst);

    let (typed_term, ty) = (
        typed_term.apply((tc.ast_arena, tc.ty_arena), &subst),
        ty.zonk(tc.ty_arena, &subst),
    );
    // TODO: Use subst to zonk our r|esulting typed_term and type
    // TODO: Figure out how to do Spans
    (Ast::new(FxHashMap::default(), typed_term), ty, tc.errors)
}

#[cfg(test)]
mod tests {
    use aiahr_core::id::Id;

    use super::*;

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
    fn test_tc_undefined_var_fails() {
        let arena = Bump::new();
        let untyped_ast = Ast::new(
            FxHashMap::default(),
            arena.alloc(Variable(VarId(0)))
        );

        let (_, ty, errors) = tc_term(&arena, &arena, untyped_ast);

        assert_eq!(ty, &ErrorTy);
        assert!(errors.contains(&TypeCheckError::VarNotDefined(VarId(0))))
    }

    #[test]
    fn test_tc_undefined_global_fails() {
        let arena = Bump::new();
        let item = (ModuleId(0), ItemId(0));
        let untyped_ast = Ast::new(
            FxHashMap::default(),
            arena.alloc(Item(item))
        );

        let (_, ty, errors) = tc_term(&arena, &arena, untyped_ast);

        assert_eq!(ty, &ErrorTy);
        assert!(errors.contains(&TypeCheckError::ItemNotDefined(item)))
    }

    #[test]
    fn test_tc_item() {
        let arena = Bump::new();
        let mut vars: IdGen<TcVar, ()> = IdGen::new();
        let arg = vars.push(());
        let ret = vars.push(());

        let scheme = Scheme {
            vars,
            constraints: vec![],
            ty: arena.alloc(FunTy(arena.alloc(VarTy(arg)), arena.alloc(VarTy(ret)))),
        };

        let item = (ModuleId::from_raw(0), ItemId::from_raw(0));

        let global_env = FxHashMap::from_iter([(item, scheme)]);

        let tc = TypeChecker::with_global_env(&arena, &arena, global_env);
        let (typed_term, ty, errors) = tc_term_with(
            tc,
            Ast::new(
                FxHashMap::default(),
                arena.alloc(Abstraction {
                    arg: VarId(0),
                    body: arena.alloc(Application {
                        func: arena.alloc(Item(item)),
                        arg: arena.alloc(Variable(VarId(0))),
                    }),
                }),
            ),
        );

        assert_eq!(errors, vec![]);
        assert_eq!(
            typed_term.root(),
            &Abstraction {
                arg: TypedVarId {
                    var: VarId(0),
                    ty: &VarTy(arg)
                },
                body: &Application {
                    func: &Item(item),
                    arg: &Variable(TypedVarId {
                        var: VarId(0),
                        ty: &VarTy(arg)
                    })
                }
            }
        );
        assert_eq!(ty, &FunTy(&VarTy(arg), &VarTy(ret)));
    }
}
