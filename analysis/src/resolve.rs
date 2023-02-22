use std::iter;

use crate::{
    base::BaseNames,
    name::{BaseName, ModuleName, Name, NameKinded},
    names::{LocalIds, Names},
    ops::{IdOps, InsertResult},
};
use aiahr_core::{
    cst::{
        self, Annotation, Constraint, EffectOp, Field, IdField, ProductRow, Qualifiers, Quantifier,
        Row, RowAtom, Scheme, SchemeAnnotation, Separated, SumRow, Type, TypeAnnotation, TypeRow,
    },
    diagnostic::{
        nameres::{NameKind, NameKinds, NameResolutionError, RejectionReason, Suggestion},
        DiagnosticSink,
    },
    id::{EffectId, EffectOpId, ItemId, ModuleId, TyVarId, VarId},
    memory::handle::RefHandle,
    nst,
    option::Transpose,
    span::{Span, SpanOf, Spanned},
};
use bumpalo::Bump;

const TERM_KINDS: NameKinds = NameKinds::EFFECT_OP
    .union(NameKinds::ITEM)
    .union(NameKinds::VAR);

// Allocates the items in `iter` on the given arena, but only if they are all `Some(..)`.
fn alloc_all<T, I>(arena: &Bump, iter: I) -> Option<&[T]>
where
    I: Iterator<Item = Option<T>>,
{
    iter.collect::<Option<Vec<T>>>()
        .map(|v| arena.alloc_slice_fill_iter(v.into_iter()) as &[T])
}

// Tries to map the given function over the elements of `separated`, returning all errors.
fn resolve_separated<'a, A, B, F>(
    arena: &'a Bump,
    separated: &Separated<'_, A>,
    f: F,
) -> Option<Separated<'a, B>>
where
    F: FnMut(&A) -> Option<B>,
{
    let mut f = f;
    let first = f(&separated.first);
    let elems = alloc_all(
        arena,
        separated.elems.iter().map(|(c, a)| Some((*c, f(a)?))),
    );
    Some(Separated {
        first: first?,
        elems: elems?,
        comma: separated.comma,
    })
}

// Tries to map the given function over the target.
fn resolve_id_field<'s, A, B, F>(field: &IdField<'s, A>, f: F) -> Option<IdField<'s, B>>
where
    F: FnOnce(&A) -> Option<B>,
{
    Some(IdField {
        label: field.label,
        sep: field.sep,
        target: f(&field.target)?,
    })
}

// Tries to map the given function over the targets of `prod`.
fn resolve_product_row<'a, 's, A, B, F>(
    arena: &'a Bump,
    prod: &ProductRow<'_, 's, A>,
    f: F,
) -> Option<ProductRow<'a, 's, B>>
where
    F: FnMut(&A) -> Option<B>,
{
    let mut f = f;
    let fields = if let Some(cs) = &prod.fields {
        Some(resolve_separated(arena, cs, |field| {
            resolve_id_field(field, &mut f)
        })?)
    } else {
        None
    };
    Some(ProductRow {
        lbrace: prod.lbrace,
        fields,
        rbrace: prod.rbrace,
    })
}

// Tries to map the given function over the target of `sum`.
fn resolve_sum_row<'s, A, B, F>(sum: &SumRow<'s, A>, f: F) -> Option<SumRow<'s, B>>
where
    F: FnOnce(&A) -> Option<B>,
{
    Some(SumRow {
        langle: sum.langle,
        field: resolve_id_field(&sum.field, f)?,
        rangle: sum.rangle,
    })
}

// Inserts a symbol into the current scope using the given function.
fn insert_symbol<'s, I, F, E>(
    kind: NameKind,
    var: SpanOf<RefHandle<'s, str>>,
    f: F,
    errors: &mut E,
) -> SpanOf<I>
where
    F: FnOnce(SpanOf<RefHandle<'s, str>>) -> InsertResult<I>,
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    let InsertResult { id, existing } = f(var);
    if let Some(orig) = existing {
        errors.add(NameResolutionError::Duplicate {
            name: var.value,
            kind,
            original: orig.span(),
            duplicate: var.span(),
        });
    }
    var.span().of(id)
}

// Inserts a variable into the current scope.
fn insert_var<'s, E>(
    var: SpanOf<RefHandle<'s, str>>,
    names: &mut Names<'_, '_, 's>,
    errors: &mut E,
) -> SpanOf<VarId>
where
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    insert_symbol(NameKind::Var, var, |v| names.insert_var(v), errors)
}

// Inserts a variable into the current scope.
fn insert_ty_var<'s, E>(
    var: SpanOf<RefHandle<'s, str>>,
    names: &mut Names<'_, '_, 's>,
    errors: &mut E,
) -> SpanOf<TyVarId>
where
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    insert_symbol(NameKind::TyVar, var, |v| names.insert_ty_var(v), errors)
}

// A module or some other value.
#[derive(Clone, Copy, Debug)]
enum ModuleOr<T> {
    Module(ModuleId),
    Value(T),
}

// Resolves a symbol or suggests names that the user might have intended.
fn resolve_or_suggest<'s, I, N, T, F>(
    iterator: I,
    names: &Names<'_, '_, 's>,
    mut f: F,
) -> Result<T, Vec<Suggestion<'s>>>
where
    N: Copy,
    I: Iterator<Item = SpanOf<N>>,
    F: FnMut(N) -> Result<T, RejectionReason>,
    Name: From<N>,
{
    let mut suggestions = Vec::new();
    for name in iterator {
        match f(name.value) {
            Ok(x) => {
                return Ok(x);
            }
            Err(why_not) => {
                suggestions.push(Suggestion {
                    name: names.get(name.value),
                    why_not,
                });
            }
        }
    }
    Err(suggestions)
}

// Resolves a symbol to a value of type `T`, using the given function to decide which names are
// valid for the symbol.
fn resolve_symbol<'s, T, F, E>(
    var: SpanOf<RefHandle<'s, str>>,
    names: &Names<'_, '_, 's>,
    errors: &mut E,
    f: F,
) -> Option<T>
where
    F: FnMut(Name) -> Result<T, RejectionReason>,
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    match resolve_or_suggest(names.find(var.value), names, f) {
        Ok(x) => Some(x),
        Err(suggestions) => {
            errors.add(NameResolutionError::NotFound {
                name: var,
                context_module: None,
                suggestions,
            });
            None
        }
    }
}

// Resolves a symbol in a given module to a value of type `T`, using the given function to decide
// which names are valid for the symbol.
fn resolve_symbol_in<'s, T, F, E>(
    module: ModuleId,
    var: SpanOf<RefHandle<'s, str>>,
    names: &Names<'_, '_, 's>,
    errors: &mut E,
    f: F,
) -> Option<T>
where
    F: FnMut(BaseName) -> Result<T, RejectionReason>,
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    match resolve_or_suggest(names.find_in(module, var.value), names, f) {
        Ok(x) => Some(x),
        Err(suggestions) => {
            errors.add(NameResolutionError::NotFound {
                name: var,
                context_module: None,
                suggestions,
            });
            None
        }
    }
}

// Resolves an effect operation symbolto a value of type `T`, using the given function to decide
// which names are valid for the symbol.
fn resolve_operation_symbol<'s, T, F, E>(
    module: ModuleId,
    effect: EffectId,
    var: SpanOf<RefHandle<'s, str>>,
    names: &Names<'_, '_, 's>,
    errors: &mut E,
    mut f: F,
) -> Option<T>
where
    F: FnMut(EffectOpId) -> Result<T, RejectionReason>,
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    match resolve_or_suggest(
        names
            .get_effect(module, effect)
            .find(var.value)
            .map(|sn| sn.map(|o| (module, effect, o))),
        names,
        |(_, _, o)| f(o),
    ) {
        Ok(x) => Some(x),
        Err(suggestions) => {
            errors.add(NameResolutionError::NotFound {
                name: var,
                context_module: None,
                suggestions,
            });
            None
        }
    }
}

// Resolves a symbol to a type name.
fn resolve_type_symbol<'s, E>(
    var: SpanOf<RefHandle<'s, str>>,
    names: &mut Names<'_, '_, 's>,
    errors: &mut E,
) -> Option<SpanOf<TyVarId>>
where
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    Some(var.map(|v| {
        let existing = names.find(v).find_map(|sn| match sn.value {
            Name::TyVar(t) => Some(t),
            _ => None,
        });
        existing.unwrap_or_else(|| insert_ty_var(var, names, errors).value)
    }))
}

// Resolves a type-level row.
fn resolve_row<'a, 's, C, D, F, E>(
    arena: &'a Bump,
    row: &Row<'_, RefHandle<'s, str>, C>,
    mut f: F,
    names: &mut Names<'_, 'a, 's>,
    errors: &mut E,
) -> Option<Row<'a, TyVarId, D>>
where
    F: FnMut(&C, &mut Names<'_, 'a, 's>, &mut E) -> Option<D>,
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    Some(match row {
        Row::Concrete(concrete) => {
            Row::Concrete(resolve_separated(arena, concrete, |c| f(c, names, errors))?)
        }
        Row::Variable(variables) => Row::Variable(resolve_separated(arena, variables, |var| {
            resolve_type_symbol(*var, names, errors)
        })?),
        Row::Mixed {
            concrete,
            vbar,
            variables,
        } => {
            let concrete = resolve_separated(arena, concrete, |c| f(c, names, errors));
            let variables = resolve_separated(arena, variables, |var| {
                resolve_type_symbol(*var, names, errors)
            });
            Row::Mixed {
                concrete: concrete?,
                vbar: *vbar,
                variables: variables?,
            }
        }
    })
}

// Resolves a row of types.
fn resolve_type_row<'a, 's, E>(
    arena: &'a Bump,
    row: &TypeRow<'_, 's, RefHandle<'s, str>>,
    names: &mut Names<'_, 'a, 's>,
    errors: &mut E,
) -> Option<TypeRow<'a, 's, TyVarId>>
where
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    resolve_row(
        arena,
        row,
        |field, names, errors| resolve_id_field(field, |ty| resolve_type(arena, ty, names, errors)),
        names,
        errors,
    )
}

/// Resolves an Aiahr type.
pub fn resolve_type<'a, 's, E>(
    arena: &'a Bump,
    type_: &Type<'_, 's, RefHandle<'s, str>>,
    names: &mut Names<'_, 'a, 's>,
    errors: &mut E,
) -> Option<&'a Type<'a, 's, TyVarId>>
where
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    Some(
        arena.alloc(match type_ {
            Type::Named(var) => Type::Named(resolve_type_symbol(*var, names, errors)?),
            Type::Sum {
                langle,
                variants,
                rangle,
            } => Type::Sum {
                langle: *langle,
                variants: resolve_type_row(arena, variants, names, errors)?,
                rangle: *rangle,
            },
            Type::Product {
                lbrace,
                fields,
                rbrace,
            } => Type::Product {
                lbrace: *lbrace,
                fields: fields
                    .map(|fields| resolve_type_row(arena, &fields, names, errors))
                    .transpose()?,
                rbrace: *rbrace,
            },
            Type::Function {
                domain,
                arrow,
                codomain,
            } => {
                let domain = resolve_type(arena, domain, names, errors);
                let codomain = resolve_type(arena, codomain, names, errors);
                Type::Function {
                    domain: domain?,
                    arrow: *arrow,
                    codomain: codomain?,
                }
            }
            Type::Parenthesized { lpar, type_, rpar } => Type::Parenthesized {
                lpar: *lpar,
                type_: resolve_type(arena, type_, names, errors)?,
                rpar: *rpar,
            },
        }) as &_,
    )
}

// Resolves a row atom.
fn resolve_row_atom<'a, 's, E>(
    arena: &'a Bump,
    atom: &RowAtom<'_, 's, RefHandle<'s, str>>,
    names: &mut Names<'_, 'a, 's>,
    errors: &mut E,
) -> Option<RowAtom<'a, 's, TyVarId>>
where
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    Some(match atom {
        RowAtom::Concrete { lpar, fields, rpar } => RowAtom::Concrete {
            lpar: *lpar,
            fields: resolve_separated(arena, fields, |field| {
                resolve_id_field(field, |ty| resolve_type(arena, ty, names, errors))
            })?,
            rpar: *rpar,
        },
        RowAtom::Variable(var) => RowAtom::Variable(resolve_type_symbol(*var, names, errors)?),
    })
}

// Resolves a type constraint.
fn resolve_constraint<'a, 's, E>(
    arena: &'a Bump,
    constraint: &Constraint<'_, 's, RefHandle<'s, str>>,
    names: &mut Names<'_, 'a, 's>,
    errors: &mut E,
) -> Option<Constraint<'a, 's, TyVarId>>
where
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    Some(match constraint {
        Constraint::RowSum {
            lhs,
            plus,
            rhs,
            eq,
            goal,
        } => {
            let lhs = resolve_row_atom(arena, lhs, names, errors);
            let rhs = resolve_row_atom(arena, rhs, names, errors);
            let goal = resolve_row_atom(arena, goal, names, errors);
            Constraint::RowSum {
                lhs: lhs?,
                plus: *plus,
                rhs: rhs?,
                eq: *eq,
                goal: goal?,
            }
        }
    })
}

// Resolves a quantifier.
fn resolve_quantifier<'s, E>(
    quantifier: &Quantifier<RefHandle<'s, str>>,
    names: &mut Names<'_, '_, 's>,
    errors: &mut E,
) -> Quantifier<TyVarId>
where
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    Quantifier {
        forall: quantifier.forall,
        var: insert_ty_var(quantifier.var, names, errors),
        dot: quantifier.dot,
    }
}

// Resolves a set of quantifiers.
fn resolve_qualifiers<'a, 's, E>(
    arena: &'a Bump,
    qualifiers: &Qualifiers<'_, 's, RefHandle<'s, str>>,
    names: &mut Names<'_, 'a, 's>,
    errors: &mut E,
) -> Option<Qualifiers<'a, 's, TyVarId>>
where
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    Some(Qualifiers {
        constraints: resolve_separated(arena, &qualifiers.constraints, |constraint| {
            resolve_constraint(arena, constraint, names, errors)
        })?,
        arrow: qualifiers.arrow,
    })
}

/// Resolves a polymorphic type.
pub fn resolve_scheme<'a, 's, E>(
    arena: &'a Bump,
    scheme: &Scheme<'_, 's, RefHandle<'s, str>>,
    names: &mut Names<'_, 'a, 's>,
    errors: &mut E,
) -> Option<&'a Scheme<'a, 's, TyVarId>>
where
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    names.subscope(|scope| {
        let quantifiers = arena.alloc_slice_fill_iter(
            scheme
                .quantifiers
                .iter()
                .map(|quant| resolve_quantifier(quant, scope, errors)),
        ) as &[_];
        let qualifiers = scheme
            .qualifiers
            .map(|quals| resolve_qualifiers(arena, &quals, scope, errors))
            .transpose();
        let type_ = resolve_type(arena, scheme.type_, scope, errors);
        Some(arena.alloc(Scheme {
            quantifiers,
            qualifiers: qualifiers?,
            type_: type_?,
        }) as &_)
    })
}

// The possible meanings of a `DotAccess` term.
#[derive(Debug)]
enum DotResolution<'a, 's> {
    Module(ModuleId),
    Effect(ModuleId, EffectId),
    EffectOp(ModuleId, EffectId, EffectOpId),
    Item(ModuleId, ItemId),
    FieldAccess {
        base: &'a nst::Term<'a, 's>,
        dot: Span,
        field: SpanOf<RefHandle<'s, str>>,
    },
}

impl<'a, 's> From<BaseName> for DotResolution<'a, 's> {
    fn from(base: BaseName) -> Self {
        match base {
            BaseName::Module(m) => DotResolution::Module(m),
            BaseName::Effect(m, e) => DotResolution::Effect(m, e),
            BaseName::EffectOp(m, e, o) => DotResolution::EffectOp(m, e, o),
            BaseName::Item(m, i) => DotResolution::Item(m, i),
        }
    }
}

/// Resolves the given pattern, accumulating bindings into `names`.
///
/// Note that this currently cannot return `None`, although it can emit errors.
pub fn resolve_pattern<'a, 's, E>(
    arena: &'a Bump,
    pattern: &cst::Pattern<'_, 's>,
    names: &mut Names<'_, 'a, 's>,
    errors: &mut E,
) -> Option<&'a nst::Pattern<'a, 's>>
where
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    Some(arena.alloc(match pattern {
        cst::Pattern::ProductRow(pr) => {
            nst::Pattern::ProductRow(resolve_product_row(arena, pr, |target| {
                resolve_pattern(arena, target, names, errors)
            })?)
        }
        cst::Pattern::SumRow(sr) => nst::Pattern::SumRow(resolve_sum_row(sr, |target| {
            resolve_pattern(arena, target, names, errors)
        })?),
        cst::Pattern::Whole(var) => nst::Pattern::Whole(insert_var(*var, names, errors)),
    }))
}

// Resolves a type annotation.
fn resolve_type_annotation<'a, 's, E>(
    arena: &'a Bump,
    annotation: &TypeAnnotation<'_, 's, RefHandle<'s, str>>,
    names: &mut Names<'_, 'a, 's>,
    errors: &mut E,
) -> Option<TypeAnnotation<'a, 's, TyVarId>>
where
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    Some(Annotation {
        colon: annotation.colon,
        type_: resolve_type(arena, annotation.type_, names, errors)?,
    })
}

// Resolves a scheme annotation.
fn resolve_scheme_annotation<'a, 's, E>(
    arena: &'a Bump,
    annotation: &SchemeAnnotation<'_, 's, RefHandle<'s, str>>,
    names: &mut Names<'_, 'a, 's>,
    errors: &mut E,
) -> Option<SchemeAnnotation<'a, 's, TyVarId>>
where
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    Some(Annotation {
        colon: annotation.colon,
        type_: resolve_scheme(arena, annotation.type_, names, errors)?,
    })
}

// Resolves nested `DotAccess` terms.
fn resolve_nested_dots<'a, 's, E>(
    arena: &'a Bump,
    base: &cst::Term<'_, 's>,
    dot: Span,
    field: SpanOf<RefHandle<'s, str>>,
    names: &mut Names<'_, 'a, 's>,
    errors: &mut E,
) -> Option<DotResolution<'a, 's>>
where
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    Some(match base {
        // (base2 . field2) . field
        cst::Term::DotAccess {
            base: base2,
            dot: dot2,
            field: field2,
        } => match resolve_nested_dots(arena, base2, *dot2, *field2, names, errors)? {
            // m . field
            DotResolution::Module(m) => resolve_symbol_in(m, field, names, errors, |name| {
                Ok(DotResolution::from(name))
            })?,
            DotResolution::Effect(m, e) => {
                resolve_operation_symbol(m, e, field, names, errors, |o| {
                    Ok(DotResolution::EffectOp(m, e, o))
                })?
            }
            DotResolution::EffectOp(_, _, _) => {
                errors.add(NameResolutionError::WrongKind {
                    expr: base.span(),
                    actual: NameKind::EffectOp,
                    expected: !NameKinds::EFFECT_OP,
                });
                return None;
            }
            DotResolution::Item(m, i) => DotResolution::FieldAccess {
                base: arena.alloc(nst::Term::ItemRef(base.span().of((m, i)))),
                dot,
                field,
            },
            // (base2n . field2n) . field
            DotResolution::FieldAccess {
                base: base2n,
                dot: dot2n,
                field: field2n,
            } => DotResolution::FieldAccess {
                base: arena.alloc(nst::Term::FieldAccess {
                    base: base2n,
                    dot: dot2n,
                    field: field2n,
                }),
                dot,
                field,
            },
        },
        // n . field
        cst::Term::SymbolRef(n) => match resolve_symbol(*n, names, errors, |name| match name {
            Name::Module(m) => Ok(ModuleOr::Module(m)),
            Name::Item(m, i) => Ok(ModuleOr::Value(nst::Term::ItemRef(base.span().of((m, i))))),
            Name::Var(v) => Ok(ModuleOr::Value(nst::Term::VariableRef(base.span().of(v)))),
            _ => Err(RejectionReason::WrongKind {
                actual: name.kind(),
                expected: NameKinds::MODULE | TERM_KINDS,
            }),
        })? {
            ModuleOr::Module(m) => resolve_symbol_in(m, field, names, errors, |name| {
                Ok(DotResolution::from(name))
            })?,
            ModuleOr::Value(value) => DotResolution::FieldAccess {
                base: arena.alloc(value),
                dot,
                field,
            },
        },
        // (expr) . field
        _ => DotResolution::FieldAccess {
            base: resolve_term(arena, base, names, errors)?,
            dot,
            field,
        },
    })
}

/// Resolves the given term.
pub fn resolve_term<'a, 's, E>(
    arena: &'a Bump,
    term: &cst::Term<'_, 's>,
    names: &mut Names<'_, 'a, 's>,
    errors: &mut E,
) -> Option<&'a nst::Term<'a, 's>>
where
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    Some(arena.alloc(match term {
        cst::Term::Binding {
            var,
            annotation,
            eq,
            value,
            semi,
            expr,
        } => {
            let value = resolve_term(arena, value, names, errors);
            names.subscope(|scope| {
                let var = insert_var(*var, scope, errors);
                let annotation = annotation
                    .map(|annotation| resolve_type_annotation(arena, &annotation, scope, errors))
                    .transpose();
                let expr = resolve_term(arena, expr, scope, errors);

                Some(nst::Term::Binding {
                    var,
                    annotation: annotation?,
                    eq: *eq,
                    value: value?,
                    semi: *semi,
                    expr: expr?,
                })
            })?
        }
        cst::Term::Handle {
            with,
            handler,
            do_,
            expr,
        } => {
            let handler = resolve_term(arena, handler, names, errors);
            let expr = resolve_term(arena, expr, names, errors);
            nst::Term::Handle {
                with: *with,
                handler: handler?,
                do_: *do_,
                expr: expr?,
            }
        }
        cst::Term::Abstraction {
            lbar,
            arg,
            annotation,
            rbar,
            body,
        } => names.subscope(|scope| {
            let arg = insert_var(*arg, scope, errors);
            let annotation = annotation
                .map(|annotation| resolve_type_annotation(arena, &annotation, scope, errors))
                .transpose();
            let body = resolve_term(arena, body, scope, errors);
            Some(nst::Term::Abstraction {
                lbar: *lbar,
                arg,
                annotation: annotation?,
                rbar: *rbar,
                body: body?,
            })
        })?,
        cst::Term::Application {
            func,
            lpar,
            arg,
            rpar,
        } => {
            let func = resolve_term(arena, func, names, errors);
            let arg = resolve_term(arena, arg, names, errors);
            nst::Term::Application {
                func: func?,
                lpar: *lpar,
                arg: arg?,
                rpar: *rpar,
            }
        }
        cst::Term::ProductRow(pr) => {
            nst::Term::ProductRow(resolve_product_row(arena, pr, |target| {
                resolve_term(arena, target, names, errors)
            })?)
        }
        cst::Term::SumRow(sr) => nst::Term::SumRow(resolve_sum_row(sr, |target| {
            resolve_term(arena, target, names, errors)
        })?),
        t @ cst::Term::DotAccess { base, dot, field } => {
            match resolve_nested_dots(arena, base, *dot, *field, names, errors)? {
                DotResolution::Module(_) => {
                    errors.add(NameResolutionError::WrongKind {
                        expr: t.span(),
                        actual: NameKind::Module,
                        expected: TERM_KINDS,
                    });
                    return None;
                }
                DotResolution::Effect(_, _) => {
                    errors.add(NameResolutionError::WrongKind {
                        expr: t.span(),
                        actual: NameKind::Effect,
                        expected: TERM_KINDS,
                    });
                    return None;
                }
                DotResolution::EffectOp(m, e, o) => nst::Term::EffectOpRef(t.span().of((m, e, o))),
                DotResolution::Item(m, i) => nst::Term::ItemRef(t.span().of((m, i))),
                DotResolution::FieldAccess { base, dot, field } => {
                    nst::Term::FieldAccess { base, dot, field }
                }
            }
        }
        cst::Term::Match {
            match_,
            langle,
            cases,
            rangle,
        } => nst::Term::Match {
            match_: *match_,
            langle: *langle,
            cases: resolve_separated(arena, cases, |field| {
                names.subscope(|scope| {
                    let pattern = resolve_pattern(arena, field.label, scope, errors);
                    let target = resolve_term(arena, field.target, scope, errors);
                    Some(Field {
                        label: pattern?,
                        sep: field.sep,
                        target: target?,
                    })
                })
            })?,
            rangle: *rangle,
        },
        cst::Term::SymbolRef(var) => resolve_symbol(*var, names, errors, |name| match name {
            Name::Item(m, i) => Ok(nst::Term::ItemRef(var.span().of((m, i)))),
            Name::Var(v) => Ok(nst::Term::VariableRef(var.span().of(v))),
            _ => Err(RejectionReason::WrongKind {
                actual: name.kind(),
                expected: TERM_KINDS,
            }),
        })?,
        cst::Term::Parenthesized { lpar, term, rpar } => nst::Term::Parenthesized {
            lpar: *lpar,
            term: resolve_term(arena, term, names, errors)?,
            rpar: *rpar,
        },
    }))
}

/// Resolves an effect operation signature.
fn resolve_effect_op<'a, 's, E>(
    arena: &'a Bump,
    opid: EffectOpId,
    op: &EffectOp<'_, 's, RefHandle<'s, str>, RefHandle<'s, str>>,
    names: &mut Names<'_, 'a, 's>,
    errors: &mut E,
) -> Option<EffectOp<'a, 's, EffectOpId, TyVarId>>
where
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    Some(EffectOp {
        name: op.name.span().of(opid),
        colon: op.colon,
        type_: resolve_type(arena, op.type_, names, errors)?,
    })
}

/// Resolves the given effect.
fn resolve_effect<'a, 's, E>(
    arena: &'a Bump,
    module: ModuleId,
    eid: EffectId,
    effect: Span,
    name: SpanOf<RefHandle<'s, str>>,
    lbrace: Span,
    ops: &[EffectOp<'_, 's, RefHandle<'s, str>, RefHandle<'s, str>>],
    rbrace: Span,
    names: &mut Names<'_, 'a, 's>,
    errors: &mut E,
) -> Option<nst::Item<'a, 's>>
where
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    Some(nst::Item::Effect {
        effect,
        name: name.span().of(eid),
        lbrace,
        ops: arena.alloc_slice_fill_iter(
            iter::zip(
                names
                    .get_effect(module, eid)
                    .iter()
                    .collect::<Vec<_>>()
                    .into_iter(),
                ops.iter(),
            )
            .map(|(opid, op)| resolve_effect_op(arena, opid, op, names, errors)),
        ),
        rbrace,
    })
}

/// Resolves the given item.
pub fn resolve_term_item<'a, 's, E>(
    arena: &'a Bump,
    id: ItemId,
    name: SpanOf<RefHandle<'s, str>>,
    annotation: Option<SchemeAnnotation<'a, 's, RefHandle<'s, str>>>,
    eq: Span,
    value: &cst::Term<'_, 's>,
    names: &mut Names<'_, 'a, 's>,
    errors: &mut E,
) -> Option<nst::Item<'a, 's>>
where
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    Some(nst::Item::Term {
        name: name.span().of(id),
        annotation: annotation
            .map(|annotation| resolve_scheme_annotation(arena, &annotation, names, errors))
            .transpose()?,
        eq,
        value: arena.alloc(resolve_term(arena, value, names, errors)?),
    })
}

/// Data generated by resolving a module.
#[derive(Debug)]
pub struct ModuleResolution<'a, 's> {
    pub locals: LocalIds<'s>,
    pub resolved_items: &'a [Option<nst::Item<'a, 's>>],
}

/// Resolves the given module.
pub fn resolve_module<'a, 's, 'b: 'a, E>(
    arena: &'a Bump,
    items: &[cst::Item<'b, 's>],
    base: BaseNames<'_, 'a, 's>,
    errors: &mut E,
) -> ModuleResolution<'a, 's>
where
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    let mut names = Names::new(&base);
    let resolved_items =
        arena.alloc_slice_fill_iter(base.iter().zip(items.iter()).map(|(name, item)| {
            match (name, item) {
                (
                    ModuleName::Effect(e),
                    &cst::Item::Effect {
                        effect,
                        name,
                        lbrace,
                        ops,
                        rbrace,
                    },
                ) => resolve_effect(
                    arena,
                    base.me(),
                    *e,
                    effect,
                    name,
                    lbrace,
                    ops,
                    rbrace,
                    &mut names,
                    errors,
                ),
                (
                    ModuleName::Item(i),
                    &cst::Item::Term {
                        name,
                        annotation,
                        eq,
                        value,
                    },
                ) => resolve_term_item(arena, *i, name, annotation, eq, value, &mut names, errors),
                _ => panic!(
                    "Expected same kinds of names, got {:?} and {:?}",
                    name, item
                ),
            }
        }));
    ModuleResolution {
        locals: names.into_local_ids(),
        resolved_items,
    }
}

#[cfg(test)]
mod tests {
    use aiahr_core::{
        diagnostic::nameres::{NameKind, NameResolutionError},
        field, h,
        id::ModuleId,
        id_field,
        memory::intern::{InternerByRef, SyncInterner},
        modules::ModuleTree,
        nitem_term, npat_prod, npat_var, nst, nterm_abs, nterm_app, nterm_dot, nterm_item,
        nterm_local, nterm_match, nterm_prod, nterm_sum, nterm_var, nterm_with, quant, scheme,
        span::Span,
        span_of, type_func, type_named, type_prod,
    };
    use aiahr_parser::{
        lexer::aiahr_lexer,
        parser::{aiahr_parser, term, to_stream},
    };
    use assert_matches::assert_matches;
    use bumpalo::Bump;
    use chumsky::Parser;
    use rustc_hash::FxHashMap;

    use crate::{
        module::ModuleNames,
        names::{LocalIds, Names},
        ops::IdOps,
        top_level::BaseBuilder,
    };

    use super::{resolve_module, resolve_term, ModuleResolution};

    const MODNAME: &str = "test_module";

    fn parse_resolve_term<'a: 's, 's, S>(
        arena: &'a Bump,
        interner: &'s S,
        input: &str,
    ) -> (
        Option<&'a nst::Term<'a, 's>>,
        LocalIds<'s>,
        Vec<NameResolutionError<'s>>,
    )
    where
        S: InternerByRef<'s, str>,
    {
        let (m, modules) = {
            let mut modules = ModuleTree::new();
            let m = modules.add_package(interner.intern_by_ref(MODNAME));
            (m, modules)
        };

        let (tokens, eoi) = aiahr_lexer(interner).lex(m, input).unwrap();
        let unresolved = term(arena).parse(to_stream(tokens, eoi)).unwrap();
        let mut errors = Vec::new();

        let mut module_names = FxHashMap::default();
        let base = BaseBuilder::new().build(arena, m, &modules, &mut module_names);
        let mut names = Names::new(&base);

        let resolved = resolve_term(arena, unresolved, &mut names, &mut errors);
        (resolved, names.into_local_ids(), errors)
    }

    fn parse_resolve_module<'a, 's, S>(
        arena: &'a Bump,
        interner: &'s S,
        input: &str,
    ) -> (
        ModuleResolution<'a, 's>,
        &'a ModuleNames<'s>,
        Vec<NameResolutionError<'s>>,
    )
    where
        S: InternerByRef<'s, str>,
    {
        let (m, modules) = {
            let mut modules = ModuleTree::new();
            let m = modules.add_package(interner.intern_by_ref(MODNAME));
            (m, modules)
        };

        let (tokens, eoi) = aiahr_lexer(interner).lex(m, input).unwrap();
        let unresolved = aiahr_parser(arena).parse(to_stream(tokens, eoi)).unwrap();
        let mut errors = Vec::new();

        let mut module_names = FxHashMap::default();
        let base = BaseBuilder::new().add_slice(unresolved, &mut errors).build(
            arena,
            m,
            &modules,
            &mut module_names,
        );

        (
            resolve_module(arena, unresolved, base, &mut errors),
            module_names[&m],
            errors,
        )
    }

    #[test]
    fn test_local_binding() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);
        let (term, locals, errs) = parse_resolve_term(&arena, &interner, "x = {}; y = {}; x");
        assert_matches!(errs[..], []);
        assert_matches!(
            term,
            Some(nterm_local!(
                x,
                nterm_prod!(),
                nterm_local!(y, nterm_prod!(), nterm_var!(x1))
            )) => {
                assert_eq!(locals.vars[x].value.0, "x");
                assert_eq!(locals.vars[y].value.0, "y");
                assert_eq!(x1, x);
            }
        );
    }

    #[test]
    fn test_local_binding_types() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);
        let (term, locals, errs) =
            parse_resolve_term(&arena, &interner, "x: a = {}; y: {} = {}; x");
        assert_matches!(errs[..], []);
        assert_matches!(
            term,
            Some(nterm_local!(
                x,
                type_named!(a),
                nterm_prod!(),
                nterm_local!(y, type_prod!(), nterm_prod!(), nterm_var!(x1))
            )) => {
                assert_eq!(locals.ty_vars[a].value.0, "a");
                assert_eq!(locals.vars[x].value.0, "x");
                assert_eq!(locals.vars[y].value.0, "y");
                assert_eq!(x1, x);
            }
        );
    }

    #[test]
    fn test_local_binding_shadowing() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);
        let (term, locals, errs) = parse_resolve_term(&arena, &interner, "x = {}; x = x; x");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some(nterm_local!(
                x_out,
                nterm_prod!(),
                nterm_local!(x_in, nterm_var!(x1), nterm_var!(x2))
            )) => {
                assert_eq!(locals.vars[x_out].value.0, "x");
                assert_eq!(locals.vars[x_in].value.0, "x");
                assert_eq!(x1, x_out);
                assert_eq!(x2, x_in);
            }
        );
    }

    #[test]
    fn test_local_binding_errors() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);
        let (term, _, errs) = parse_resolve_term(&arena, &interner, "x = y; z");
        assert_matches!(term, None);
        assert_matches!(
            errs[..],
            [
                NameResolutionError::NotFound {
                    name: span_of!(h!("y")),
                    context_module: None,
                    ..
                },
                NameResolutionError::NotFound {
                    name: span_of!(h!("z")),
                    context_module: None,
                    ..
                }
            ]
        );
    }

    #[test]
    fn test_handler() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);
        let (term, locals, errs) = parse_resolve_term(&arena, &interner, "x = {}; with x do x");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some(nterm_local!(
                x,
                nterm_prod!(),
                nterm_with!(nterm_var!(x1), nterm_var!(x2))
            )) => {
                assert_eq!(locals.vars[x].value.0, "x");
                assert_eq!(x1, x);
                assert_eq!(x2, x);
            }
        );
    }

    #[test]
    fn test_handler_errors() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);
        let (term, _, errs) = parse_resolve_term(&arena, &interner, "with h do x");
        assert_matches!(term, None);
        assert_matches!(
            errs[..],
            [
                NameResolutionError::NotFound {
                    name: span_of!(h!("h")),
                    context_module: None,
                    ..
                },
                NameResolutionError::NotFound {
                    name: span_of!(h!("x")),
                    context_module: None,
                    ..
                }
            ]
        );
    }

    #[test]
    fn test_abstraction() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);
        let (term, locals, errs) = parse_resolve_term(&arena, &interner, "|x| |y| y(x)");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some(nterm_abs!(
                x,
                nterm_abs!(
                    y,
                    nterm_app!(nterm_var!(y1), nterm_var!(x1))
                )
            )) => {
                assert_eq!(locals.vars[x].value.0, "x");
                assert_eq!(locals.vars[y].value.0, "y");
                assert_eq!(x1, x);
                assert_eq!(y1, y);
            }
        );
    }

    #[test]
    fn test_abstraction_types() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);
        let (term, locals, errs) =
            parse_resolve_term(&arena, &interner, "|x: {}| |y: a -> b| y(x)");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some(nterm_abs!(
                x,
                type_prod!(),
                nterm_abs!(
                    y,
                    type_func!(type_named!(a), type_named!(b)),
                    nterm_app!(nterm_var!(y1), nterm_var!(x1))
                )
            )) => {
                assert_eq!(locals.ty_vars[a].value.0, "a");
                assert_eq!(locals.ty_vars[b].value.0, "b");
                assert_eq!(locals.vars[x].value.0, "x");
                assert_eq!(locals.vars[y].value.0, "y");
                assert_eq!(x1, x);
                assert_eq!(y1, y);
            }
        );
    }

    #[test]
    fn test_abstraction_shadowing() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);
        let (term, locals, errs) = parse_resolve_term(&arena, &interner, "|x| |x| x(x)");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some(nterm_abs!(
                x_out,
                nterm_abs!(
                    x_in,
                    nterm_app!(nterm_var!(x1), nterm_var!(x2))
                )
            )) => {
                assert_eq!(locals.vars[x_out].value.0, "x");
                assert_eq!(locals.vars[x_in].value.0, "x");
                assert_eq!(x1, x_in);
                assert_eq!(x2, x_in);
            }
        );
    }

    #[test]
    fn test_application() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);
        let (term, locals, errs) = parse_resolve_term(&arena, &interner, "|x| x(x)");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some(nterm_abs!(
                x,
                nterm_app!(nterm_var!(x1), nterm_var!(x2))
            )) => {
                assert_eq!(locals.vars[x].value.0, "x");
                assert_eq!(x1, x);
                assert_eq!(x2, x);
            }
        );
    }

    #[test]
    fn test_application_errors() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);
        let (term, _, errs) = parse_resolve_term(&arena, &interner, "f(x)");
        assert_matches!(term, None);
        assert_matches!(
            errs[..],
            [
                NameResolutionError::NotFound {
                    name: span_of!(h!("f")),
                    context_module: None,
                    ..
                },
                NameResolutionError::NotFound {
                    name: span_of!(h!("x")),
                    context_module: None,
                    ..
                }
            ]
        );
    }

    #[test]
    fn test_product_row() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);
        let (term, locals, errs) =
            parse_resolve_term(&arena, &interner, "x = {}; {a = x, b = {x = x}}");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some(nterm_local!(x, nterm_prod!(),
                nterm_prod!(
                id_field!("a", nterm_var!(x1)),
                id_field!(
                    "b",
                    nterm_prod!(id_field!("x", nterm_var!(x2)))
                ),
            ))) => {
                assert_eq!(locals.vars[x].value.0, "x");
                assert_eq!(x1, x);
                assert_eq!(x2, x);
            }
        );
    }

    #[test]
    fn test_product_row_errors() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);
        let (term, _, errs) = parse_resolve_term(&arena, &interner, "{x = y, z = x}");
        assert_matches!(term, None);
        assert_matches!(
            errs[..],
            [
                NameResolutionError::NotFound {
                    name: span_of!(h!("y")),
                    context_module: None,
                    ..
                },
                NameResolutionError::NotFound {
                    name: span_of!(h!("x")),
                    context_module: None,
                    ..
                }
            ]
        );
    }

    #[test]
    fn test_sum_row() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);
        let (term, locals, errs) = parse_resolve_term(&arena, &interner, "|x| <a = x>");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some(nterm_abs!(x, nterm_sum!(id_field!("a", nterm_var!(x1))))) => {
                assert_eq!(locals.vars[x].value.0, "x");
                assert_eq!(x1, x);
            }
        );
    }

    #[test]
    fn test_sum_row_errors() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);
        let (term, _, errs) = parse_resolve_term(&arena, &interner, "<x = x>");
        assert_matches!(term, None);
        assert_matches!(
            errs[..],
            [NameResolutionError::NotFound {
                name: span_of!(h!("x")),
                context_module: None,
                ..
            }]
        );
    }

    #[test]
    fn test_dot_access() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);
        let (term, locals, errs) = parse_resolve_term(&arena, &interner, "id = |x| x; {x = id}.x");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some(nterm_local!(
                id,
                nterm_abs!(x, nterm_var!(x1)),
                nterm_dot!(
                    nterm_prod!(id_field!("x", nterm_var!(id1))),
                    "x"
                )
            )) => {
                assert_eq!(locals.vars[id].value.0, "id");
                assert_eq!(locals.vars[x].value.0, "x");
                assert_eq!(x1, x);
                assert_eq!(id1, id);
            }
        );
    }

    #[test]
    fn test_dot_access_errors() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);
        let (term, _, errs) = parse_resolve_term(&arena, &interner, "x.a");
        assert_matches!(term, None);
        assert_matches!(
            errs[..],
            [NameResolutionError::NotFound {
                name: span_of!(h!("x")),
                context_module: None,
                ..
            }]
        );
    }

    #[test]
    fn test_match() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);
        let (term, locals, errs) =
            parse_resolve_term(&arena, &interner, "match <{a = x} => x, y => y>");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some(nterm_match!(
                field!(npat_prod!(id_field!("a", npat_var!(x))), nterm_var!(x1)),
                field!(npat_var!(y), nterm_var!(y1))
            )) => {
                assert_eq!(locals.vars[x].value.0, "x");
                assert_eq!(locals.vars[y].value.0, "y");
                assert_eq!(x1, x);
                assert_eq!(y1, y);
            }
        );
    }

    #[test]
    fn test_match_errors() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);
        let (term, _, errs) =
            parse_resolve_term(&arena, &interner, "match <{a = x} => f(x), {} => z>");
        assert_matches!(term, None);
        assert_matches!(
            errs[..],
            [
                NameResolutionError::NotFound {
                    name: span_of!(h!("f")),
                    context_module: None,
                    ..
                },
                NameResolutionError::NotFound {
                    name: span_of!(h!("z")),
                    context_module: None,
                    ..
                }
            ]
        );

        let (term, locals, errs) =
            parse_resolve_term(&arena, &interner, "match <{a = x, b = x} => x(x)>");
        assert_matches!(
            errs[..],
            [NameResolutionError::Duplicate {
                name: h!("x"),
                kind: NameKind::Var,
                original: Span { end, ..},
                duplicate: Span { start, ..},
            }] => {
                assert!(end.byte < start.byte, "{} < {}", end.byte, start.byte);
            }
        );
        assert_matches!(
            term,
            Some(nterm_match!(field!(
                npat_prod!(
                    id_field!("a", npat_var!(x)),
                    id_field!("b", npat_var!(x_again))
                ),
                nterm_app!(nterm_var!(x1), nterm_var!(x2))
            ))) => {
                assert_eq!(locals.vars[x].value.0, "x");
                assert_eq!(locals.vars[x_again].value.0, "x");
                assert_eq!(x1, x);
                assert_eq!(x2, x);
            }
        );
    }

    #[test]
    fn test_mixed_shadowing() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);
        let (term, locals, errs) =
            parse_resolve_term(&arena, &interner, "x = {}; |x| match <x => x>");
        assert_matches!(errs[..], []);
        assert_matches!(
            term,
            Some(nterm_local!(x_top, nterm_prod!(), nterm_abs!(x_mid, nterm_match!(field!(npat_var!(x_bot), nterm_var!(x1)))))) => {
                assert_eq!(locals.vars[x_top].value.0, "x");
                assert_eq!(locals.vars[x_mid].value.0, "x");
                assert_eq!(locals.vars[x_bot].value.0, "x");
                assert_eq!(x1, x_bot);
            }
        );
    }

    #[test]
    fn test_schemes() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);
        let (res, ids, errs) =
            parse_resolve_module(&arena, &interner, "foo : forall a. a -> a = |x| x");
        assert_matches!(errs[..], []);
        assert_matches!(
            res.resolved_items[..],
            [
                Some(nitem_term!(foo, scheme!(quant!(a), None, type_func!(type_named!(a1), type_named!(a2))), nterm_abs!(x, nterm_var!(x1)))),
            ] => {
                assert_eq!(ids.get(foo).value.0, "foo");
                assert_eq!(res.locals.ty_vars[a].value.0, "a");
                assert_eq!(res.locals.vars[x].value.0, "x");
                assert_eq!(a1, a);
                assert_eq!(a2, a);
                assert_eq!(x1, x);
            }
        );
    }

    #[test]
    fn test_top_level_letrec() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);
        let (res, ids, errs) = parse_resolve_module(&arena, &interner, "foo = bar\nbar = foo");
        assert_matches!(errs[..], []);
        assert_matches!(
            res.resolved_items[..],
            [
                Some(nitem_term!(foo, nterm_item!(mbar, bar1))),
                Some(nitem_term!(bar, nterm_item!(mfoo, foo1)))
            ] => {
                assert_eq!(ids.get(foo).value.0, "foo");
                assert_eq!(ids.get(bar).value.0, "bar");
                assert_eq!(mbar, ModuleId(0));
                assert_eq!(mfoo, ModuleId(0));
                assert_eq!(bar1, bar);
                assert_eq!(foo1, foo);
            }
        );
    }

    #[test]
    fn test_top_level_errors() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);
        let (res, _, errs) = parse_resolve_module(&arena, &interner, "foo = x\nbar = y");
        assert_matches!(res.resolved_items[..], [None, None]);
        assert_matches!(
            errs[..],
            [
                NameResolutionError::NotFound {
                    name: span_of!(h!("x")),
                    context_module: None,
                    ..
                },
                NameResolutionError::NotFound {
                    name: span_of!(h!("y")),
                    context_module: None,
                    ..
                }
            ]
        );
    }
}
