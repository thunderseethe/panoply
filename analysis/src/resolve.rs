use crate::{
    base::BaseNames,
    name::{BaseName, ModuleName, Name},
    names::{LocalIds, Names},
};
use aiahr_core::{
    cst::{self, Field, IdField, ProductRow, Separated, SumRow},
    diagnostic::{
        nameres::{NameKind, NameKinds, NameResolutionError, RejectionReason, Suggestion},
        DiagnosticSink,
    },
    id::{ItemId, ModuleId},
    memory::handle::RefHandle,
    nst,
    span::{Span, SpanOf, Spanned},
};
use bumpalo::Bump;

const TERM_KINDS: NameKinds = NameKinds::ITEM.union(NameKinds::VAR);

/// Extension trait to construct not-found errors from find results.
pub trait OkOrEmit<'s>: Sized {
    type Ok;

    fn ok_or_emit<E>(self, name: SpanOf<RefHandle<'s, str>>, errors: &mut E) -> Option<Self::Ok>
    where
        E: DiagnosticSink<NameResolutionError<'s>>;

    fn ok_or_emit_in<E>(
        self,
        module: ModuleId,
        name: SpanOf<RefHandle<'s, str>>,
        errors: &mut E,
    ) -> Option<Self::Ok>
    where
        E: DiagnosticSink<NameResolutionError<'s>>;
}

impl<'s, T> OkOrEmit<'s> for Result<T, Vec<Suggestion<'s>>> {
    type Ok = T;

    fn ok_or_emit<E>(
        self,
        name: SpanOf<RefHandle<'s, str>>,
        errors: &mut E,
    ) -> Option<<Self as OkOrEmit<'s>>::Ok>
    where
        E: DiagnosticSink<NameResolutionError<'s>>,
    {
        match self {
            Ok(ret) => Some(ret),
            Err(suggestions) => {
                errors.add(NameResolutionError::NotFound {
                    name,
                    context_module: None,
                    suggestions,
                });
                None
            }
        }
    }

    fn ok_or_emit_in<E>(
        self,
        module: ModuleId,
        name: SpanOf<RefHandle<'s, str>>,
        errors: &mut E,
    ) -> Option<<Self as OkOrEmit<'s>>::Ok>
    where
        E: DiagnosticSink<NameResolutionError<'s>>,
    {
        match self {
            Ok(ret) => Some(ret),
            Err(suggestions) => {
                errors.add(NameResolutionError::NotFound {
                    name,
                    context_module: Some(module),
                    suggestions,
                });
                None
            }
        }
    }
}

// Allocates the items in `iter` on the given arena, but only if they are all `Some(..)`.
fn alloc_all<'a, T, I>(arena: &'a Bump, iter: I) -> Option<&'a [T]>
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

// The possible meanings of a `DotAccess` term.
#[derive(Debug)]
enum DotResolution<'a, 's> {
    Module(ModuleId),
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
        cst::Pattern::Whole(var) => nst::Pattern::Whole(var.span_map(|var| {
            names
                .insert_var(*var)
                .emit_and_unwrap(*var, NameKind::Var, errors)
        })),
    }))
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
            DotResolution::Module(m) => names
                .find_in(m, field.value, |name| Ok(DotResolution::from(name)))
                .ok_or_emit_in(m, field, errors)?,
            // (m.i) . field
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
        cst::Term::SymbolRef(n) => names
            .find(n.value, |name| match name {
                Name::Module(m) => Ok(names
                    .find_in(m, field.value, |n2| Ok(DotResolution::from(n2)))
                    .ok_or_emit_in(m, field, errors)),
                Name::Item(m, i) => Ok(Some(DotResolution::FieldAccess {
                    base: arena.alloc(nst::Term::ItemRef(base.span().of((m, i)))),
                    dot,
                    field,
                })),
                Name::TyVar(_) => Err(RejectionReason::WrongKind {
                    actual: NameKind::TyVar,
                    expected: TERM_KINDS | NameKinds::MODULE,
                }),
                Name::Var(v) => Ok(Some(DotResolution::FieldAccess {
                    base: arena.alloc(nst::Term::VariableRef(base.span().of(v))),
                    dot,
                    field,
                })),
            })
            .ok_or_emit(*n, errors)
            .flatten()?,
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
    Some(
        arena.alloc(match term {
            cst::Term::Binding {
                var,
                annotation: _,
                eq,
                value,
                semi,
                expr,
            } => {
                // TODO: resolve annotation.
                let value = resolve_term(arena, value, names, errors);

                names.subscope(|scope| {
                    let id = scope
                        .insert_var(*var)
                        .emit_and_unwrap(*var, NameKind::Var, errors);
                    let expr = resolve_term(arena, expr, scope, errors);

                    Some(nst::Term::Binding {
                        var: var.span().of(id),
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
                annotation: _,
                rbar,
                body,
            } => names.subscope(|scope| {
                let id = scope
                    .insert_var(*arg)
                    .emit_and_unwrap(*arg, NameKind::Var, errors);
                Some(nst::Term::Abstraction {
                    lbar: *lbar,
                    arg: arg.span().of(id),
                    rbar: *rbar,
                    body: resolve_term(arena, body, scope, errors)?,
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
            cst::Term::SymbolRef(var) => names
                .find(var.value, |name| match name {
                    Name::Module(_) => Err(RejectionReason::WrongKind {
                        actual: NameKind::Module,
                        expected: TERM_KINDS,
                    }),
                    Name::Item(m, i) => Ok(nst::Term::ItemRef(var.span().of((m, i)))),
                    Name::TyVar(_) => Err(RejectionReason::WrongKind {
                        actual: NameKind::TyVar,
                        expected: TERM_KINDS,
                    }),
                    Name::Var(v) => Ok(nst::Term::VariableRef(var.span().of(v))),
                })
                .ok_or_emit(*var, errors)?,
            cst::Term::Parenthesized { lpar, term, rpar } => nst::Term::Parenthesized {
                lpar: *lpar,
                term: resolve_term(arena, term, names, errors)?,
                rpar: *rpar,
            },
        }),
    )
}

/// Resolves the given item.
pub fn resolve_item<'a, 's, E>(
    arena: &'a Bump,
    id: ItemId,
    item: &cst::Item<'_, 's>,
    names: &mut Names<'_, 'a, 's>,
    errors: &mut E,
) -> Option<nst::Item<'a, 's>>
where
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    Some(match item {
        cst::Item::Term {
            name,
            annotation: _,
            eq,
            value,
        } => nst::Item::Term {
            name: name.span().of(id),
            eq: *eq,
            value: arena.alloc(resolve_term(arena, value, names, errors)?),
        },
    })
}

/// Data generated by resolving a module.
#[derive(Debug)]
pub struct ModuleResolution<'a, 's> {
    pub locals: LocalIds<'s>,
    pub resolved_items: &'a [Option<nst::Item<'a, 's>>],
}

/// Resolves the given module.
pub fn resolve_module<'a, 's, E>(
    arena: &'a Bump,
    items: &[cst::Item<'_, 's>],
    base: BaseNames<'_, 'a, 's>,
    errors: &mut E,
) -> ModuleResolution<'a, 's>
where
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    let mut names = Names::new(&base);
    let resolved_items =
        arena.alloc_slice_fill_iter(base.iter().zip(items.iter()).map(|pair| match pair {
            (ModuleName::Item(i), item @ &cst::Item::Term { .. }) => {
                resolve_item(arena, *i, item, &mut names, errors)
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
        id::{Ids, ModuleId, VarId},
        id_field,
        memory::{
            arena::BumpArena,
            handle::RefHandle,
            intern::{InternerByRef, SyncInterner},
        },
        modules::ModuleTree,
        nitem_term, npat_prod, npat_var, nst, nterm_abs, nterm_app, nterm_dot, nterm_item,
        nterm_local, nterm_match, nterm_prod, nterm_sum, nterm_var, nterm_with,
        span::{Span, SpanOf},
        span_of,
    };
    use aiahr_parser::{
        lexer::aiahr_lexer,
        parser::{aiahr_parser, term, to_stream},
    };
    use assert_matches::assert_matches;
    use bumpalo::Bump;
    use chumsky::Parser;
    use rustc_hash::FxHashMap;

    use crate::{module::ModuleNames, names::Names, ops::IdOps, top_level::BaseBuilder};

    use super::{resolve_module, resolve_term, ModuleResolution};

    const MODNAME: &'static str = "test_module";

    fn parse_resolve_term<'a: 's, 's, S>(
        arena: &'a Bump,
        interner: &'s S,
        input: &str,
    ) -> (
        Option<&'a nst::Term<'a, 's>>,
        Box<Ids<VarId, SpanOf<RefHandle<'s, str>>>>,
        Vec<NameResolutionError<'s>>,
    )
    where
        S: InternerByRef<str>,
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
        (resolved, names.into_local_ids().vars, errors)
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
        S: InternerByRef<str>,
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
        let interner = SyncInterner::new(BumpArena::new());
        let (term, vars, errs) = parse_resolve_term(&arena, &interner, "x = {}; y = {}; x");
        assert_matches!(
            term,
            Some(nterm_local!(
                x,
                nterm_prod!(),
                nterm_local!(y, nterm_prod!(), nterm_var!(x1))
            )) => {
                assert_eq!(vars[x].value.0, "x");
                assert_eq!(vars[y].value.0, "y");
                assert_eq!(x1, x);
            }
        );
        assert_matches!(errs[..], []);
    }

    #[test]
    fn test_local_binding_shadowing() {
        let arena = Bump::new();
        let interner = SyncInterner::new(BumpArena::new());
        let (term, vars, errs) = parse_resolve_term(&arena, &interner, "x = {}; x = x; x");
        assert_matches!(term,
            Some(nterm_local!(
                x_out,
                nterm_prod!(),
                nterm_local!(x_in, nterm_var!(x1), nterm_var!(x2))
            )) => {
                assert_eq!(vars[x_out].value.0, "x");
                assert_eq!(vars[x_in].value.0, "x");
                assert_eq!(x1, x_out);
                assert_eq!(x2, x_in);
            }
        );
        assert_matches!(errs[..], []);
    }

    #[test]
    fn test_local_binding_errors() {
        let arena = Bump::new();
        let interner = SyncInterner::new(BumpArena::new());
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
        let interner = SyncInterner::new(BumpArena::new());
        let (term, vars, errs) = parse_resolve_term(&arena, &interner, "x = {}; with x do x");
        assert_matches!(term,
            Some(nterm_local!(
                x,
                nterm_prod!(),
                nterm_with!(nterm_var!(x1), nterm_var!(x2))
            )) => {
                assert_eq!(vars[x].value.0, "x");
                assert_eq!(x1, x);
                assert_eq!(x2, x);
            }
        );
        assert_matches!(errs[..], []);
    }

    #[test]
    fn test_handler_errors() {
        let arena = Bump::new();
        let interner = SyncInterner::new(BumpArena::new());
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
        let interner = SyncInterner::new(BumpArena::new());
        let (term, vars, errs) = parse_resolve_term(&arena, &interner, "|x| |y| y(x)");
        assert_matches!(term,
            Some(nterm_abs!(
                x,
                nterm_abs!(
                    y,
                    nterm_app!(nterm_var!(y1), nterm_var!(x1))
                )
            )) => {
                assert_eq!(vars[x].value.0, "x");
                assert_eq!(vars[y].value.0, "y");
                assert_eq!(x1, x);
                assert_eq!(y1, y);
            }
        );
        assert_matches!(errs[..], []);
    }

    #[test]
    fn test_abstraction_shadowing() {
        let arena = Bump::new();
        let interner = SyncInterner::new(BumpArena::new());
        let (term, vars, errs) = parse_resolve_term(&arena, &interner, "|x| |x| x(x)");
        assert_matches!(term,
            Some(nterm_abs!(
                x_out,
                nterm_abs!(
                    x_in,
                    nterm_app!(nterm_var!(x1), nterm_var!(x2))
                )
            )) => {
                assert_eq!(vars[x_out].value.0, "x");
                assert_eq!(vars[x_in].value.0, "x");
                assert_eq!(x1, x_in);
                assert_eq!(x2, x_in);
            }
        );
        assert_matches!(errs[..], []);
    }

    #[test]
    fn test_application() {
        let arena = Bump::new();
        let interner = SyncInterner::new(BumpArena::new());
        let (term, vars, errs) = parse_resolve_term(&arena, &interner, "|x| x(x)");
        assert_matches!(term,
            Some(nterm_abs!(
                x,
                nterm_app!(nterm_var!(x1), nterm_var!(x2))
            )) => {
                assert_eq!(vars[x].value.0, "x");
                assert_eq!(x1, x);
                assert_eq!(x2, x);
            }
        );
        assert_matches!(errs[..], []);
    }

    #[test]
    fn test_application_errors() {
        let arena = Bump::new();
        let interner = SyncInterner::new(BumpArena::new());
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
        let interner = SyncInterner::new(BumpArena::new());
        let (term, vars, errs) =
            parse_resolve_term(&arena, &interner, "x = {}; {a = x, b = {x = x}}");
        assert_matches!(term,
            Some(nterm_local!(x, nterm_prod!(),
                nterm_prod!(
                id_field!("a", nterm_var!(x1)),
                id_field!(
                    "b",
                    nterm_prod!(id_field!("x", nterm_var!(x2)))
                ),
            ))) => {
                assert_eq!(vars[x].value.0, "x");
                assert_eq!(x1, x);
                assert_eq!(x2, x);
            }
        );
        assert_matches!(errs[..], []);
    }

    #[test]
    fn test_product_row_errors() {
        let arena = Bump::new();
        let interner = SyncInterner::new(BumpArena::new());
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
        let interner = SyncInterner::new(BumpArena::new());
        let (term, vars, errs) = parse_resolve_term(&arena, &interner, "|x| <a = x>");
        assert_matches!(term,
            Some(nterm_abs!(x, nterm_sum!(id_field!("a", nterm_var!(x1))))) => {
                assert_eq!(vars[x].value.0, "x");
                assert_eq!(x1, x);
            }
        );
        assert_matches!(errs[..], []);
    }

    #[test]
    fn test_sum_row_errors() {
        let arena = Bump::new();
        let interner = SyncInterner::new(BumpArena::new());
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
        let interner = SyncInterner::new(BumpArena::new());
        let (term, vars, errs) = parse_resolve_term(&arena, &interner, "id = |x| x; {x = id}.x");
        assert_matches!(term,
            Some(nterm_local!(
                id,
                nterm_abs!(x, nterm_var!(x1)),
                nterm_dot!(
                    nterm_prod!(id_field!("x", nterm_var!(id1))),
                    "x"
                )
            )) => {
                assert_eq!(vars[id].value.0, "id");
                assert_eq!(vars[x].value.0, "x");
                assert_eq!(x1, x);
                assert_eq!(id1, id);
            }
        );
        assert_matches!(errs[..], []);
    }

    #[test]
    fn test_dot_access_errors() {
        let arena = Bump::new();
        let interner = SyncInterner::new(BumpArena::new());
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
        let interner = SyncInterner::new(BumpArena::new());
        let (term, vars, errs) =
            parse_resolve_term(&arena, &interner, "match <{a = x} => x, y => y>");
        assert_matches!(term,
            Some(nterm_match!(
                field!(npat_prod!(id_field!("a", npat_var!(x))), nterm_var!(x1)),
                field!(npat_var!(y), nterm_var!(y1))
            )) => {
                assert_eq!(vars[x].value.0, "x");
                assert_eq!(vars[y].value.0, "y");
                assert_eq!(x1, x);
                assert_eq!(y1, y);
            }
        );
        assert_matches!(errs[..], []);
    }

    #[test]
    fn test_match_errors() {
        let arena = Bump::new();
        let interner = SyncInterner::new(BumpArena::new());
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

        let (term, vars, errs) =
            parse_resolve_term(&arena, &interner, "match <{a = x, b = x} => x(x)>");
        assert_matches!(
            term,
            Some(nterm_match!(field!(
                npat_prod!(
                    id_field!("a", npat_var!(x)),
                    id_field!("b", npat_var!(x_again))
                ),
                nterm_app!(nterm_var!(x1), nterm_var!(x2))
            ))) => {
                assert_eq!(vars[x].value.0, "x");
                assert_eq!(vars[x_again].value.0, "x");
                assert_eq!(x1, x);
                assert_eq!(x2, x);
            }
        );
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
        )
    }

    #[test]
    fn test_mixed_shadowing() {
        let arena = Bump::new();
        let interner = SyncInterner::new(BumpArena::new());
        let (term, vars, errs) =
            parse_resolve_term(&arena, &interner, "x = {}; |x| match <x => x>");
        assert_matches!(term,
            Some(nterm_local!(x_top, nterm_prod!(), nterm_abs!(x_mid, nterm_match!(field!(npat_var!(x_bot), nterm_var!(x1)))))) => {
                assert_eq!(vars[x_top].value.0, "x");
                assert_eq!(vars[x_mid].value.0, "x");
                assert_eq!(vars[x_bot].value.0, "x");
                assert_eq!(x1, x_bot);
            }
        );
        assert_matches!(errs[..], []);
    }

    #[test]
    fn test_top_level_letrec() {
        let arena = Bump::new();
        let interner = SyncInterner::new(BumpArena::new());
        let (res, ids, errs) = parse_resolve_module(&arena, &interner, "foo = bar\nbar = foo");
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
        assert_matches!(errs[..], []);
    }

    #[test]
    fn test_top_level_errors() {
        let arena = Bump::new();
        let interner = SyncInterner::new(BumpArena::new());
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
