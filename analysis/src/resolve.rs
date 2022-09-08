use crate::names::Names;
use aiahr_core::{
    cst::{CommaSep, Field, IdField, Item, Pattern, ProductRow, SumRow, Term},
    diagnostic::{nameres::NameResolutionError, DiagnosticSink},
    handle::{Handle, RefHandle},
    span::SpanOf,
};
use bumpalo::Bump;

// Allocates the items in `iter` on the given arena, but only if they are all `Some(..)`.
fn alloc_all<'a, 'i, T, I>(arena: &'a Bump, iter: I) -> Option<&'a [T]>
where
    I: Iterator<Item = Option<T>>,
{
    iter.collect::<Option<Vec<T>>>()
        .map(|v| arena.alloc_slice_fill_iter(v.into_iter()) as &[T])
}

// Tries to map the given function over the elements of `comma_sep`, returning all errors.
fn resolve_comma_sep<'a, A, B, F>(
    arena: &'a Bump,
    comma_sep: &CommaSep<'_, A>,
    f: F,
) -> Option<CommaSep<'a, B>>
where
    F: FnMut(&A) -> Option<B>,
{
    let mut f = f;
    let first = f(&comma_sep.first);
    let elems = alloc_all(
        arena,
        comma_sep.elems.iter().map(|(c, a)| Some((*c, f(a)?))),
    );
    Some(CommaSep {
        first: first?,
        elems: elems?,
        comma: comma_sep.comma,
    })
}

// Tries to map the given function over the target.
fn resolve_id_field<'i, A, B, F>(field: &IdField<'i, A>, f: F) -> Option<IdField<'i, B>>
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
fn resolve_product_row<'a, 'i, A, B, F>(
    arena: &'a Bump,
    prod: &ProductRow<'_, 'i, A>,
    f: F,
) -> Option<ProductRow<'a, 'i, B>>
where
    F: FnMut(&A) -> Option<B>,
{
    let mut f = f;
    let fields = if let Some(cs) = &prod.fields {
        Some(resolve_comma_sep(arena, &cs, |field| {
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
fn resolve_sum_row<'i, A, B, F>(sum: &SumRow<'i, A>, f: F) -> Option<SumRow<'i, B>>
where
    F: FnOnce(&A) -> Option<B>,
{
    Some(SumRow {
        langle: sum.langle,
        field: resolve_id_field(&sum.field, f)?,
        rangle: sum.rangle,
    })
}

/// Resolves the given pattern, accumulating bindings into `names`.
pub fn resolve_pattern<'a, 'i, 'n, E: DiagnosticSink<NameResolutionError<'i>>>(
    arena: &'a Bump,
    names: &'n mut Names<'_, 'i>,
    pattern: &Pattern<'_, 'i, &'i str>,
    errors: &mut E,
) -> Option<&'a Pattern<'a, 'i, RefHandle<'i, str>>> {
    Some(arena.alloc(match pattern {
        Pattern::ProductRow(pr) => Pattern::ProductRow(resolve_product_row(arena, pr, |target| {
            resolve_pattern(arena, names, target, errors)
        })?),
        Pattern::SumRow(sr) => Pattern::SumRow(resolve_sum_row(sr, |target| {
            resolve_pattern(arena, names, target, errors)
        })?),
        Pattern::Whole(var) => names.insert(*var, errors).map(Pattern::Whole)?,
    }))
}

/// Resolves the given term, reporting errors to `errors`.
pub fn resolve_term<'a, 'i, E: DiagnosticSink<NameResolutionError<'i>>>(
    arena: &'a Bump,
    names: &Names<'_, 'i>,
    term: &Term<'_, 'i, &'i str>,
    errors: &mut E,
) -> Option<&'a Term<'a, 'i, RefHandle<'i, str>>> {
    Some(arena.alloc(match term {
        Term::Binding {
            var,
            eq,
            value,
            semi,
            expr,
        } => {
            let value = resolve_term(arena, names, value, errors);
            let expr = resolve_term(arena, &names.subscope_with_one(*var), expr, errors);
            Term::Binding {
                var: var.map(Handle),
                eq: *eq,
                value: value?,
                semi: *semi,
                expr: expr?,
            }
        }
        Term::Handle {
            with,
            handler,
            do_,
            expr,
        } => {
            let handler = resolve_term(arena, names, handler, errors);
            let expr = resolve_term(arena, names, expr, errors);
            Term::Handle {
                with: *with,
                handler: handler?,
                do_: *do_,
                expr: expr?,
            }
        }
        Term::Abstraction {
            lbar,
            arg,
            rbar,
            body,
        } => Term::Abstraction {
            lbar: *lbar,
            arg: arg.map(Handle),
            rbar: *rbar,
            body: resolve_term(arena, &names.subscope_with_one(*arg), body, errors)?,
        },
        Term::Application {
            func,
            lpar,
            arg,
            rpar,
        } => {
            let func = resolve_term(arena, names, func, errors);
            let arg = resolve_term(arena, names, arg, errors);
            Term::Application {
                func: func?,
                lpar: *lpar,
                arg: arg?,
                rpar: *rpar,
            }
        }
        Term::ProductRow(pr) => Term::ProductRow(resolve_product_row(arena, pr, |target| {
            resolve_term(arena, names, target, errors)
        })?),
        Term::SumRow(sr) => Term::SumRow(resolve_sum_row(sr, |target| {
            resolve_term(arena, names, target, errors)
        })?),
        Term::DotAccess { base, dot, field } => Term::DotAccess {
            base: resolve_term(arena, names, base, errors)?,
            dot: *dot,
            field: *field,
        },
        Term::Match {
            match_,
            langle,
            cases,
            rangle,
        } => Term::Match {
            match_: *match_,
            langle: *langle,
            cases: resolve_comma_sep(arena, cases, |field| {
                let mut scope = names.subscope();
                let pattern = resolve_pattern(arena, &mut scope, field.label, errors);
                let target = resolve_term(arena, &scope, field.target, errors);
                Some(Field {
                    label: pattern?,
                    sep: field.sep,
                    target: target?,
                })
            })?,
            rangle: *rangle,
        },
        Term::SymbolRef(var) => Term::SymbolRef(names.get(*var, errors)?),
        Term::Parenthesized { lpar, term, rpar } => Term::Parenthesized {
            lpar: *lpar,
            term: resolve_term(arena, names, term, errors)?,
            rpar: *rpar,
        },
    }))
}

/// Resolves the given item, reporting errors to `errors`.
pub fn resolve_item<'a, 'i, E: DiagnosticSink<NameResolutionError<'i>>>(
    arena: &'a Bump,
    names: &Names<'_, 'i>,
    item: &Item<'_, 'i, &'i str>,
    errors: &mut E,
) -> Option<Item<'a, 'i, RefHandle<'i, str>>> {
    Some(match item {
        Item::Term { name, eq, value } => Item::Term {
            name: name.map(Handle),
            eq: *eq,
            value: arena.alloc(resolve_term(arena, names, value, errors)?),
        },
    })
}

/// Resolves the given module, reporting errors to `errors`.
pub fn resolve_module<'a, 'i, E>(
    arena: &'a Bump,
    items: &[Item<'_, 'i, &'i str>],
    errors: &mut E,
) -> ResolvedModule<'a, 'i>
where
    E: DiagnosticSink<NameResolutionError<'i>>,
{
    // Collect top-level names first so they can reference each other in `letrec` fashion. We'll do
    // recursion checking later.
    let (names, ..) = Names::with_top_level(
        items.iter().map(|item| match item {
            Item::Term { name, .. } => *name,
        }),
        errors,
    );

    let (succeeded, failed) = items
        .iter()
        .map(|item| {
            (
                match item {
                    Item::Term { name, .. } => name,
                },
                resolve_item(arena, &names, item, errors),
            )
        })
        .fold(
            (Vec::new(), Vec::new()),
            |(mut succeeded, mut failed), (&name, item)| {
                match item {
                    Some(item) => {
                        succeeded.push(item);
                    }
                    None => {
                        failed.push(name);
                    }
                };
                (succeeded, failed)
            },
        );
    ResolvedModule { succeeded, failed }
}

/// A resolved module.
#[derive(Debug)]
pub struct ResolvedModule<'a, 'i> {
    /// Top-level items that were successfully resolved.
    pub succeeded: Vec<Item<'a, 'i, RefHandle<'i, str>>>,

    /// Top-level items that could not be resolved.
    pub failed: Vec<SpanOf<&'i str>>,
}

#[cfg(test)]
mod tests {
    use std::iter;

    use aiahr_core::{
        cst::{Item, Term},
        diagnostic::nameres::NameResolutionError,
        field,
        handle::RefHandle,
        id_field, item_term, pat_prod, pat_var,
        span::SpanOf,
        span_of, term_abs, term_app, term_dot, term_local, term_match, term_prod, term_sum,
        term_sym, term_with,
    };
    use aiahr_parser::{
        lexer::aiahr_lexer,
        parser::{aiahr_parser, term, to_stream},
    };
    use assert_matches::assert_matches;
    use bumpalo::Bump;
    use chumsky::Parser;

    use crate::names::Names;

    use super::{resolve_module, resolve_term};

    fn parse_resolve_term<'a, 'i>(
        arena: &'a Bump,
        input: &'i str,
    ) -> Result<&'a Term<'a, 'i, RefHandle<'i, str>>, Vec<NameResolutionError<'i>>> {
        let (tokens, eoi) = aiahr_lexer().lex(input).unwrap();
        let unresolved = term(arena).parse(to_stream(tokens, eoi)).unwrap();
        let mut errors = Vec::new();
        let resolved = resolve_term(
            arena,
            &Names::with_top_level(iter::empty(), &mut errors).0,
            unresolved,
            &mut errors,
        );
        resolved.ok_or(errors)
    }

    fn parse_resolve_module<'a, 'i>(
        arena: &'a Bump,
        input: &'i str,
    ) -> Result<Vec<Item<'a, 'i, RefHandle<'i, str>>>, Vec<NameResolutionError<'i>>> {
        let (tokens, eoi) = aiahr_lexer().lex(input).unwrap();
        let unresolved = aiahr_parser(arena).parse(to_stream(tokens, eoi)).unwrap();
        let mut errors = Vec::new();
        let resolved = resolve_module(arena, unresolved, &mut errors);
        if errors.is_empty() {
            Ok(resolved.succeeded)
        } else {
            Err(errors)
        }
    }

    #[test]
    fn test_local_binding() {
        assert_matches!(
            parse_resolve_term(&Bump::new(), "x = {}; y = {}; x"),
            Ok(term_local!(
                x,
                term_prod!(),
                term_local!(y, term_prod!(), term_sym!(x1))
            )) => {
                assert_eq!(x.0, "x");
                assert_eq!(y.0, "y");
                assert_eq!(x1, x);
            }
        );
    }

    #[test]
    fn test_local_binding_shadowing() {
        assert_matches!(
            parse_resolve_term(&Bump::new(), "x = {}; x = x; x"),
            Ok(term_local!(
                x_out,
                term_prod!(),
                term_local!(x_in, term_sym!(x1), term_sym!(x2))
            )) => {
                assert_eq!(x_out.0, "x");
                assert_eq!(x_in.0, "x");
                assert_eq!(x1, x_out);
                assert_eq!(x2, x_in);
            }
        );
    }

    #[test]
    fn test_local_binding_errors() {
        assert_matches!(
            parse_resolve_term(&Bump::new(), "x = y; z").unwrap_err()[..],
            [
                NameResolutionError::NotFound(span_of!("y")),
                NameResolutionError::NotFound(span_of!("z"))
            ]
        );
    }

    #[test]
    fn test_handler() {
        assert_matches!(
            parse_resolve_term(&Bump::new(), "x = {}; with x do x"),
            Ok(term_local!(
                x,
                term_prod!(),
                term_with!(term_sym!(x1), term_sym!(x2))
            )) => {
                assert_eq!(x.0, "x");
                assert_eq!(x1, x);
                assert_eq!(x2, x);
            }
        );
    }

    #[test]
    fn test_handler_errors() {
        assert_matches!(
            parse_resolve_term(&Bump::new(), "with h do x").unwrap_err()[..],
            [
                NameResolutionError::NotFound(span_of!("h")),
                NameResolutionError::NotFound(span_of!("x"))
            ]
        );
    }

    #[test]
    fn test_abstraction() {
        assert_matches!(
            parse_resolve_term(&Bump::new(), "|x| |y| y(x)"),
            Ok(term_abs!(
                x,
                term_abs!(
                    y,
                    term_app!(term_sym!(y1), term_sym!(x1))
                )
            )) => {
                assert_eq!(x.0, "x");
                assert_eq!(y.0, "y");
                assert_eq!(x1, x);
                assert_eq!(y1, y);
            }
        );
    }

    #[test]
    fn test_abstraction_shadowing() {
        assert_matches!(
            parse_resolve_term(&Bump::new(), "|x| |x| x(x)"),
            Ok(term_abs!(
                x_out,
                term_abs!(
                    x_in,
                    term_app!(term_sym!(x1), term_sym!(x2))
                )
            )) => {
                assert_eq!(x_out.0, "x");
                assert_eq!(x_in.0, "x");
                assert_eq!(x1, x_in);
                assert_eq!(x2, x_in);
            }
        );
    }

    #[test]
    fn test_application() {
        assert_matches!(
            parse_resolve_term(&Bump::new(), "|x| x(x)"),
            Ok(term_abs!(
                x,
                term_app!(term_sym!(x1), term_sym!(x2))
            )) => {
                assert_eq!(x.0, "x");
                assert_eq!(x1, x);
                assert_eq!(x2, x);
            }
        );
    }

    #[test]
    fn test_application_errors() {
        assert_matches!(
            parse_resolve_term(&Bump::new(), "f(x)").unwrap_err()[..],
            [
                NameResolutionError::NotFound(span_of!("f")),
                NameResolutionError::NotFound(span_of!("x"))
            ]
        );
    }

    #[test]
    fn test_product_row() {
        assert_matches!(
            parse_resolve_term(&Bump::new(), "x = {}; {a = x, b = {x = x}}"),
            Ok(term_local!(x, term_prod!(),
                term_prod!(
                id_field!("a", term_sym!(x1)),
                id_field!(
                    "b",
                    term_prod!(id_field!("x", term_sym!(x2)))
                ),
            ))) => {
                assert_eq!(x.0, "x");
                assert_eq!(x1, x);
                assert_eq!(x2, x);
            }
        );
    }

    #[test]
    fn test_product_row_errors() {
        assert_matches!(
            parse_resolve_term(&Bump::new(), "{x = y, z = x}").unwrap_err()[..],
            [
                NameResolutionError::NotFound(span_of!("y")),
                NameResolutionError::NotFound(span_of!("x"))
            ]
        );
    }

    #[test]
    fn test_sum_row() {
        assert_matches!(
            parse_resolve_term(&Bump::new(), "|x| <a = x>"),
            Ok(term_abs!(x, term_sum!(id_field!("a", term_sym!(x1))))) => {
                assert_eq!(x.0, "x");
                assert_eq!(x1, x);
            }
        );
    }

    #[test]
    fn test_sum_row_errors() {
        assert_matches!(
            parse_resolve_term(&Bump::new(), "<x = x>").unwrap_err()[..],
            [NameResolutionError::NotFound(span_of!("x"))]
        );
    }

    #[test]
    fn test_dot_access() {
        assert_matches!(
            parse_resolve_term(&Bump::new(), "id = |x| x; {x = id}.x"),
            Ok(term_local!(
                id,
                term_abs!(x, term_sym!(x1)),
                term_dot!(
                    term_prod!(id_field!("x", term_sym!(id1))),
                    "x"
                )
            )) => {
                assert_eq!(id.0, "id");
                assert_eq!(x.0, "x");
                assert_eq!(x1, x);
                assert_eq!(id1, id);
            }
        )
    }

    #[test]
    fn test_dot_access_errors() {
        assert_matches!(
            parse_resolve_term(&Bump::new(), "x.a").unwrap_err()[..],
            [NameResolutionError::NotFound(span_of!("x"))]
        );
    }

    #[test]
    fn test_match() {
        assert_matches!(
            parse_resolve_term(&Bump::new(), "match <{a = x} => x, y => y>"),
            Ok(term_match!(
                field!(pat_prod!(id_field!("a", pat_var!(x))), term_sym!(x1)),
                field!(pat_var!(y), term_sym!(y1))
            )) => {
                assert_eq!(x.0, "x");
                assert_eq!(y.0, "y");
                assert_eq!(x1, x);
                assert_eq!(y1, y);
            }
        )
    }

    #[test]
    fn test_match_errors() {
        assert_matches!(
            parse_resolve_term(&Bump::new(), "match <{a = x} => f(x), {} => z>").unwrap_err()[..],
            [
                NameResolutionError::NotFound(span_of!("f")),
                NameResolutionError::NotFound(span_of!("z"))
            ]
        );
        assert_matches!(
            parse_resolve_term(&Bump::new(), "match <{a = x, b = x} => x(x)>").unwrap_err()[..],
            [NameResolutionError::Duplicate {
                original: SpanOf { value: "x", end, ..},
                duplicate: SpanOf { value: "x", start, ..},
            }] => {
                assert!(end.byte < start.byte);
            }
        )
    }

    #[test]
    fn test_mixed_shadowing() {
        assert_matches!(
            parse_resolve_term(&Bump::new(), "x = {}; |x| match <x => x>"),
            Ok(term_local!(x_top, term_prod!(), term_abs!(x_mid, term_match!(field!(pat_var!(x_bot), term_sym!(x1)))))) => {
                assert_eq!(x_top.0, "x");
                assert_eq!(x_mid.0, "x");
                assert_eq!(x_bot.0, "x");
                assert_eq!(x1, x_bot);
            }
        )
    }

    #[test]
    fn test_top_level_letrec() {
        assert_matches!(
            parse_resolve_module(&Bump::new(), "foo = bar\nbar = foo").unwrap()[..],
            [
                item_term!(foo, term_sym!(bar1)),
                item_term!(bar, term_sym!(foo1))
            ] => {
                assert_eq!(foo.0, "foo");
                assert_eq!(bar.0, "bar");
                assert_eq!(bar1, bar);
                assert_eq!(foo1, foo);
            }
        );
    }

    #[test]
    fn test_top_level_errors() {
        assert_matches!(
            parse_resolve_module(&Bump::new(), "foo = x\nbar = y").unwrap_err()[..],
            [
                NameResolutionError::NotFound(span_of!("x")),
                NameResolutionError::NotFound(span_of!("y"))
            ]
        );
    }
}
