use std::collections::{hash_map::Entry, HashMap};

use crate::{
    base::BaseNames,
    modules::{Member, ModuleTree},
    names::{Name, Names},
};
use aiahr_core::{
    cst::{self, Field, IdField, Pattern, ProductRow, Separated, SumRow},
    diagnostic::{nameres::NameResolutionError, DiagnosticSink},
    id::{ItemId, ModuleId, VarId},
    nst,
    span::{Span, SpanOf, Spanned},
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
        Some(resolve_separated(arena, &cs, |field| {
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
///
/// Note that this currently cannot return `None`, although it can emit errors.
pub fn resolve_pattern<'a, 'i, E>(
    arena: &'a Bump,
    names: &mut Names<'_, '_, 'i>,
    pattern: &Pattern<'_, 'i, &'i str>,
    errors: &mut E,
) -> Option<&'a Pattern<'a, 'i, VarId>>
where
    E: DiagnosticSink<NameResolutionError<'i>>,
{
    Some(arena.alloc(match pattern {
        Pattern::ProductRow(pr) => Pattern::ProductRow(resolve_product_row(arena, pr, |target| {
            resolve_pattern(arena, names, target, errors)
        })?),
        Pattern::SumRow(sr) => Pattern::SumRow(resolve_sum_row(sr, |target| {
            resolve_pattern(arena, names, target, errors)
        })?),
        Pattern::Whole(var) => Pattern::Whole(names.insert(*var, None, errors)),
    }))
}

// The possible meanings of a `DotAccess` term.
#[derive(Debug)]
enum DotResolution<'a, 'i> {
    Module(ModuleId),
    Item(ModuleId, ItemId),
    FieldAccess {
        base: &'a nst::Term<'a, 'i>,
        dot: Span,
        field: SpanOf<&'i str>,
    },
}

impl<'a, 'i> DotResolution<'a, 'i> {
    fn from_member(parent: ModuleId, memb: Member) -> DotResolution<'a, 'i> {
        match memb {
            Member::Module(m) => DotResolution::Module(m),
            Member::Item(i) => DotResolution::Item(parent, i),
        }
    }
}

// Resolves nested `DotAccess` terms.
fn resolve_nested_dots<'a, 'i, E>(
    arena: &'a Bump,
    names: &Names<'_, '_, 'i>,
    base: &cst::Term<'_, 'i, &'i str>,
    dot: Span,
    field: SpanOf<&'i str>,
    errors: &mut E,
) -> Option<DotResolution<'a, 'i>>
where
    E: DiagnosticSink<NameResolutionError<'i>>,
{
    Some(match base {
        // (base2 . field2) . field
        cst::Term::DotAccess {
            base: base2,
            dot: dot2,
            field: field2,
        } => match resolve_nested_dots(arena, names, base2, *dot2, *field2, errors)? {
            // m . field
            DotResolution::Module(m) => match names.get_in(m, field, errors)? {
                Member::Module(m2) => DotResolution::Module(m2),
                Member::Item(i) => DotResolution::Item(m, i),
            },
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
        cst::Term::SymbolRef(n) => match names.get(*n, errors)?.value {
            Name::Module(m) => DotResolution::from_member(m, names.get_in(m, field, errors)?),
            Name::Item(m, i) => DotResolution::FieldAccess {
                base: arena.alloc(nst::Term::ItemRef(base.span().of((m, i)))),
                dot,
                field,
            },
            Name::Variable(v) => DotResolution::FieldAccess {
                base: arena.alloc(nst::Term::VariableRef(base.span().of(v))),
                dot,
                field,
            },
        },
        // (expr) . field
        _ => DotResolution::FieldAccess {
            base: resolve_term(arena, names, base, errors)?,
            dot,
            field,
        },
    })
}

/// Resolves the given term, reporting errors to `errors`.
pub fn resolve_term<'a, 'i, E>(
    arena: &'a Bump,
    names: &Names<'_, '_, 'i>,
    term: &cst::Term<'_, 'i, &'i str>,
    errors: &mut E,
) -> Option<&'a nst::Term<'a, 'i>>
where
    E: DiagnosticSink<NameResolutionError<'i>>,
{
    Some(arena.alloc(match term {
        cst::Term::Binding {
            var,
            eq,
            value,
            semi,
            expr,
        } => {
            let slot = names.reserve(*var);
            let value = resolve_term(arena, names, value, errors);

            let mut scope = names.subscope();
            let id = scope.insert(*var, Some(slot), errors);
            let expr = resolve_term(arena, &mut scope, expr, errors);

            nst::Term::Binding {
                var: id,
                eq: *eq,
                value: value?,
                semi: *semi,
                expr: expr?,
            }
        }
        cst::Term::Handle {
            with,
            handler,
            do_,
            expr,
        } => {
            let handler = resolve_term(arena, names, handler, errors);
            let expr = resolve_term(arena, names, expr, errors);
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
            rbar,
            body,
        } => {
            let mut scope = names.subscope();
            let id = scope.insert(*arg, None, errors);
            nst::Term::Abstraction {
                lbar: *lbar,
                arg: id,
                rbar: *rbar,
                body: resolve_term(arena, &mut scope, body, errors)?,
            }
        }
        cst::Term::Application {
            func,
            lpar,
            arg,
            rpar,
        } => {
            let func = resolve_term(arena, names, func, errors);
            let arg = resolve_term(arena, names, arg, errors);
            nst::Term::Application {
                func: func?,
                lpar: *lpar,
                arg: arg?,
                rpar: *rpar,
            }
        }
        cst::Term::ProductRow(pr) => {
            nst::Term::ProductRow(resolve_product_row(arena, pr, |target| {
                resolve_term(arena, names, target, errors)
            })?)
        }
        cst::Term::SumRow(sr) => nst::Term::SumRow(resolve_sum_row(sr, |target| {
            resolve_term(arena, names, target, errors)
        })?),
        t @ cst::Term::DotAccess { base, dot, field } => {
            match resolve_nested_dots(arena, names, base, *dot, *field, errors)? {
                DotResolution::Module(m) => {
                    errors.add(NameResolutionError::ModuleTerm(t.span().of(m)));
                    None?
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
                let mut scope = names.subscope();
                let pattern = resolve_pattern(arena, &mut scope, field.label, errors);
                let target = resolve_term(arena, &mut scope, field.target, errors);
                Some(Field {
                    label: pattern?,
                    sep: field.sep,
                    target: target?,
                })
            })?,
            rangle: *rangle,
        },
        cst::Term::SymbolRef(var) => {
            let name = names.get(*var, errors)?;
            match name.value {
                Name::Module(m) => {
                    errors.add(NameResolutionError::ModuleTerm(name.span().of(m)));
                    None?
                }
                Name::Item(m, i) => nst::Term::ItemRef(name.span().of((m, i))),
                Name::Variable(v) => nst::Term::VariableRef(name.span().of(v)),
            }
        }
        cst::Term::Parenthesized { lpar, term, rpar } => nst::Term::Parenthesized {
            lpar: *lpar,
            term: resolve_term(arena, names, term, errors)?,
            rpar: *rpar,
        },
    }))
}

/// Resolves the given item, reporting errors to `errors`.
pub fn resolve_item<'a, 'i, E>(
    arena: &'a Bump,
    names: &BaseNames<'_, 'i>,
    id: ItemId,
    item: &cst::Item<'_, 'i, &'i str>,
    errors: &mut E,
) -> Option<nst::Item<'a, 'i>>
where
    E: DiagnosticSink<NameResolutionError<'i>>,
{
    Some(match item {
        cst::Item::Term { name, eq, value } => nst::Item::Term {
            name: name.span().of(id),
            eq: *eq,
            value: arena.alloc(resolve_term(arena, &Names::new(names), value, errors)?),
        },
    })
}

/// The result of resolving an item.
#[derive(Debug)]
pub enum ItemResolution<'a, 'i> {
    Succeeded(nst::Item<'a, 'i>),
    Failed(SpanOf<&'i str>),
}

#[derive(Debug)]
pub struct ModuleResolution<'a, 'i> {
    pub items: Vec<SpanOf<&'i str>>,
    pub vars: Vec<SpanOf<&'i str>>,
    pub item_names: HashMap<&'i str, ItemId>,
    pub resolved_items: Vec<ItemResolution<'a, 'i>>,
}

/// Resolves the given module, reporting errors to `errors`.
pub fn resolve_module<'a, 'i, E>(
    arena: &'a Bump,
    this: ModuleId,
    mtree: &ModuleTree<'_, 'i>,
    items: &[cst::Item<'_, 'i, &'i str>],
    errors: &mut E,
) -> ModuleResolution<'a, 'i>
where
    E: DiagnosticSink<NameResolutionError<'i>>,
{
    let its = items.iter().map(cst::Item::name).collect();

    // Collect top-level names first so they can reference each other in `letrec` fashion. We'll do
    // recursion checking later.
    let inames = {
        let mut inames: HashMap<&str, SpanOf<usize>> = HashMap::new();
        for (i, item) in items.iter().enumerate() {
            match inames.entry(item.name().value) {
                Entry::Occupied(o) => errors.add(NameResolutionError::Duplicate {
                    original: o.get().span().of(o.key()),
                    duplicate: item.name(),
                }),
                Entry::Vacant(v) => {
                    v.insert(item.name().span().of(i));
                }
            }
        }
        inames
            .into_iter()
            .map(|(n, is)| (n, ItemId(is.value)))
            .collect()
    };

    let base = BaseNames::new(this, mtree, &inames);
    let res = items
        .iter()
        .enumerate()
        .map(
            |(i, item)| match resolve_item(arena, &base, ItemId(i), item, errors) {
                Some(it) => ItemResolution::Succeeded(it),
                None => ItemResolution::Failed(item.name()),
            },
        )
        .collect();
    let vars = base.into_vars();
    ModuleResolution {
        items: its,
        vars,
        item_names: inames,
        resolved_items: res,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use aiahr_core::{
        diagnostic::nameres::NameResolutionError, field, id::ModuleId, id_field, nitem_term, nst,
        nterm_abs, nterm_app, nterm_dot, nterm_item, nterm_local, nterm_match, nterm_prod,
        nterm_sum, nterm_var, nterm_with, pat_prod, pat_var, span::SpanOf, span_of,
    };
    use aiahr_parser::{
        lexer::aiahr_lexer,
        parser::{aiahr_parser, term, to_stream},
    };
    use assert_matches::assert_matches;
    use bumpalo::Bump;
    use chumsky::Parser;

    use crate::{base::BaseNames, modules::ModuleTree, names::Names};

    use super::{resolve_module, resolve_term, ItemResolution};

    fn parse_resolve_term<'a, 'i>(
        arena: &'a Bump,
        input: &'i str,
    ) -> (
        Option<&'a nst::Term<'a, 'i>>,
        Vec<SpanOf<&'i str>>,
        Vec<NameResolutionError<'i>>,
    ) {
        let (tokens, eoi) = aiahr_lexer().lex(input).unwrap();
        let unresolved = term(arena).parse(to_stream(tokens, eoi)).unwrap();
        let mut errors = Vec::new();

        let inames = HashMap::new();
        let mtree = ModuleTree::new();
        let base = BaseNames::new(ModuleId(0), &mtree, &inames);
        let names = Names::new(&base);

        let resolved = resolve_term(arena, &names, unresolved, &mut errors);
        (resolved, base.into_vars(), errors)
    }

    fn parse_resolve_module<'a, 'i>(
        arena: &'a Bump,
        input: &'i str,
    ) -> (
        Vec<ItemResolution<'a, 'i>>,
        Vec<SpanOf<&'i str>>,
        Vec<SpanOf<&'i str>>,
        Vec<NameResolutionError<'i>>,
    ) {
        let (tokens, eoi) = aiahr_lexer().lex(input).unwrap();
        let unresolved = aiahr_parser(arena).parse(to_stream(tokens, eoi)).unwrap();
        let mut errors = Vec::new();
        let resolved = resolve_module(
            arena,
            ModuleId(0),
            &ModuleTree::new(),
            unresolved,
            &mut errors,
        );
        (
            resolved.resolved_items,
            resolved.items,
            resolved.vars,
            errors,
        )
    }

    #[test]
    fn test_local_binding() {
        let arena = Bump::new();
        let (term, vars, errs) = parse_resolve_term(&arena, "x = {}; y = {}; x");
        assert_matches!(
            term,
            Some(nterm_local!(
                x,
                nterm_prod!(),
                nterm_local!(y, nterm_prod!(), nterm_var!(x1))
            )) => {
                assert_eq!(vars[x.0].value, "x");
                assert_eq!(vars[y.0].value, "y");
                assert_eq!(x1, x);
            }
        );
        assert_matches!(errs[..], []);
    }

    #[test]
    fn test_local_binding_shadowing() {
        let arena = Bump::new();
        let (term, vars, errs) = parse_resolve_term(&arena, "x = {}; x = x; x");
        assert_matches!(term,
            Some(nterm_local!(
                x_out,
                nterm_prod!(),
                nterm_local!(x_in, nterm_var!(x1), nterm_var!(x2))
            )) => {
                assert_eq!(vars[x_out.0].value, "x");
                assert_eq!(vars[x_in.0].value, "x");
                assert_eq!(x1, x_out);
                assert_eq!(x2, x_in);
            }
        );
        assert_matches!(errs[..], []);
    }

    #[test]
    fn test_local_binding_errors() {
        let arena = Bump::new();
        let (term, _, errs) = parse_resolve_term(&arena, "x = y; z");
        assert_matches!(term, None);
        assert_matches!(
            errs[..],
            [
                NameResolutionError::NotFound(span_of!("y")),
                NameResolutionError::NotFound(span_of!("z"))
            ]
        );
    }

    #[test]
    fn test_handler() {
        let arena = Bump::new();
        let (term, vars, errs) = parse_resolve_term(&arena, "x = {}; with x do x");
        assert_matches!(term,
            Some(nterm_local!(
                x,
                nterm_prod!(),
                nterm_with!(nterm_var!(x1), nterm_var!(x2))
            )) => {
                assert_eq!(vars[x.0].value, "x");
                assert_eq!(x1, x);
                assert_eq!(x2, x);
            }
        );
        assert_matches!(errs[..], []);
    }

    #[test]
    fn test_handler_errors() {
        let arena = Bump::new();
        let (term, _, errs) = parse_resolve_term(&arena, "with h do x");
        assert_matches!(term, None);
        assert_matches!(
            errs[..],
            [
                NameResolutionError::NotFound(span_of!("h")),
                NameResolutionError::NotFound(span_of!("x"))
            ]
        );
    }

    #[test]
    fn test_abstraction() {
        let arena = Bump::new();
        let (term, vars, errs) = parse_resolve_term(&arena, "|x| |y| y(x)");
        assert_matches!(term,
            Some(nterm_abs!(
                x,
                nterm_abs!(
                    y,
                    nterm_app!(nterm_var!(y1), nterm_var!(x1))
                )
            )) => {
                assert_eq!(vars[x.0].value, "x");
                assert_eq!(vars[y.0].value, "y");
                assert_eq!(x1, x);
                assert_eq!(y1, y);
            }
        );
        assert_matches!(errs[..], []);
    }

    #[test]
    fn test_abstraction_shadowing() {
        let arena = Bump::new();
        let (term, vars, errs) = parse_resolve_term(&arena, "|x| |x| x(x)");
        assert_matches!(term,
            Some(nterm_abs!(
                x_out,
                nterm_abs!(
                    x_in,
                    nterm_app!(nterm_var!(x1), nterm_var!(x2))
                )
            )) => {
                assert_eq!(vars[x_out.0].value, "x");
                assert_eq!(vars[x_in.0].value, "x");
                assert_eq!(x1, x_in);
                assert_eq!(x2, x_in);
            }
        );
        assert_matches!(errs[..], []);
    }

    #[test]
    fn test_application() {
        let arena = Bump::new();
        let (term, vars, errs) = parse_resolve_term(&arena, "|x| x(x)");
        assert_matches!(term,
            Some(nterm_abs!(
                x,
                nterm_app!(nterm_var!(x1), nterm_var!(x2))
            )) => {
                assert_eq!(vars[x.0].value, "x");
                assert_eq!(x1, x);
                assert_eq!(x2, x);
            }
        );
        assert_matches!(errs[..], []);
    }

    #[test]
    fn test_application_errors() {
        let arena = Bump::new();
        let (term, _, errs) = parse_resolve_term(&arena, "f(x)");
        assert_matches!(term, None);
        assert_matches!(
            errs[..],
            [
                NameResolutionError::NotFound(span_of!("f")),
                NameResolutionError::NotFound(span_of!("x"))
            ]
        );
    }

    #[test]
    fn test_product_row() {
        let arena = Bump::new();
        let (term, vars, errs) = parse_resolve_term(&arena, "x = {}; {a = x, b = {x = x}}");
        assert_matches!(term,
            Some(nterm_local!(x, nterm_prod!(),
                nterm_prod!(
                id_field!("a", nterm_var!(x1)),
                id_field!(
                    "b",
                    nterm_prod!(id_field!("x", nterm_var!(x2)))
                ),
            ))) => {
                assert_eq!(vars[x.0].value, "x");
                assert_eq!(x1, x);
                assert_eq!(x2, x);
            }
        );
        assert_matches!(errs[..], []);
    }

    #[test]
    fn test_product_row_errors() {
        let arena = Bump::new();
        let (term, _, errs) = parse_resolve_term(&arena, "{x = y, z = x}");
        assert_matches!(term, None);
        assert_matches!(
            errs[..],
            [
                NameResolutionError::NotFound(span_of!("y")),
                NameResolutionError::NotFound(span_of!("x"))
            ]
        );
    }

    #[test]
    fn test_sum_row() {
        let arena = Bump::new();
        let (term, vars, errs) = parse_resolve_term(&arena, "|x| <a = x>");
        assert_matches!(term,
            Some(nterm_abs!(x, nterm_sum!(id_field!("a", nterm_var!(x1))))) => {
                assert_eq!(vars[x.0].value, "x");
                assert_eq!(x1, x);
            }
        );
        assert_matches!(errs[..], []);
    }

    #[test]
    fn test_sum_row_errors() {
        let arena = Bump::new();
        let (term, _, errs) = parse_resolve_term(&arena, "<x = x>");
        assert_matches!(term, None);
        assert_matches!(errs[..], [NameResolutionError::NotFound(span_of!("x"))]);
    }

    #[test]
    fn test_dot_access() {
        let arena = Bump::new();
        let (term, vars, errs) = parse_resolve_term(&arena, "id = |x| x; {x = id}.x");
        assert_matches!(term,
            Some(nterm_local!(
                id,
                nterm_abs!(x, nterm_var!(x1)),
                nterm_dot!(
                    nterm_prod!(id_field!("x", nterm_var!(id1))),
                    "x"
                )
            )) => {
                assert_eq!(vars[id.0].value, "id");
                assert_eq!(vars[x.0].value, "x");
                assert_eq!(x1, x);
                assert_eq!(id1, id);
            }
        );
        assert_matches!(errs[..], []);
    }

    #[test]
    fn test_dot_access_errors() {
        let arena = Bump::new();
        let (term, _, errs) = parse_resolve_term(&arena, "x.a");
        assert_matches!(term, None);
        assert_matches!(errs[..], [NameResolutionError::NotFound(span_of!("x"))]);
    }

    #[test]
    fn test_match() {
        let arena = Bump::new();
        let (term, vars, errs) = parse_resolve_term(&arena, "match <{a = x} => x, y => y>");
        assert_matches!(term,
            Some(nterm_match!(
                field!(pat_prod!(id_field!("a", pat_var!(x))), nterm_var!(x1)),
                field!(pat_var!(y), nterm_var!(y1))
            )) => {
                assert_eq!(vars[x.0].value, "x");
                assert_eq!(vars[y.0].value, "y");
                assert_eq!(x1, x);
                assert_eq!(y1, y);
            }
        );
        assert_matches!(errs[..], []);
    }

    #[test]
    fn test_match_errors() {
        let arena = Bump::new();
        let (term, _, errs) = parse_resolve_term(&arena, "match <{a = x} => f(x), {} => z>");
        assert_matches!(term, None);
        assert_matches!(
            errs[..],
            [
                NameResolutionError::NotFound(span_of!("f")),
                NameResolutionError::NotFound(span_of!("z"))
            ]
        );
        let (term, vars, errs) = parse_resolve_term(&arena, "match <{a = x, b = x} => x(x)>");
        assert_matches!(
            term,
            Some(nterm_match!(field!(
                pat_prod!(
                    id_field!("a", pat_var!(x)),
                    id_field!("b", pat_var!(x_again))
                ),
                nterm_app!(nterm_var!(x1), nterm_var!(x2))
            ))) => {
                assert_eq!(vars[x.0].value, "x");
                assert_eq!(vars[x_again.0].value, "x");
                assert_eq!(x1, x);
                assert_eq!(x2, x);
            }
        );
        assert_matches!(
            errs[..],
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
        let arena = Bump::new();
        let (term, vars, errs) = parse_resolve_term(&arena, "x = {}; |x| match <x => x>");
        assert_matches!(term,
            Some(nterm_local!(x_top, nterm_prod!(), nterm_abs!(x_mid, nterm_match!(field!(pat_var!(x_bot), nterm_var!(x1)))))) => {
                assert_eq!(vars[x_top.0].value, "x");
                assert_eq!(vars[x_mid.0].value, "x");
                assert_eq!(vars[x_bot.0].value, "x");
                assert_eq!(x1, x_bot);
            }
        );
        assert_matches!(errs[..], []);
    }

    #[test]
    fn test_top_level_letrec() {
        let arena = Bump::new();
        let (res, items, _, errs) = parse_resolve_module(&arena, "foo = bar\nbar = foo");
        assert_matches!(
            res[..],
            [
                ItemResolution::Succeeded(nitem_term!(foo, nterm_item!(mbar, bar1))),
                ItemResolution::Succeeded(nitem_term!(bar, nterm_item!(mfoo, foo1)))
            ] => {
                assert_eq!(items[foo.0].value, "foo");
                assert_eq!(items[bar.0].value, "bar");
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
        let (res, _, _, errs) = parse_resolve_module(&arena, "foo = x\nbar = y");
        assert_matches!(
            res[..],
            [
                ItemResolution::Failed(span_of!("foo")),
                ItemResolution::Failed(span_of!("bar")),
            ]
        );
        assert_matches!(
            errs[..],
            [
                NameResolutionError::NotFound(span_of!("x")),
                NameResolutionError::NotFound(span_of!("y"))
            ]
        );
    }
}
