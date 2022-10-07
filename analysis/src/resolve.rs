use std::collections::{hash_map::Entry, HashMap};

use crate::{
    base::BaseNames,
    modules::{Member, ModuleTree},
    names::{Name, Names},
};
use aiahr_core::{
    cst::{self, Field, IdField, ProductRow, Separated, SumRow},
    diagnostic::{nameres::NameResolutionError, DiagnosticSink},
    id::{IdGen, Ids, ItemId, ModuleId, VarId},
    if_none::IfNone,
    memory::handle::RefHandle,
    nst,
    span::{Span, SpanOf, Spanned},
};
use bumpalo::Bump;

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

/// Resolves the given pattern, accumulating bindings into `names`.
///
/// Note that this currently cannot return `None`, although it can emit errors.
pub fn resolve_pattern<'a, 's, E>(
    arena: &'a Bump,
    names: &mut Names<'_, '_, 's>,
    pattern: &cst::Pattern<'_, 's>,
    vars: &mut IdGen<VarId, SpanOf<RefHandle<'s, str>>>,
    errors: &mut E,
) -> Option<&'a nst::Pattern<'a, 's>>
where
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    Some(arena.alloc(match pattern {
        cst::Pattern::ProductRow(pr) => {
            nst::Pattern::ProductRow(resolve_product_row(arena, pr, |target| {
                resolve_pattern(arena, names, target, vars, errors)
            })?)
        }
        cst::Pattern::SumRow(sr) => nst::Pattern::SumRow(resolve_sum_row(sr, |target| {
            resolve_pattern(arena, names, target, vars, errors)
        })?),
        cst::Pattern::Whole(var) => nst::Pattern::Whole(var.span_map(|var| {
            let id = vars.push(*var);
            if let Some(orig) = names.insert(var.value, id) {
                errors.add(NameResolutionError::Duplicate {
                    name: var.value,
                    original: vars[orig].span(),
                    duplicate: var.span(),
                })
            }
            id
        })),
    }))
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

impl<'a, 's> DotResolution<'a, 's> {
    fn from_member(parent: ModuleId, memb: Member) -> DotResolution<'a, 's> {
        match memb {
            Member::Module(m) => DotResolution::Module(m),
            Member::Item(i) => DotResolution::Item(parent, i),
        }
    }
}

// Resolves nested `DotAccess` terms.
fn resolve_nested_dots<'a, 's, E>(
    arena: &'a Bump,
    names: &Names<'_, '_, 's>,
    base: &cst::Term<'_, 's>,
    dot: Span,
    field: SpanOf<RefHandle<'s, str>>,
    vars: &mut IdGen<VarId, SpanOf<RefHandle<'s, str>>>,
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
        } => match resolve_nested_dots(arena, names, base2, *dot2, *field2, vars, errors)? {
            // m . field
            DotResolution::Module(m) => match names.get_in(m, field.value).if_none(|| {
                errors.add(NameResolutionError::NotFoundIn {
                    module: m,
                    name: field,
                })
            })? {
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
        cst::Term::SymbolRef(n) => {
            match names
                .get(n.value)
                .if_none(|| errors.add(NameResolutionError::NotFound(*n)))?
            {
                Name::Module(m) => DotResolution::from_member(
                    m,
                    names.get_in(m, field.value).if_none(|| {
                        errors.add(NameResolutionError::NotFoundIn {
                            module: m,
                            name: field,
                        })
                    })?,
                ),
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
            }
        }
        // (expr) . field
        _ => DotResolution::FieldAccess {
            base: resolve_term(arena, names, base, vars, errors)?,
            dot,
            field,
        },
    })
}

/// Resolves the given term, reporting errors to `errors`.
pub fn resolve_term<'a, 's, E>(
    arena: &'a Bump,
    names: &Names<'_, '_, 's>,
    term: &cst::Term<'_, 's>,
    vars: &mut IdGen<VarId, SpanOf<RefHandle<'s, str>>>,
    errors: &mut E,
) -> Option<&'a nst::Term<'a, 's>>
where
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    Some(
        arena.alloc(match term {
            cst::Term::Binding {
                var,
                eq,
                value,
                semi,
                expr,
            } => {
                let id = vars.push(*var);
                let value = resolve_term(arena, names, value, vars, errors);

                let mut scope = names.subscope();
                if let Some(orig) = scope.insert(var.value, id) {
                    errors.add(NameResolutionError::Duplicate {
                        name: var.value,
                        original: vars[orig].span(),
                        duplicate: var.span(),
                    })
                }
                let expr = resolve_term(arena, &mut scope, expr, vars, errors);

                nst::Term::Binding {
                    var: var.span().of(id),
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
                let handler = resolve_term(arena, names, handler, vars, errors);
                let expr = resolve_term(arena, names, expr, vars, errors);
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
                let id = vars.push(*arg);
                if let Some(orig) = scope.insert(arg.value, id) {
                    errors.add(NameResolutionError::Duplicate {
                        name: arg.value,
                        original: vars[orig].span(),
                        duplicate: arg.span(),
                    })
                }
                nst::Term::Abstraction {
                    lbar: *lbar,
                    arg: arg.span().of(id),
                    rbar: *rbar,
                    body: resolve_term(arena, &mut scope, body, vars, errors)?,
                }
            }
            cst::Term::Application {
                func,
                lpar,
                arg,
                rpar,
            } => {
                let func = resolve_term(arena, names, func, vars, errors);
                let arg = resolve_term(arena, names, arg, vars, errors);
                nst::Term::Application {
                    func: func?,
                    lpar: *lpar,
                    arg: arg?,
                    rpar: *rpar,
                }
            }
            cst::Term::ProductRow(pr) => {
                nst::Term::ProductRow(resolve_product_row(arena, pr, |target| {
                    resolve_term(arena, names, target, vars, errors)
                })?)
            }
            cst::Term::SumRow(sr) => nst::Term::SumRow(resolve_sum_row(sr, |target| {
                resolve_term(arena, names, target, vars, errors)
            })?),
            t @ cst::Term::DotAccess { base, dot, field } => {
                match resolve_nested_dots(arena, names, base, *dot, *field, vars, errors)? {
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
                    let pattern = resolve_pattern(arena, &mut scope, field.label, vars, errors);
                    let target = resolve_term(arena, &mut scope, field.target, vars, errors);
                    Some(Field {
                        label: pattern?,
                        sep: field.sep,
                        target: target?,
                    })
                })?,
                rangle: *rangle,
            },
            cst::Term::SymbolRef(var) => {
                match names
                    .get(var.value)
                    .if_none(|| errors.add(NameResolutionError::NotFound(*var)))?
                {
                    Name::Module(m) => {
                        errors.add(NameResolutionError::ModuleTerm(var.span().of(m)));
                        None?
                    }
                    Name::Item(m, i) => nst::Term::ItemRef(var.span().of((m, i))),
                    Name::Variable(v) => nst::Term::VariableRef(var.span().of(v)),
                }
            }
            cst::Term::Parenthesized { lpar, term, rpar } => nst::Term::Parenthesized {
                lpar: *lpar,
                term: resolve_term(arena, names, term, vars, errors)?,
                rpar: *rpar,
            },
        }),
    )
}

/// Resolves the given item, reporting errors to `errors`.
pub fn resolve_item<'a, 's, E>(
    arena: &'a Bump,
    names: &BaseNames<'_, 's>,
    id: ItemId,
    item: &cst::Item<'_, 's>,
    vars: &mut IdGen<VarId, SpanOf<RefHandle<'s, str>>>,
    errors: &mut E,
) -> Option<nst::Item<'a, 's>>
where
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    Some(match item {
        cst::Item::Term { name, eq, value } => nst::Item::Term {
            name: name.span().of(id),
            eq: *eq,
            value: arena.alloc(resolve_term(
                arena,
                &Names::new(names),
                value,
                vars,
                errors,
            )?),
        },
    })
}

/// The result of resolving an item.
#[derive(Debug)]
pub enum ItemResolution<'a, 's> {
    Succeeded(nst::Item<'a, 's>),
    Failed(SpanOf<RefHandle<'s, str>>),
}

#[derive(Debug)]
pub struct ModuleResolution<'a, 's> {
    pub items: Box<Ids<ItemId, SpanOf<RefHandle<'s, str>>>>,
    pub vars: Box<Ids<VarId, SpanOf<RefHandle<'s, str>>>>,
    pub item_names: HashMap<RefHandle<'s, str>, ItemId>,
    pub resolved_items: Vec<ItemResolution<'a, 's>>,
}

/// Resolves the given module, reporting errors to `errors`.
pub fn resolve_module<'a, 's, E>(
    arena: &'a Bump,
    this: ModuleId,
    mtree: &ModuleTree<'_, 's>,
    items: &[cst::Item<'_, 's>],
    errors: &mut E,
) -> ModuleResolution<'a, 's>
where
    E: DiagnosticSink<NameResolutionError<'s>>,
{
    let its = items.iter().map(cst::Item::name).collect();

    // Collect top-level names first so they can reference each other in `letrec` fashion. We'll do
    // recursion checking later.
    let inames = {
        let mut inames: HashMap<RefHandle<'s, str>, SpanOf<usize>> = HashMap::new();
        for (i, item) in items.iter().enumerate() {
            match inames.entry(item.name().value) {
                Entry::Occupied(o) => errors.add(NameResolutionError::Duplicate {
                    name: *o.key(),
                    original: o.get().span(),
                    duplicate: item.name().span(),
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
    let mut vars = IdGen::new();
    let res = items
        .iter()
        .enumerate()
        .map(
            |(i, item)| match resolve_item(arena, &base, ItemId(i), item, &mut vars, errors) {
                Some(it) => ItemResolution::Succeeded(it),
                None => ItemResolution::Failed(item.name()),
            },
        )
        .collect();
    ModuleResolution {
        items: its,
        vars: vars.into_boxed_ids(),
        item_names: inames,
        resolved_items: res,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use aiahr_core::{
        diagnostic::nameres::NameResolutionError,
        field, h,
        id::{IdGen, Ids, ItemId, ModuleId, VarId},
        id_field,
        memory::{
            arena::BumpArena,
            handle::RefHandle,
            intern::{InternerByRef, SyncInterner},
        },
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

    use crate::{base::BaseNames, modules::ModuleTree, names::Names};

    use super::{resolve_module, resolve_term, ItemResolution};

    fn parse_resolve_term<'a, 's, S>(
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
        let (tokens, eoi) = aiahr_lexer(interner).lex(input).unwrap();
        let unresolved = term(arena).parse(to_stream(tokens, eoi)).unwrap();
        let mut errors = Vec::new();

        let inames = HashMap::new();
        let mtree = ModuleTree::new();
        let base = BaseNames::new(ModuleId(0), &mtree, &inames);
        let mut vars = IdGen::new();
        let names = Names::new(&base);

        let resolved = resolve_term(arena, &names, unresolved, &mut vars, &mut errors);
        (resolved, vars.into_boxed_ids(), errors)
    }

    fn parse_resolve_module<'a, 's, S>(
        arena: &'a Bump,
        interner: &'s S,
        input: &str,
    ) -> (
        Vec<ItemResolution<'a, 's>>,
        Box<Ids<ItemId, SpanOf<RefHandle<'s, str>>>>,
        Box<Ids<VarId, SpanOf<RefHandle<'s, str>>>>,
        Vec<NameResolutionError<'s>>,
    )
    where
        S: InternerByRef<str>,
    {
        let (tokens, eoi) = aiahr_lexer(interner).lex(input).unwrap();
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
                NameResolutionError::NotFound(span_of!(h!("y"))),
                NameResolutionError::NotFound(span_of!(h!("z")))
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
                NameResolutionError::NotFound(span_of!(h!("h"))),
                NameResolutionError::NotFound(span_of!(h!("x")))
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
                NameResolutionError::NotFound(span_of!(h!("f"))),
                NameResolutionError::NotFound(span_of!(h!("x")))
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
                NameResolutionError::NotFound(span_of!(h!("y"))),
                NameResolutionError::NotFound(span_of!(h!("x")))
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
        assert_matches!(errs[..], [NameResolutionError::NotFound(span_of!(h!("x")))]);
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
        assert_matches!(errs[..], [NameResolutionError::NotFound(span_of!(h!("x")))]);
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
                NameResolutionError::NotFound(span_of!(h!("f"))),
                NameResolutionError::NotFound(span_of!(h!("z")))
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
        let (res, items, _, errs) = parse_resolve_module(&arena, &interner, "foo = bar\nbar = foo");
        assert_matches!(
            res[..],
            [
                ItemResolution::Succeeded(nitem_term!(foo, nterm_item!(mbar, bar1))),
                ItemResolution::Succeeded(nitem_term!(bar, nterm_item!(mfoo, foo1)))
            ] => {
                assert_eq!(items[foo].value.0, "foo");
                assert_eq!(items[bar].value.0, "bar");
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
        let (res, _, _, errs) = parse_resolve_module(&arena, &interner, "foo = x\nbar = y");
        assert_matches!(
            res[..],
            [
                ItemResolution::Failed(span_of!(h!("foo"))),
                ItemResolution::Failed(span_of!(h!("bar"))),
            ]
        );
        assert_matches!(
            errs[..],
            [
                NameResolutionError::NotFound(span_of!(h!("x"))),
                NameResolutionError::NotFound(span_of!(h!("y")))
            ]
        );
    }
}
