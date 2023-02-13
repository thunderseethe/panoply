use aiahr_core::ast::Direction;
use aiahr_core::span::Spanned;
use aiahr_core::{
    ast,
    ast::{Ast, Term::*},
    id::VarId,
    nst,
    span::Span,
};
use bumpalo::Bump;
use rustc_hash::FxHashMap;

/// Desugar a NST into an AST.
/// This removes syntax sugar and lowers down into AST which contains a subset of Nodes availabe in
/// the NST.
pub fn desugar<'n, 's: 'a, 'a>(arena: &'a Bump, nst: &'n nst::Term<'n, 's>) -> Ast<'a, VarId> {
    fn ds<'n, 's: 'a, 'a>(
        arena: &'a Bump,
        spans: &mut FxHashMap<&'a ast::Term<'a, VarId>, Span>,
        nst: &'n nst::Term<'n, 's>,
    ) -> &'a ast::Term<'a, VarId> {
        // TODO: Finish this impl
        // This is a stub of functionality right now. Mostly to act as a gate to what can reach the
        // type checker. Anything not implemented in this function isn't handled by the typechecker
        // yet.
        let ast = match nst {
            nst::Term::VariableRef(var) => arena.alloc(ast::Term::Variable(var.value)) as &_,
            nst::Term::ItemRef(item) => arena.alloc(ast::Term::Item(item.value)) as &_,
            nst::Term::Binding {
                var, value, expr, ..
            } => {
                let value = ds(arena, spans, value);
                let expr = ds(arena, spans, expr);
                let func = arena.alloc(Abstraction {
                    arg: var.value,
                    body: expr,
                });
                arena.alloc(Application { func, arg: value }) as &_
            }
            nst::Term::Abstraction { arg, body, .. } => {
                let body = ds(arena, spans, body);
                arena.alloc(Abstraction {
                    arg: arg.value,
                    body,
                }) as &_
            }
            nst::Term::Application { func, arg, .. } => {
                let func = ds(arena, spans, func);
                let arg = ds(arena, spans, arg);
                arena.alloc(Application { func, arg }) as &_
            }
            nst::Term::Parenthesized { term, .. } => {
                // We'll replace the span of this node with the parenthesized span
                ds(arena, spans, term) as &_
            }
            nst::Term::ProductRow(product) => match product.fields {
                None => arena.alloc(Unit),
                Some(fields) => {
                    let head = arena.alloc(Label {
                        label: fields.first.label.value,
                        term: ds(arena, spans, fields.first.target),
                    });
                    fields.elems.iter().fold(head, |concat, (_, field)| {
                        let right = arena.alloc(Label {
                            label: field.label.value,
                            term: ds(arena, spans, field.target),
                        });
                        arena.alloc(Concat {
                            left: concat,
                            right,
                        })
                    })
                }
            },
            nst::Term::FieldAccess { base, field, .. } => {
                let term = ds(arena, spans, base);
                arena.alloc(Unlabel {
                    label: field.value,
                    term: arena.alloc(Project {
                        direction: Direction::Right,
                        term,
                    }),
                })
            }
            nst::Term::SumRow(sum) => {
                let term = ds(arena, spans, sum.field.target);
                arena.alloc(Inject {
                    direction: Direction::Right,
                    term: arena.alloc(Label {
                        label: sum.field.label.value,
                        term,
                    }),
                })
            }
            // This is gonna take a little more work.
            nst::Term::Match { .. } => {
                // TODO: Figure out desugaring patterns into binds.
                // Maybe detour through a stage to ensure that each match is only one layer of
                // patterns?
                todo!()
            }
            nst::Term::Handle { .. } => todo!(),
        };
        spans.insert(ast, nst.span());
        ast
    }
    let mut spans = FxHashMap::default();
    let tree = ds(arena, &mut spans, nst);
    Ast::new(spans, tree)
}

#[cfg(test)]
mod tests {
    use super::*;
    use aiahr_core::cst::{Field, IdField, ProductRow, Separated};
    use aiahr_core::id::ModuleId;
    use aiahr_core::memory::arena::BumpArena;
    use aiahr_core::memory::intern::InternerByRef;
    use aiahr_core::memory::intern::SyncInterner;
    use aiahr_core::{id::VarId, loc::Loc, nst, span::SpanOf};
    use bumpalo::Bump;

    const MOD: ModuleId = ModuleId(0);

    fn random_loc() -> Loc {
        Loc {
            module: MOD,
            byte: rand::random(),
            line: rand::random(),
            col: rand::random(),
        }
    }
    fn random_span() -> Span {
        Span {
            start: random_loc(),
            end: random_loc(),
        }
    }
    fn random_span_of<T>(value: T) -> SpanOf<T> {
        let span = random_span();
        SpanOf {
            start: span.start,
            value,
            end: span.end,
        }
    }

    #[test]
    fn test_desugar_var() {
        let arena = Bump::new();
        let var = random_span_of(VarId(0));
        let nst = arena.alloc(nst::Term::VariableRef(var));
        let ast = desugar(&arena, nst);

        assert_eq!(ast.tree, &Variable(var.value));
        assert_eq!(
            ast.span_of(ast.tree),
            Some(&Span {
                start: var.start,
                end: var.end,
            })
        );
    }

    #[test]
    fn test_desugar_abs() {
        let arena = Bump::new();
        let start = random_span();
        let span_of_var = random_span_of(VarId(0));
        let nst = arena.alloc(nst::Term::Abstraction {
            lbar: start,
            arg: random_span_of(VarId(0)),
            annotation: None,
            rbar: random_span(),
            body: arena.alloc(nst::Term::VariableRef(span_of_var)),
        });
        let ast = desugar(&arena, nst);

        assert_eq!(
            ast.tree,
            &Abstraction {
                arg: VarId(0),
                body: &Variable(VarId(0))
            }
        );
        assert_eq!(
            ast.span_of(ast.tree),
            Some(&Span {
                start: start.start,
                end: span_of_var.end,
            })
        );
    }

    #[test]
    fn test_desugar_app() {
        let arena = Bump::new();
        let start = random_span_of(VarId(0));
        let end = random_span();
        let ast = desugar(
            &arena,
            &nst::Term::Application {
                func: arena.alloc(nst::Term::VariableRef(start)),
                lpar: random_span(),
                arg: arena.alloc(nst::Term::VariableRef(random_span_of(VarId(1)))),
                rpar: end,
            },
        );

        assert_eq!(
            ast.tree,
            &Application {
                func: &Variable(VarId(0)),
                arg: &Variable(VarId(1))
            }
        );
        assert_eq!(
            ast.span_of(ast.tree),
            Some(&Span {
                start: start.start,
                end: end.end,
            })
        );
    }

    #[test]
    fn test_desugar_binding() {
        let arena = Bump::new();
        let start = random_span_of(VarId(2));
        let end = random_span_of(VarId(123));
        let ast = desugar(
            &arena,
            arena.alloc(nst::Term::Binding {
                var: start,
                annotation: None,
                eq: random_span(),
                value: arena.alloc(nst::Term::VariableRef(random_span_of(VarId(10)))),
                semi: random_span(),
                expr: arena.alloc(nst::Term::VariableRef(end)),
            }),
        );

        assert_eq!(
            ast.tree,
            &Application {
                func: &Abstraction {
                    arg: VarId(2),
                    body: &Variable(VarId(123)),
                },
                arg: &Variable(VarId(10)),
            }
        );
        assert_eq!(
            ast.span_of(ast.tree),
            Some(&Span {
                start: start.start,
                end: end.end,
            })
        );
    }

    #[test]
    fn test_desugar_unit() {
        let arena = Bump::new();
        let start = random_span();
        let end = random_span();
        let ast = desugar(
            &arena,
            arena.alloc(nst::Term::ProductRow(ProductRow {
                lbrace: start,
                fields: None,
                rbrace: end,
            })),
        );

        assert_eq!(ast.tree, &Unit);
        assert_eq!(
            ast.span_of(ast.tree),
            Some(&Span {
                start: start.start,
                end: end.end,
            })
        );
    }

    #[test]
    fn test_desugar_product() {
        let arena = Bump::new();
        let interner = SyncInterner::new(BumpArena::new());

        let a = interner.intern_by_ref("abc");
        let b = interner.intern_by_ref("def");
        let c = interner.intern_by_ref("ghi");

        let start = random_span();
        let end = random_span();
        let ast = desugar(
            &arena,
            arena.alloc(nst::Term::ProductRow(ProductRow {
                lbrace: start,
                fields: Some(Separated {
                    first: IdField {
                        label: random_span_of(a),
                        sep: random_span(),
                        target: arena.alloc(nst::Term::VariableRef(random_span_of(VarId(0)))),
                    },
                    elems: &[
                        (
                            random_span(),
                            IdField {
                                label: random_span_of(b),
                                sep: random_span(),
                                target: arena
                                    .alloc(nst::Term::VariableRef(random_span_of(VarId(1)))),
                            },
                        ),
                        (
                            random_span(),
                            IdField {
                                label: random_span_of(c),
                                sep: random_span(),
                                target: arena
                                    .alloc(nst::Term::VariableRef(random_span_of(VarId(2)))),
                            },
                        ),
                    ],
                    comma: None,
                }),
                rbrace: end,
            })),
        );

        assert_eq!(
            ast.tree,
            &Concat {
                left: &Concat {
                    left: &Label {
                        label: a,
                        term: &Variable(VarId(0))
                    },
                    right: &Label {
                        label: b,
                        term: &Variable(VarId(1))
                    },
                },
                right: &Label {
                    label: c,
                    term: &Variable(VarId(2))
                },
            }
        );
        assert_eq!(
            ast.span_of(ast.tree),
            Some(&Span {
                start: start.start,
                end: end.end,
            })
        );
    }

    #[test]
    fn test_desugar_field_access() {
        let arena = Bump::new();
        let interner = SyncInterner::new(BumpArena::new());

        let state = interner.intern_by_ref("state");

        let base = random_span_of(VarId(0));
        let field = random_span_of(state);
        let nst = arena.alloc(nst::Term::FieldAccess {
            base: arena.alloc(nst::Term::VariableRef(base)),
            dot: random_span(),
            field,
        });

        let ast = desugar(&arena, nst);
        assert_eq!(
            ast.tree,
            &Unlabel {
                label: state,
                term: &Project {
                    direction: Direction::Right,
                    term: &Variable(VarId(0))
                }
            }
        );
        assert_eq!(
            ast.span_of(ast.tree),
            Some(&Span {
                start: base.start,
                end: field.end,
            }),
        );
    }

    #[test]
    fn test_desugar_sum() {
        let arena = Bump::new();
        let interner = SyncInterner::new(BumpArena::new());

        let tru = interner.intern_by_ref("true");

        let langle = random_span();
        let rangle = random_span();
        let nst = arena.alloc(nst::Term::SumRow(aiahr_core::cst::SumRow {
            langle,
            field: Field {
                label: random_span_of(tru),
                sep: random_span(),
                target: arena.alloc(nst::Term::VariableRef(random_span_of(VarId(0)))),
            },
            rangle,
        }));

        let ast = desugar(&arena, nst);
        assert_eq!(
            ast.tree,
            &Inject {
                direction: Direction::Right,
                term: &Label {
                    label: tru,
                    term: &Variable(VarId(0))
                }
            }
        );
        assert_eq!(
            ast.span_of(ast.tree),
            Some(&Span {
                start: langle.start,
                end: rangle.end,
            }),
        );
    }
}
