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
pub fn desugar<'n, 's, 'a>(arena: &'a Bump, nst: &'n nst::Term<'n, 's>) -> Ast<'a, VarId> {
    fn ds<'n, 's, 'a>(
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
            nst::Term::Handle { .. } => todo!(),
            nst::Term::ProductRow(_) => todo!(),
            nst::Term::SumRow(_) => todo!(),
            nst::Term::FieldAccess { .. } => todo!(),
            nst::Term::Match { .. } => todo!(),
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
    use aiahr_core::{id::VarId, loc::Loc, nst, span::SpanOf};
    use bumpalo::Bump;

    fn random_loc() -> Loc {
        Loc {
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
}
