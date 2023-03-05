use std::ops::Not;

use aiahr_core::ast::Direction;
use aiahr_core::id::{Id, IdGen};
use aiahr_core::memory::handle::{self, RefHandle};
use aiahr_core::nst::Pattern;
use aiahr_core::span::{SpanOf, Spanned};
use aiahr_core::{
    ast,
    ast::{Ast, Term::*},
    id::VarId,
    nst,
    span::Span,
};
use bumpalo::Bump;
use rustc_hash::FxHashMap;

struct DesugarCtx<'a, 'ctx, 's> {
    arena: &'ctx Bump,
    vars: &'a mut IdGen<VarId, RefHandle<'s, str>>,
    spans: FxHashMap<&'ctx ast::Term<'ctx, VarId>, Span>,
}

impl<'a, 'ctx, 's: 'ctx> DesugarCtx<'a, 'ctx, 's> {
    fn new(arena: &'ctx Bump, vars: &'a mut IdGen<VarId, RefHandle<'s, str>>) -> Self {
        Self {
            arena,
            vars,
            spans: FxHashMap::default(),
        }
    }

    /// Desugar a NST Term into it's corresponding AST Term.
    fn ds_term<'n>(
        &mut self,
        nst: &'n nst::Term<'n, 's>,
    ) -> Result<&'ctx ast::Term<'ctx, VarId>, PatternMatchError> {
        let mk_term = |term, spans: &mut FxHashMap<&'ctx ast::Term<'ctx, VarId>, Span>| {
            let t = self.arena.alloc(term) as &_;
            spans.insert(t, nst.span());
            t
        };
        let ast = match nst {
            nst::Term::VariableRef(var) => self.arena.alloc(ast::Term::Variable(var.value)) as &_,
            nst::Term::ItemRef(item) => self.arena.alloc(ast::Term::Item(item.value)) as &_,
            nst::Term::EffectOpRef(SpanOf {
                value: (_, _, op), ..
            }) => self.arena.alloc(ast::Term::Operation(*op)) as &_,
            nst::Term::Binding {
                var, value, expr, ..
            } => {
                let value = self.ds_term(value)?;
                let expr = self.ds_term(expr)?;
                let func = mk_term(
                    Abstraction {
                        arg: var.value,
                        body: expr,
                    },
                    &mut self.spans,
                );
                self.arena.alloc(Application { func, arg: value }) as &_
            }
            nst::Term::Abstraction { arg, body, .. } => {
                let body = self.ds_term(body)?;
                self.arena.alloc(Abstraction {
                    arg: arg.value,
                    body,
                }) as &_
            }
            nst::Term::Application { func, arg, .. } => {
                let func = self.ds_term(func)?;
                let arg = self.ds_term(arg)?;
                self.arena.alloc(Application { func, arg }) as &_
            }
            nst::Term::Parenthesized { term, .. } => {
                // We'll replace the span of this node with the parenthesized span
                self.ds_term(term)?
            }
            nst::Term::ProductRow(product) => match product.fields {
                None => self.arena.alloc(Unit),
                Some(fields) => {
                    let head = self.arena.alloc(Label {
                        label: fields.first.label.value,
                        term: self.ds_term(fields.first.target)?,
                    }) as &_;
                    self.spans
                        .insert(head, fields.first.label.join_spans(fields.first.target));
                    fields.elems.iter().fold(Ok(head), |concat, (_, field)| {
                        let right = self.arena.alloc(Label {
                            label: field.label.value,
                            term: self.ds_term(field.target)?,
                        }) as &_;
                        self.spans
                            .insert(right, field.label.join_spans(field.target));
                        Ok(self.arena.alloc(Concat {
                            left: concat?,
                            right,
                        }))
                    })?
                }
            },
            nst::Term::FieldAccess { base, field, .. } => {
                let term = self.ds_term(base)?;
                self.arena.alloc(Unlabel {
                    label: field.value,
                    term: mk_term(
                        Project {
                            direction: Direction::Right,
                            term,
                        },
                        &mut self.spans,
                    ),
                })
            }
            nst::Term::SumRow(sum) => {
                let term = self.ds_term(sum.field.target)?;
                self.arena.alloc(Inject {
                    direction: Direction::Right,
                    term: mk_term(
                        Label {
                            label: sum.field.label.value,
                            term,
                        },
                        &mut self.spans,
                    ),
                })
            }
            // This is gonna take a little more work.
            nst::Term::Match { cases, .. } => {
                let matrix = cases
                    .elements()
                    .map(|field| Ok((vec![*field.label], self.ds_term(field.target)?)))
                    .collect::<Result<_, _>>()?;
                self.desugar_pattern_matrix(&mut [], matrix)?
            }
            nst::Term::Handle { .. } => todo!(),
        };
        self.spans.insert(ast, nst.span());
        Ok(ast)
    }

    /// Compile a matrix of patterns into an AST term that performs pattern matching.
    fn desugar_pattern_matrix<'p>(
        &mut self,
        occurences: &mut [VarId],
        matrix: ClauseMatrix<'p, 's, 'ctx>,
    ) -> Result<&'ctx ast::Term<'ctx, VarId>, PatternMatchError> {
        let mk_term = |spans: &mut FxHashMap<&'ctx ast::Term<'ctx, VarId>, Span>, term, span| {
            let t = self.arena.alloc(term) as &_;
            spans.insert(t, span);
            t
        };
        if matrix.is_empty() {
            Err(PatternMatchError::NonExhaustivePatterns)
        // Row is all wild cards
        } else if matrix
            .first()
            .iter()
            .all(|pat| matches!(pat, Pattern::Whole(_)))
        {
            Ok(matrix
                .first()
                .iter()
                .rfold(matrix.arms[0], |body, pat| match pat {
                    Pattern::Whole(var) => self.arena.alloc(Abstraction {
                        arg: var.value,
                        body,
                    }),
                    _ => unreachable!(),
                }))
        } else {
            let constrs = matrix
                .col_constr(0)
                .collect::<std::collections::BTreeMap<_, _>>();
            let mut matches = constrs.into_iter().map(|(c, p)| match c {
                Constructor::ProductRow(ref lbls) => {
                    let top_level = self.vars.push(handle::Handle(""));
                    let binders = (0..lbls.len())
                        .map(|_| self.vars.push(handle::Handle("")))
                        .collect::<Vec<_>>();
                    let mut occs = binders;
                    // replace first occurence by binder introduced here
                    occs.extend(occurences.iter_mut().skip(1).map(|var| *var));
                    let init =
                        self.desugar_pattern_matrix(occs.as_mut_slice(), matrix.specialize(&c))?;
                    let body = lbls.iter().cloned().fold(init, |body, lbl| {
                        let destructure = self.arena.alloc(Unlabel {
                            label: lbl,
                            term: self.arena.alloc(Project {
                                direction: Direction::Right,
                                term: self.arena.alloc(Variable(top_level)),
                            }),
                        });
                        mk_term(
                            &mut self.spans,
                            Application {
                                func: body,
                                arg: destructure,
                            },
                            p.span(),
                        )
                    });
                    Ok(mk_term(
                        &mut self.spans,
                        Abstraction {
                            arg: top_level,
                            body,
                        },
                        p.span(),
                    ))
                }
                Constructor::SumRow(label) => {
                    let binder = self.vars.push(handle::Handle(""));
                    let mut occs = vec![binder];
                    occs.extend(occurences.iter_mut().skip(1).map(|var| *var));
                    let func = self.desugar_pattern_matrix(&mut occs, matrix.specialize(&c))?;
                    let term = mk_term(&mut self.spans, Variable(binder), p.span());
                    let arg = mk_term(&mut self.spans, Unlabel { label, term }, p.span());
                    let body = mk_term(&mut self.spans, Application { func, arg }, p.span());
                    Ok(mk_term(
                        &mut self.spans,
                        Abstraction { arg: binder, body },
                        p.span(),
                    ))
                }
                Constructor::WildCard => {
                    if let Pattern::Whole(var) = p {
                        // For a wild card we don't need a let binding so just pass through binder as is
                        let body = self.desugar_pattern_matrix(occurences, matrix.default())?;
                        Ok(mk_term(
                            &mut self.spans,
                            Abstraction {
                                arg: var.value,
                                body,
                            },
                            p.span(),
                        ))
                    } else {
                        unreachable!()
                    }
                }
            });
            // we know this can't be empty
            let head: Result<&'ctx ast::Term<'ctx, VarId>, PatternMatchError> =
                matches.next().unwrap();
            let rest = matches.collect::<Vec<_>>();
            rest.into_iter().fold(head, |a, b| {
                let a = a?;
                let b = b?;
                let span = Span::join(&self.spans[a], &self.spans[b]);
                Ok(mk_term(&mut self.spans, Branch { left: a, right: b }, span))
            })
        }
    }
}

/// Desugar a NST into an AST.
/// This removes syntax sugar and lowers down into AST which contains a subset of Nodes availabe in
/// the NST.
pub fn desugar<'n, 's: 'a, 'a>(
    arena: &'a Bump,
    vars: &mut IdGen<VarId, RefHandle<'s, str>>,
    nst: &'n nst::Term<'n, 's>,
) -> Result<Ast<'a, VarId>, PatternMatchError> {
    let mut ds_ctx = DesugarCtx::new(arena, vars);
    let tree = ds_ctx.ds_term(nst)?;
    Ok(Ast::new(ds_ctx.spans, tree))
}

struct ClauseMatrix<'p, 's, 't> {
    pats: Vec<Vec<Pattern<'p, 's>>>,
    arms: Vec<&'t ast::Term<'t, VarId>>,
}

impl<'p, 's, 't> FromIterator<(Vec<Pattern<'p, 's>>, &'t ast::Term<'t, VarId>)>
    for ClauseMatrix<'p, 's, 't>
{
    fn from_iter<T: IntoIterator<Item = (Vec<Pattern<'p, 's>>, &'t ast::Term<'t, VarId>)>>(
        iter: T,
    ) -> Self {
        let (mut pats, mut arms) = (vec![], vec![]);
        iter.into_iter().for_each(|(pat, arm)| {
            pats.push(pat);
            arms.push(arm);
        });
        ClauseMatrix { pats, arms }
    }
}

impl<'p, 's, 't> ClauseMatrix<'p, 's, 't> {
    fn is_empty(&self) -> bool {
        debug_assert!(
            (self.pats.is_empty() && self.arms.is_empty())
                || (self.pats.is_empty().not() && self.arms.is_empty().not())
        );
        self.pats.is_empty()
    }

    fn first(&self) -> &[Pattern<'p, 's>] {
        debug_assert!(self.pats.is_empty().not());
        self.pats[0].as_slice()
    }

    fn col_constr<'a>(
        &'a self,
        col_index: usize,
    ) -> impl Iterator<Item = (Constructor<'s>, &'a Pattern<'p, 's>)> + 'a {
        self.pats
            .iter()
            .map(move |col| (Constructor::from(&col[col_index]), &col[col_index]))
    }

    pub(crate) fn specialize(&self, constr: &Constructor<'s>) -> ClauseMatrix<'p, 's, 't> {
        self.pats
            .iter()
            .zip(self.arms.iter())
            .filter_map(|(pat, arm)| {
                constr.matches(&pat[0]).map(|mut pats| {
                    pats.extend(pat[1..].iter().cloned());
                    (pats, *arm)
                })
            })
            .collect()
    }

    fn default(&self) -> ClauseMatrix<'p, 's, 't> {
        self.pats
            .iter()
            .zip(self.arms.iter())
            .filter_map(|(pats, arm)| match pats[0] {
                Pattern::Whole(_) => Some((pats[1..].to_vec(), *arm)),
                _ => None,
            })
            .collect()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum PatternMatchError {
    NonExhaustivePatterns,
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone)]
enum Constructor<'s> {
    ProductRow(Vec<RefHandle<'s, str>>),
    SumRow(RefHandle<'s, str>),
    WildCard,
}
impl<'s> Constructor<'s> {
    fn matches<'p>(&self, pat: &Pattern<'p, 's>) -> Option<Vec<Pattern<'p, 's>>> {
        let bogus_var_id = aiahr_core::span::SpanOf {
            start: aiahr_core::loc::Loc {
                byte: 0,
                line: 0,
                col: 0,
                module: aiahr_core::id::ModuleId::from_raw(0),
            },
            value: VarId::from_raw(0),
            end: aiahr_core::loc::Loc {
                byte: 0,
                line: 0,
                col: 0,
                module: aiahr_core::id::ModuleId::from_raw(0),
            },
        };
        match (self, pat) {
            (Constructor::ProductRow(lbls), Pattern::ProductRow(rows)) => rows
                .fields
                .iter()
                .flat_map(|fields| fields.elements())
                .zip(lbls)
                .map(|(row, lbl)| row.label.value.eq(lbl).then_some(*row.target))
                .collect::<Option<Vec<_>>>(),
            (Constructor::SumRow(lbl), Pattern::SumRow(row)) if row.field.label.value.eq(lbl) => {
                Some(vec![*row.field.target])
            }
            (Constructor::WildCard, Pattern::Whole(_)) => Some(vec![]),
            // A wild card always matches and produces sub wild card patterns for each pattern our
            // match would have
            (Constructor::WildCard, Pattern::SumRow(_)) => Some(vec![Pattern::Whole(bogus_var_id)]),
            (Constructor::WildCard, Pattern::ProductRow(rows)) => Some(
                rows.into_iter()
                    .map(|_| Pattern::Whole(bogus_var_id))
                    .collect(),
            ),
            _ => None,
        }
    }
}

impl<'s> From<&Pattern<'_, 's>> for Constructor<'s> {
    fn from(pat: &Pattern<'_, 's>) -> Self {
        match pat {
            Pattern::ProductRow(rows) => Constructor::ProductRow(
                rows.fields
                    .map(|sep| sep.elements().map(|field| field.label.value).collect())
                    .unwrap_or_default(),
            ),
            Pattern::SumRow(row) => Constructor::SumRow(row.field.label.value),
            Pattern::Whole(_) => Constructor::WildCard,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use aiahr_core::cst::{Field, IdField, ProductRow, Separated, SumRow};
    use aiahr_core::memory::handle as hand;
    use aiahr_core::memory::intern::InternerByRef;
    use aiahr_core::memory::intern::SyncInterner;
    use aiahr_core::{id::VarId, nst};
    use aiahr_test::{cst::*, span::*};
    use bumpalo::Bump;

    #[test]
    fn test_desugar_var() {
        let arena = Bump::new();
        let mut vars = IdGen::new();
        let var = random_span_of(vars.push(hand::Handle("0")));
        let nst = arena.alloc(nst::Term::VariableRef(var));
        let ast = desugar(&arena, &mut vars, nst).unwrap();

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
        let mut vars = IdGen::new();
        let start = random_span();
        let x = vars.push(hand::Handle("0"));
        let span_of_var = random_span_of(x);
        let nst = arena.alloc(nst::Term::Abstraction {
            lbar: start,
            arg: random_span_of(VarId(0)),
            annotation: None,
            rbar: random_span(),
            body: arena.alloc(nst::Term::VariableRef(span_of_var)),
        });
        let ast = desugar(&arena, &mut vars, nst).unwrap();

        assert_eq!(
            ast.tree,
            &Abstraction {
                arg: x,
                body: &Variable(x)
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
        let mut vars = IdGen::new();
        let start = random_span_of(VarId(0));
        let end = random_span();
        let ast = desugar(
            &arena,
            &mut vars,
            &nst::Term::Application {
                func: arena.alloc(nst::Term::VariableRef(start)),
                lpar: random_span(),
                arg: arena.alloc(nst::Term::VariableRef(random_span_of(VarId(1)))),
                rpar: end,
            },
        )
        .unwrap();

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
        let mut vars = IdGen::new();
        let start = random_span_of(VarId(2));
        let end = random_span_of(VarId(123));
        let ast = desugar(
            &arena,
            &mut vars,
            arena.alloc(nst::Term::Binding {
                var: start,
                annotation: None,
                eq: random_span(),
                value: arena.alloc(nst::Term::VariableRef(random_span_of(VarId(10)))),
                semi: random_span(),
                expr: arena.alloc(nst::Term::VariableRef(end)),
            }),
        )
        .unwrap();

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
            &mut IdGen::new(),
            arena.alloc(nst::Term::ProductRow(ProductRow {
                lbrace: start,
                fields: None,
                rbrace: end,
            })),
        )
        .unwrap();

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
        let interner = SyncInterner::new(&arena);

        let a = interner.intern_by_ref("abc");
        let b = interner.intern_by_ref("def");
        let c = interner.intern_by_ref("ghi");

        let start = random_span();
        let end = random_span();
        let ast = desugar(
            &arena,
            &mut IdGen::new(),
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
        )
        .unwrap();

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
        let interner = SyncInterner::new(&arena);

        let state = interner.intern_by_ref("state");

        let base = random_span_of(VarId(0));
        let field = random_span_of(state);
        let nst = arena.alloc(nst::Term::FieldAccess {
            base: arena.alloc(nst::Term::VariableRef(base)),
            dot: random_span(),
            field,
        });

        let ast = desugar(&arena, &mut IdGen::new(), nst).unwrap();
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
        let interner = SyncInterner::new(&arena);

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

        let ast = desugar(&arena, &mut IdGen::new(), nst).unwrap();
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

    #[test]
    fn test_desugar_match_sum() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);

        let a = interner.intern_by_ref("A");
        let b = interner.intern_by_ref("B");
        let c = interner.intern_by_ref("C");

        let nst = arena.alloc(nst::Term::Match {
            match_: random_span(),
            langle: random_span(),
            rangle: random_span(),
            cases: Separated {
                first: Field {
                    label: arena.alloc(Pattern::SumRow(aiahr_core::cst::SumRow {
                        langle: random_span(),
                        field: Field {
                            label: random_span_of(a),
                            sep: random_span(),
                            target: arena.alloc(Pattern::Whole(random_span_of(VarId(0)))),
                        },
                        rangle: random_span(),
                    })),
                    sep: random_span(),
                    target: arena.alloc(nst::Term::VariableRef(random_span_of(VarId(0)))),
                },
                elems: &*arena.alloc_slice_fill_iter([
                    (
                        random_span(),
                        Field {
                            label: &*arena.alloc(Pattern::SumRow(aiahr_core::cst::SumRow {
                                langle: random_span(),
                                field: Field {
                                    label: random_span_of(b),
                                    sep: random_span(),
                                    target: arena.alloc(Pattern::Whole(random_span_of(VarId(1)))),
                                },
                                rangle: random_span(),
                            })),
                            sep: random_span(),
                            target: &*arena.alloc(nst::Term::VariableRef(random_span_of(VarId(1)))),
                        },
                    ),
                    (
                        random_span(),
                        Field {
                            label: &*arena.alloc(Pattern::SumRow(aiahr_core::cst::SumRow {
                                langle: random_span(),
                                field: Field {
                                    label: random_span_of(c),
                                    sep: random_span(),
                                    target: &*arena.alloc(Pattern::Whole(random_span_of(VarId(2)))),
                                },
                                rangle: random_span(),
                            })),
                            sep: random_span(),
                            target: &*arena.alloc(nst::Term::VariableRef(random_span_of(VarId(2)))),
                        },
                    ),
                ]),
                comma: None,
            },
        });

        let mut vars = [a, b, c].into_iter().collect();
        let ast = desugar(&arena, &mut vars, nst).unwrap();
        assert_eq!(
            ast.root(),
            &Branch {
                left: &Branch {
                    left: &Abstraction {
                        arg: VarId(3),
                        body: &Application {
                            func: &Abstraction {
                                arg: VarId(2),
                                body: &Variable(VarId(2))
                            },
                            arg: &Unlabel {
                                label: c,
                                term: &Variable(VarId(3))
                            }
                        }
                    },
                    right: &Abstraction {
                        arg: VarId(4),
                        body: &Application {
                            func: &Abstraction {
                                arg: VarId(1),
                                body: &Variable(VarId(1))
                            },
                            arg: &Unlabel {
                                label: b,
                                term: &Variable(VarId(4))
                            }
                        }
                    }
                },
                right: &Abstraction {
                    arg: VarId(5),
                    body: &Application {
                        func: &Abstraction {
                            arg: VarId(0),
                            body: &Variable(VarId(0))
                        },
                        arg: &Unlabel {
                            label: a,
                            term: &Variable(VarId(5))
                        }
                    }
                }
            }
        )
    }

    #[test]
    fn test_desugar_match_sum_with_default() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);

        let a = interner.intern_by_ref("A");
        let b = interner.intern_by_ref("B");
        let c = interner.intern_by_ref("C");
        let w = interner.intern_by_ref("_");

        let nst = arena.alloc(nst::Term::Match {
            match_: random_span(),
            langle: random_span(),
            rangle: random_span(),
            cases: Separated {
                first: Field {
                    label: arena.alloc(Pattern::SumRow(aiahr_core::cst::SumRow {
                        langle: random_span(),
                        field: Field {
                            label: random_span_of(a),
                            sep: random_span(),
                            target: arena.alloc(Pattern::Whole(random_span_of(VarId(0)))),
                        },
                        rangle: random_span(),
                    })),
                    sep: random_span(),
                    target: arena.alloc(nst::Term::VariableRef(random_span_of(VarId(0)))),
                },
                elems: &*arena.alloc_slice_fill_iter([
                    (
                        random_span(),
                        Field {
                            label: &*arena.alloc(Pattern::SumRow(aiahr_core::cst::SumRow {
                                langle: random_span(),
                                field: Field {
                                    label: random_span_of(b),
                                    sep: random_span(),
                                    target: arena.alloc(Pattern::Whole(random_span_of(VarId(1)))),
                                },
                                rangle: random_span(),
                            })),
                            sep: random_span(),
                            target: &*arena.alloc(nst::Term::VariableRef(random_span_of(VarId(1)))),
                        },
                    ),
                    (
                        random_span(),
                        Field {
                            label: &*arena.alloc(Pattern::SumRow(aiahr_core::cst::SumRow {
                                langle: random_span(),
                                field: Field {
                                    label: random_span_of(c),
                                    sep: random_span(),
                                    target: &*arena.alloc(Pattern::Whole(random_span_of(VarId(2)))),
                                },
                                rangle: random_span(),
                            })),
                            sep: random_span(),
                            target: &*arena.alloc(nst::Term::VariableRef(random_span_of(VarId(2)))),
                        },
                    ),
                    (
                        random_span(),
                        Field {
                            label: &*arena.alloc(Pattern::Whole(random_span_of(VarId(3)))),
                            sep: random_span(),
                            target: &*arena.alloc(nst::Term::VariableRef(random_span_of(VarId(3)))),
                        },
                    ),
                ]),
                comma: Some(random_span()),
            },
        });

        let mut vars = [a, b, c, w].into_iter().collect();
        let ast = desugar(&arena, &mut vars, nst).unwrap();
        assert_eq!(
            ast.root(),
            &Branch {
                left: &Branch {
                    left: &Branch {
                        left: &Abstraction {
                            arg: VarId(4),
                            body: &Application {
                                func: &Abstraction {
                                    arg: VarId(2),
                                    body: &Variable(VarId(2))
                                },
                                arg: &Unlabel {
                                    label: c,
                                    term: &Variable(VarId(4))
                                }
                            }
                        },
                        right: &Abstraction {
                            arg: VarId(5),
                            body: &Application {
                                func: &Abstraction {
                                    arg: VarId(1),
                                    body: &Variable(VarId(1))
                                },
                                arg: &Unlabel {
                                    label: b,
                                    term: &Variable(VarId(5))
                                }
                            }
                        }
                    },
                    right: &Abstraction {
                        arg: VarId(6),
                        body: &Application {
                            func: &Abstraction {
                                arg: VarId(0),
                                body: &Variable(VarId(0))
                            },
                            arg: &Unlabel {
                                label: a,
                                term: &Variable(VarId(6))
                            }
                        }
                    }
                },
                right: &Abstraction {
                    arg: VarId(3),
                    body: &Variable(VarId(3))
                }
            }
        )
    }

    #[test]
    fn test_desugar_match_prod() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);

        let a = interner.intern_by_ref("A");
        let b = interner.intern_by_ref("B");
        let c = interner.intern_by_ref("C");

        let nst = arena.alloc(nst::Term::Match {
            match_: random_span(),
            langle: random_span(),
            rangle: random_span(),
            cases: Separated {
                first: Field {
                    label: arena.alloc(Pattern::ProductRow(ProductRow {
                        lbrace: random_span(),
                        fields: Some(Separated {
                            first: Field {
                                label: random_span_of(a),
                                sep: random_span(),
                                target: arena.alloc(Pattern::Whole(random_span_of(VarId(0)))) as &_,
                            },
                            elems: &*arena.alloc_slice_fill_iter([
                                (
                                    random_span(),
                                    Field {
                                        label: random_span_of(b),
                                        sep: random_span(),
                                        target: arena
                                            .alloc(Pattern::Whole(random_span_of(VarId(1))))
                                            as &_,
                                    },
                                ),
                                (
                                    random_span(),
                                    Field {
                                        label: random_span_of(c),
                                        sep: random_span(),
                                        target: arena
                                            .alloc(Pattern::Whole(random_span_of(VarId(2))))
                                            as &_,
                                    },
                                ),
                            ]),
                            comma: None,
                        }),
                        rbrace: random_span(),
                    })),
                    sep: random_span(),
                    target: arena.alloc(nst::Term::VariableRef(random_span_of(VarId(1)))),
                },
                elems: &[],
                comma: None,
            },
        });

        let mut vars = [a, b, c].into_iter().collect();
        let ast = desugar(&arena, &mut vars, nst).unwrap();
        assert_eq!(
            ast.root(),
            &Abstraction {
                arg: VarId(3),
                body: &Application {
                    func: &Application {
                        func: &Application {
                            func: &Abstraction {
                                arg: VarId(0),
                                body: &Abstraction {
                                    arg: VarId(1),
                                    body: &Abstraction {
                                        arg: VarId(2),
                                        body: &Variable(VarId(1))
                                    }
                                }
                            },
                            arg: &Unlabel {
                                label: a,
                                term: &Project {
                                    direction: Direction::Right,
                                    term: &Variable(VarId(3))
                                }
                            }
                        },
                        arg: &Unlabel {
                            label: b,
                            term: &Project {
                                direction: Direction::Right,
                                term: &Variable(VarId(3))
                            }
                        }
                    },
                    arg: &Unlabel {
                        label: c,
                        term: &Project {
                            direction: Direction::Right,
                            term: &Variable(VarId(3))
                        }
                    }
                }
            }
        )
    }

    #[test]
    fn test_desugar_match_nested_patterns() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);

        let a = interner.intern_by_ref("A");
        let b = interner.intern_by_ref("B");
        let c = interner.intern_by_ref("C");
        let x = interner.intern_by_ref("x");
        let y = interner.intern_by_ref("y");
        let z = interner.intern_by_ref("z");

        let nst = arena.alloc(nst::Term::Match {
            match_: random_span(),
            langle: random_span(),
            rangle: random_span(),
            cases: random_sep(
                &arena,
                [
                    random_field(
                        arena.alloc(Pattern::ProductRow(ProductRow {
                            lbrace: random_span(),
                            fields: Some(random_sep(
                                &arena,
                                [
                                    random_field(
                                        random_span_of(x),
                                        arena.alloc(Pattern::SumRow(SumRow {
                                            langle: random_span(),
                                            field: random_field(
                                                random_span_of(a),
                                                arena
                                                    .alloc(Pattern::Whole(random_span_of(VarId(0))))
                                                    as &_,
                                            ),
                                            rangle: random_span(),
                                        })) as &_,
                                    ),
                                    random_field(
                                        random_span_of(y),
                                        arena.alloc(Pattern::SumRow(SumRow {
                                            langle: random_span(),
                                            field: random_field(
                                                random_span_of(b),
                                                arena
                                                    .alloc(Pattern::Whole(random_span_of(VarId(1))))
                                                    as &_,
                                            ),
                                            rangle: random_span(),
                                        })) as &_,
                                    ),
                                    random_field(
                                        random_span_of(z),
                                        arena.alloc(Pattern::Whole(random_span_of(VarId(2)))) as &_,
                                    ),
                                ],
                            )),
                            rbrace: random_span(),
                        })) as &_,
                        arena.alloc(nst::Term::VariableRef(random_span_of(VarId(0)))) as &_,
                    ),
                    random_field(
                        arena.alloc(Pattern::ProductRow(ProductRow {
                            lbrace: random_span(),
                            fields: Some(random_sep(
                                &arena,
                                [
                                    random_field(
                                        random_span_of(x),
                                        arena.alloc(Pattern::Whole(random_span_of(VarId(0)))) as &_,
                                    ),
                                    random_field(
                                        random_span_of(y),
                                        arena.alloc(Pattern::SumRow(SumRow {
                                            langle: random_span(),
                                            field: random_field(
                                                random_span_of(b),
                                                arena
                                                    .alloc(Pattern::Whole(random_span_of(VarId(1))))
                                                    as &_,
                                            ),
                                            rangle: random_span(),
                                        })) as &_,
                                    ),
                                    random_field(
                                        random_span_of(z),
                                        arena.alloc(Pattern::Whole(random_span_of(VarId(2)))) as &_,
                                    ),
                                ],
                            )),
                            rbrace: random_span(),
                        })) as &_,
                        arena.alloc(nst::Term::VariableRef(random_span_of(VarId(1)))) as &_,
                    ),
                    random_field(
                        arena.alloc(Pattern::ProductRow(ProductRow {
                            lbrace: random_span(),
                            fields: Some(random_sep(
                                &arena,
                                [
                                    random_field(
                                        random_span_of(x),
                                        arena.alloc(Pattern::Whole(random_span_of(VarId(0)))) as &_,
                                    ),
                                    random_field(
                                        random_span_of(y),
                                        arena.alloc(Pattern::Whole(random_span_of(VarId(1)))) as &_,
                                    ),
                                    random_field(
                                        random_span_of(z),
                                        arena.alloc(Pattern::SumRow(SumRow {
                                            langle: random_span(),
                                            field: random_field(
                                                random_span_of(c),
                                                arena
                                                    .alloc(Pattern::Whole(random_span_of(VarId(2))))
                                                    as &_,
                                            ),
                                            rangle: random_span(),
                                        })) as &_,
                                    ),
                                ],
                            )),
                            rbrace: random_span(),
                        })) as &_,
                        arena.alloc(nst::Term::VariableRef(random_span_of(VarId(2)))) as &_,
                    ),
                ],
            ),
        });

        let mut vars = [a, b, c].into_iter().collect();
        let ast = desugar(&arena, &mut vars, nst).unwrap();
        assert_eq!(
            ast.root(),
            &Abstraction {
                arg: VarId(3),
                body: &Application {
                    func: &Application {
                        func: &Application {
                            func: &Branch {
                                left: &Abstraction {
                                    arg: VarId(7),
                                    body: &Application {
                                        func: &Abstraction {
                                            arg: VarId(0),
                                            body: &Abstraction {
                                                arg: VarId(8),
                                                body: &Application {
                                                    func: &Abstraction {
                                                        arg: VarId(1),
                                                        body: &Abstraction {
                                                            arg: VarId(2),
                                                            body: &Variable(VarId(0))
                                                        }
                                                    },
                                                    arg: &Unlabel {
                                                        label: b,
                                                        term: &Variable(VarId(8))
                                                    }
                                                }
                                            }
                                        },
                                        arg: &Unlabel {
                                            label: a,
                                            term: &Variable(VarId(7))
                                        }
                                    }
                                },
                                right: &Abstraction {
                                    arg: VarId(0),
                                    body: &Branch {
                                        left: &Abstraction {
                                            arg: VarId(9),
                                            body: &Application {
                                                func: &Abstraction {
                                                    arg: VarId(1),
                                                    body: &Abstraction {
                                                        arg: VarId(2),
                                                        body: &Variable(VarId(1))
                                                    }
                                                },
                                                arg: &Unlabel {
                                                    label: b,
                                                    term: &Variable(VarId(9))
                                                }
                                            }
                                        },
                                        right: &Abstraction {
                                            arg: VarId(1),
                                            body: &Abstraction {
                                                arg: VarId(10),
                                                body: &Application {
                                                    func: &Abstraction {
                                                        arg: VarId(2),
                                                        body: &Variable(VarId(2))
                                                    },
                                                    arg: &Unlabel {
                                                        label: c,
                                                        term: &Variable(VarId(10))
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            },
                            arg: &Unlabel {
                                label: x,
                                term: &Project {
                                    direction: Direction::Right,
                                    term: &Variable(VarId(3))
                                }
                            }
                        },
                        arg: &Unlabel {
                            label: y,
                            term: &Project {
                                direction: Direction::Right,
                                term: &Variable(VarId(3))
                            }
                        }
                    },
                    arg: &Unlabel {
                        label: z,
                        term: &Project {
                            direction: Direction::Right,
                            term: &Variable(VarId(3))
                        }
                    }
                }
            }
        )
    }
}
