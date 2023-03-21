use std::ops::Not;

use aiahr_core::ast::Direction;
use aiahr_core::cst::{self, Field, RowAtom, Scheme, Type};
use aiahr_core::id::{Id, IdGen, TyVarId};
use aiahr_core::ident::Ident;
use aiahr_core::nst::Pattern;
use aiahr_core::span::{SpanOf, Spanned};
use aiahr_core::ty::row::Row;
use aiahr_core::ty::{Evidence, InDb, MkTy, Ty, TyScheme, TypeKind};
use aiahr_core::{
    ast,
    ast::{Ast, Term::*},
    id::VarId,
    nst,
    span::Span,
};
use bumpalo::Bump;
use rustc_hash::FxHashMap;

#[salsa::jar(db = Db)]
pub struct Jar();
pub trait Db: salsa::DbWithJar<Jar> + aiahr_core::Db {}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + aiahr_core::Db {}

struct DesugarCtx<'a, 'ctx> {
    db: &'a dyn crate::Db,
    arena: &'ctx Bump,
    vars: &'a mut IdGen<VarId, Ident>,
    ty_vars: &'a mut IdGen<TyVarId, Ident>,
    spans: FxHashMap<&'ctx ast::Term<'ctx, VarId>, Span>,
}

impl<'a, 'ctx> DesugarCtx<'a, 'ctx> {
    fn new(
        db: &'a dyn crate::Db,
        arena: &'ctx Bump,
        vars: &'a mut IdGen<VarId, Ident>,
        ty_vars: &'a mut IdGen<TyVarId, Ident>,
    ) -> Self {
        Self {
            db,
            arena,
            vars,
            ty_vars,
            spans: FxHashMap::default(),
        }
    }

    /// Desugar a NST Term into it's corresponding AST Term.
    fn ds_term<'n>(
        &mut self,
        nst: &'n nst::Term<'n>,
    ) -> Result<&'ctx ast::Term<'ctx, VarId>, PatternMatchError> {
        let mk_term = |term, spans: &mut FxHashMap<&'ctx ast::Term<'ctx, VarId>, Span>| {
            let t = self.arena.alloc(term) as &_;
            spans.insert(t, nst.span());
            t
        };
        let ast = match nst {
            nst::Term::VariableRef(var) => self.arena.alloc(ast::Term::Variable(var.value)) as &_,
            nst::Term::ItemRef(item) => self.arena.alloc(ast::Term::Item(item.value)) as &_,
            nst::Term::EffectOpRef(SpanOf { value, .. }) => {
                self.arena.alloc(ast::Term::Operation(*value)) as &_
            }
            nst::Term::Binding {
                var,
                annotation,
                value,
                expr,
                ..
            } => {
                let mut value = self.ds_term(value)?;
                // If our binding is annotated wrap our value in it's annotated type
                if let Some(ann) = annotation {
                    let ty = self.ds_type(ann.type_);
                    value = mk_term(Annotated { ty, term: value }, &mut self.spans)
                }
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
            nst::Term::Abstraction {
                arg,
                annotation,
                body,
                ..
            } => {
                let body = self.ds_term(body)?;
                // If there's no annotation, we double insert spans which is fine as our map is
                // idempotent
                let mut term = mk_term(
                    Abstraction {
                        arg: arg.value,
                        body,
                    },
                    &mut self.spans,
                );
                // Our annotation here is for the argument, so we want to annotate our whole
                // abstraction as, abs : ann -> tv<0> where return type is a fresh type var.
                if let Some(ann) = annotation {
                    let arg_ty = self.ds_type(ann.type_);
                    let ret_ty = self.db.as_core_db().mk_ty(TypeKind::VarTy(
                        self.ty_vars.push(self.db.ident_str("__generated__")),
                    ));
                    let ty = self.db.as_core_db().mk_ty(TypeKind::FunTy(arg_ty, ret_ty));
                    term = self.arena.alloc(Annotated { ty, term });
                }
                term
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

    fn ds_row<'n>(
        &mut self,
        row: &'n cst::Row<TyVarId, Field<SpanOf<Ident>, &'n Type<'n, TyVarId>>>,
    ) -> Row<InDb> {
        match row {
            cst::Row::Concrete(closed) => Row::Closed(
                self.db.as_core_db().construct_row(
                    closed
                        .elements()
                        .map(|field| (field.label.value, self.ds_type(field.target)))
                        .collect(),
                ),
            ),
            cst::Row::Variable(vars) => {
                if !vars.elems.is_empty() {
                    // TODO: Handle desugar-ing multiple variables
                    unimplemented!()
                } else {
                    Row::Open(vars.first.value)
                }
            }
            cst::Row::Mixed { .. } => unimplemented!(),
        }
    }

    fn ds_type<'n>(&mut self, nst: &'n cst::Type<'n, TyVarId>) -> Ty<InDb> {
        match nst {
            cst::Type::Named(ty_var) => self.db.as_core_db().mk_ty(TypeKind::VarTy(ty_var.value)),
            cst::Type::Sum { variants, .. } => self
                .db
                .as_core_db()
                .mk_ty(TypeKind::SumTy(self.ds_row(variants))),
            cst::Type::Product { fields, .. } => self.db.as_core_db().mk_ty(TypeKind::ProdTy(
                fields
                    .as_ref()
                    .map(|row| self.ds_row(row))
                    .unwrap_or(Row::Closed(self.db.as_core_db().empty_row())),
            )),
            cst::Type::Function {
                domain, codomain, ..
            } => self.db.as_core_db().mk_ty(TypeKind::FunTy(
                self.ds_type(domain),
                self.ds_type(codomain),
            )),
            cst::Type::Parenthesized { type_, .. } => self.ds_type(type_),
        }
    }

    fn ds_row_atom<'n>(&mut self, row_atom: &'n RowAtom<'n, TyVarId>) -> Row<InDb> {
        match row_atom {
            RowAtom::Concrete { fields, .. } => Row::Closed(
                self.db.as_core_db().construct_row(
                    fields
                        .elements()
                        .map(|field| (field.label.value, self.ds_type(field.target)))
                        .collect(),
                ),
            ),
            RowAtom::Variable(ty_var) => Row::Open(ty_var.value),
        }
    }

    fn ds_scheme<'n>(&mut self, nst: &'n Scheme<'n, TyVarId>) -> TyScheme<InDb> {
        let bound = nst
            .quantifiers
            .iter()
            .map(|quant| quant.var.value)
            .collect();
        let constrs = nst
            .qualifiers
            .map(|qual| {
                qual.constraints
                    .elements()
                    .map(|constr| match constr {
                        cst::Constraint::RowSum { lhs, rhs, goal, .. } => Evidence::Row {
                            left: self.ds_row_atom(lhs),
                            right: self.ds_row_atom(rhs),
                            goal: self.ds_row_atom(goal),
                        },
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or(vec![]);
        let ty = self.ds_type(nst.type_);
        let eff = Row::Open(self.ty_vars.push(self.db.ident_str("__generated__")));
        TyScheme {
            bound,
            constrs,
            eff,
            ty,
        }
    }

    /// Compile a matrix of patterns into an AST term that performs pattern matching.
    fn desugar_pattern_matrix<'p>(
        &mut self,
        occurences: &mut [VarId],
        matrix: ClauseMatrix<'p, 'ctx>,
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
                    let generated_var = self.db.ident_str("__generated__");
                    let top_level = self.vars.push(generated_var);
                    let binders = (0..lbls.len())
                        .map(|_| self.vars.push(generated_var))
                        .collect::<Vec<_>>();
                    let mut occs = binders;
                    // replace first occurence by binder introduced here
                    occs.extend(occurences.iter_mut().skip(1).map(|var| *var));
                    let init =
                        self.desugar_pattern_matrix(occs.as_mut_slice(), matrix.specialize(&c))?;
                    let body = lbls.iter().cloned().fold(init, |body, label| {
                        let destructure = self.arena.alloc(Unlabel {
                            label,
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
                    let binder = self.vars.push(self.db.ident_str("__generated__"));
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
pub fn desugar<'n, 'a>(
    db: &dyn crate::Db,
    arena: &'a Bump,
    vars: &mut IdGen<VarId, Ident>,
    ty_vars: &mut IdGen<TyVarId, Ident>,
    nst: nst::Item<'n>,
) -> Result<Ast<'a, VarId>, PatternMatchError> {
    match nst {
        nst::Item::Effect { .. } => todo!(),
        nst::Item::Term {
            name,
            annotation,
            value,
            ..
        } => {
            let mut ds_ctx = DesugarCtx::new(db, arena, vars, ty_vars);
            let tree = ds_ctx.ds_term(value)?;
            Ok(match annotation {
                Some(scheme) => {
                    let scheme = ds_ctx.ds_scheme(scheme.type_);
                    Ast::with_ann(name.value, ds_ctx.spans, scheme, tree)
                }
                None => Ast::new(name.value, ds_ctx.spans, tree),
            })
        }
    }
}

struct ClauseMatrix<'p, 't> {
    pats: Vec<Vec<Pattern<'p>>>,
    arms: Vec<&'t ast::Term<'t, VarId>>,
}

impl<'p, 't> FromIterator<(Vec<Pattern<'p>>, &'t ast::Term<'t, VarId>)> for ClauseMatrix<'p, 't> {
    fn from_iter<T: IntoIterator<Item = (Vec<Pattern<'p>>, &'t ast::Term<'t, VarId>)>>(
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

impl<'p, 't> ClauseMatrix<'p, 't> {
    fn is_empty(&self) -> bool {
        debug_assert!(
            (self.pats.is_empty() && self.arms.is_empty())
                || (self.pats.is_empty().not() && self.arms.is_empty().not())
        );
        self.pats.is_empty()
    }

    fn first(&self) -> &[Pattern<'p>] {
        debug_assert!(self.pats.is_empty().not());
        self.pats[0].as_slice()
    }

    fn col_constr<'a>(
        &'a self,
        col_index: usize,
    ) -> impl Iterator<Item = (Constructor, &'a Pattern<'p>)> + 'a {
        self.pats
            .iter()
            .map(move |col| (Constructor::from(&col[col_index]), &col[col_index]))
    }

    pub(crate) fn specialize(&self, constr: &Constructor) -> ClauseMatrix<'p, 't> {
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

    fn default(&self) -> ClauseMatrix<'p, 't> {
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
enum Constructor {
    ProductRow(Vec<Ident>),
    SumRow(Ident),
    WildCard,
}
impl Constructor {
    fn matches<'p>(&self, pat: &Pattern<'p>) -> Option<Vec<Pattern<'p>>> {
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

impl From<&Pattern<'_>> for Constructor {
    fn from(pat: &Pattern<'_>) -> Self {
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
    use aiahr_core::Db;
    use aiahr_core::{id::VarId, nst};
    use aiahr_test::nst::random_term_item;
    use aiahr_test::{cst::*, span::*};
    use bumpalo::Bump;

    #[derive(Default)]
    #[salsa::db(crate::Jar, aiahr_core::Jar)]
    struct TestDatabase {
        storage: salsa::Storage<Self>,
    }
    impl salsa::Database for TestDatabase {}

    #[test]
    fn test_desugar_var() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let mut vars = IdGen::new();
        let var = random_span_of(vars.push(db.ident_str("0")));
        let nst = random_term_item(arena.alloc(nst::Term::VariableRef(var)));
        let ast = desugar(&db, &arena, &mut vars, &mut IdGen::new(), nst).unwrap();

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
        let db = TestDatabase::default();
        let mut vars = IdGen::new();
        let start = random_span();
        let x = vars.push(db.ident_str("0"));
        let span_of_var = random_span_of(x);
        let nst = random_term_item(arena.alloc(nst::Term::Abstraction {
            lbar: start,
            arg: random_span_of(VarId(0)),
            annotation: None,
            rbar: random_span(),
            body: arena.alloc(nst::Term::VariableRef(span_of_var)),
        }));
        let ast = desugar(&db, &arena, &mut vars, &mut IdGen::new(), nst).unwrap();

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
        let db = TestDatabase::default();
        let mut vars = IdGen::new();
        let start = random_span_of(VarId(0));
        let end = random_span();
        let ast = desugar(
            &db,
            &arena,
            &mut vars,
            &mut IdGen::new(),
            random_term_item(arena.alloc(nst::Term::Application {
                func: arena.alloc(nst::Term::VariableRef(start)),
                lpar: random_span(),
                arg: arena.alloc(nst::Term::VariableRef(random_span_of(VarId(1)))),
                rpar: end,
            })),
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
        let db = TestDatabase::default();
        let mut vars = IdGen::new();
        let start = random_span_of(VarId(2));
        let end = random_span_of(VarId(123));
        let ast = desugar(
            &db,
            &arena,
            &mut vars,
            &mut IdGen::new(),
            random_term_item(arena.alloc(nst::Term::Binding {
                var: start,
                annotation: None,
                eq: random_span(),
                value: arena.alloc(nst::Term::VariableRef(random_span_of(VarId(10)))),
                semi: random_span(),
                expr: arena.alloc(nst::Term::VariableRef(end)),
            })),
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
        let db = TestDatabase::default();
        let start = random_span();
        let end = random_span();
        let ast = desugar(
            &db,
            &arena,
            &mut IdGen::new(),
            &mut IdGen::new(),
            random_term_item(arena.alloc(nst::Term::ProductRow(ProductRow {
                lbrace: start,
                fields: None,
                rbrace: end,
            }))),
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
        let db = TestDatabase::default();

        let a = db.ident_str("abc");
        let b = db.ident_str("def");
        let c = db.ident_str("ghi");

        let start = random_span();
        let end = random_span();
        let ast = desugar(
            &db,
            &arena,
            &mut IdGen::new(),
            &mut IdGen::new(),
            random_term_item(arena.alloc(nst::Term::ProductRow(ProductRow {
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
                                target:
                                    arena.alloc(nst::Term::VariableRef(random_span_of(VarId(1)))),
                            },
                        ),
                        (
                            random_span(),
                            IdField {
                                label: random_span_of(c),
                                sep: random_span(),
                                target:
                                    arena.alloc(nst::Term::VariableRef(random_span_of(VarId(2)))),
                            },
                        ),
                    ],
                    comma: None,
                }),
                rbrace: end,
            }))),
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
        let db = TestDatabase::default();

        let state = db.ident_str("state");

        let base = random_span_of(VarId(0));
        let field = random_span_of(state);
        let nst = random_term_item(arena.alloc(nst::Term::FieldAccess {
            base: arena.alloc(nst::Term::VariableRef(base)),
            dot: random_span(),
            field,
        }));

        let ast = desugar(&db, &arena, &mut IdGen::new(), &mut IdGen::new(), nst).unwrap();
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
        let db = TestDatabase::default();

        let tru = db.ident_str("true");

        let langle = random_span();
        let rangle = random_span();
        let nst = random_term_item(arena.alloc(nst::Term::SumRow(aiahr_core::cst::SumRow {
            langle,
            field: Field {
                label: random_span_of(tru),
                sep: random_span(),
                target: arena.alloc(nst::Term::VariableRef(random_span_of(VarId(0)))),
            },
            rangle,
        })));

        let ast = desugar(&db, &arena, &mut IdGen::new(), &mut IdGen::new(), nst).unwrap();
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
        let db = TestDatabase::default();

        let a = db.ident_str("A");
        let b = db.ident_str("B");
        let c = db.ident_str("C");

        let nst = random_term_item(arena.alloc(nst::Term::Match {
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
        }));

        let mut vars = [a, b, c].into_iter().collect();
        let ast = desugar(&db, &arena, &mut vars, &mut IdGen::new(), nst).unwrap();
        assert_eq!(
            ast.root(),
            &Branch {
                left: &Branch {
                    left: &Abstraction {
                        arg: VarId(3),
                        body: &Application {
                            func: &Abstraction {
                                arg: VarId(0),
                                body: &Variable(VarId(0))
                            },
                            arg: &Unlabel {
                                label: a,
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
                            arg: VarId(2),
                            body: &Variable(VarId(2))
                        },
                        arg: &Unlabel {
                            label: c,
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
        let db = TestDatabase::default();

        let a = db.ident_str("A");
        let b = db.ident_str("B");
        let c = db.ident_str("C");
        let w = db.ident_str("_");

        let nst = random_term_item(arena.alloc(nst::Term::Match {
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
        }));

        let mut vars = [a, b, c, w].into_iter().collect();
        let ast = desugar(&db, &arena, &mut vars, &mut IdGen::new(), nst).unwrap();
        assert_eq!(
            ast.root(),
            &Branch {
                left: &Branch {
                    left: &Branch {
                        left: &Abstraction {
                            arg: VarId(4),
                            body: &Application {
                                func: &Abstraction {
                                    arg: VarId(0),
                                    body: &Variable(VarId(0))
                                },
                                arg: &Unlabel {
                                    label: a,
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
                                arg: VarId(2),
                                body: &Variable(VarId(2))
                            },
                            arg: &Unlabel {
                                label: c,
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
        let db = TestDatabase::default();

        let a = db.ident_str("A");
        let b = db.ident_str("B");
        let c = db.ident_str("C");

        let nst = random_term_item(arena.alloc(nst::Term::Match {
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
                                        target:
                                            arena.alloc(Pattern::Whole(random_span_of(VarId(1))))
                                                as &_,
                                    },
                                ),
                                (
                                    random_span(),
                                    Field {
                                        label: random_span_of(c),
                                        sep: random_span(),
                                        target:
                                            arena.alloc(Pattern::Whole(random_span_of(VarId(2))))
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
        }));

        let mut vars = [a, b, c].into_iter().collect();
        let ast = desugar(&db, &arena, &mut vars, &mut IdGen::new(), nst).unwrap();
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
        let db = TestDatabase::default();

        let a = db.ident_str("A");
        let b = db.ident_str("B");
        let c = db.ident_str("C");
        let x = db.ident_str("x");
        let y = db.ident_str("y");
        let z = db.ident_str("z");

        let nst = random_term_item(arena.alloc(nst::Term::Match {
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
                                            field:
                                                random_field(
                                                    random_span_of(a),
                                                    arena.alloc(Pattern::Whole(random_span_of(
                                                        VarId(0),
                                                    )))
                                                        as &_,
                                                ),
                                            rangle: random_span(),
                                        })) as &_,
                                    ),
                                    random_field(
                                        random_span_of(y),
                                        arena.alloc(Pattern::SumRow(SumRow {
                                            langle: random_span(),
                                            field:
                                                random_field(
                                                    random_span_of(b),
                                                    arena.alloc(Pattern::Whole(random_span_of(
                                                        VarId(1),
                                                    )))
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
                                            field:
                                                random_field(
                                                    random_span_of(b),
                                                    arena.alloc(Pattern::Whole(random_span_of(
                                                        VarId(1),
                                                    )))
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
                                            field:
                                                random_field(
                                                    random_span_of(c),
                                                    arena.alloc(Pattern::Whole(random_span_of(
                                                        VarId(2),
                                                    )))
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
        }));

        let mut vars = [a, b, c, x, y, z].into_iter().collect();
        let ast = desugar(&db, &arena, &mut vars, &mut IdGen::new(), nst).unwrap();
        assert_eq!(
            ast.root(),
            &Abstraction {
                arg: VarId(6),
                body: &Application {
                    func: &Application {
                        func: &Application {
                            func: &Branch {
                                left: &Abstraction {
                                    arg: VarId(10),
                                    body: &Application {
                                        func: &Abstraction {
                                            arg: VarId(0),
                                            body: &Abstraction {
                                                arg: VarId(11),
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
                                                        term: &Variable(VarId(11))
                                                    }
                                                }
                                            }
                                        },
                                        arg: &Unlabel {
                                            label: a,
                                            term: &Variable(VarId(10))
                                        }
                                    }
                                },
                                right: &Abstraction {
                                    arg: VarId(0),
                                    body: &Branch {
                                        left: &Abstraction {
                                            arg: VarId(12),
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
                                                    term: &Variable(VarId(12))
                                                }
                                            }
                                        },
                                        right: &Abstraction {
                                            arg: VarId(1),
                                            body: &Abstraction {
                                                arg: VarId(13),
                                                body: &Application {
                                                    func: &Abstraction {
                                                        arg: VarId(2),
                                                        body: &Variable(VarId(2))
                                                    },
                                                    arg: &Unlabel {
                                                        label: c,
                                                        term: &Variable(VarId(13))
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
                                    term: &Variable(VarId(6))
                                }
                            }
                        },
                        arg: &Unlabel {
                            label: y,
                            term: &Project {
                                direction: Direction::Right,
                                term: &Variable(VarId(6))
                            }
                        }
                    },
                    arg: &Unlabel {
                        label: z,
                        term: &Project {
                            direction: Direction::Right,
                            term: &Variable(VarId(6))
                        }
                    }
                }
            }
        )
    }
}
