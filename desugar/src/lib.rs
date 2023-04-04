use std::ops::Not;

use aiahr_core::ast::indexed::{Ast, Item, SalsaItem, Term, Term::*};
use aiahr_core::ast::{self, AstModule, Direction};
use aiahr_core::cst::{self, Field, RowAtom, Scheme, Type};
use aiahr_core::id::{EffectId, EffectOpId, Id, IdGen, ItemId, ModuleId, TyVarId};
use aiahr_core::ident::Ident;
use aiahr_core::indexed::ReferenceAllocate;
use aiahr_core::modules::{module_of, Module};
use aiahr_core::nst::Pattern;
use aiahr_core::span::{SpanOf, Spanned};
use aiahr_core::ty::row::Row;
use aiahr_core::ty::{Evidence, MkTy, Ty, TyScheme, TypeKind};
use aiahr_core::Top;
use aiahr_core::{id::VarId, nst, span::Span};
use bumpalo::Bump;
use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;

#[salsa::jar(db = Db)]
pub struct Jar(
    desugar_module,
    desugar_item,
    desugar_item_of_id,
    effect_of,
    effect_op_tyscheme_of,
);
pub trait Db: salsa::DbWithJar<Jar> + aiahr_core::Db + aiahr_analysis::Db {
    fn as_desugar_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<crate::Jar>>::as_jar_db(self)
    }

    fn desugar_module_of(&self, module: Module) -> AstModule {
        self.desugar_module_id_of(module.name(self.as_core_db()))
    }

    fn desugar_module_id_of(&self, module_id: ModuleId) -> AstModule {
        let nameres_module = self.nameres_module_of(module_id);
        desugar_module(self.as_desugar_db(), nameres_module)
    }
}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + aiahr_core::Db + aiahr_analysis::Db {}

/// Desugar an NST Module into an AST module.
/// This will desugar all items in NST moduels into their corresponding AST items.
#[salsa::tracked]
pub fn desugar_module(db: &dyn crate::Db, module: aiahr_analysis::NameResModule) -> AstModule {
    let core_db = db.as_core_db();
    let resolution = module.items(db.as_analysis_db());
    let ty_vars = resolution.local_ids.ty_vars.len();
    let vars = resolution.local_ids.vars.len();
    AstModule::new(
        core_db,
        module.module(db.as_analysis_db()),
        resolution
            .resolved_items
            .iter()
            .flat_map(|opt_item| opt_item.as_ref().into_iter())
            .map(|nst_item| desugar_item(db, *nst_item, vars, ty_vars))
            .collect(),
    )
}

/// Desugar an NST Item into an AST Item.
#[salsa::tracked]
pub fn desugar_item(
    db: &dyn crate::Db,
    item: aiahr_analysis::SalsaItem,
    vars: usize,
    ty_vars: usize,
) -> ast::indexed::SalsaItem {
    let arena = Bump::new();
    let mut alloc = nst::indexed::NstRefAlloc::new(&arena, item.alloc(db.as_analysis_db()));
    // TODO: Handle separation of name based Ids and desugar generated Ids better.
    let mut ty_vars = IdGen::from_iter((0..ty_vars).map(|_| false));
    let mut vars = IdGen::from_iter((0..vars).map(|_| false));

    let ast_res = desugar(
        db,
        &mut vars,
        &mut ty_vars,
        item.data(db.as_analysis_db()).ref_alloc(&mut alloc),
    );

    let salsa_ast = match ast_res {
        Ok(item) => item,
        Err(_pat_err) => {
            todo!()
        }
    };

    SalsaItem::new(db.as_core_db(), salsa_ast)
}

#[salsa::tracked]
pub fn desugar_item_of_id(
    db: &dyn crate::Db,
    _top: Top,
    module_id: ModuleId,
    item_id: ItemId,
) -> SalsaItem {
    let nameres_module = db.nameres_module_of(module_id);
    let ast_module = desugar_module(db, nameres_module);
    ast_module
        .items(db.as_core_db())
        .iter()
        .find(|item| match item.item(db.as_core_db()) {
            Item::Effect(_) => false,
            Item::Function(ast) => ast.name == item_id,
        })
        .cloned()
        .unwrap_or_else(|| {
            panic!(
                "ICE: Created ItemId {:?} without corresponding Item",
                item_id
            )
        })
}

#[salsa::tracked]
pub fn effect_of(db: &dyn crate::Db, module: Module, effect_id: EffectId) -> ast::EffectItem {
    let ast_mod = db.desugar_module_of(module);
    ast_mod
        .items(db.as_core_db())
        .iter()
        .find_map(|item| match item.item(db.as_core_db()) {
            Item::Effect(eff_item) => (eff_item.name == effect_id).then_some(eff_item),
            Item::Function(_) => None,
        })
        .unwrap_or_else(|| {
            panic!(
                "ICE: Constructed EffectId {:?} without an Effect definition",
                effect_id
            )
        })
}

#[salsa::tracked]
pub fn effect_op_tyscheme_of(
    db: &dyn crate::Db,
    top: aiahr_core::Top,
    module_id: ModuleId,
    eff_id: EffectId,
    op_id: EffectOpId,
) -> TyScheme {
    let module = module_of(db.as_core_db(), top, module_id);
    let eff = effect_of(db, module, eff_id);
    eff.ops
        .iter()
        .find_map(|opt_op| {
            opt_op
                .as_ref()
                .and_then(|op| (op.0 == op_id).then_some(op.1.clone()))
        })
        .unwrap_or_else(|| {
            panic!(
                "ICE: Constructed EffectOpId {:?} with an Effect Operation defintion",
                op_id
            )
        })
}

/// Desugar a NST into an AST.
/// This removes syntax sugar and lowers down into AST which contains a subset of Nodes availabe in
/// the NST.
pub fn desugar(
    db: &dyn crate::Db,
    vars: &mut IdGen<VarId, bool>,
    ty_vars: &mut IdGen<TyVarId, bool>,
    nst: nst::Item<'_>,
) -> Result<ast::indexed::Item<VarId>, PatternMatchError> {
    let terms = la_arena::Arena::default();
    let mut ds_ctx = DesugarCtx::new(db, terms, vars, ty_vars);
    Ok(match nst {
        nst::Item::Effect { name, ops, .. } => Item::Effect(ast::EffectItem {
            name: name.value,
            ops: ops
                .iter()
                .map(|opt_op| {
                    opt_op.map(|op| {
                        let ty = ds_ctx.ds_type(op.type_);
                        let eff_var = ds_ctx.ty_vars.push(true);
                        (
                            op.name.value,
                            TyScheme {
                                bound: vec![eff_var],
                                constrs: vec![],
                                eff: Row::Open(eff_var),
                                ty,
                            },
                        )
                    })
                })
                .collect(),
        }),
        nst::Item::Term {
            name,
            annotation,
            value,
            ..
        } => {
            let tree = ds_ctx.ds_term(value)?;
            Item::Function(match annotation {
                Some(scheme) => {
                    let scheme = ds_ctx.ds_scheme(scheme.type_);
                    Ast::new(name.value, ds_ctx.spans, scheme, ds_ctx.terms, tree)
                }
                None => Ast::with_untyped(name.value, ds_ctx.spans, ds_ctx.terms, tree),
            })
        }
    })
}

struct DesugarCtx<'a> {
    db: &'a dyn crate::Db,
    terms: Arena<Term<VarId>>,
    pub(crate) vars: &'a mut IdGen<VarId, bool>,
    pub(crate) ty_vars: &'a mut IdGen<TyVarId, bool>,
    spans: FxHashMap<Idx<Term<VarId>>, Span>,
}

impl<'a> DesugarCtx<'a> {
    fn new(
        db: &'a dyn crate::Db,
        terms: Arena<Term<VarId>>,
        vars: &'a mut IdGen<VarId, bool>,
        ty_vars: &'a mut IdGen<TyVarId, bool>,
    ) -> Self {
        Self {
            db,
            terms,
            vars,
            ty_vars,
            spans: FxHashMap::default(),
        }
    }

    fn mk_term(&mut self, span: Span, term: Term<VarId>) -> Idx<Term<VarId>> {
        let t = self.terms.alloc(term);
        self.spans.insert(t, span);
        t
    }

    /// Desugar a NST Term into it's corresponding AST Term.
    fn ds_term<'n>(
        &mut self,
        nst: &'n nst::Term<'n>,
    ) -> Result<Idx<Term<VarId>>, PatternMatchError> {
        let ast = match nst {
            nst::Term::VariableRef(var) => self.terms.alloc(Term::Variable(var.value)),
            nst::Term::ItemRef(item) => self.terms.alloc(Term::Item(item.value)),
            nst::Term::EffectOpRef(SpanOf { value, .. }) => {
                self.terms.alloc(Term::Operation(*value))
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
                    value = self.mk_term(nst.span(), Annotated { ty, term: value })
                }
                let expr = self.ds_term(expr)?;
                let func = self.mk_term(
                    nst.span(),
                    Abstraction {
                        arg: var.value,
                        body: expr,
                    },
                );
                self.terms.alloc(Application { func, arg: value })
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
                let mut term = self.mk_term(
                    nst.span(),
                    Abstraction {
                        arg: arg.value,
                        body,
                    },
                );
                // Our annotation here is for the argument, so we want to annotate our whole
                // abstraction as, abs : ann -> tv<0> where return type is a fresh type var.
                if let Some(ann) = annotation {
                    let arg_ty = self.ds_type(ann.type_);
                    let ret_ty = self
                        .db
                        .as_core_db()
                        .mk_ty(TypeKind::VarTy(self.ty_vars.push(true)));
                    let ty = self.db.as_core_db().mk_ty(TypeKind::FunTy(arg_ty, ret_ty));
                    term = self.terms.alloc(Annotated { ty, term });
                }
                term
            }
            nst::Term::Application { func, arg, .. } => {
                let func = self.ds_term(func)?;
                let arg = self.ds_term(arg)?;
                self.terms.alloc(Application { func, arg })
            }
            nst::Term::Parenthesized { term, .. } => {
                // We replace the span of this node with the parenthesized span
                self.ds_term(term)?
            }
            nst::Term::ProductRow(product) => match product.fields {
                None => self.terms.alloc(Unit),
                Some(fields) => {
                    let term = self.ds_term(fields.first.target)?;
                    let head = self.terms.alloc(Label {
                        label: fields.first.label.value,
                        term,
                    });
                    self.spans
                        .insert(head, fields.first.label.join_spans(fields.first.target));
                    fields.elems.iter().fold(Ok(head), |concat, (_, field)| {
                        let term = self.ds_term(field.target)?;
                        let right = self.terms.alloc(Label {
                            label: field.label.value,
                            term,
                        });
                        self.spans
                            .insert(right, field.label.join_spans(field.target));
                        Ok(self.terms.alloc(Concat {
                            left: concat?,
                            right,
                        }))
                    })?
                }
            },
            nst::Term::FieldAccess { base, field, .. } => {
                let term = self.ds_term(base)?;
                let unlabel = Unlabel {
                    label: field.value,
                    term: self.mk_term(
                        nst.span(),
                        Project {
                            direction: Direction::Right,
                            term,
                        },
                    ),
                };
                self.terms.alloc(unlabel)
            }
            nst::Term::SumRow(sum) => {
                let term = self.ds_term(sum.field.target)?;
                let inj = Inject {
                    direction: Direction::Right,
                    term: self.mk_term(
                        nst.span(),
                        Label {
                            label: sum.field.label.value,
                            term,
                        },
                    ),
                };
                self.terms.alloc(inj)
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
    ) -> Row {
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

    fn ds_type<'n>(&mut self, nst: &'n cst::Type<'n, TyVarId>) -> Ty {
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

    fn ds_row_atom<'n>(&mut self, row_atom: &'n RowAtom<'n, TyVarId>) -> Row {
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

    fn ds_scheme<'n>(&mut self, nst: &'n Scheme<'n, TyVarId>) -> TyScheme {
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
        let eff = Row::Open(self.ty_vars.push(true));
        TyScheme {
            bound,
            constrs,
            eff,
            ty,
        }
    }

    /// Compile a matrix of patterns into an AST term that performs pattern matching.
    fn desugar_pattern_matrix(
        &mut self,
        occurences: &mut [VarId],
        matrix: ClauseMatrix<'_>,
    ) -> Result<Idx<Term<VarId>>, PatternMatchError> {
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
                    Pattern::Whole(var) => self.mk_term(
                        pat.span(),
                        Abstraction {
                            arg: var.value,
                            body,
                        },
                    ),
                    _ => unreachable!(),
                }))
        } else {
            let constrs = matrix
                .col_constr(0)
                .collect::<std::collections::BTreeMap<_, _>>();
            let mut matches = constrs.into_iter().map(|(c, p)| match c {
                Constructor::ProductRow(ref lbls) => {
                    let top_level = self.vars.push(true);
                    let binders = (0..lbls.len())
                        .map(|_| self.vars.push(true))
                        .collect::<Vec<_>>();
                    let mut occs = binders;
                    // replace first occurence by binder introduced here
                    occs.extend(occurences.iter_mut().skip(1).map(|var| *var));
                    let init =
                        self.desugar_pattern_matrix(occs.as_mut_slice(), matrix.specialize(&c))?;
                    let body = lbls.iter().cloned().fold(init, |body, label| {
                        let term = self.mk_term(p.span(), Variable(top_level));
                        let term = self.mk_term(
                            p.span(),
                            Project {
                                direction: Direction::Right,
                                term,
                            },
                        );
                        let destructure = self.mk_term(p.span(), Unlabel { label, term });
                        self.mk_term(
                            p.span(),
                            Application {
                                func: body,
                                arg: destructure,
                            },
                        )
                    });
                    Ok(self.mk_term(
                        p.span(),
                        Abstraction {
                            arg: top_level,
                            body,
                        },
                    ))
                }
                Constructor::SumRow(label) => {
                    let binder = self.vars.push(true);
                    let mut occs = vec![binder];
                    occs.extend(occurences.iter_mut().skip(1).map(|var| *var));
                    let func = self.desugar_pattern_matrix(&mut occs, matrix.specialize(&c))?;
                    let term = self.mk_term(p.span(), Variable(binder));
                    let arg = self.mk_term(p.span(), Unlabel { label, term });
                    let body = self.mk_term(p.span(), Application { func, arg });
                    Ok(self.mk_term(p.span(), Abstraction { arg: binder, body }))
                }
                Constructor::WildCard => {
                    if let Pattern::Whole(var) = p {
                        // For a wild card we don't need a let binding so just pass through binder as is
                        let body = self.desugar_pattern_matrix(occurences, matrix.default())?;
                        Ok(self.mk_term(
                            p.span(),
                            Abstraction {
                                arg: var.value,
                                body,
                            },
                        ))
                    } else {
                        unreachable!()
                    }
                }
            });
            // we know this can't be empty
            let head: Result<Idx<Term<VarId>>, PatternMatchError> = matches.next().unwrap();
            let rest = matches.collect::<Vec<_>>();
            rest.into_iter().fold(head, |a, b| {
                let a = a?;
                let b = b?;
                let span = Span::join(&self.spans[&a], &self.spans[&b]);
                Ok(self.mk_term(span, Branch { left: a, right: b }))
            })
        }
    }
}

struct ClauseMatrix<'p> {
    pats: Vec<Vec<Pattern<'p>>>,
    arms: Vec<Idx<Term<VarId>>>,
}

impl<'p> FromIterator<(Vec<Pattern<'p>>, Idx<Term<VarId>>)> for ClauseMatrix<'p> {
    fn from_iter<T: IntoIterator<Item = (Vec<Pattern<'p>>, Idx<Term<VarId>>)>>(iter: T) -> Self {
        let (mut pats, mut arms) = (vec![], vec![]);
        iter.into_iter().for_each(|(pat, arm)| {
            pats.push(pat);
            arms.push(arm);
        });
        ClauseMatrix { pats, arms }
    }
}

impl<'p> ClauseMatrix<'p> {
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

    pub(crate) fn specialize(&self, constr: &Constructor) -> ClauseMatrix<'p> {
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

    fn default(&self) -> ClauseMatrix<'p> {
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
    use expect_test::expect;

    #[derive(Default)]
    #[salsa::db(crate::Jar, aiahr_core::Jar, aiahr_analysis::Jar, aiahr_parser::Jar)]
    struct TestDatabase {
        storage: salsa::Storage<Self>,
    }
    impl salsa::Database for TestDatabase {}

    #[test]
    fn test_desugar_var() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let mut vars = IdGen::new();
        let var = random_span_of(vars.push(false));
        let nst = random_term_item(arena.alloc(nst::Term::VariableRef(var)));
        let ast = desugar(&db, &mut vars, &mut IdGen::new(), nst)
            .unwrap()
            .unwrap_func();

        assert_eq!(ast.view(ast.tree), &Variable(var.value));
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
        let x = vars.push(false);
        let span_of_var = random_span_of(x);
        let nst = random_term_item(arena.alloc(nst::Term::Abstraction {
            lbar: start,
            arg: random_span_of(VarId(0)),
            annotation: None,
            rbar: random_span(),
            body: arena.alloc(nst::Term::VariableRef(span_of_var)),
        }));
        let ast = desugar(&db, &mut vars, &mut IdGen::new(), nst)
            .unwrap()
            .unwrap_func();

        assert_eq!(
            ast.span_of(ast.tree),
            Some(&Span {
                start: start.start,
                end: span_of_var.end,
            })
        );

        let mut w = String::new();
        ast.pretty(&db, &pretty::BoxAllocator)
            .render_fmt(100, &mut w)
            .unwrap();
        let expect = expect!["(|var<0>| var<0>)"];
        expect.assert_eq(&w);
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
            &mut vars,
            &mut IdGen::new(),
            random_term_item(arena.alloc(nst::Term::Application {
                func: arena.alloc(nst::Term::VariableRef(start)),
                lpar: random_span(),
                arg: arena.alloc(nst::Term::VariableRef(random_span_of(VarId(1)))),
                rpar: end,
            })),
        )
        .unwrap()
        .unwrap_func();

        assert_eq!(
            ast.span_of(ast.tree),
            Some(&Span {
                start: start.start,
                end: end.end,
            })
        );
        let mut w = String::new();
        ast.pretty(&db, &pretty::BoxAllocator)
            .render_fmt(100, &mut w)
            .unwrap();
        let expect = expect!["var<0>(var<1>)"];
        expect.assert_eq(&w);
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
        .unwrap()
        .unwrap_func();

        assert_eq!(
            ast.span_of(ast.tree),
            Some(&Span {
                start: start.start,
                end: end.end,
            })
        );
        let mut w = String::new();
        ast.pretty(&db, &pretty::BoxAllocator)
            .render_fmt(100, &mut w)
            .unwrap();
        let expect = expect!["(|var<2>| var<123>)(var<10>)"];
        expect.assert_eq(&w);
    }

    #[test]
    fn test_desugar_unit() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let start = random_span();
        let end = random_span();
        let ast = desugar(
            &db,
            &mut IdGen::new(),
            &mut IdGen::new(),
            random_term_item(arena.alloc(nst::Term::ProductRow(ProductRow {
                lbrace: start,
                fields: None,
                rbrace: end,
            }))),
        )
        .unwrap()
        .unwrap_func();

        assert_eq!(
            ast.span_of(ast.tree),
            Some(&Span {
                start: start.start,
                end: end.end,
            })
        );
        assert_eq!(ast.view(ast.tree), &Unit);
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
        .unwrap()
        .unwrap_func();

        assert_eq!(
            ast.span_of(ast.tree),
            Some(&Span {
                start: start.start,
                end: end.end,
            })
        );
        let mut w = String::new();
        ast.pretty(&db, &pretty::BoxAllocator)
            .render_fmt(100, &mut w)
            .unwrap();
        let expect = expect!["((abc = var<0> *** def = var<1>) *** ghi = var<2>)"];
        expect.assert_eq(&w);
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

        let ast = desugar(&db, &mut IdGen::new(), &mut IdGen::new(), nst)
            .unwrap()
            .unwrap_func();

        assert_eq!(
            ast.span_of(ast.tree),
            Some(&Span {
                start: base.start,
                end: field.end,
            }),
        );
        let mut w = String::new();
        ast.pretty(&db, &pretty::BoxAllocator)
            .render_fmt(100, &mut w)
            .unwrap();
        let expect = expect!["prj<R>(var<0>).state"];
        expect.assert_eq(&w);
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

        let ast = desugar(&db, &mut IdGen::new(), &mut IdGen::new(), nst)
            .unwrap()
            .unwrap_func();

        assert_eq!(
            ast.span_of(ast.tree),
            Some(&Span {
                start: langle.start,
                end: rangle.end,
            }),
        );
        let mut w = String::new();
        ast.pretty(&db, &pretty::BoxAllocator)
            .render_fmt(100, &mut w)
            .unwrap();
        let expect = expect!["inj<R>(true = var<0>)"];
        expect.assert_eq(&w);
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

        let mut vars = [false, false, false].into_iter().collect();
        let ast = desugar(&db, &mut vars, &mut IdGen::new(), nst)
            .unwrap()
            .unwrap_func();
        let mut w = String::new();
        ast.pretty(&db, &pretty::BoxAllocator)
            .render_fmt(100, &mut w)
            .unwrap();
        let expect = expect![[r#"
            (((|var<3>| (|var<0>| var<0>)(var<3>.A)) +++ (|var<4>| (|var<1>| var<1>)(var<4>.B))) +++ (|var<5>|
            (|var<2>| var<2>)(var<5>.C)))"#]];
        expect.assert_eq(&w);
    }

    #[test]
    fn test_desugar_match_sum_with_default() {
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

        let mut vars = [false, false, false, false].into_iter().collect();
        let ast = desugar(&db, &mut vars, &mut IdGen::new(), nst)
            .unwrap()
            .unwrap_func();
        let mut w = String::new();
        ast.pretty(&db, &pretty::BoxAllocator)
            .render_fmt(100, &mut w)
            .unwrap();
        let expect = expect![[r#"
            ((((|var<4>| (|var<0>| var<0>)(var<4>.A)) +++ (|var<5>| (|var<1>| var<1>)(var<5>.B))) +++ (|var<6>|
            (|var<2>| var<2>)(var<6>.C))) +++ (|var<3>| var<3>))"#]];
        expect.assert_eq(&w);
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

        let mut vars = [false, false, false].into_iter().collect();
        let ast = desugar(&db, &mut vars, &mut IdGen::new(), nst)
            .unwrap()
            .unwrap_func();
        let mut w = String::new();
        ast.pretty(&db, &pretty::BoxAllocator)
            .render_fmt(100, &mut w)
            .unwrap();
        let expect = expect![[r#"
            (|var<3>| (|var<0>| (|var<1>| (|var<2>|
            var<1>)))(prj<R>(var<3>).A)(prj<R>(var<3>).B)(prj<R>(var<3>).C))"#]];
        expect.assert_eq(&w);
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

        let mut vars = [false, false, false, false, false, false]
            .into_iter()
            .collect();
        let ast = desugar(&db, &mut vars, &mut IdGen::new(), nst)
            .unwrap()
            .unwrap_func();

        let mut w = String::new();
        ast.pretty(&db, &pretty::BoxAllocator)
            .render_fmt(100, &mut w)
            .unwrap();
        let expect = expect![[r#"
            (|var<6>| ((|var<10>| (|var<0>| (|var<11>| (|var<1>| (|var<2>| var<0>))(var<11>.B)))(var<10>.A)) +++
            (|var<0>| ((|var<12>| (|var<1>| (|var<2>| var<1>))(var<12>.B)) +++ (|var<1>| (|var<13>| (|var<2>|
            var<2>)(var<13>.C))))))(prj<R>(var<6>).x)(prj<R>(var<6>).y)(prj<R>(var<6>).z))"#]];
        expect.assert_eq(&w);
    }
}
