use std::ops::Not;

use aiahr_ast::{
    self as ast, Ast, AstModule, Direction, Item, SalsaItem,
    Term::{self, *},
};
use aiahr_core::{
    id::{EffectId, EffectOpId, Id, IdGen, ItemId, ModuleId, TyVarId, VarId},
    ident::Ident,
    modules::{module_of, Module},
    span::{Span, SpanOf, Spanned},
    Top,
};
use aiahr_cst::{
    self as cst,
    nameres::{self as nst, NstIndxAlloc},
    Field,
};
use aiahr_ty::{row::Row, Evidence, MkTy, Ty, TyScheme, TypeKind};
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
pub trait Db: salsa::DbWithJar<Jar> + aiahr_nameres::Db + aiahr_ast::Db + aiahr_ty::Db {
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
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + aiahr_nameres::Db + aiahr_ast::Db + aiahr_ty::Db
{}

/// Desugar an NST Module into an AST module.
/// This will desugar all items in NST moduels into their corresponding AST items.
#[salsa::tracked]
pub fn desugar_module(db: &dyn crate::Db, module: aiahr_nameres::NameResModule) -> AstModule {
    let resolution = module.items(db.as_nameres_db());
    let ty_vars = resolution.local_ids.ty_vars.len();
    let vars = resolution.local_ids.vars.len();
    AstModule::new(
        db.as_ast_db(),
        module.module(db.as_nameres_db()),
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
    item: aiahr_nameres::SalsaItem,
    vars: usize,
    ty_vars: usize,
) -> ast::SalsaItem {
    // TODO: Handle separation of name based Ids and desugar generated Ids better.
    let mut ty_vars = IdGen::from_iter((0..ty_vars).map(|_| false));
    let mut vars = IdGen::from_iter((0..vars).map(|_| false));

    let ast_res = desugar(
        db,
        &mut vars,
        &mut ty_vars,
        item.alloc(db.as_nameres_db()),
        item.data(db.as_nameres_db()),
    );

    let salsa_ast = match ast_res {
        Ok(item) => item,
        Err(_pat_err) => {
            todo!()
        }
    };

    SalsaItem::new(db.as_ast_db(), salsa_ast)
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
        .items(db.as_ast_db())
        .iter()
        .find(|item| match item.item(db.as_ast_db()) {
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
        .items(db.as_ast_db())
        .iter()
        .find_map(|item| match item.item(db.as_ast_db()) {
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
    arenas: &NstIndxAlloc,
    nst: &nst::Item,
) -> Result<ast::Item<VarId>, PatternMatchError> {
    let terms = la_arena::Arena::default();
    let mut ds_ctx = DesugarCtx::new(db, terms, arenas, vars, ty_vars);
    Ok(match nst {
        nst::Item::Effect { name, ops, .. } => Item::Effect(ast::EffectItem {
            name: name.value,
            ops: ops
                .iter()
                .map(|opt_op| {
                    opt_op.as_ref().map(|op| {
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
            let tree = ds_ctx.ds_term(*value)?;
            Item::Function(match annotation {
                Some(scheme) => {
                    let scheme = ds_ctx.ds_scheme(&scheme.type_);
                    Ast::new(name.value, ds_ctx.spans, scheme, ds_ctx.terms, tree)
                }
                None => Ast::with_untyped(name.value, ds_ctx.spans, ds_ctx.terms, tree),
            })
        }
    })
}

struct DesugarCtx<'a> {
    db: &'a dyn crate::Db,
    arenas: &'a NstIndxAlloc,
    terms: Arena<Term<VarId>>,
    pub(crate) vars: &'a mut IdGen<VarId, bool>,
    pub(crate) ty_vars: &'a mut IdGen<TyVarId, bool>,
    spans: FxHashMap<Idx<Term<VarId>>, Span>,
}

impl<'a> DesugarCtx<'a> {
    fn new(
        db: &'a dyn crate::Db,
        terms: Arena<Term<VarId>>,
        arenas: &'a NstIndxAlloc,
        vars: &'a mut IdGen<VarId, bool>,
        ty_vars: &'a mut IdGen<TyVarId, bool>,
    ) -> Self {
        Self {
            db,
            terms,
            arenas,
            vars,
            ty_vars,
            spans: FxHashMap::default(),
        }
    }

    fn mk_term(&mut self, span: impl Spanned, term: Term<VarId>) -> Idx<Term<VarId>> {
        let t = self.terms.alloc(term);
        self.spans.insert(t, span.span());
        t
    }

    /// Desugar a NST Term into it's corresponding AST Term.
    fn ds_term(&mut self, nst: Idx<nst::Term>) -> Result<Idx<Term<VarId>>, PatternMatchError> {
        let nst = &self.arenas[nst];
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
                let mut value = self.ds_term(*value)?;
                // If our binding is annotated wrap our value in it's annotated type
                if let Some(ann) = annotation {
                    let ty = self.ds_type(ann.type_);
                    value = self.mk_term(nst.spanned(self.arenas), Annotated { ty, term: value })
                }
                let expr = self.ds_term(*expr)?;
                let func = self.mk_term(
                    nst.spanned(self.arenas),
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
                let body = self.ds_term(*body)?;
                // If there's no annotation, we double insert spans which is fine as our map is
                // idempotent
                let mut term = self.mk_term(
                    nst.spanned(self.arenas),
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
                        .as_ty_db()
                        .mk_ty(TypeKind::VarTy(self.ty_vars.push(true)));
                    let ty = self.db.as_ty_db().mk_ty(TypeKind::FunTy(arg_ty, ret_ty));
                    term = self.terms.alloc(Annotated { ty, term });
                }
                term
            }
            nst::Term::Application { func, arg, .. } => {
                let func = self.ds_term(*func)?;
                let arg = self.ds_term(*arg)?;
                self.terms.alloc(Application { func, arg })
            }
            nst::Term::Parenthesized { term, .. } => {
                // We replace the span of this node with the parenthesized span
                self.ds_term(*term)?
            }
            nst::Term::ProductRow(product) => match &product.fields {
                None => self.terms.alloc(Unit),
                Some(fields) => {
                    let term = self.ds_term(fields.first.target)?;
                    let head = self.terms.alloc(Label {
                        label: fields.first.label.value,
                        term,
                    });
                    self.spans.insert(
                        head,
                        fields
                            .first
                            .label
                            .join_spans(&self.arenas[fields.first.target].spanned(self.arenas)),
                    );
                    fields.elems.iter().fold(Ok(head), |concat, (_, field)| {
                        let term = self.ds_term(field.target)?;
                        let right = self.terms.alloc(Label {
                            label: field.label.value,
                            term,
                        });
                        self.spans.insert(
                            right,
                            field
                                .label
                                .join_spans(&self.arenas[field.target].spanned(self.arenas)),
                        );
                        Ok(self.terms.alloc(Concat {
                            left: concat?,
                            right,
                        }))
                    })?
                }
            },
            nst::Term::FieldAccess { base, field, .. } => {
                let term = self.ds_term(*base)?;
                let unlabel = Unlabel {
                    label: field.value,
                    term: self.mk_term(
                        nst.spanned(self.arenas),
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
                        nst.spanned(self.arenas),
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
                    .map(|field| {
                        Ok((
                            vec![self.arenas[field.label].clone()],
                            self.ds_term(field.target)?,
                        ))
                    })
                    .collect::<Result<_, _>>()?;
                self.desugar_pattern_matrix(&mut [], matrix)?
            }
            nst::Term::Handle { .. } => todo!(),
        };
        self.spans.insert(ast, nst.spanned(self.arenas).span());
        Ok(ast)
    }

    fn ds_row(
        &mut self,
        row: &cst::Row<TyVarId, Field<SpanOf<Ident>, Idx<cst::Type<TyVarId>>>>,
    ) -> Row {
        match row {
            cst::Row::Concrete(closed) => Row::Closed(
                self.db.as_ty_db().construct_row(
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

    fn ds_type(&mut self, nst: Idx<cst::Type<TyVarId>>) -> Ty {
        match &self.arenas[nst] {
            cst::Type::Named(ty_var) => self.db.as_ty_db().mk_ty(TypeKind::VarTy(ty_var.value)),
            cst::Type::Sum { variants, .. } => self
                .db
                .as_ty_db()
                .mk_ty(TypeKind::SumTy(self.ds_row(variants))),
            cst::Type::Product { fields, .. } => self.db.as_ty_db().mk_ty(TypeKind::ProdTy(
                fields
                    .as_ref()
                    .map(|row| self.ds_row(row))
                    .unwrap_or(Row::Closed(self.db.as_ty_db().empty_row())),
            )),
            cst::Type::Function {
                domain, codomain, ..
            } => self.db.as_ty_db().mk_ty(TypeKind::FunTy(
                self.ds_type(*domain),
                self.ds_type(*codomain),
            )),
            cst::Type::Parenthesized { type_, .. } => self.ds_type(*type_),
        }
    }

    fn ds_row_atom(&mut self, row_atom: &cst::RowAtom<TyVarId>) -> Row {
        match row_atom {
            cst::RowAtom::Concrete { fields, .. } => Row::Closed(
                self.db.as_ty_db().construct_row(
                    fields
                        .elements()
                        .map(|field| (field.label.value, self.ds_type(field.target)))
                        .collect(),
                ),
            ),
            cst::RowAtom::Variable(ty_var) => Row::Open(ty_var.value),
        }
    }

    fn ds_scheme(&mut self, nst: &cst::Scheme<TyVarId>) -> TyScheme {
        let bound = nst
            .quantifiers
            .iter()
            .map(|quant| quant.var.value)
            .collect();
        let constrs = nst
            .qualifiers
            .as_ref()
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
        matrix: ClauseMatrix,
    ) -> Result<Idx<Term<VarId>>, PatternMatchError> {
        if matrix.is_empty() {
            Err(PatternMatchError::NonExhaustivePatterns)
        // Row is all wild cards
        } else if matrix
            .first()
            .iter()
            .all(|pat| matches!(pat, nst::Pattern::Whole(_)))
        {
            Ok(matrix
                .first()
                .iter()
                .rfold(matrix.arms[0], |body, pat| match pat {
                    nst::Pattern::Whole(var) => self.mk_term(
                        var.span(),
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
                    let init = self.desugar_pattern_matrix(
                        occs.as_mut_slice(),
                        matrix.specialize(&c, self.arenas),
                    )?;
                    let body = lbls.iter().cloned().fold(init, |body, label| {
                        let term = self.mk_term(p, Variable(top_level));
                        let term = self.mk_term(
                            p,
                            Project {
                                direction: Direction::Right,
                                term,
                            },
                        );
                        let destructure = self.mk_term(p, Unlabel { label, term });
                        self.mk_term(
                            p,
                            Application {
                                func: body,
                                arg: destructure,
                            },
                        )
                    });
                    Ok(self.mk_term(
                        p,
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
                    let func =
                        self.desugar_pattern_matrix(&mut occs, matrix.specialize(&c, self.arenas))?;
                    let term = self.mk_term(p, Variable(binder));
                    let arg = self.mk_term(p, Unlabel { label, term });
                    let body = self.mk_term(p, Application { func, arg });
                    Ok(self.mk_term(p, Abstraction { arg: binder, body }))
                }
                Constructor::WildCard => {
                    if let nst::Pattern::Whole(var) = p {
                        // For a wild card we don't need a let binding so just pass through binder as is
                        let body = self.desugar_pattern_matrix(occurences, matrix.default())?;
                        Ok(self.mk_term(
                            var.span(),
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

struct ClauseMatrix {
    pats: Vec<Vec<nst::Pattern>>,
    arms: Vec<Idx<Term<VarId>>>,
}

impl FromIterator<(Vec<nst::Pattern>, Idx<Term<VarId>>)> for ClauseMatrix {
    fn from_iter<T: IntoIterator<Item = (Vec<nst::Pattern>, Idx<Term<VarId>>)>>(iter: T) -> Self {
        let (mut pats, mut arms) = (vec![], vec![]);
        iter.into_iter().for_each(|(pat, arm)| {
            pats.push(pat);
            arms.push(arm);
        });
        ClauseMatrix { pats, arms }
    }
}

impl ClauseMatrix {
    fn is_empty(&self) -> bool {
        debug_assert!(
            (self.pats.is_empty() && self.arms.is_empty())
                || (self.pats.is_empty().not() && self.arms.is_empty().not())
        );
        self.pats.is_empty()
    }

    fn first(&self) -> &[nst::Pattern] {
        debug_assert!(self.pats.is_empty().not());
        self.pats[0].as_slice()
    }

    fn col_constr(
        &self,
        col_index: usize,
    ) -> impl Iterator<Item = (Constructor, &nst::Pattern)> + '_ {
        self.pats
            .iter()
            .map(move |col| (Constructor::from(&col[col_index]), &col[col_index]))
    }

    pub(crate) fn specialize(&self, constr: &Constructor, alloc: &NstIndxAlloc) -> ClauseMatrix {
        self.pats
            .iter()
            .zip(self.arms.iter())
            .filter_map(|(pat, arm)| {
                constr.matches(&pat[0], alloc).map(|mut pats| {
                    pats.extend(pat[1..].iter().cloned());
                    (pats, *arm)
                })
            })
            .collect()
    }

    fn default(&self) -> ClauseMatrix {
        self.pats
            .iter()
            .zip(self.arms.iter())
            .filter_map(|(pats, arm)| match pats[0] {
                nst::Pattern::Whole(_) => Some((pats[1..].to_vec(), *arm)),
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
    fn matches(&self, pat: &nst::Pattern, alloc: &NstIndxAlloc) -> Option<Vec<nst::Pattern>> {
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
            (Constructor::ProductRow(lbls), nst::Pattern::ProductRow(rows)) => rows
                .fields
                .iter()
                .flat_map(|fields| fields.elements())
                .zip(lbls)
                .map(|(row, lbl)| row.label.value.eq(lbl).then(|| alloc[row.target].clone()))
                .collect::<Option<Vec<_>>>(),
            (Constructor::SumRow(lbl), nst::Pattern::SumRow(row))
                if row.field.label.value.eq(lbl) =>
            {
                Some(vec![alloc[row.field.target].clone()])
            }
            (Constructor::WildCard, nst::Pattern::Whole(_)) => Some(vec![]),
            // A wild card always matches and produces sub wild card patterns for each pattern our
            // match would have
            (Constructor::WildCard, nst::Pattern::SumRow(_)) => {
                Some(vec![nst::Pattern::Whole(bogus_var_id)])
            }
            (Constructor::WildCard, nst::Pattern::ProductRow(rows)) => Some(
                rows.into_iter()
                    .map(|_| nst::Pattern::Whole(bogus_var_id))
                    .collect(),
            ),
            _ => None,
        }
    }
}

impl From<&nst::Pattern> for Constructor {
    fn from(pat: &nst::Pattern) -> Self {
        match pat {
            nst::Pattern::ProductRow(rows) => Constructor::ProductRow(
                rows.fields
                    .as_ref()
                    .map(|sep| sep.elements().map(|field| field.label.value).collect())
                    .unwrap_or_default(),
            ),
            nst::Pattern::SumRow(row) => Constructor::SumRow(row.field.label.value),
            nst::Pattern::Whole(_) => Constructor::WildCard,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::Db as DesugarDb;

    use super::*;
    use aiahr_ast as ast;
    use aiahr_core::file::{SourceFile, SourceFileSet};
    use aiahr_nameres::Db;
    use expect_test::expect;

    #[derive(Default)]
    #[salsa::db(
        crate::Jar,
        aiahr_ast::Jar,
        aiahr_core::Jar,
        aiahr_nameres::Jar,
        aiahr_parser::Jar,
        aiahr_ty::Jar
    )]
    struct TestDatabase {
        storage: salsa::Storage<Self>,
    }
    impl salsa::Database for TestDatabase {}

    const MOD: ModuleId = ModuleId(0);
    const WIDTH: usize = 100;

    fn ds_snippet<'db>(
        db: &'db TestDatabase,
        input: &str,
    ) -> (aiahr_nameres::SalsaItem, &'db ast::SalsaItem) {
        let mut content = "item = ".to_string();
        content.push_str(input);
        let file = SourceFile::new(db, MOD, PathBuf::from("test.aiahr"), content);
        SourceFileSet::new(db, vec![file]);
        let namesres_module = db.nameres_module_of(MOD);
        (
            namesres_module
                .items(db)
                .resolved_items
                .first()
                .unwrap()
                .unwrap(),
            db.desugar_module_id_of(MOD).items(db).first().unwrap(),
        )
    }

    #[test]
    fn test_desugar_abs() {
        let db = TestDatabase::default();

        let (nst_item, ast_item) = ds_snippet(&db, "|x| x");
        let ast = ast_item.item(&db).unwrap_func();

        assert_eq!(ast.span_of(ast.tree), Some(&nst_item.span_of(&db)));

        let pretty_ast = ast
            .pretty(&db, &pretty::BoxAllocator)
            .pretty(WIDTH)
            .to_string();
        let expect = expect!["(|var<0>| var<0>)"];
        expect.assert_eq(&pretty_ast);
    }

    #[test]
    fn test_desugar_app() {
        let db = TestDatabase::default();

        let (nst_item, ast_item) = ds_snippet(&db, "|f| |x| f(x)");
        let ast = ast_item.item(&db).unwrap_func();

        assert_eq!(ast.span_of(ast.tree), Some(&nst_item.span_of(&db)));
        let pretty_ast = ast
            .pretty(&db, &pretty::BoxAllocator)
            .pretty(WIDTH)
            .to_string();
        let expect = expect!["(|var<0>| (|var<1>| var<0>(var<1>)))"];
        expect.assert_eq(&pretty_ast);
    }

    #[test]
    fn test_desugar_binding() {
        let db = TestDatabase::default();

        let (nst_item, ast_item) = ds_snippet(&db, "|a||b| x = a; x(b)");
        let ast = ast_item.item(&db).unwrap_func();

        assert_eq!(ast.span_of(ast.tree), Some(&nst_item.span_of(&db)));
        let pretty_ast = ast
            .pretty(&db, &pretty::BoxAllocator)
            .pretty(WIDTH)
            .to_string();
        let expect = expect!["(|var<0>| (|var<1>| (|var<2>| var<2>(var<1>))(var<0>)))"];
        expect.assert_eq(&pretty_ast);
    }

    #[test]
    fn test_desugar_unit() {
        let db = TestDatabase::default();

        let (nst_item, ast_item) = ds_snippet(&db, "{}");
        let ast = ast_item.item(&db).unwrap_func();

        assert_eq!(ast.span_of(ast.tree), Some(&nst_item.span_of(&db)));
        assert_eq!(ast.view(ast.tree), &Unit);
    }

    #[test]
    fn test_desugar_product() {
        let db = TestDatabase::default();

        let (nst_item, ast_item) = ds_snippet(&db, "|x||y||z| { abc = x, def = y, ghi = z }");
        let ast = ast_item.item(&db).unwrap_func();

        assert_eq!(ast.span_of(ast.tree), Some(&nst_item.span_of(&db)));
        let pretty_ast = ast
            .pretty(&db, &pretty::BoxAllocator)
            .pretty(WIDTH)
            .to_string();
        let expect = expect![
            "(|var<0>| (|var<1>| (|var<2>| ((abc = var<0> *** def = var<1>) *** ghi = var<2>))))"
        ];
        expect.assert_eq(&pretty_ast);
    }

    #[test]
    fn test_desugar_field_access() {
        let db = TestDatabase::default();

        let (nst_item, ast_item) = ds_snippet(&db, "|base| base.state");
        let ast = ast_item.item(&db).unwrap_func();

        assert_eq!(ast.span_of(ast.tree), Some(&nst_item.span_of(&db)),);
        let pretty_ast = ast
            .pretty(&db, &pretty::BoxAllocator)
            .pretty(WIDTH)
            .to_string();
        let expect = expect!["(|var<0>| prj<R>(var<0>).state)"];
        expect.assert_eq(&pretty_ast);
    }

    #[test]
    fn test_desugar_sum() {
        let db = TestDatabase::default();

        let (nst_item, ast_item) = ds_snippet(&db, "< true = {} >");
        let ast = ast_item.item(&db).unwrap_func();

        assert_eq!(ast.span_of(ast.tree), Some(&nst_item.span_of(&db)),);
        let pretty_ast = ast
            .pretty(&db, &pretty::BoxAllocator)
            .pretty(WIDTH)
            .to_string();
        let expect = expect!["inj<R>(true = {})"];
        expect.assert_eq(&pretty_ast);
    }

    #[test]
    fn test_desugar_match_sum() {
        let db = TestDatabase::default();
        let (nst_item, ast_item) = ds_snippet(
            &db,
            r#"
match <
    <A = x> => x,
    <B = y> => y,
    <C = z> => z
>
"#,
        );
        let ast = ast_item.item(&db).unwrap_func();
        let pretty_ast = ast
            .pretty(&db, &pretty::BoxAllocator)
            .pretty(WIDTH)
            .to_string();
        assert_eq!(ast.span_of(ast.root()), Some(&nst_item.span_of(&db)));
        let expect = expect![[r#"
            (((|var<3>| (|var<0>| var<0>)(var<3>.A)) +++ (|var<4>| (|var<1>| var<1>)(var<4>.B))) +++ (|var<5>|
            (|var<2>| var<2>)(var<5>.C)))"#]];
        expect.assert_eq(&pretty_ast);
    }

    #[test]
    fn test_desugar_match_sum_with_default() {
        let db = TestDatabase::default();
        let (nst_item, ast_item) = ds_snippet(
            &db,
            r#"
match <
  <A = x> => x,
  <B = y> => y,
  <C = z> => z,
  w => w
>
"#,
        );
        let ast = ast_item.item(&db).unwrap_func();
        assert_eq!(ast.span_of(ast.tree), Some(&nst_item.span_of(&db)));
        let pretty_ast = ast
            .pretty(&db, &pretty::BoxAllocator)
            .pretty(WIDTH)
            .to_string();
        let expect = expect![[r#"
            ((((|var<4>| (|var<0>| var<0>)(var<4>.A)) +++ (|var<5>| (|var<1>| var<1>)(var<5>.B))) +++ (|var<6>|
            (|var<2>| var<2>)(var<6>.C))) +++ (|var<3>| var<3>))"#]];
        expect.assert_eq(&pretty_ast);
    }

    #[test]
    fn test_desugar_match_prod() {
        let db = TestDatabase::default();
        let (nst_item, ast_item) = ds_snippet(
            &db,
            r#"
match <
  { A = x, B = y, C = z } => y
>
"#,
        );
        let ast = ast_item.item(&db).unwrap_func();
        let pretty_ast = ast
            .pretty(&db, &pretty::BoxAllocator)
            .pretty(WIDTH)
            .to_string();
        assert_eq!(ast.span_of(ast.tree), Some(&nst_item.span_of(&db)));
        let expect = expect![[r#"
            (|var<3>| (|var<0>| (|var<1>| (|var<2>|
            var<1>)))(prj<R>(var<3>).A)(prj<R>(var<3>).B)(prj<R>(var<3>).C))"#]];
        expect.assert_eq(&pretty_ast);
    }

    #[test]
    fn test_desugar_match_nested_patterns() {
        let db = TestDatabase::default();
        let (nst_item, ast_item) = ds_snippet(
            &db,
            r#"
match <
    { x = <A = a>, y = <B = b>, z = c } => a,
    { x = a, y = <B = b>, z = c }  => b,
    { x = a, y = b, z = <C = c> } => c
>
"#,
        );
        let ast = ast_item.item(&db).unwrap_func();

        let pretty_ast = ast
            .pretty(&db, &pretty::BoxAllocator)
            .pretty(WIDTH)
            .to_string();
        assert_eq!(ast.span_of(ast.tree), Some(&nst_item.span_of(&db)));
        let expect = expect![[r#"
            (|var<9>| ((|var<13>| (|var<0>| (|var<14>| (|var<1>| (|var<2>| var<0>))(var<14>.B)))(var<13>.A)) +++
            (|var<6>| ((|var<15>| (|var<4>| (|var<5>| var<4>))(var<15>.B)) +++ (|var<7>| (|var<16>| (|var<8>|
            var<8>)(var<16>.C))))))(prj<R>(var<9>).x)(prj<R>(var<9>).y)(prj<R>(var<9>).z))"#]];
        expect.assert_eq(&pretty_ast);
    }
}
