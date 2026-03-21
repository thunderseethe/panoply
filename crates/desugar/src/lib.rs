use std::{
  collections::BTreeMap,
  ops::{Not, Range},
};

use ast::{
  Ast, AstModule, Direction,
  Term::{self, *},
};
use ast::{AstEffect, AstTerm};
use base::{
  id::{EffectName, EffectOpName, IdGen, TermName, TyVarId, VarId},
  ident::Ident,
  loc::Loc,
  modules::Module,
  span::{Span, Spanned},
};
use cst::{
  AstNode, GreenNode, HasName, Name, Panoply, Pattern, RowAtom, SyntaxNode, TermAtom, TermDefn,
  TermPostfix, TermPrefix, Type,
};
use la_arena::{Arena, Idx};
use nameres::{
  Handle, InScopeName, NameResEffect, NameResTerm, nameres_module_of, nameres_term_of,
};
use rustc_hash::{FxHashMap, FxHashSet};
use ty::{
  Evidence, InDb, MkTy, Ty, TyScheme, TypeKind,
  row::{Row, ScopedRow, Simple},
};

use salsa::Database as Db;

/// Desugar an NST Module into an AST module.
/// This will desugar all items in NST moduels into their corresponding AST items.
#[salsa::tracked]
pub fn desugar_module<'db>(db: &'db dyn salsa::Database, module: Module) -> AstModule<'db> {
  let nameres_db = db;

  let nameres_module = nameres_module_of(db, module);

  AstModule::new(
    db,
    module,
    nameres_module
      .terms(nameres_db)
      .values()
      .map(|term| desugar_term(db, module, *term))
      .collect(),
    nameres_module
      .effects(nameres_db)
      .values()
      .map(|effect| desugar_effect(db, module, *effect))
      .collect(),
  )
}

#[salsa::tracked]
fn desugar_effect<'db>(
  db: &'db dyn crate::Db,
  module: Module,
  effect: NameResEffect<'db>,
) -> AstEffect<'db> {
  // TODO: Handle separation of name based Ids and desugar generated Ids better.

  let nameres_module = nameres_module_of(db, module);

  let mut vars = effect.vars(db).to_gen().into_iter().map(|_| true).collect();
  let mut ty_vars = effect
    .ty_vars(db)
    .to_gen()
    .into_iter()
    .map(|_| true)
    .collect();
  let names = nameres_module.names(db);

  let handle = effect.data(db).clone();
  let eff_defn = desugar_effect_defn(db, &mut vars, &mut ty_vars, names, handle.into_node());

  AstEffect::new(db, effect.name(db), eff_defn)
}

pub fn desugar_term_of<'db>(db: &'db dyn salsa::Database, term: TermName) -> AstTerm<'db> {
  let module = term.module(db);
  let nameres_term = nameres_term_of(db, term);
  desugar_term(db, module, nameres_term)
}

#[salsa::tracked]
fn desugar_term<'db>(
  db: &'db dyn salsa::Database,
  module: Module,
  term: NameResTerm<'db>,
) -> AstTerm<'db> {
  // TODO: Handle separation of name based Ids and desugar generated Ids better.
  let nameres_module = nameres_module_of(db, module);

  let mut vars = term.vars(db).to_gen().into_iter().map(|_| true).collect();
  let mut ty_vars = term
    .ty_vars(db)
    .to_gen()
    .into_iter()
    .map(|_| true)
    .collect();
  let names = nameres_module.names(db);

  let handle = term.data(db).clone();
  let term_defn_res = desugar_term_defn(db, &mut vars, &mut ty_vars, names, handle.into_node());

  let term_defn = match term_defn_res {
    Ok(term) => term,
    Err(_pat_err) => {
      todo!()
    }
  };

  AstTerm::new(db, term.name(db), term_defn)
}

#[salsa::tracked]
pub fn desugar_item_of_id<'db>(db: &'db dyn crate::Db, term_name: TermName) -> AstTerm<'db> {
  let module = term_name.module(db);
  let ast_module = desugar_module(db, module);
  ast_module
    .terms(db)
    .iter()
    .find(|term| term.name(db) == term_name)
    .cloned()
    .unwrap_or_else(|| {
      panic!(
        "ICE: Created TermName {:?} without corresponding Term",
        term_name.name(db).text(db)
      )
    })
}

#[salsa::tracked]
pub fn effect_of<'db>(db: &'db dyn crate::Db, effect_name: EffectName) -> AstEffect<'db> {
  let module = effect_name.module(db);
  let ast_mod = desugar_module(db, module);
  *ast_mod
    .effects(db)
    .iter()
    .find(|effect| effect.name(db) == effect_name)
    .unwrap_or_else(|| {
      panic!(
        "ICE: Constructed EffectName {:?} without an Effect definition",
        effect_name.name(db).text(db)
      )
    })
}

#[salsa::tracked]
pub fn effect_op_tyscheme_of(db: &dyn crate::Db, effect_op: EffectOpName) -> TyScheme {
  let effect = effect_op.effect(db);
  let eff = effect_of(db, effect);
  eff
    .data(db)
    .ops
    .iter()
    .find_map(|opt_op| {
      opt_op.as_ref().and_then(|op| {
        (op.0 == effect_op).then_some(op.1.clone())
      })
    })
    .unwrap_or_else(|| {
      panic!(
        "ICE: Constructed EffectOpName {:?} without an Effect Operation definition",
        effect_op.name(db).text(db)
      )
    })
}

pub(crate) fn desugar_effect_defn(
  db: &dyn crate::Db,
  vars: &mut IdGen<VarId, bool>,
  ty_vars: &mut IdGen<TyVarId, bool>,
  names: &BTreeMap<Handle<Name>, InScopeName>,
  eff: cst::EffectDefn,
) -> ast::EffectDefn {
  let terms = la_arena::Arena::default();
  let mut root: SyntaxNode<Panoply> = eff.syntax().clone();
  while let Some(parent) = root.parent() {
    root = parent;
  }
  let root: GreenNode = root.green().into();

  let mut ds_ctx = DesugarCtx::new(db, root.clone(), names, terms, vars, ty_vars);
  let outer_eff = ds_ctx.ty_vars.push(true);
  let name = eff.name().expect("Failure");
  let handle = Handle::new(root.clone(), name);

  let ops = eff
    .ops()
    .into_iter()
    .flatten()
    .map(|op| {
      let scheme = ds_ctx.ds_scheme_with_eff(
        op.annotation()
          .expect("Missing annotation")
          .scheme()
          .expect("Misssing scheme"),
        Row::Open(outer_eff),
      );
      let handle = Handle::new(root.clone(), op.name().expect("Missing name"));
      Some((
        match names.get(&handle) {
          Some(InScopeName::EffectOp(id)) => *id,
          _ => unreachable!(),
        },
        scheme,
      ))
    })
    .collect();

  ast::EffectDefn {
    name: match names.get(&handle) {
      Some(InScopeName::Effect(id)) => *id,
      name => unreachable!("{:?}", name),
    },
    ops,
  }
}

pub(crate) fn desugar_term_defn(
  db: &dyn crate::Db,
  vars: &mut IdGen<VarId, bool>,
  ty_vars: &mut IdGen<TyVarId, bool>,
  names: &BTreeMap<Handle<Name>, InScopeName>,
  term: TermDefn,
) -> Result<ast::Ast<VarId>, PatternMatchError> {
  let terms = la_arena::Arena::default();
  let mut root: SyntaxNode<Panoply> = term.syntax().clone();
  while let Some(parent) = root.parent() {
    root = parent;
  }
  let root: GreenNode = root.green().into();
  let mut ds_ctx = DesugarCtx::new(db, root, names, terms, vars, ty_vars);
  let tree = ds_ctx.ds_term(term.term().expect("Failure"))?;
  Ok(match term.annotation() {
    Some(ref scheme) => {
      let scheme = ds_ctx.ds_scheme(scheme.scheme().expect("Failure"));
      Ast::new(ds_ctx.spans, scheme, ds_ctx.terms, tree)
    }
    None => Ast::with_untyped(ds_ctx.spans, ds_ctx.terms, tree),
  })
}

fn fix_up(range: Range<usize>) -> Span {
  Span {
    start: Loc { byte: range.start },
    end: Loc { byte: range.end },
  }
}

struct DesugarCtx<'a> {
  db: &'a dyn crate::Db,
  names: &'a BTreeMap<Handle<Name>, InScopeName>,
  root: GreenNode,
  terms: Arena<Term<VarId>>,
  pub(crate) vars: &'a mut IdGen<VarId, bool>,
  pub(crate) ty_vars: &'a mut IdGen<TyVarId, bool>,
  spans: FxHashMap<Idx<Term<VarId>>, Span>,
}

impl<'a> DesugarCtx<'a> {
  fn new(
    db: &'a dyn crate::Db,
    root: GreenNode,
    names: &'a BTreeMap<Handle<Name>, InScopeName>,
    terms: Arena<Term<VarId>>,
    vars: &'a mut IdGen<VarId, bool>,
    ty_vars: &'a mut IdGen<TyVarId, bool>,
  ) -> Self {
    Self {
      db,
      root,
      names,
      terms,
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

  fn ds_term_postfix(&mut self, post: TermPostfix) -> Result<Idx<Term<VarId>>, PatternMatchError> {
    let syn = post.syntax();
    let span = fix_up(syn.text_range().into());
    match post {
      TermPostfix::Arg(arg_postfix) => {
        let func = self.ds_term_postfix(arg_postfix.base().expect("Failure"))?;
        let arg = self.ds_term(arg_postfix.arg().expect("Failure"))?;
        Ok(self.mk_term(span, ast::Term::Application { func, arg }))
      }
      TermPostfix::Field(field_postfix) => {
        let term = self.ds_term_postfix(field_postfix.base().expect("Failure"))?;
        let name = field_postfix.name().expect("Failure");
        let label = Ident::new(self.db, name.text());
        let term = self.mk_term(
          span,
          ast::Term::Project {
            direction: Direction::Right,
            term,
          },
        );
        Ok(self.mk_term(span, ast::Term::Unlabel { label, term }))
      }
      TermPostfix::Atom(term_atom) => match term_atom {
        TermAtom::Var(var_expr) => {
          let name = var_expr.name().expect("Failure");
          let mut syn = name.syntax().clone();
          while let Some(parens) = syn.parent() {
            syn = parens;
          }
          let root = syn.green().into();
          let term = match self.names[&Handle::new(root, name)] {
            InScopeName::TermVar(_, id) => ast::Term::Variable(id),
            InScopeName::Term(name) => ast::Term::Item(name),
            InScopeName::EffectOp(name) => ast::Term::Operation(name),
            name => unreachable!("{:?}", name),
          };
          Ok(self.mk_term(span, term))
        }
        TermAtom::Int(int) => Ok(self.mk_term(span, ast::Term::Int(int.int().expect("Failure")))),
        TermAtom::Paren(parenthesized_expr) => {
          self.ds_term(parenthesized_expr.term().expect("Failure"))
        }
        TermAtom::Prod(prod_expr) => {
          let mut fields = prod_expr.fields();
          Ok(match fields.next() {
            None => self.mk_term(span, ast::Term::Unit),
            Some(field) => {
              let label = Ident::new(self.db, field.name().expect("Failure").text());
              let term = self.ds_term(field.term().expect("Failure"))?;

              let mut span = fix_up(field.syntax().text_range().into());
              let mut row = self.mk_term(span, ast::Term::Label { label, term });

              for field in fields {
                let label = Ident::new(self.db, field.name().expect("Failure").text());
                let term = self.ds_term(field.term().expect("Failure"))?;

                let field_span = fix_up(field.syntax().text_range().into());
                let right = self.mk_term(field_span, ast::Term::Label { label, term });

                span = span.join_spans(&field_span);
                row = self.mk_term(span, ast::Term::Concat { left: row, right });
              }
              row
            }
          })
        }
        TermAtom::Sum(sum) => {
          let label = Ident::new(self.db, sum.name().expect("Failure").text());
          let term = self.ds_term(sum.term().expect("Failure"))?;
          let term = self.mk_term(span, ast::Term::Label { label, term });
          Ok(self.mk_term(
            span,
            ast::Term::Inject {
              direction: Direction::Right,
              term,
            },
          ))
        }
        TermAtom::Match(match_expr) => {
          let matrix = match_expr
            .arms()
            .map(|arm| {
              Ok((
                vec![arm.pattern().expect("Failure")],
                self.ds_term(arm.term().expect("Failure"))?,
              ))
            })
            .collect::<Result<_, _>>()?;
          let term = self.desugar_pattern_matrix(&mut [], matrix)?;
          dbg!(&self.terms[term]);
          Ok(term)
        }
      },
    }
  }

  /// Desugar a NST Term into it's corresponding AST Term.
  fn ds_term(&mut self, nst: cst::Term) -> Result<Idx<Term<VarId>>, PatternMatchError> {
    let mut ops = nst.body().into_iter().flat_map(|ops| ops.ops());
    let cst_term = ops
      .next()
      .expect("We should've emitted an error and not made it to desugar");
    let mut span = fix_up(cst_term.syntax().text_range().into());
    let mut term = self.ds_term_postfix(cst_term)?;
    for op in ops {
      span = span.join_spans(&fix_up(op.syntax().text_range().into()));
      let right = self.ds_term_postfix(op)?;
      term = self.mk_term(span, ast::Term::Concat { left: term, right });
    }

    let prefixes = nst.prefixes().into_iter().flatten().collect::<Vec<_>>();
    for prefix in prefixes.into_iter().rev() {
      match prefix {
        TermPrefix::With(with_prefix) => {
          let handler = self.ds_term(with_prefix.term().expect("Failure"))?;
          span = fix_up(with_prefix.syntax().text_range().into()).join_spans(&span);
          term = self.mk_term(
            span,
            ast::Term::Handle {
              handler,
              body: term,
            },
          );
        }
        TermPrefix::Closure(closure_prefix) => {
          let name = closure_prefix.name().expect("Failure");
          let handle = Handle::new(self.root.clone(), name);
          span = fix_up(closure_prefix.syntax().text_range().into()).join_spans(&span);
          term = self.mk_term(
            span,
            ast::Term::Abstraction {
              arg: match self.names.get(&handle) {
                Some(InScopeName::TermVar(_, id)) => *id,
                _ => unreachable!(),
              },
              body: term,
            },
          );
        }
        TermPrefix::Let(let_prefix) => {
          let name = let_prefix.name().expect("Failure");
          let handle = Handle::new(self.root.clone(), name);

          let defn = self.ds_term(let_prefix.defn().expect("Failure"))?;
          span = fix_up(let_prefix.syntax().text_range().into()).join_spans(&span);
          let func = self.mk_term(
            span,
            ast::Term::Abstraction {
              arg: match self.names.get(&handle) {
                Some(InScopeName::TermVar(_, id)) => *id,
                _ => unreachable!(),
              },
              body: term,
            },
          );
          term = self.mk_term(span, ast::Term::Application { func, arg: defn });
        }
      }
    }

    let span = fix_up(nst.syntax().text_range().into());
    self.spans.insert(term, span);
    Ok(term)
  }

  fn ds_row(&mut self, row: cst::Row) -> Row<Simple> {
    match row {
      cst::Row::Concrete(closed) => Row::Closed(
        self.db.construct_simple_row(
          closed
            .fields()
            .map(|field| {
              let label = Ident::new(self.db, field.name().expect("Failure").text());
              let value = self.ds_type(field.ty().expect("Failure"));
              (label, value)
            })
            .collect(),
        ),
      ),
      cst::Row::Variable(vars) => {
        let name = vars
          .variables()
          .next()
          .expect("TODO: Handle multiple variables");
        let handle = Handle::new(self.root.clone(), name);
        Row::Open(match self.names.get(&handle) {
          Some(InScopeName::EffectTyVar(_, id)) => *id,
          Some(InScopeName::TermTyVar(_, id)) => *id,
          // Type is not valid here because this is a row.
          _ => unreachable!(),
        })
      }
      cst::Row::Mixed { .. } => unimplemented!(),
    }
  }

  fn ds_type_with_eff(&mut self, nst: cst::Type, eff: ScopedRow<InDb>) -> Ty {
    match nst {
      Type::Name(name_type) => {
        let handle = Handle::new(self.root.clone(), name_type.name().expect("Failure"));
        self
          .db
          .mk_ty(TypeKind::VarTy(match self.names.get(&handle) {
            Some(InScopeName::TermTyVar(_, id)) => *id,
            Some(InScopeName::EffectTyVar(_, id)) => *id,
            Some(InScopeName::Type(type_name))
              if type_name.name(self.db) == Ident::new(self.db, "Int") =>
            {
              return self.db.mk_ty(TypeKind::IntTy);
            }
            _ => unreachable!(),
          }))
      }
      Type::Sum(sum_type) => self.db.mk_ty(TypeKind::SumTy(
        sum_type
          .row()
          .map(|row| self.ds_row(row))
          .unwrap_or(Row::Closed(self.db.empty_row())),
      )),
      Type::Product(product_type) => self.db.mk_ty(TypeKind::ProdTy(
        product_type
          .row()
          .map(|row| self.ds_row(row))
          .unwrap_or(Row::Closed(self.db.empty_row())),
      )),
      Type::Function(function_type) => {
        let tys = function_type
          .tys()
          .map(|ty| self.ds_type_with_eff(ty, eff))
          .collect::<Vec<_>>();
        tys
          .into_iter()
          .rev()
          .reduce(|r, l| self.db.mk_ty(TypeKind::FunTy(l, eff, r)))
          .expect("Unexpected empty function type")
      }
      Type::Paren(paren_type) => self.ds_type_with_eff(paren_type.ty().expect("Failure"), eff),
    }
  }

  fn ds_type(&mut self, ty: cst::Type) -> Ty {
    match ty {
      cst::Type::Name(name_type) => {
        let name = name_type.name().expect("Failure");
        let handle = Handle::new(self.root.clone(), name);
        let id = match self.names.get(&handle) {
          Some(InScopeName::EffectTyVar(_, id)) => id,
          Some(InScopeName::TermTyVar(_, id)) => id,
          Some(InScopeName::Type(ty)) if ty.name(self.db) == Ident::new(self.db, "Int") => {
            return self.db.mk_ty(TypeKind::IntTy);
          }
          _ => unreachable!(),
        };
        self.db.mk_ty(TypeKind::VarTy(*id))
      }
      cst::Type::Sum(sum_type) => {
        let row = self.ds_row(sum_type.row().expect("Failure"));
        self.db.mk_ty(TypeKind::SumTy(row))
      }
      cst::Type::Product(product_type) => {
        let row = self.ds_row(product_type.row().expect("Failure"));
        self.db.mk_ty(TypeKind::ProdTy(row))
      }
      cst::Type::Function(function_type) => {
        let tys = function_type
          .tys()
          .map(|ty| self.ds_type(ty))
          .collect::<Vec<_>>();
        let eff = self.ty_vars.push(true);
        tys
          .into_iter()
          .rev()
          .reduce(|r, l| self.db.mk_ty(TypeKind::FunTy(l, Row::Open(eff), r)))
          .expect("Unexpected empty function type")
      }
      cst::Type::Paren(paren_type) => self.ds_type(paren_type.ty().expect("Failure")),
    }
  }

  fn ds_row_atom(&mut self, row_atom: cst::RowAtom) -> Row<Simple> {
    match row_atom {
      RowAtom::Concrete(concrete) => Row::Closed(
        self.db.construct_simple_row(
          concrete
            .fields()
            .map(|field| {
              let name = field.name().expect("Failure");
              let ty = field.ty().expect("Failure");
              (Ident::new(self.db, name.text()), self.ds_type(ty))
            })
            .collect(),
        ),
      ),
      RowAtom::Variable(variable) => {
        let name = variable.name().expect("Failure");
        let handle = Handle::new(self.root.clone(), name);
        Row::Open(match self.names.get(&handle) {
          Some(InScopeName::TermTyVar(_, id)) => *id,
          Some(InScopeName::EffectTyVar(_, id)) => *id,
          _ => unreachable!(),
        })
      }
    }
  }

  fn ds_scheme_with_eff(&mut self, scheme: cst::TypeScheme, eff: ScopedRow<InDb>) -> TyScheme {
    let bound = scheme
      .quantifiers()
      .into_iter()
      .flatten()
      .map(|quant| {
        let name = quant.name().expect("Failure");
        let handle = Handle::new(self.root.clone(), name);
        match self.names.get(&handle) {
          Some(InScopeName::EffectTyVar(_, id)) => *id,
          Some(InScopeName::TermTyVar(_, id)) => *id,
          _ => unreachable!(),
        }
      })
      .collect::<FxHashSet<_>>();
    let mut data_row_bound = FxHashSet::default();
    let constrs = scheme
      .constraints()
      .into_iter()
      .flatten()
      .map(|constr| {
        let left = self.ds_row_atom(constr.left().expect("Failure"));
        if let Row::Open(row_var) = &left {
          data_row_bound.insert(*row_var);
        }

        let right = self.ds_row_atom(constr.right().expect("Failure"));
        if let Row::Open(row_var) = &right {
          data_row_bound.insert(*row_var);
        }

        let goal = self.ds_row_atom(constr.goal().expect("Failure"));
        if let Row::Open(row_var) = &goal {
          data_row_bound.insert(*row_var);
        }
        Evidence::DataRow { left, right, goal }
      })
      .collect::<Vec<_>>();

    let ty = self.ds_type_with_eff(scheme.ty().expect("Failure"), eff);
    data_row_bound.extend(ty.row_vars(self.db));

    TyScheme {
      bound_ty: bound.difference(&data_row_bound).copied().collect(),
      bound_data_row: data_row_bound.into_iter().collect(),
      // We currently don't have syntax for effects so we just bind the generated effect
      bound_eff_row: match eff {
        Row::Open(var) => vec![var],
        _ => vec![],
      },
      constrs,
      eff,
      ty,
    }
  }

  fn ds_scheme(&mut self, scheme: cst::TypeScheme) -> TyScheme {
    let bound = scheme
      .quantifiers()
      .into_iter()
      .flatten()
      .map(|quant| {
        let name = quant.name().expect("Failure");
        let handle = Handle::new(self.root.clone(), name);
        match self.names.get(&handle) {
          Some(InScopeName::EffectTyVar(_, id)) => *id,
          Some(InScopeName::TermTyVar(_, id)) => *id,
          _ => unreachable!(),
        }
      })
      .collect::<FxHashSet<_>>();
    let mut data_row_bound = FxHashSet::default();
    let constrs = scheme
      .constraints()
      .into_iter()
      .flatten()
      .map(|constr| {
        let left = self.ds_row_atom(constr.left().expect("Failure"));
        if let Row::Open(row_var) = &left {
          data_row_bound.insert(*row_var);
        }

        let right = self.ds_row_atom(constr.right().expect("Failure"));
        if let Row::Open(row_var) = &right {
          data_row_bound.insert(*row_var);
        }

        let goal = self.ds_row_atom(constr.goal().expect("Failure"));
        if let Row::Open(row_var) = &goal {
          data_row_bound.insert(*row_var);
        }
        Evidence::DataRow { left, right, goal }
      })
      .collect::<Vec<_>>();

    /*cst::Constraint::RowSum { lhs, rhs, goal, .. } => {
      let left = self.ds_row_atom(lhs);
      if let Row::Open(row_var) = &left {
        data_row_bound.insert(*row_var);
      }
      let right = self.ds_row_atom(rhs);
      if let Row::Open(row_var) = &right {
        data_row_bound.insert(*row_var);
      }
      let goal = self.ds_row_atom(goal);
      if let Row::Open(row_var) = &goal {
        data_row_bound.insert(*row_var);
      }
      Evidence::DataRow { left, right, goal }
    }*/

    let ty = self.ds_type(scheme.ty().expect("Failure"));
    data_row_bound.extend(ty.row_vars(self.db));

    let eff = self.ty_vars.push(true);

    TyScheme {
      bound_ty: bound.difference(&data_row_bound).copied().collect(),
      bound_data_row: data_row_bound.into_iter().collect(),
      // We currently don't have syntax for effects so we just bind the generated effect
      bound_eff_row: vec![eff],
      constrs,
      eff: Row::Open(eff),
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
      .all(|pat| matches!(pat, Pattern::Whole(_)))
    {
      Ok(
        matrix
          .first()
          .iter()
          .rfold(matrix.arms[0], |body, pat| match pat {
            Pattern::Whole(var) => self.mk_term(
              fix_up(var.syntax().text_range().into()),
              Abstraction {
                arg: match self.names.get(&Handle::new(
                  self.root.clone(),
                  var.name().expect("Failure"),
                )) {
                  Some(InScopeName::TermVar(_, var)) => *var,
                  _ => unreachable!(),
                },
                body,
              },
            ),
            _ => unreachable!(),
          }),
      )
    } else {
      let constrs = matrix
        .col_constr(0)
        .collect::<std::collections::BTreeMap<_, _>>();
      let mut matches = constrs.into_iter().map(|(c, p)| match c {
        Constructor::ProductRow(ref lbls) => {
          let span = fix_up(p.syntax().text_range().into());
          let top_level = self.vars.push(true);
          let binders = (0..lbls.len())
            .map(|_| self.vars.push(true))
            .collect::<Vec<_>>();
          let mut occs = binders;
          // replace first occurence by binder introduced here
          occs.extend(occurences.iter_mut().skip(1).map(|var| *var));
          let init = self.desugar_pattern_matrix(occs.as_mut_slice(), matrix.specialize(&c))?;
          let body = lbls.iter().cloned().fold(init, |body, label| {
            let term = self.mk_term(span, Variable(top_level));
            let term = self.mk_term(
              span,
              Project {
                direction: Direction::Right,
                term,
              },
            );
            let destructure = self.mk_term(
              span,
              Unlabel {
                label: Ident::new(self.db, label),
                term,
              },
            );
            self.mk_term(
              span,
              Application {
                func: body,
                arg: destructure,
              },
            )
          });
          Ok(self.mk_term(
            span,
            Abstraction {
              arg: top_level,
              body,
            },
          ))
        }
        Constructor::SumRow(ref label) => {
          let span = fix_up(p.syntax().text_range().into());
          let binder = self.vars.push(true);
          let mut occs = vec![binder];
          occs.extend(occurences.iter_mut().skip(1).map(|var| *var));
          let func = self.desugar_pattern_matrix(&mut occs, matrix.specialize(&c))?;
          let term = self.mk_term(span, Variable(binder));
          let arg = self.mk_term(
            span,
            Unlabel {
              label: Ident::new(self.db, label),
              term,
            },
          );
          let body = self.mk_term(span, Application { func, arg });
          Ok(self.mk_term(span, Abstraction { arg: binder, body }))
        }
        Constructor::WildCard => {
          if let Pattern::Whole(var) = p {
            // For a wild card we don't need a let binding so just pass through binder as is
            let body = self.desugar_pattern_matrix(occurences, matrix.default())?;
            let handle = Handle::new(self.root.clone(), var.name().expect("Failure"));
            Ok(self.mk_term(
              fix_up(var.syntax().text_range().into()),
              Abstraction {
                arg: match self.names.get(&handle) {
                  Some(InScopeName::TermVar(_, id)) => *id,
                  _ => unreachable!(),
                },
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
  pats: Vec<Vec<Pattern>>,
  arms: Vec<Idx<Term<VarId>>>,
}

impl FromIterator<(Vec<Pattern>, Idx<Term<VarId>>)> for ClauseMatrix {
  fn from_iter<T: IntoIterator<Item = (Vec<Pattern>, Idx<Term<VarId>>)>>(iter: T) -> Self {
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

  fn first(&self) -> &[Pattern] {
    debug_assert!(self.pats.is_empty().not());
    self.pats[0].as_slice()
  }

  fn col_constr(&self, col_index: usize) -> impl Iterator<Item = (Constructor, &Pattern)> + '_ {
    self
      .pats
      .iter()
      .map(move |col| (Constructor::from(&col[col_index]), &col[col_index]))
  }

  pub(crate) fn specialize(&self, constr: &Constructor) -> ClauseMatrix {
    self
      .pats
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

  fn default(&self) -> ClauseMatrix {
    self
      .pats
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
  ProductRow(Vec<String>),
  SumRow(String),
  WildCard,
}
impl Constructor {
  fn matches(&self, pat: &Pattern) -> Option<Vec<Pattern>> {
    match (self, pat) {
      (Constructor::ProductRow(lbls), Pattern::Prod(rows)) => rows
        .fields()
        .zip(lbls)
        .map(|(row, lbl)| {
          let text = row.name().map(|name| name.text());
          if text.as_ref().eq(&Some(lbl)) {
            Some(row.pattern().unwrap())
          } else {
            None
          }
        })
        .collect::<Option<Vec<_>>>(),
      (Constructor::SumRow(lbl), Pattern::Sum(row))
        if row
          .field()
          .and_then(|field| field.name())
          .unwrap()
          .text()
          .eq(lbl) =>
      {
        Some(vec![row.field().and_then(|field| field.pattern()).unwrap()])
      }
      (Constructor::WildCard, Pattern::Whole(_)) => Some(vec![]),
      // A wild card always matches and produces sub wild card patterns for each pattern our
      // match would have
      (Constructor::WildCard, Pattern::Sum(row)) => Some(vec![
        row.field().and_then(|row| row.pattern()).expect("Failure"),
      ]),
      (Constructor::WildCard, Pattern::Prod(rows)) => Some(
        rows
          .fields()
          .map(|field| field.pattern().expect("Failure"))
          .collect(),
      ),
      _ => None,
    }
  }
}

impl From<&Pattern> for Constructor {
  fn from(pat: &Pattern) -> Self {
    match pat {
      Pattern::Prod(rows) => Constructor::ProductRow(
        rows
          .fields()
          .flat_map(|field| field.name().map(|name| name.text()).into_iter())
          .collect(),
      ),
      Pattern::Sum(row) => Constructor::SumRow(
        row
          .field()
          .and_then(|field| field.name())
          .expect("Failure")
          .text(),
      ),
      Pattern::Whole(_) => Constructor::WildCard,
    }
  }
}

#[cfg(test)]
mod tests {
  use std::path::PathBuf;

  use super::*;
  use base::file::{FileId, SourceFile, SourceFileSet};
  use expect_test::expect;
  use nameres::nameres_term_of;
  use parser::root_module_for_file;

  #[derive(Default, Clone)]
  #[salsa::db]
  struct TestDatabase {
    storage: salsa::Storage<Self>,
  }
  #[salsa::db]
  impl salsa::Database for TestDatabase {}

  const WIDTH: usize = 100;

  fn ds_snippet<'db>(
    db: &'db dyn salsa::Database,
    input: &str,
  ) -> (nameres::NameResTerm<'db>, ast::AstTerm<'db>) {
    let mut content = "defn item = ".to_string();
    content.push_str(input);
    let file = SourceFile::new(db, FileId::new(db, PathBuf::from("test")), content);
    SourceFileSet::new(db, vec![file]);
    let module = root_module_for_file(db, file);
    let name = Ident::new(db, "item");
    let name = TermName::new(db, name, module);
    (nameres_term_of(db, name), desugar_term_of(db, name))
  }

  #[test]
  fn test_desugar_int() {
    let db = TestDatabase::default();

    let (nst_item, ast_item) = ds_snippet(&db, "12354");
    let ast = ast_item.data(&db);

    assert_eq!(ast.span_of(ast.tree), Some(&nst_item.span_of(&db)));

    let pretty_ast = ast
      .pretty(&db, &pretty::BoxAllocator)
      .pretty(WIDTH)
      .to_string();
    let expect = expect!["12354"];
    expect.assert_eq(&pretty_ast);
  }

  #[test]
  fn test_desugar_abs() {
    let db = TestDatabase::default();

    let (nst_item, ast_item) = ds_snippet(&db, "|x| x");
    let ast = ast_item.data(&db);

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
    let ast = ast_item.data(&db);

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

    let (nst_item, ast_item) = ds_snippet(&db, "|a||b| let x = a; x(b)");
    let ast = ast_item.data(&db);

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
    let ast = ast_item.data(&db);

    assert_eq!(ast.span_of(ast.tree), Some(&nst_item.span_of(&db)));
    assert_eq!(ast.view(ast.tree), &Unit);
  }

  #[test]
  fn test_desugar_product() {
    let db = TestDatabase::default();

    let (nst_item, ast_item) = ds_snippet(&db, "|x||y||z| { abc = x, def = y, ghi = z }");
    let ast = ast_item.data(&db);

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
    let ast = ast_item.data(&db);

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
    let ast = ast_item.data(&db);

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
    let ast = ast_item.data(&db);

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
    let ast = ast_item.data(&db);

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
    let ast = ast_item.data(&db);

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
    let ast = ast_item.data(&db);

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
