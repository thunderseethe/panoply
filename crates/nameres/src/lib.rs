use std::{collections::BTreeMap, hash::Hash, ops::Range};

use base::{
  file::FileId,
  id::{EffectName, EffectOpName, IdGen, TermName, TyVarId, VarId},
  ident::Ident,
  loc::Loc,
  modules::Module,
  span::Span,
};
use base::{
  file::{SourceFile, file_for_id},
  id::{Ids, TypeName},
};
use cst::{
  EffectDefn, HasAstPtr, HasName, Item, Items, Name, Panoply, Pattern, Row, RowAtom, Term,
  TermAtom, TermDefn, TermPostfix, TermPrefix, Type, TypeScheme,
};
use parser::{ParseFile, all_modules, locate, parse_module, parse_module_of};
use rowan::{
  GreenNode, SyntaxNode, TextRange,
  ast::{AstNode, AstPtr, SyntaxNodePtr},
};
use rustc_hash::FxHashMap;
use salsa::{Accumulator, Update};

pub fn nameres_term_of<'db>(db: &'db dyn salsa::Database, term_name: TermName) -> NameResTerm<'db> {
  term_defn(db, term_name)
}

pub fn name_at_position<'db>(
  db: &'db dyn salsa::Database,
  file: FileId,
  line: u32,
  col: u32,
) -> Option<InScopeName> {
  name_at_loc(db, file, line, col)
}

pub fn all_nameres_errors<'db>(db: &'db dyn salsa::Database) -> Vec<NameResDiag> {
  all_modules(db)
    .iter()
    .flat_map(|module| {
      nameres_module_of::accumulated::<NameResDiag>(db, *module)
        .into_iter()
        .cloned()
    })
    .collect()
}

pub fn nameres_module_for_file_id<'db>(
  db: &'db dyn salsa::Database,
  file_id: FileId,
) -> ModuleNamespace<'db> {
  let file = ::base::file::file_for_id(db, file_id);
  nameres_module_for_file(db, file)
}

pub fn nameres_module_for_file<'db>(
  db: &'db dyn salsa::Database,
  file: SourceFile,
) -> ModuleNamespace<'db> {
  let parse_file = parse_module(db, file);
  nameres_module(db, parse_file)
}

#[salsa::tracked]
fn effect_defn<'db>(db: &'db dyn salsa::Database, eff_name: EffectName) -> NameResEffect<'db> {
  let nameres_module = nameres_module_of(db, eff_name.module(db));
  *nameres_module
    .effects(db)
    .values()
    .find(|eff| eff.name(db) == eff_name)
    .unwrap_or_else(|| {
      panic!(
        "ICE: Constructed EffectName {:?} with no effect definition",
        eff_name.name(db).text(db)
      )
    })
}

#[salsa::tracked]
fn term_defn<'db>(db: &'db dyn salsa::Database, term_name: TermName) -> NameResTerm<'db> {
  let nameres_module = nameres_module_of(db, term_name.module(db));
  *nameres_module
    .terms(db)
    .values()
    .find(|term| term.name(db) == term_name)
    .unwrap_or_else(|| {
      panic!(
        "ICE: Constructed TermName {:?} with no term definition",
        term_name.name(db).text(db)
      )
    })
}

#[salsa::tracked]
pub struct NameResTerm<'db> {
  pub name: TermName,
  #[returns(ref)]
  pub data: Handle<TermDefn>,
  #[returns(ref)]
  pub vars: Box<Ids<VarId, Handle<Name>>>,
  #[returns(ref)]
  pub ty_vars: Box<Ids<TyVarId, Handle<Name>>>,
}

impl<'db> NameResTerm<'db> {
  pub fn span_of(&self, db: &dyn salsa::Database) -> Span {
    let ras: TermDefn = self.data(db).clone().into_node();
    let range = ras.term().expect("Failure").syntax().text_range();
    Span {
      start: Loc {
        byte: range.start().into(),
      },
      end: Loc {
        byte: range.end().into(),
      },
    }
  }
}

#[salsa::tracked]
pub struct NameResEffect<'db> {
  pub name: EffectName,
  #[returns(ref)]
  pub data: Handle<EffectDefn>,
  #[returns(ref)]
  pub vars: Box<Ids<VarId, Handle<Name>>>,
  #[returns(ref)]
  pub ty_vars: Box<Ids<TyVarId, Handle<Name>>>,
}

#[derive(Clone, Debug)]
pub struct OpaqueHandle {
  root: GreenNode,
  ptr: SyntaxNodePtr<Panoply>,
}

impl OpaqueHandle {
  fn new(root: GreenNode, node: impl AstNode<Language = Panoply>) -> Self {
    Self {
      root,
      ptr: SyntaxNodePtr::new(node.syntax()),
    }
  }

  fn syntax(self) -> SyntaxNode<Panoply> {
    self
      .ptr
      .to_node(&SyntaxNode::<Panoply>::new_root(self.root))
  }
}

#[derive(Debug, Update)]
pub struct Handle<N: AstNode + 'static> {
  pub root: GreenNode,
  ptr: AstPtr<N>,
}
impl<N: AstNode + PartialOrd> PartialOrd for Handle<N> {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    if self.root == other.root {
      let self_ptr = self.ptr.to_node(&SyntaxNode::new_root(self.root.clone()));
      let other_ptr = other.ptr.to_node(&SyntaxNode::new_root(other.root.clone()));
      self_ptr.partial_cmp(&other_ptr)
    } else {
      None
    }
  }
}
impl<N: AstNode + PartialOrd> Ord for Handle<N> {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self
      .partial_cmp(other)
      .unwrap_or_else(|| panic!("Compared two Handles from different trees"))
  }
}

impl<N: AstNode<Language = Panoply>> Handle<N> {
  fn opaque(self) -> OpaqueHandle {
    OpaqueHandle {
      root: self.root,
      ptr: self.ptr.syntax_node_ptr(),
    }
  }

  pub fn into_node(self) -> N {
    self.ptr.to_node(&SyntaxNode::new_root(self.root))
  }

  pub fn text_range(&self) -> TextRange {
    self.ptr.syntax_node_ptr().text_range()
  }
}
impl<N: AstNode> PartialEq for Handle<N> {
  fn eq(&self, other: &Self) -> bool {
    self.root == other.root && self.ptr == other.ptr
  }
}
impl<N: AstNode> Eq for Handle<N> {}
impl<N: AstNode> Hash for Handle<N> {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.root.hash(state);
    self.ptr.hash(state);
  }
}
impl<N: AstNode> Clone for Handle<N> {
  fn clone(&self) -> Self {
    Self {
      root: self.root.clone(),
      ptr: self.ptr.clone(),
    }
  }
}

impl<N: HasAstPtr> Handle<N> {
  pub fn new(root: GreenNode, node: N) -> Self {
    Self {
      root,
      ptr: node.ast_ptr(),
    }
  }
}

#[salsa::tracked]
pub struct ModuleNamespace<'db> {
  #[returns(ref)]
  pub terms: BTreeMap<Handle<TermDefn>, NameResTerm<'db>>,
  #[returns(ref)]
  pub effects: BTreeMap<Handle<EffectDefn>, NameResEffect<'db>>,
  #[returns(ref)]
  pub names: BTreeMap<Handle<Name>, InScopeName>,
}

/// A kind of name.
#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum NameKind {
  Module = 0b1,
  Effect = 0b10,
  EffectOp = 0b100,
  Item = 0b1000,
  TyVar = 0b10000,
  Var = 0b100000,
  Type = 0b1000000,
}

impl NameKind {
  /// The English name for a name kind, with an indefinite article (i.e., "a" or "an").
  pub fn indefinite_noun(&self) -> &'static str {
    match self {
      NameKind::Module => "a module",
      NameKind::Effect => "an effect",
      NameKind::EffectOp => "an effect operation",
      NameKind::Item => "a top-level item",
      NameKind::TyVar => "a type variable",
      NameKind::Var => "a local variable",
      NameKind::Type => "a top-level type",
    }
  }
}

#[derive(Debug, Clone)]
pub enum NameResError {
  UndefinedName(Handle<Name>),
  WrongKind(NameKind, Vec<NameKind>, OpaqueHandle),
  DuplicateName {
    previous: OpaqueHandle,
    current: OpaqueHandle,
    kind: NameKind,
  },
}

#[salsa::accumulator]
#[derive(Debug, Clone)]
pub struct NameResDiag {
  error: NameResError,
}

#[salsa::tracked]
pub fn nameres_module<'db>(
  db: &'db dyn salsa::Database,
  parse_module: ParseFile<'db>,
) -> ModuleNamespace<'db> {
  let cst_module = parse_module.data(db);
  let module = parse_module.module(db);
  let items = Items::new(cst_module.clone());

  let mut terms = BTreeMap::default();
  let mut effects = BTreeMap::default();
  let mut errors = vec![];
  let mut top_level_scope: FxHashMap<Ident, (InScopeName, OpaqueHandle)> = FxHashMap::default();
  let mut names = BTreeMap::default();

  let mut to_resolve_terms = vec![];
  let mut to_resolve_effects = vec![];
  // First pass over items collects all top level names and marks any duplicates.
  for item in items.items() {
    match item {
      Item::Term(term) => {
        let Some(name) = term.name() else {
          continue;
        };
        let ident = Ident::new(db, name.text());
        let handle = Handle::new(cst_module.clone(), name);
        let name = TermName::new(db, ident, module);
        names.insert(handle, InScopeName::Term(name));
        let handle = Handle::new(cst_module.clone(), term.clone());

        if let Some((_, previous)) = top_level_scope.get(&ident) {
          errors.push(NameResError::DuplicateName {
            previous: previous.clone(),
            current: OpaqueHandle::new(cst_module.clone(), term),
            kind: NameKind::Item,
          });
          continue;
        }
        top_level_scope.insert(ident, (InScopeName::Term(name), handle.opaque()));
        to_resolve_terms.push((name, term));
      }
      Item::Effect(effect) => {
        let Some(name) = effect.name() else {
          continue;
        };
        let ident = Ident::new(db, name.text());
        let handle = Handle::new(cst_module.clone(), name);
        let name = EffectName::new(db, ident, module);
        names.insert(handle, InScopeName::Effect(name));
        let handle = Handle::new(cst_module.clone(), effect.clone());

        if let Some((_, previous)) = top_level_scope.get(&ident) {
          errors.push(NameResError::DuplicateName {
            previous: previous.clone(),
            current: OpaqueHandle::new(cst_module.clone(), effect),
            kind: NameKind::Effect,
          });
          continue;
        }
        top_level_scope.insert(ident, (InScopeName::Effect(name), handle.opaque()));
        to_resolve_effects.push((name, effect));
      }
    }
  }

  let int_ident = Ident::new(db, "Int");
  let int_type = TypeName::new(db, int_ident, module);

  top_level_scope.insert(
    int_ident,
    (
      InScopeName::Type(int_type),
      OpaqueHandle::new(cst_module.clone(), Items::new(cst_module.clone())),
    ),
  );

  // Second pass over terms and effects to resolve the names within each body
  for (effect_name, effect) in to_resolve_effects {
    let mut ctx = NameResolution {
      db,
      root: cst_module.clone(),
      ty_var_gen: IdGen::default(),
      var_gen: IdGen::default(),
      scope: top_level_scope.clone(),
      errors: &mut errors,
      names: &mut names,
    };

    for op in effect.ops().into_iter().flatten() {
      let Some(name) = op.name() else {
        continue;
      };
      let ident = Ident::new(db, name.text());
      let op_name = EffectOpName::new(db, ident, effect_name);
      let handle = Handle::new(cst_module.clone(), name.clone());
      ctx
        .names
        .insert(handle.clone(), InScopeName::EffectOp(op_name));

      if let Some((_, previous)) = top_level_scope.get(&ident) {
        ctx.errors.push(NameResError::DuplicateName {
          previous: previous.clone(),
          current: OpaqueHandle::new(cst_module.clone(), op),
          kind: NameKind::EffectOp,
        });
        continue;
      }
      top_level_scope.insert(ident, (InScopeName::EffectOp(op_name), handle.opaque()));

      if let Some(scheme) = op.annotation().and_then(|ann| ann.scheme()) {
        ctx.resolve_scheme(scheme, SchemeContext::Effect(op_name));
      }
    }

    let handle = Handle::new(cst_module.clone(), effect);
    effects.insert(
      handle.clone(),
      NameResEffect::new(
        db,
        effect_name,
        handle,
        ctx.var_gen.into_boxed_ids(),
        ctx.ty_var_gen.into_boxed_ids(),
      ),
    );
  }

  for (name, term) in to_resolve_terms {
    let mut ctx = NameResolution {
      db,
      root: cst_module.clone(),
      ty_var_gen: IdGen::default(),
      var_gen: IdGen::default(),
      scope: top_level_scope.clone(),
      errors: &mut errors,
      names: &mut names,
    };
    ctx.with_scope(|this| {
      if let Some(scheme) = term.annotation().and_then(|ann| ann.scheme()) {
        this.resolve_scheme(scheme, SchemeContext::Term(name));
      }
      if let Some(body) = term.term() {
        this.resolve_term(body, name);
      }
    });

    let handle = Handle::new(cst_module.clone(), term);
    terms.insert(
      handle.clone(),
      NameResTerm::new(
        db,
        name,
        handle,
        ctx.var_gen.into_boxed_ids(),
        ctx.ty_var_gen.into_boxed_ids(),
      ),
    );
  }

  let names = ModuleNamespace::new(db, terms, effects, names);

  for error in errors {
    NameResDiag { error }.accumulate(db)
  }

  names
}

#[derive(Clone, Copy)]
enum SchemeContext {
  Term(TermName),
  Effect(EffectOpName),
}

struct NameResolution<'a, 'b> {
  db: &'a dyn salsa::Database,
  root: GreenNode,
  ty_var_gen: IdGen<TyVarId, Handle<Name>>,
  var_gen: IdGen<VarId, Handle<Name>>,
  scope: FxHashMap<Ident, (InScopeName, OpaqueHandle)>,
  names: &'b mut BTreeMap<Handle<Name>, InScopeName>,
  errors: &'b mut Vec<NameResError>,
}

impl<'db> NameResolution<'db, '_> {
  fn with_scope<T>(&mut self, body: impl FnOnce(&mut Self) -> T) -> T {
    let scope = self.scope.clone();
    let res = body(self);
    self.scope = scope;
    res
  }

  fn bind_ty_var(&mut self, name: Name, ctx: SchemeContext) {
    let ident = Ident::new(self.db, name.text());
    let handle = Handle::new(self.root.clone(), name);
    let ty_var_id = self.ty_var_gen.push(handle.clone());
    let in_scope_name = match ctx {
      SchemeContext::Term(term_name) => InScopeName::TermTyVar(term_name, ty_var_id),
      SchemeContext::Effect(effect_op_name) => InScopeName::EffectTyVar(effect_op_name, ty_var_id),
    };
    self.names.insert(handle.clone(), in_scope_name);
    self.scope.insert(ident, (in_scope_name, handle.opaque()));
  }

  fn bind_var(&mut self, name: Name, ctx: TermName) {
    let ident = Ident::new(self.db, name.text());
    let handle = Handle::new(self.root.clone(), name);
    let var_id = self.var_gen.push(handle.clone());
    let name = InScopeName::TermVar(ctx, var_id);
    self.names.insert(handle.clone(), name);
    self.scope.insert(ident, (name, handle.opaque()));
  }

  fn resolve_ty_var(&mut self, name: Name, ctx: SchemeContext) {
    let ident = Ident::new(self.db, name.text());

    let handle = Handle::new(self.root.clone(), name.clone());
    let resolved_name = match self.scope.get(&ident) {
      Some((name @ InScopeName::EffectTyVar(_, _), _))
      | Some((name @ InScopeName::TermTyVar(_, _), _))
      | Some((name @ InScopeName::Type(_), _)) => name,
      Some((other, _)) => {
        return self.errors.push(NameResError::WrongKind(
          other.kind(),
          vec![NameKind::TyVar],
          handle.opaque(),
        ));
      }
      None => return self.bind_ty_var(name, ctx),
    };
    self.names.insert(handle, *resolved_name);
  }

  fn resolve_var(&mut self, name: Name) {
    let ident = Ident::new(self.db, name.text());
    let handle = Handle::new(self.root.clone(), name);
    let resolved_name = match self.scope.get(&ident) {
      Some((name @ InScopeName::TermVar(_, _), _)) => name,
      Some((name @ InScopeName::Term(_), _)) => name,
      Some((name @ InScopeName::Effect(_), _)) => name,
      Some((name @ InScopeName::EffectOp(_), _)) => name,
      Some((other, _)) => {
        self.errors.push(NameResError::WrongKind(
          other.kind(),
          vec![
            NameKind::Var,
            NameKind::Item,
            NameKind::Effect,
            NameKind::EffectOp,
          ],
          handle.opaque(),
        ));
        return;
      }
      None => {
        self.errors.push(NameResError::UndefinedName(handle));
        return;
      }
    };
    self.names.insert(handle, *resolved_name);
  }

  fn resolve_scheme(&mut self, scheme: TypeScheme, ctx: SchemeContext) {
    let Some(binders) = scheme.quantifiers() else {
      // Invalid syntax, assume a parse error occurred and bail.
      return;
    };

    self.with_scope(|this| {
      for name in binders.flat_map(|bind| bind.name().into_iter()) {
        this.bind_ty_var(name, ctx);
      }

      if let Some(constraints) = scheme.constraints() {
        for constraint in constraints {
          if let Some(left) = constraint.left() {
            this.resolve_row_atom(left, ctx);
          }
          if let Some(right) = constraint.right() {
            this.resolve_row_atom(right, ctx);
          }
          if let Some(goal) = constraint.goal() {
            this.resolve_row_atom(goal, ctx);
          }
        }
      }

      if let Some(ty) = scheme.ty() {
        this.resolve_type(ty, ctx);
      }
    })
  }

  fn resolve_row_atom(&mut self, row_atom: RowAtom, ctx: SchemeContext) {
    match row_atom {
      RowAtom::Concrete(concrete) => {
        for field in concrete.fields() {
          field.ty().inspect(|ty| self.resolve_type(ty.clone(), ctx));
        }
      }
      RowAtom::Variable(variable) => {
        variable
          .name()
          .inspect(|name| self.resolve_ty_var(name.clone(), ctx));
      }
    }
  }

  fn resolve_row(&mut self, row: Row, ctx: SchemeContext) {
    match row {
      Row::Variable(variable_row) => {
        for name in variable_row.variables() {
          self.resolve_ty_var(name, ctx);
        }
      }
      Row::Concrete(concrete_row) => {
        for field in concrete_row.fields() {
          if let Some(ty) = field.ty() {
            self.resolve_type(ty, ctx);
          }
        }
      }
      Row::Mixed(mixed_row) => {
        for field in mixed_row
          .concrete()
          .map(|concrete| concrete.fields())
          .into_iter()
          .flatten()
        {
          if let Some(ty) = field.ty() {
            self.resolve_type(ty, ctx);
          }
        }
        for name in mixed_row
          .variable()
          .map(|var| var.variables())
          .into_iter()
          .flatten()
        {
          self.resolve_ty_var(name, ctx);
        }
      }
    }
  }

  fn resolve_type(&mut self, ty: Type, ctx: SchemeContext) {
    match ty {
      Type::Name(variable) => {
        let Some(name) = variable.name() else {
          return;
        };
        self.resolve_ty_var(name, ctx);
      }
      Type::Sum(sum_type) => {
        if let Some(row) = sum_type.row() {
          self.resolve_row(row, ctx);
        }
      }
      Type::Product(product_type) => {
        if let Some(row) = product_type.row() {
          self.resolve_row(row, ctx);
        }
      }
      Type::Paren(paren_type) => {
        if let Some(ty) = paren_type.ty() {
          self.resolve_type(ty, ctx);
        }
      }
      Type::Function(function_type) => {
        for ty in function_type.tys() {
          self.resolve_type(ty, ctx);
        }
      }
    }
  }

  fn resolve_term(&mut self, expr: Term, ctx: TermName) {
    self.with_scope(|this| {
      for prefix in expr.prefixes().into_iter().flatten() {
        match prefix {
          TermPrefix::With(with_prefix) => {
            if let Some(term) = with_prefix.term() {
              this.resolve_term(term, ctx);
            }
          }
          TermPrefix::Closure(closure_prefix) => {
            if let Some(ty) = closure_prefix.annotation().and_then(|ann| ann.ty()) {
              this.resolve_type(ty, SchemeContext::Term(ctx));
            }
            let Some(name) = closure_prefix.name() else {
              continue;
            };
            this.bind_var(name, ctx);
          }
          TermPrefix::Let(let_prefix) => {
            if let Some(ty) = let_prefix.annotation().and_then(|ann| ann.ty()) {
              this.resolve_type(ty, SchemeContext::Term(ctx));
            }
            // Resolve let body before adding binding to scope to ensure correct namespacing.
            if let Some(term) = let_prefix.defn() {
              this.resolve_term(term, ctx);
            }
            let Some(name) = let_prefix.name() else {
              continue;
            };
            this.bind_var(name, ctx);
          }
        }
      }

      if let Some(body) = expr.body() {
        for postfix in body.ops() {
          this.resolve_term_postfix(postfix, ctx);
        }
      }
    })
  }

  fn resolve_term_postfix(&mut self, postfix: TermPostfix, ctx: TermName) {
    match postfix {
      TermPostfix::Arg(arg) => {
        arg
          .base()
          .inspect(|postfix| self.resolve_term_postfix(postfix.clone(), ctx));
        arg
          .arg()
          .inspect(|term| self.resolve_term(term.clone(), ctx));
      }
      TermPostfix::Field(field) => {
        field
          .base()
          .inspect(|postfix| self.resolve_term_postfix(postfix.clone(), ctx));
      }
      TermPostfix::Atom(term_atom) => self.resolve_term_atom(term_atom, ctx),
    }
  }

  fn resolve_term_atom(&mut self, term_atom: TermAtom, ctx: TermName) {
    match term_atom {
      TermAtom::Var(var_expr) => {
        var_expr
          .name()
          .inspect(|name| self.resolve_var(name.clone()));
      }
      TermAtom::Int(_) => {}
      TermAtom::Paren(expr) => {
        expr
          .term()
          .inspect(|term| self.resolve_term(term.clone(), ctx));
      }
      TermAtom::Prod(prod_expr) => {
        for term in prod_expr.fields().filter_map(|field| field.term()) {
          self.resolve_term(term, ctx);
        }
      }
      TermAtom::Sum(sum_expr) => {
        if let Some(term) = sum_expr.term() {
          self.resolve_term(term, ctx);
        }
      }
      TermAtom::Match(match_expr) => {
        for arm in match_expr.arms() {
          let Some(pat) = arm.pattern() else {
            if let Some(term) = arm.term() {
              self.resolve_term(term, ctx);
            }
            continue;
          };
          self.with_scope(|this| {
            let mut seen = FxHashMap::default();
            this.resolve_pattern(pat, ctx, &mut seen);

            if let Some(term) = arm.term() {
              this.resolve_term(term, ctx);
            }
          })
        }
      }
    }
  }

  fn resolve_pattern(
    &mut self,
    pat: Pattern,
    ctx: TermName,
    seen: &mut FxHashMap<Ident, Handle<Name>>,
  ) {
    match pat {
      Pattern::Prod(prod_pattern) => {
        for pat in prod_pattern.fields().filter_map(|field| field.pattern()) {
          self.resolve_pattern(pat, ctx, seen);
        }
      }
      Pattern::Sum(sum_pattern) => {
        sum_pattern
          .field()
          .and_then(|field| field.pattern())
          .inspect(|pat| self.resolve_pattern(pat.clone(), ctx, seen));
      }
      Pattern::Whole(whole_pattern) => {
        whole_pattern.name().inspect(|name| {
          self.bind_var(name.clone(), ctx);
          let ident = Ident::new(self.db, name.text());
          let current = Handle::new(self.root.clone(), name.clone());
          let old_name = seen.insert(ident, current.clone());
          if let Some(previous) = old_name {
            self.errors.push(NameResError::DuplicateName {
              previous: previous.opaque(),
              current: current.opaque(),
              kind: NameKind::Var,
            });
          }
        });
      }
    }
  }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum InScopeName {
  Effect(EffectName),
  EffectOp(EffectOpName),
  Term(TermName),
  Type(TypeName),
  TermTyVar(TermName, TyVarId),
  TermVar(TermName, VarId),
  // VarIds cannot appear in effect definitions
  EffectTyVar(EffectOpName, TyVarId),
}
impl InScopeName {
  pub fn kind(&self) -> NameKind {
    match self {
      InScopeName::Effect(_) => NameKind::Effect,
      InScopeName::EffectOp(_) => NameKind::EffectOp,
      InScopeName::Term(_) => NameKind::Item,
      InScopeName::TermTyVar(_, _) => NameKind::TyVar,
      InScopeName::TermVar(_, _) => NameKind::Var,
      InScopeName::EffectTyVar(_, _) => NameKind::TyVar,
      InScopeName::Type(_) => NameKind::Type,
    }
  }
  pub fn module(&self, db: &dyn salsa::Database) -> Module {
    match self {
      InScopeName::Effect(eff) => eff.module(db),
      InScopeName::EffectOp(eff_op) | InScopeName::EffectTyVar(eff_op, _) => {
        eff_op.effect(db).module(db)
      }
      InScopeName::Term(term) | InScopeName::TermTyVar(term, _) | InScopeName::TermVar(term, _) => {
        term.module(db)
      }
      InScopeName::Type(ty) => ty.module(db),
    }
  }

  pub fn span(&self, db: &dyn salsa::Database) -> Span {
    let core_db = db;
    let module = self.module(core_db);
    let namespace = nameres_module_of(db, module);
    let (handle, _) = namespace
      .names(db)
      .iter()
      .find(|(_, name)| *name == self)
      .expect("Name missing handle");
    let range: Range<usize> = handle.text_range().into();
    Span {
      start: Loc { byte: range.start },
      end: Loc { byte: range.end },
    }
  }
}

#[salsa::tracked]
pub fn nameres_module_of<'db>(
  db: &'db dyn salsa::Database,
  module: Module,
) -> ModuleNamespace<'db> {
  let parse_module = parse_module_of(db, module);
  nameres_module(db, parse_module)
}

#[salsa::tracked]
fn item_name<'db>(db: &'db dyn salsa::Database, term: TermName) -> Ident {
  term.name(db)
}

#[salsa::tracked]
pub fn id_for_name<'db>(
  db: &'db dyn salsa::Database,
  module: Module,
  name: Ident,
) -> Option<TermName> {
  let namespace_module = nameres_module_of(db, module);
  namespace_module.terms(db).values().find_map(|term| {
    let term_name = term.name(db);
    (term_name.name(db) == name).then_some(term_name)
  })
}

#[salsa::tracked]
pub fn effect_name<'db>(db: &'db dyn salsa::Database, effect: EffectName) -> Ident {
  effect.name(db)
}

#[salsa::tracked]
pub fn effect_member_name<'db>(db: &'db dyn salsa::Database, effect_op: EffectOpName) -> Ident {
  effect_op.name(db)
}

#[salsa::tracked(returns(ref))]
pub fn effect_members<'db>(db: &'db dyn salsa::Database, effect: EffectName) -> Vec<EffectOpName> {
  let module = effect.module(db);
  let name_res = nameres_module_of(db, module);

  let node = effect_defn(db, effect).data(db);
  let root = node.root.clone();
  let effect = node.clone().into_node();
  effect
    .ops()
    .into_iter()
    .flatten()
    .filter_map(|op| {
      op.name()
        .and_then(|name| {
          let handle = Handle::new(root.clone(), name);
          name_res.names(db).get(&handle)
        })
        .map(|in_scope_name| match in_scope_name {
          InScopeName::EffectOp(effect_op_name) => effect_op_name,
          _ => unreachable!(),
        })
        .cloned()
    })
    .collect()
}

#[salsa::tracked]
pub fn lookup_effect_by_member_names<'db>(
  db: &'db dyn salsa::Database,
  module: Module,
  members: Box<[Ident]>,
) -> Option<EffectName> {
  let mut members: Vec<Ident> = members.as_ref().to_vec();
  members.sort_by_key(|id| id.text(db));

  lookup_effect_by(db, module, |_, eff_names|
            // if length's don't match up we don't need to iterate
            members.len() == eff_names.len()
                && eff_names
                    .iter()
                    .all(|name| members.binary_search_by_key(&name.name(db).text(db), |id| id.text(db)).is_ok()))
}

#[salsa::tracked]
pub fn lookup_effect_by_name<'db>(
  db: &'db dyn salsa::Database,
  module: Module,
  effect_name: Ident,
) -> Option<EffectName> {
  lookup_effect_by(db, module, |name, _| name == effect_name)
}

fn lookup_effect_by<'db>(
  db: &'db dyn salsa::Database,
  module: Module,
  mut find_by: impl FnMut(Ident, &[EffectOpName]) -> bool,
) -> Option<EffectName> {
  let names = nameres_module_of(db, module);

  names
    .effects(db)
    .values()
    .find(|effect| {
      let name = effect.name(db);
      let ident = name.name(db);
      let ops = effect_members(db, name);
      find_by(ident, ops)
    })
    .map(|effect| effect.name(db))
}

#[salsa::tracked(returns(ref))]
fn effect_handler_order<'db>(db: &'db dyn salsa::Database, eff_name: EffectName) -> Vec<Ident> {
  let mut members = effect_members(db, eff_name)
    .iter()
    .map(|eff_op| effect_member_name(db, *eff_op))
    .collect::<Vec<_>>();

  // Insert `return` so it get's ordered as well.
  members.push(Ident::new(db, "return"));
  members.sort_by_key(|id| id.text(db));
  members
}

#[salsa::tracked]
pub fn effect_handler_return_index<'db>(db: &'db dyn salsa::Database, eff: EffectName) -> usize {
  let return_id = Ident::new(db, "return");
  effect_handler_order(db, eff)
    .binary_search_by_key(&return_id.text(db), |id| id.text(db))
    .unwrap_or_else(|_| {
      panic!(
        "ICE: Created handler order for effect {:?} that did not contain `return`",
        eff.name(db),
      )
    })
}

#[salsa::tracked]
pub fn effect_handler_op_index<'db>(db: &'db dyn salsa::Database, eff_op: EffectOpName) -> usize {
  let member_id = effect_member_name(db, eff_op);
  let eff = eff_op.effect(db);
  effect_handler_order(db, eff)
    .binary_search_by_key(&member_id.text(db), |id| id.text(db))
    .unwrap_or_else(|_| {
      panic!(
        "ICE: Created handler order for effect {:?} that did not contain member {:?}",
        eff.name(db).text(db),
        eff_op.name(db).text(db)
      )
    })
}

#[salsa::tracked(returns(ref))]
pub fn module_effects<'db>(db: &'db dyn salsa::Database, module: Module) -> Vec<EffectName> {
  let nameres_module = nameres_module_of(db, module);
  nameres_module
    .effects(db)
    .values()
    .map(|effect| effect.name(db))
    .collect()
}

#[salsa::tracked(returns(ref))]
pub fn all_effects<'db>(db: &'db dyn salsa::Database) -> Vec<EffectName> {
  let module_tree = all_modules(db);
  let mut effects = module_tree
    .iter()
    .flat_map(|module| module_effects(db, *module).iter().copied())
    .collect::<Vec<_>>();

  effects.sort_by_key(|eff| eff.name(db).text(db));
  effects
}

#[salsa::tracked]
pub fn effect_vector_index<'db>(db: &'db dyn salsa::Database, effect: EffectName) -> usize {
  let effects = all_effects(db);
  effects
    .binary_search_by_key(&effect.name(db).text(db), |effect| effect.name(db).text(db))
    .unwrap_or_else(|_| {
      panic!(
        "ICE: {:?} expected effect to exist but it was not found",
        effect.name(db).text(db)
      )
    })
}

#[salsa::tracked]
fn name_at_loc<'db>(
  db: &'db dyn salsa::Database,
  file_id: FileId,
  line: u32,
  col: u32,
) -> Option<InScopeName> {
  let loc = locate(db, file_id, line, col)?;
  let file = file_for_id(db, file_id);

  let parse_file = parse_module(db, file);
  let cst = parse_file.data(db);
  let namespace = nameres_module_for_file(db, file);

  let root = SyntaxNode::<Panoply>::new_root(cst.clone());
  let byte: u32 = loc.byte.try_into().unwrap();
  let token_at_offset = root.token_at_offset(byte.into());
  let token = token_at_offset.left_biased()?;
  let node = token.parent()?;

  let name = Name::cast(node)?;
  namespace.names(db).get(&Handle::new(cst, name)).cloned()
  /*
  // TODO: We could be more efficient here by ordering our defs by span and then binary searching
  // so we more quickly locate the item we want to dfs into.
  for effect in nst_module.effects(db).iter().flat_map(|eff| eff.iter()) {
    let eff_defn = effect.data(db);
    if !eff_defn.span().contains(loc) {
      continue;
    }
    if eff_defn.name.span().contains(loc) {
      return Some(InScopeName::Effect(eff_defn.name.value));
    }
    let ops = eff_defn.ops.iter().flat_map(|op| op.iter());
    for op in ops {
      if op.name.span().contains(loc) {
        return Some(InScopeName::EffectOp(op.name.value));
      }
      let dfs = FindSpanName {
        defn_name: op.name.value,
        indices: effect.alloc(db),
        needle: loc,
      };
      if let ControlFlow::Break(found) = dfs.traverse_type(op.type_) {
        return Some(found);
      }
    }
  }
  for term in nst_module.terms(db).iter().flat_map(|term| term.iter()) {
    let term_defn = term.data(db);
    if term_defn.name.span().contains(loc) {
      return Some(InScopeName::Term(term_defn.name.value));
    }
    let dfs = FindSpanName {
      defn_name: term_defn.name.value,
      indices: term.alloc(db),
      needle: loc,
    };
    if let ControlFlow::Break(found) = dfs.traverse_term(term_defn.value) {
      return Some(found);
    }
  }
  None*/
}

/*
struct FindSpanName<'a, Name> {
  defn_name: Name,
  indices: &'a NstIndxAlloc,
  needle: Loc,
}
impl<T, Name> IdxView<T> for FindSpanName<'_, Name>
where
  NstIndxAlloc: IdxView<T>,
{
  fn view(&self, idx: Idx<T>) -> &T {
    self.indices.view(idx)
  }
}

impl DfsTraverseNst for FindSpanName<'_, EffectOpName> {
  type Out = InScopeName;

  fn traverse_ty_var(&self, name: &SpanOf<TyVarId>) -> ControlFlow<Self::Out> {
    if name.span().contains(self.needle) {
      ControlFlow::Break(InScopeName::EffectTyVar(self.defn_name, name.value))
    } else {
      ControlFlow::Continue(())
    }
  }

  fn should_traverse_term(&self, _: &TermDefn) -> bool {
    false
  }
  fn should_traverse_pat(&self, _: &Pattern) -> bool {
    false
  }

  fn should_traverse_type(&self, ty: &Type<TyVarId>) -> bool {
    ty.spanned(self.indices).span().contains(self.needle)
  }
  fn should_traverse_row(&self, row: &Row<TyVarId, IdField<Idx<Type<TyVarId>>>>) -> bool {
    row.spanned(self.indices).span().contains(self.needle)
  }
}
impl DfsTraverseNst for FindSpanName<'_, TermName> {
  type Out = InScopeName;

  fn traverse_var(&self, name: &SpanOf<VarId>) -> std::ops::ControlFlow<Self::Out> {
    if name.span().contains(self.needle) {
      ControlFlow::Break(InScopeName::TermVar(self.defn_name, name.value))
    } else {
      ControlFlow::Continue(())
    }
  }

  fn traverse_ty_var(&self, name: &SpanOf<TyVarId>) -> ControlFlow<Self::Out> {
    if name.span().contains(self.needle) {
      ControlFlow::Break(InScopeName::TermTyVar(self.defn_name, name.value))
    } else {
      ControlFlow::Continue(())
    }
  }

  fn traverse_effect_op(&self, name: &SpanOf<EffectOpName>) -> ControlFlow<Self::Out> {
    if name.span().contains(self.needle) {
      ControlFlow::Break(InScopeName::EffectOp(name.value))
    } else {
      ControlFlow::Continue(())
    }
  }

  fn traverse_term_name(&self, name: &SpanOf<TermName>) -> ControlFlow<Self::Out> {
    if name.span().contains(self.needle) {
      ControlFlow::Break(InScopeName::Term(name.value))
    } else {
      ControlFlow::Continue(())
    }
  }

  fn should_traverse_term(&self, term: &TermDefn) -> bool {
    term.spanned(self.indices).span().contains(self.needle)
  }
  fn should_traverse_type(&self, ty: &Type<TyVarId>) -> bool {
    ty.spanned(self.indices).span().contains(self.needle)
  }
  fn should_traverse_pat(&self, pat: &Pattern) -> bool {
    pat.span().contains(self.needle)
  }
  fn should_traverse_row(&self, row: &Row<TyVarId, IdField<Idx<Type<TyVarId>>>>) -> bool {
    row.spanned(self.indices).span().contains(self.needle)
  }
}
*/

#[cfg(test)]
mod tests {
  use std::{ops::Range, path::PathBuf};

  use assert_matches::assert_matches;
  use base::{
    diagnostic::{
      error::PanoplyError,
      nameres::{NameKind, NameResolutionError},
    },
    file::{FileId, SourceFile, SourceFileSet},
  };
  use cst::{Item, Items, TermDefn};
  use parser::{AstNode, parse_module};
  use pretty::{DocAllocator, DocBuilder, RcAllocator};

  use crate::{
    InScopeName, ModuleNamespace, NameResError, all_nameres_errors, nameres_module_for_file,
  };

  #[derive(Default, Clone)]
  #[salsa::db]
  struct TestDatabase {
    storage: salsa::Storage<Self>,
  }
  #[salsa::db]
  impl salsa::Database for TestDatabase {}

  fn parse_resolve_module<S: ToString>(
    db: &'_ TestDatabase,
    input: S,
  ) -> (Items, ModuleNamespace<'_>, Vec<NameResError>) {
    let file_id = FileId::new(db, PathBuf::from("test"));
    let file = SourceFile::new(db, file_id, input.to_string());
    let _ = SourceFileSet::new(db, vec![file]);

    let nameres_module = nameres_module_for_file(db, file);

    let errors = all_nameres_errors(db)
      .into_iter()
      .map(|name_res| name_res.error)
      .collect();

    (
      Items::new(parse_module(db, file).data(db)),
      nameres_module,
      errors,
    )
  }

  fn parse_resolve_term<'db>(
    db: &'db TestDatabase,
    input: &str,
  ) -> (Option<TermDefn>, ModuleNamespace<'db>, Vec<NameResError>) {
    // Wrap our term in an item def
    let mut content = "defn item = ".to_string();
    content.push_str(input);

    let (items, namespace, errors) = parse_resolve_module(db, content);

    (
      items.items().find_map(|item| match item {
        Item::Term(term_defn) => Some(term_defn),
        Item::Effect(_) => None,
      }),
      namespace,
      errors,
    )
  }

  impl<'db> ModuleNamespace<'db> {
    fn prettyprint(&self, db: &'db TestDatabase) -> String {
      let rc_alloc = RcAllocator;
      let mut names = self.names(db).iter().collect::<Vec<_>>();
      names.sort_by_key(|(handle, _)| {
        let range: Range<usize> = handle.ptr.syntax_node_ptr().text_range().into();
        (range.start, range.end)
      });
      let doc: pretty::DocBuilder<RcAllocator> = rc_alloc.intersperse(
        names.into_iter().map(|(handle, name)| {
          let syn_name = handle.clone().into_node();
          let syn = syn_name.syntax();
          name
            .pretty(db, &rc_alloc)
            .append(rc_alloc.line())
            .append("=>")
            .append(rc_alloc.line())
            .append(rc_alloc.text(format!("{:#?}", syn)))
        }),
        rc_alloc.line(),
      );

      let mut s = String::new();
      doc
        .render_fmt(80, &mut s)
        .expect("If this fails I have no backup plan at all");
      s
    }
  }

  impl InScopeName {
    fn pretty<'a>(&self, db: &'a TestDatabase, a: &'a RcAllocator) -> DocBuilder<'a, RcAllocator> {
      match self {
        InScopeName::Effect(effect_name) => a.text(effect_name.name(db).text(db)),
        InScopeName::EffectOp(effect_op_name) => a
          .text(effect_op_name.effect(db).name(db).text(db))
          .append(".")
          .append(a.text(effect_op_name.name(db).text(db))),
        InScopeName::Term(term_name) => a.text(term_name.name(db).text(db)),
        InScopeName::TermTyVar(term_name, ty_var_id) => a
          .text(term_name.name(db).text(db))
          .append(".ty_vars")
          .append(a.as_string(ty_var_id.0).brackets()),
        InScopeName::TermVar(term_name, var_id) => a
          .text(term_name.name(db).text(db))
          .append(".vars")
          .append(a.as_string(var_id.0).brackets()),
        InScopeName::EffectTyVar(effect_op_name, ty_var_id) => a
          .text(effect_op_name.effect(db).name(db).text(db))
          .append(".")
          .append(a.text(effect_op_name.name(db).text(db)))
          .append(".ty_vars")
          .append(a.as_string(ty_var_id.0).brackets()),
        InScopeName::Type(ty) => a.text(ty.name(db).text(db)),
      }
    }
  }

  #[test]
  fn test_local_binding() {
    let db = TestDatabase::default();
    let (_, names, errs) = parse_resolve_term(&db, "let x = {}; let y = {}; x");
    assert_matches!(errs[..], []);
    let expect = expect_test::expect![[r#"
        item
        =>
        Ident@5..10
          Identifier@5..9 "item"
          Whitespace@9..10 " "

        item.vars[0]
        =>
        Ident@16..18
          Identifier@16..17 "x"
          Whitespace@17..18 " "

        item.vars[1]
        =>
        Ident@28..30
          Identifier@28..29 "y"
          Whitespace@29..30 " "

        item.vars[0]
        =>
        Ident@36..37
          Identifier@36..37 "x"
    "#]];
    expect.assert_eq(&names.prettyprint(&db))
  }

  #[test]
  fn test_local_binding_types() {
    let db = TestDatabase::default();
    let (_, names, errs) = parse_resolve_term(&db, "let x: a = {}; let y: {} = {}; x");
    assert_matches!(errs[..], []);
    let expect = expect_test::expect![[r#"
        item
        =>
        Ident@5..10
          Identifier@5..9 "item"
          Whitespace@9..10 " "

        item.vars[0]
        =>
        Ident@16..17
          Identifier@16..17 "x"

        item.ty_vars[0]
        =>
        Ident@19..21
          Identifier@19..20 "a"
          Whitespace@20..21 " "

        item.vars[1]
        =>
        Ident@31..32
          Identifier@31..32 "y"

        item.vars[0]
        =>
        Ident@43..44
          Identifier@43..44 "x"
    "#]];
    expect.assert_eq(&names.prettyprint(&db))
  }

  #[test]
  fn test_local_binding_shadowing() {
    let db = TestDatabase::default();
    let (_, names, errs) = parse_resolve_term(&db, "let x = {}; let x = x; x");
    assert_matches!(errs[..], []);
    let expect = expect_test::expect![[r#"
        item
        =>
        Ident@5..10
          Identifier@5..9 "item"
          Whitespace@9..10 " "

        item.vars[0]
        =>
        Ident@16..18
          Identifier@16..17 "x"
          Whitespace@17..18 " "

        item.vars[1]
        =>
        Ident@28..30
          Identifier@28..29 "x"
          Whitespace@29..30 " "

        item.vars[0]
        =>
        Ident@32..33
          Identifier@32..33 "x"

        item.vars[1]
        =>
        Ident@35..36
          Identifier@35..36 "x"
    "#]];
    expect.assert_eq(&names.prettyprint(&db))
  }

  #[test]
  fn test_local_binding_errors() {
    let db = TestDatabase::default();
    let (_, _, errs) = parse_resolve_term(&db, "let x = y; z");
    assert_matches!(
      &errs[..],
      [
        NameResError::UndefinedName(y),
        NameResError::UndefinedName(z),
      ] /*=> {
            assert_eq!(y, "y");
            assert_eq!(z, "z");
        }*/
    );
  }

  #[test]
  fn test_handler() {
    let db = TestDatabase::default();
    let (_, names, errs) = parse_resolve_term(&db, "let x = {}; with x do x");
    assert_matches!(errs[..], []);
    let expect = expect_test::expect![[r#"
        item
        =>
        Ident@5..10
          Identifier@5..9 "item"
          Whitespace@9..10 " "

        item.vars[0]
        =>
        Ident@16..18
          Identifier@16..17 "x"
          Whitespace@17..18 " "

        item.vars[0]
        =>
        Ident@29..31
          Identifier@29..30 "x"
          Whitespace@30..31 " "

        item.vars[0]
        =>
        Ident@34..35
          Identifier@34..35 "x"
    "#]];
    expect.assert_eq(&names.prettyprint(&db))
  }

  #[test]
  fn test_handler_errors() {
    let db = TestDatabase::default();
    let (_, _, errs) = parse_resolve_term(&db, "with h do x");
    assert_matches!(
      &errs[..],
      [
        NameResError::UndefinedName(h),
        NameResError::UndefinedName(x),
      ] /*=> {
            assert_eq!(h, "h");
            assert_eq!(x, "x");
        }*/
    );
  }

  #[test]
  fn test_abstraction() {
    let db = TestDatabase::default();
    let (_, names, errs) = parse_resolve_term(&db, "|x| |y| y(x)");
    assert_matches!(errs[..], []);
    let expect = expect_test::expect![[r#"
        item
        =>
        Ident@5..10
          Identifier@5..9 "item"
          Whitespace@9..10 " "

        item.vars[0]
        =>
        Ident@13..14
          Identifier@13..14 "x"

        item.vars[1]
        =>
        Ident@17..18
          Identifier@17..18 "y"

        item.vars[1]
        =>
        Ident@20..21
          Identifier@20..21 "y"

        item.vars[0]
        =>
        Ident@22..23
          Identifier@22..23 "x"
    "#]];
    expect.assert_eq(&names.prettyprint(&db))
  }

  #[test]
  fn test_abstraction_types() {
    let db = TestDatabase::default();
    let (_, names, errs) = parse_resolve_term(&db, "|x: {}| |y: a -> b| y(x)");
    assert_matches!(errs[..], []);
    let expect = expect_test::expect![[r#"
        item
        =>
        Ident@5..10
          Identifier@5..9 "item"
          Whitespace@9..10 " "

        item.vars[0]
        =>
        Ident@13..14
          Identifier@13..14 "x"

        item.vars[1]
        =>
        Ident@21..22
          Identifier@21..22 "y"

        item.ty_vars[0]
        =>
        Ident@24..26
          Identifier@24..25 "a"
          Whitespace@25..26 " "

        item.ty_vars[1]
        =>
        Ident@29..30
          Identifier@29..30 "b"

        item.vars[1]
        =>
        Ident@32..33
          Identifier@32..33 "y"

        item.vars[0]
        =>
        Ident@34..35
          Identifier@34..35 "x"
    "#]];
    expect.assert_eq(&names.prettyprint(&db))
  }

  #[test]
  fn test_abstraction_shadowing() {
    let db = TestDatabase::default();
    let (_, names, errs) = parse_resolve_term(&db, "|x| |x| x(x)");
    assert_matches!(errs[..], []);
    let expect = expect_test::expect![[r#"
        item
        =>
        Ident@5..10
          Identifier@5..9 "item"
          Whitespace@9..10 " "

        item.vars[0]
        =>
        Ident@13..14
          Identifier@13..14 "x"

        item.vars[1]
        =>
        Ident@17..18
          Identifier@17..18 "x"

        item.vars[1]
        =>
        Ident@20..21
          Identifier@20..21 "x"

        item.vars[1]
        =>
        Ident@22..23
          Identifier@22..23 "x"
    "#]];
    expect.assert_eq(&names.prettyprint(&db))
  }

  #[test]
  fn test_application() {
    let db = TestDatabase::default();
    let (_, names, errs) = parse_resolve_term(&db, "|x| x(x)");
    assert_matches!(errs[..], []);
    let expect = expect_test::expect![[r#"
        item
        =>
        Ident@5..10
          Identifier@5..9 "item"
          Whitespace@9..10 " "

        item.vars[0]
        =>
        Ident@13..14
          Identifier@13..14 "x"

        item.vars[0]
        =>
        Ident@16..17
          Identifier@16..17 "x"

        item.vars[0]
        =>
        Ident@18..19
          Identifier@18..19 "x"
    "#]];
    expect.assert_eq(&names.prettyprint(&db))
  }

  #[test]
  fn test_application_errors() {
    let db = TestDatabase::default();
    let (_, _, errs) = parse_resolve_term(&db, "f(x)");
    assert_matches!(
      &errs[..],
      [
        NameResError::UndefinedName(f),
        NameResError::UndefinedName(x),
      ] /* => {
            assert_eq!(f, "f");
            assert_eq!(x, "x");
        }*/
    );
  }

  #[test]
  fn test_product_row() {
    let db = TestDatabase::default();
    let (_, names, errs) = parse_resolve_term(&db, "let x = {}; {a = x, b = {x = x}}");
    assert_matches!(errs[..], []);
    let expect = expect_test::expect![[r#"
        item
        =>
        Ident@5..10
          Identifier@5..9 "item"
          Whitespace@9..10 " "

        item.vars[0]
        =>
        Ident@16..18
          Identifier@16..17 "x"
          Whitespace@17..18 " "

        item.vars[0]
        =>
        Ident@29..30
          Identifier@29..30 "x"

        item.vars[0]
        =>
        Ident@41..42
          Identifier@41..42 "x"
    "#]];
    expect.assert_eq(&names.prettyprint(&db))
  }

  #[test]
  fn test_product_row_errors() {
    let db = TestDatabase::default();
    let (_, _, errs) = parse_resolve_term(&db, "{x = y, z = x}");
    assert_matches!(
      &errs[..],
      [
        NameResError::UndefinedName(y),
        NameResError::UndefinedName(x)
      ] /* => {
            assert_eq!(y, "y");
            assert_eq!(x, "x");
        }*/
    );
  }

  #[test]
  fn test_sum_row() {
    let db = TestDatabase::default();
    let (term, names, errs) = parse_resolve_term(&db, "|x| <a = x>");
    println!("{:#?}", term);
    assert_matches!(errs[..], []);
    let expect = expect_test::expect![[r#"
        item
        =>
        Ident@5..10
          Identifier@5..9 "item"
          Whitespace@9..10 " "

        item.vars[0]
        =>
        Ident@13..14
          Identifier@13..14 "x"

        item.vars[0]
        =>
        Ident@21..22
          Identifier@21..22 "x"
    "#]];
    expect.assert_eq(&names.prettyprint(&db))
  }

  #[test]
  fn test_sum_row_errors() {
    let db = TestDatabase::default();
    let (_, _, errs) = parse_resolve_term(&db, "<x = x>");
    assert_matches!(
      &errs[..],
      [NameResError::UndefinedName(x)] /* => {
                                           assert_eq!(x, "x");
                                       }*/
    );
  }

  #[test]
  fn test_dot_access() {
    let db = TestDatabase::default();
    let (_, names, errs) = parse_resolve_term(&db, "let id = |x| x; {x = id}.x");
    assert_matches!(errs[..], []);
    let expect = expect_test::expect![[r#"
        item
        =>
        Ident@5..10
          Identifier@5..9 "item"
          Whitespace@9..10 " "

        item.vars[1]
        =>
        Ident@16..19
          Identifier@16..18 "id"
          Whitespace@18..19 " "

        item.vars[0]
        =>
        Ident@22..23
          Identifier@22..23 "x"

        item.vars[0]
        =>
        Ident@25..26
          Identifier@25..26 "x"

        item.vars[1]
        =>
        Ident@33..35
          Identifier@33..35 "id"
    "#]];
    expect.assert_eq(&names.prettyprint(&db))
  }

  #[test]
  fn test_dot_access_errors() {
    let db = TestDatabase::default();
    let (_, _, errs) = parse_resolve_term(&db, "x.a");
    assert_matches!(
      &errs[..],
      [NameResError::UndefinedName(x)] /* => {
                                           assert_eq!(x, "x");
                                       }*/
    );
  }

  #[test]
  fn test_match() {
    let db = TestDatabase::default();
    let (_, names, errs) = parse_resolve_term(&db, "match <{a = x} => x, y => y>");
    assert_matches!(errs[..], []);
    let expect = expect_test::expect![[r#"
        item
        =>
        Ident@5..10
          Identifier@5..9 "item"
          Whitespace@9..10 " "

        item.vars[0]
        =>
        Ident@24..25
          Identifier@24..25 "x"

        item.vars[0]
        =>
        Ident@30..31
          Identifier@30..31 "x"

        item.vars[1]
        =>
        Ident@33..35
          Identifier@33..34 "y"
          Whitespace@34..35 " "

        item.vars[1]
        =>
        Ident@38..39
          Identifier@38..39 "y"
    "#]];
    expect.assert_eq(&names.prettyprint(&db))
  }

  #[test]
  fn test_match_error_name_not_found() {
    let db = TestDatabase::default();
    let (_, _, errs) = parse_resolve_term(&db, "match <{a = x} => f(x), {} => z>");
    assert_matches!(
      &errs[..],
      [
        NameResError::UndefinedName(f),
        NameResError::UndefinedName(z)
      ] /* => {
            assert_eq!(f, "f");
            assert_eq!(z, "z");
        }*/
    );
  }

  #[test]
  fn test_match_error_duplicate_name() {
    let db = TestDatabase::default();
    let (_, names, errs) = parse_resolve_term(&db, "match <{a = x, b = x} => x(x)>");
    assert_matches!(
      &errs[..],
      [NameResError::DuplicateName { .. }] /* => {
                                               assert!(end < start, "{} < {}", end, start);
                                               assert_eq!(x, "x");
                                           }*/
    );
    let expect = expect_test::expect![[r#"
        item
        =>
        Ident@5..10
          Identifier@5..9 "item"
          Whitespace@9..10 " "

        item.vars[0]
        =>
        Ident@24..25
          Identifier@24..25 "x"

        item.vars[1]
        =>
        Ident@31..32
          Identifier@31..32 "x"

        item.vars[1]
        =>
        Ident@37..38
          Identifier@37..38 "x"

        item.vars[1]
        =>
        Ident@39..40
          Identifier@39..40 "x"
    "#]];
    expect.assert_eq(&names.prettyprint(&db))
  }

  #[test]
  fn test_mixed_shadowing() {
    let db = TestDatabase::default();
    let (_, names, errs) = parse_resolve_term(&db, "let x = {}; |x| match <x => x>");
    assert_matches!(errs[..], []);
    let expect = expect_test::expect![[r#"
        item
        =>
        Ident@5..10
          Identifier@5..9 "item"
          Whitespace@9..10 " "

        item.vars[0]
        =>
        Ident@16..18
          Identifier@16..17 "x"
          Whitespace@17..18 " "

        item.vars[1]
        =>
        Ident@25..26
          Identifier@25..26 "x"

        item.vars[2]
        =>
        Ident@35..37
          Identifier@35..36 "x"
          Whitespace@36..37 " "

        item.vars[2]
        =>
        Ident@40..41
          Identifier@40..41 "x"
    "#]];
    expect.assert_eq(&names.prettyprint(&db))
  }

  #[test]
  fn test_schemes() {
    let db = TestDatabase::default();
    let (_, names, errs) = parse_resolve_module(&db, "defn foo : forall a. a -> a = |x| x");
    assert_matches!(errs[..], []);
    let expect = expect_test::expect![[r#"
        foo
        =>
        Ident@5..9
          Identifier@5..8 "foo"
          Whitespace@8..9 " "

        foo.ty_vars[0]
        =>
        Ident@18..19
          Identifier@18..19 "a"

        foo.ty_vars[0]
        =>
        Ident@21..23
          Identifier@21..22 "a"
          Whitespace@22..23 " "

        foo.ty_vars[0]
        =>
        Ident@26..28
          Identifier@26..27 "a"
          Whitespace@27..28 " "

        foo.vars[0]
        =>
        Ident@31..32
          Identifier@31..32 "x"

        foo.vars[0]
        =>
        Ident@34..35
          Identifier@34..35 "x"
    "#]];
    expect.assert_eq(&names.prettyprint(&db))
  }

  #[test]
  fn test_int_ty() {
    let db = TestDatabase::default();
    let (_, names, errs) = parse_resolve_module(&db, "defn foobar : Int = 5");

    assert_matches!(errs[..], []);
    let expect = expect_test::expect![[r#"
        foobar
        =>
        Ident@5..12
          Identifier@5..11 "foobar"
          Whitespace@11..12 " "

        Int
        =>
        Ident@14..18
          Identifier@14..17 "Int"
          Whitespace@17..18 " "
    "#]];
    expect.assert_eq(&names.prettyprint(&db))
  }

  #[test]
  fn test_top_level_letrec() {
    let db = TestDatabase::default();
    let (_, names, errs) = parse_resolve_module(&db, "defn foo = bar\ndefn bar = foo");
    assert_matches!(errs[..], []);
    let expect = expect_test::expect![[r#"
        foo
        =>
        Ident@5..9
          Identifier@5..8 "foo"
          Whitespace@8..9 " "

        bar
        =>
        Ident@11..15
          Identifier@11..14 "bar"
          Whitespace@14..15 "\n"

        bar
        =>
        Ident@20..24
          Identifier@20..23 "bar"
          Whitespace@23..24 " "

        foo
        =>
        Ident@26..29
          Identifier@26..29 "foo"
    "#]];
    expect.assert_eq(&names.prettyprint(&db))
  }

  #[test]
  fn test_top_level_errors() {
    let db = TestDatabase::default();
    let (_, _, errs) = parse_resolve_module(&db, "defn foo = x\ndefn bar = y");
    assert_matches!(
      &errs[..],
      [
        NameResError::UndefinedName(x),
        NameResError::UndefinedName(y)
      ] /* => {
            assert_eq!(x, "x");
            assert_eq!(y, "y");
        }*/
    );
  }
}
