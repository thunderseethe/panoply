use core::panic;
use std::{hash::Hash, ops::Range};

use ::base::{
  diagnostic::{
    error::{PanoplyError, PanoplyErrors},
    nameres::{NameKind, NameKinds, NameResolutionError},
  },
  file::{FileId, SourceFile},
  id::{EffectName, EffectOpName, IdGen, TermName, TyVarId, VarId},
  ident::Ident,
  loc::Loc,
  modules::Module,
  span::Span,
};
use base::id::{Ids, TypeName};
use cst::{
  EffectDefn, HasAstPtr, HasName, Item, Items, Name, Panoply, Pattern, Row, RowAtom, Term,
  TermAtom, TermDefn, TermPostfix, TermPrefix, Type, TypeScheme,
};
use parser::ParseFile;
use rowan::{
  GreenNode, SyntaxNode, TextRange,
  ast::{AstNode, AstPtr, SyntaxNodePtr},
};
use rustc_hash::FxHashMap;
use salsa::{AsId, tracked};

pub mod name;

#[salsa::jar(db = Db)]
pub struct Jar(
  NameResTerm,
  NameResEffect,
  ModuleNamespace,
  all_effects,
  effect_defn,
  effect_name,
  effect_member_name,
  effect_members,
  effect_handler_order,
  effect_handler_return_index,
  effect_handler_op_index,
  effect_vector_index,
  id_for_name,
  item_name,
  lookup_effect_by_member_names,
  lookup_effect_by_name,
  module_effects,
  name_at_loc,
  nameres_module,
  nameres_module_of,
  term_defn,
);
pub trait Db: salsa::DbWithJar<Jar> + parser::Db {
  fn as_nameres_db(&self) -> &dyn crate::Db {
    <Self as salsa::DbWithJar<crate::Jar>>::as_jar_db(self)
  }

  fn nameres_module_of(&self, module: Module) -> ModuleNamespace {
    nameres_module_of(self.as_nameres_db(), module)
  }

  fn nameres_module_for_file(&self, file: SourceFile) -> ModuleNamespace {
    let parse_file = self.parse_module(file);
    nameres_module(self.as_nameres_db(), parse_file)
  }

  fn nameres_module_for_file_id(&self, file_id: FileId) -> ModuleNamespace {
    let file = ::base::file::file_for_id(self.as_core_db(), file_id);
    self.nameres_module_for_file(file)
  }

  fn item_name(&self, term: TermName) -> Ident {
    item_name(self.as_nameres_db(), term)
  }

  fn id_for_name(&self, module: Module, name: Ident) -> Option<TermName> {
    id_for_name(self.as_nameres_db(), module, name)
  }

  fn nameres_effect_of(&self, eff_name: EffectName) -> NameResEffect {
    effect_defn(self.as_nameres_db(), eff_name)
  }

  fn nameres_term_of(&self, term_name: TermName) -> NameResTerm {
    term_defn(self.as_nameres_db(), term_name)
  }

  fn name_at_position(&self, file: FileId, line: u32, col: u32) -> Option<InScopeName> {
    name_at_loc(self.as_nameres_db(), file, line, col)
  }

  fn all_nameres_errors(&self) -> Vec<PanoplyError> {
    self
      .all_modules()
      .iter()
      .flat_map(|module| {
        nameres_module_of::accumulated::<PanoplyErrors>(self.as_nameres_db(), *module).into_iter()
      })
      .collect()
  }

  fn nameres_errors(&self, file_id: FileId) -> Vec<PanoplyError> {
    let module = self.root_module_for_file(self.file_for_id(file_id));
    nameres_module_of::accumulated::<PanoplyErrors>(self.as_nameres_db(), module)
  }
}
impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> + parser::Db {}

#[salsa::tracked]
fn effect_defn(db: &dyn crate::Db, eff_name: EffectName) -> NameResEffect {
  let nameres_module = db.nameres_module_of(eff_name.module(db.as_core_db()));
  *nameres_module
    .effects(db)
    .values()
    .find(|eff| eff.name(db) == eff_name)
    .unwrap_or_else(|| {
      panic!(
        "ICE: Constructed EffectName {:?} with no effect definition",
        eff_name.name(db.as_core_db()).text(db.as_core_db())
      )
    })
}

#[salsa::tracked]
fn term_defn(db: &dyn crate::Db, term_name: TermName) -> NameResTerm {
  let nameres_module = db.nameres_module_of(term_name.module(db.as_core_db()));
  *nameres_module
    .terms(db)
    .values()
    .find(|term| term.name(db) == term_name)
    .unwrap_or_else(|| {
      panic!(
        "ICE: Constructed TermName {:?} with no term definition",
        term_name.name(db.as_core_db()).text(db.as_core_db())
      )
    })
}

#[salsa::tracked]
pub struct NameResTerm {
  #[id]
  pub name: TermName,
  #[return_ref]
  pub data: Handle<TermDefn>,
  #[return_ref]
  pub vars: Box<Ids<VarId, Handle<Name>>>,
  #[return_ref]
  pub ty_vars: Box<Ids<TyVarId, Handle<Name>>>,
}

impl NameResTerm {
  pub fn span_of(&self, db: &dyn crate::Db) -> Span {
    //let alloc = self.alloc(db);
    //alloc.spanned(alloc).span()
    let ras: TermDefn = self.data(db).clone().into_node();
    let range = ras.term().expect("Failure").syntax().text_range();
    Span {
      start: Loc {
        file: FileId::from_id(salsa::Id::from_u32(0)),
        byte: range.start().into(),
        line: 0,
        col: 0,
      },
      end: Loc {
        file: FileId::from_id(salsa::Id::from_u32(0)),
        byte: range.end().into(),
        line: 0,
        col: 0,
      },
    }
  }
}

#[salsa::tracked]
pub struct NameResEffect {
  #[id]
  pub name: EffectName,
  #[return_ref]
  pub data: Handle<EffectDefn>,
  #[return_ref]
  pub vars: Box<Ids<VarId, Handle<Name>>>,
  #[return_ref]
  pub ty_vars: Box<Ids<TyVarId, Handle<Name>>>,
}

/*#[salsa::tracked]
pub struct NameResModule {
  #[id]
  pub module: Module,
  // This is a map of all in scope names, from BaseNames
  #[return_ref]
  pub names: FxHashMap<Module, ModuleNames>,
  #[return_ref]
  pub terms: Vec<Option<NameResTerm>>,
  #[return_ref]
  pub effects: Vec<Option<NameResEffect>>,
}*/

#[derive(Clone)]
struct OpaqueHandle {
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

#[derive(Debug)]
pub struct Handle<N: AstNode> {
  pub root: GreenNode,
  ptr: AstPtr<N>,
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

#[tracked]
pub struct ModuleNamespace {
  #[return_ref]
  pub terms: FxHashMap<Handle<TermDefn>, NameResTerm>,
  #[return_ref]
  pub effects: FxHashMap<Handle<EffectDefn>, NameResEffect>,
  #[return_ref]
  pub names: FxHashMap<Handle<Name>, InScopeName>,
}

enum NameResError {
  UndefinedName(Handle<Name>),
  WrongKind(NameKind, NameKinds, OpaqueHandle),
  DuplicateName {
    previous: OpaqueHandle,
    current: OpaqueHandle,
    kind: NameKind,
  },
}

#[salsa::tracked]
pub fn nameres_module(db: &dyn crate::Db, parse_module: ParseFile) -> ModuleNamespace {
  let cst_module = parse_module.data(db.as_parser_db());
  let module = parse_module.module(db.as_parser_db());
  let items = Items::new(cst_module.clone());

  let mut terms = FxHashMap::default();
  let mut effects = FxHashMap::default();
  let mut errors = vec![];
  let mut top_level_scope: FxHashMap<Ident, (InScopeName, OpaqueHandle)> = FxHashMap::default();
  let mut names = FxHashMap::default();

  let mut to_resolve_terms = vec![];
  let mut to_resolve_effects = vec![];
  // First pass over items collects all top level names and marks any duplicates.
  for item in items.items() {
    match item {
      Item::Term(term) => {
        let Some(name) = term.name() else {
          continue;
        };
        let ident = db.ident(name.text());
        let handle = Handle::new(cst_module.clone(), name);
        let name = TermName::new(db.as_core_db(), ident, module);
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
        let ident = db.ident(name.text());
        let handle = Handle::new(cst_module.clone(), name);
        let name = EffectName::new(db.as_core_db(), ident, module);
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

  let int_ident = db.ident_str("Int");
  let int_type = TypeName::new(db, int_ident, module);

  top_level_scope.insert(
    int_ident,
    (
      InScopeName::Type(int_type),
      OpaqueHandle::new(cst_module.clone(), Items::new(cst_module.clone())),
    ),
  );

  let mut errors = vec![];

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
      let ident = db.ident(name.text());
      let op_name = EffectOpName::new(db.as_core_db(), ident, effect_name);
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

  /*for (name, defn) in bodies {
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
      if let Some(scheme) = defn.annotation().and_then(|ann| ann.scheme()) {
        this.resolve_scheme(scheme, SchemeContext::Term(name));
      }
      if let Some(body) = defn.term() {
        this.resolve_term(body, name);
      }
    });

    term_defns.push(NameResTerm::new(
      db,
      name,
      Handle::new(cst_module.clone(), defn),
      ctx.var_gen.into_boxed_ids(),
      ctx.ty_var_gen.into_boxed_ids(),
    ))
  }*/

  let names = ModuleNamespace::new(db, terms, effects, names);
  for error in errors {
    PanoplyErrors::push(
      db,
      // TODO: Wrap this up as a helper.
      match error {
        NameResError::UndefinedName(handle) => {
          let name = handle.into_node();
          NameResolutionError::NotFound {
            name: db.ident(name.text()),
            span: name.syntax().text_range().into(),
            context_module: None,
            suggestions: vec![],
          }
        }
        NameResError::WrongKind(actual, expected, opaque_handle) => {
          let syn = opaque_handle.syntax();
          NameResolutionError::WrongKind {
            expr: syn.text_range().into(),
            actual,
            expected,
          }
        }
        NameResError::DuplicateName {
          previous,
          current,
          kind,
        } => {
          let syn = current.syntax();
          NameResolutionError::Duplicate {
            name: db.ident(syn.text().to_string()),
            kind,
            original: previous.ptr.text_range().into(),
            duplicate: syn.text_range().into(),
          }
        }
      }
      .into(),
    );
  }

  names
}

#[derive(Clone, Copy)]
enum SchemeContext {
  Term(TermName),
  Effect(EffectOpName),
}

struct NameResolution<'a> {
  db: &'a dyn crate::Db,
  root: GreenNode,
  ty_var_gen: IdGen<TyVarId, Handle<Name>>,
  var_gen: IdGen<VarId, Handle<Name>>,
  names: &'a mut FxHashMap<Handle<Name>, InScopeName>,
  scope: FxHashMap<Ident, (InScopeName, OpaqueHandle)>,
  errors: &'a mut Vec<NameResError>,
}

impl NameResolution<'_> {
  fn with_scope<T>(&mut self, body: impl FnOnce(&mut Self) -> T) -> T {
    let scope = self.scope.clone();
    let res = body(self);
    self.scope = scope;
    res
  }

  fn bind_ty_var(&mut self, name: Name, ctx: SchemeContext) {
    let ident = self.db.ident(name.text());
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
    let ident = self.db.ident(name.text());
    let handle = Handle::new(self.root.clone(), name);
    let var_id = self.var_gen.push(handle.clone());
    let name = InScopeName::TermVar(ctx, var_id);
    self.names.insert(handle.clone(), name);
    self.scope.insert(ident, (name, handle.opaque()));
  }

  fn resolve_ty_var(&mut self, name: Name, ctx: SchemeContext) {
    let ident = self.db.ident(name.text());

    let handle = Handle::new(self.root.clone(), name.clone());
    let resolved_name = match self.scope.get(&ident) {
      Some((name @ InScopeName::EffectTyVar(_, _), _))
      | Some((name @ InScopeName::TermTyVar(_, _), _))
      | Some((name @ InScopeName::Type(_), _)) => name,
      Some((other, _)) => {
        return self.errors.push(NameResError::WrongKind(
          other.kind(),
          NameKinds::TY_VAR,
          handle.opaque(),
        ));
      }
      None => return self.bind_ty_var(name, ctx),
    };
    self.names.insert(handle, *resolved_name);
  }

  fn resolve_var(&mut self, name: Name) {
    let ident = self.db.ident(name.text());
    let handle = Handle::new(self.root.clone(), name);
    let resolved_name = match self.scope.get(&ident) {
      Some((name @ InScopeName::TermVar(_, _), _)) => name,
      Some((name @ InScopeName::Term(_), _)) => name,
      Some((name @ InScopeName::Effect(_), _)) => name,
      Some((name @ InScopeName::EffectOp(_), _)) => name,
      Some((other, _)) => {
        self.errors.push(NameResError::WrongKind(
          other.kind(),
          NameKinds::VAR | NameKinds::ITEM | NameKinds::EFFECT | NameKinds::EFFECT_OP,
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
          let ident = self.db.ident(name.text());
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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
  pub fn module(&self, db: &dyn ::base::Db) -> Module {
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

  pub fn span(&self, db: &dyn crate::Db) -> Span {
    let core_db = db.as_core_db();
    let module = self.module(core_db);
    let namespace = db.nameres_module_of(module);
    let (handle, _) = namespace
      .names(db)
      .iter()
      .find(|(_, name)| *name == self)
      .expect("Name missing handle");
    let range: Range<usize> = handle.text_range().into();
    Span {
      start: Loc {
        file: FileId::from_id(salsa::Id::from_u32(0)),
        byte: range.start,
        line: 0,
        col: 0,
      },
      end: Loc {
        file: FileId::from_id(salsa::Id::from_u32(0)),
        byte: range.end,
        line: 0,
        col: 0,
      },
    }

    /*match self {
      InScopeName::Effect(eff) => {
        let module_names = &nameres_module.names(db.as_nameres_db())[&module];
        module_names.get(*eff).span()
      }
      InScopeName::EffectOp(eff_op) => {
        let effect = eff_op.effect(core_db);
        let nameres_module = db.nameres_module_of(module);
        let module_names = &nameres_module.names(db.as_nameres_db())[&module];
        module_names.get_effect(&effect).get(*eff_op).span()
      }
      InScopeName::Term(term) => {
        let nameres_module = db.nameres_module_of(module);
        let module_names = &nameres_module.names(db.as_nameres_db())[&module];
        module_names.get(*term).span()
      }
      InScopeName::TermTyVar(term, ty_var) => {
        db.nameres_term_of(*term).locals(db.as_nameres_db()).ty_vars[*ty_var].span()
      }
      InScopeName::TermVar(term, var) => {
        db.nameres_term_of(*term).locals(db.as_nameres_db()).vars[*var].span()
      }
      InScopeName::EffectTyVar(eff_op, ty_var) => {
        let effect = eff_op.effect(db.as_core_db());
        db.nameres_effect_of(effect)
          .locals(db.as_nameres_db())
          .ty_vars[*ty_var]
          .span()
      }
    }*/
  }
}

#[salsa::tracked]
fn nameres_module_of(db: &dyn crate::Db, module: Module) -> ModuleNamespace {
  let parse_module = db.parse_module_of(module);
  nameres_module(db, parse_module)
}

#[salsa::tracked]
fn item_name(db: &dyn crate::Db, term: TermName) -> Ident {
  term.name(db.as_core_db())
}

#[salsa::tracked]
fn id_for_name(db: &dyn crate::Db, module: Module, name: Ident) -> Option<TermName> {
  let namespace_module = db.nameres_module_of(module);
  namespace_module
    .terms(db.as_nameres_db())
    .values()
    .find_map(|term| {
      let term_name = term.name(db);
      if term_name.name(db.as_core_db()) == name {
        Some(term_name)
      } else {
        None
      }
    })
}

#[salsa::tracked]
pub fn effect_name(db: &dyn crate::Db, effect: EffectName) -> Ident {
  effect.name(db.as_core_db())
}

#[salsa::tracked]
pub fn effect_member_name(db: &dyn crate::Db, effect_op: EffectOpName) -> Ident {
  effect_op.name(db.as_core_db())
}

#[salsa::tracked(return_ref)]
pub fn effect_members(db: &dyn crate::Db, effect: EffectName) -> Vec<EffectOpName> {
  let module = effect.module(db.as_core_db());
  let name_res = db.nameres_module_of(module);

  let node = db.nameres_effect_of(effect).data(db);
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
pub fn lookup_effect_by_member_names(
  db: &dyn crate::Db,
  module: Module,
  members: Box<[Ident]>,
) -> Option<EffectName> {
  let mut members: Vec<Ident> = members.as_ref().to_vec();
  members.sort();

  lookup_effect_by(db, module, |_, eff_names|
            // if length's don't match up we don't need to iterate
            members.len() == eff_names.len()
                && eff_names
                    .iter()
                    .all(|name| members.binary_search(&name.name(db.as_core_db())).is_ok()))
}

#[salsa::tracked]
pub fn lookup_effect_by_name(
  db: &dyn crate::Db,
  module: Module,
  effect_name: Ident,
) -> Option<EffectName> {
  lookup_effect_by(db, module, |name, _| name == effect_name)
}

fn lookup_effect_by(
  db: &dyn crate::Db,
  module: Module,
  mut find_by: impl FnMut(Ident, &[EffectOpName]) -> bool,
) -> Option<EffectName> {
  let names = db.nameres_module_of(module);

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

#[salsa::tracked(return_ref)]
fn effect_handler_order(db: &dyn crate::Db, eff_name: EffectName) -> Vec<Ident> {
  let mut members = effect_members(db, eff_name)
    .iter()
    .map(|eff_op| effect_member_name(db, *eff_op))
    .collect::<Vec<_>>();

  // Insert `return` so it get's ordered as well.
  members.push(db.ident_str("return"));
  members.sort();
  members
}

#[salsa::tracked]
pub fn effect_handler_return_index(db: &dyn crate::Db, eff: EffectName) -> usize {
  let return_id = db.ident_str("return");
  effect_handler_order(db, eff)
    .binary_search(&return_id)
    .unwrap_or_else(|_| {
      panic!(
        "ICE: Created handler order for effect {:?} that did not contain `return`",
        eff.name(db.as_core_db()),
      )
    })
}

#[salsa::tracked]
pub fn effect_handler_op_index(db: &dyn crate::Db, eff_op: EffectOpName) -> usize {
  let member_id = effect_member_name(db, eff_op);
  let eff = eff_op.effect(db.as_core_db());
  effect_handler_order(db, eff)
    .binary_search(&member_id)
    .unwrap_or_else(|_| {
      panic!(
        "ICE: Created handler order for effect {:?} that did not contain member {:?}",
        eff.name(db.as_core_db()).text(db.as_core_db()),
        eff_op.name(db.as_core_db()).text(db.as_core_db())
      )
    })
}

#[salsa::tracked(return_ref)]
pub fn module_effects(db: &dyn crate::Db, module: Module) -> Vec<EffectName> {
  let nameres_module = db.nameres_module_of(module);
  nameres_module
    .effects(db)
    .values()
    .map(|effect| effect.name(db))
    .collect()
}

#[salsa::tracked(return_ref)]
pub fn all_effects(db: &dyn crate::Db) -> Vec<EffectName> {
  let module_tree = db.all_modules();
  let mut effects = module_tree
    .iter()
    .flat_map(|module| module_effects(db, *module).iter().copied())
    .collect::<Vec<_>>();

  effects.sort();
  effects
}

#[salsa::tracked]
pub fn effect_vector_index(db: &dyn crate::Db, effect: EffectName) -> usize {
  let effects = all_effects(db);
  effects.binary_search(&effect).unwrap_or_else(|_| {
    panic!(
      "ICE: {:?} expected effect to exist but it was not found",
      effect.name(db.as_core_db()).text(db.as_core_db())
    )
  })
}

#[salsa::tracked]
fn name_at_loc(db: &dyn crate::Db, file_id: FileId, line: u32, col: u32) -> Option<InScopeName> {
  let loc = db.locate(file_id, line, col)?;
  let file = db.file_for_id(file_id);

  let parse_file = db.parse_module(file);
  let cst = parse_file.data(db);
  let namespace = db.nameres_module_for_file(file);

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
    diagnostic::nameres::{NameKind, NameResolutionError},
    file::{FileId, SourceFile, SourceFileSet},
  };
  use cst::{Item, Items, TermDefn};
  use parser::{AstNode, Db};
  use pretty::{DocAllocator, DocBuilder, RcAllocator};

  use crate::{Db as NameResDb, InScopeName, ModuleNamespace};

  #[derive(Default)]
  #[salsa::db(crate::Jar, base::Jar, parser::Jar)]
  struct TestDatabase {
    storage: salsa::Storage<Self>,
  }
  impl salsa::Database for TestDatabase {}

  fn parse_resolve_module<S: ToString>(
    db: &TestDatabase,
    input: S,
  ) -> (Items, ModuleNamespace, Vec<NameResolutionError>) {
    let file_id = FileId::new(db, PathBuf::from("test"));
    let file = SourceFile::new(db, file_id, input.to_string());
    let _ = SourceFileSet::new(db, vec![file]);

    let nameres_module = db.nameres_module_for_file(file);

    let errors = db
      .all_nameres_errors()
      .into_iter()
      .map(|err| match err {
        base::diagnostic::error::PanoplyError::NameResError(name_res) => name_res,
        err => unreachable!("{:?}", err),
      })
      .collect();

    (
      Items::new(db.parse_module(file).data(db)),
      nameres_module,
      errors,
    )
  }

  fn parse_resolve_term(
    db: &TestDatabase,
    input: &str,
  ) -> (Option<TermDefn>, ModuleNamespace, Vec<NameResolutionError>) {
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

  impl ModuleNamespace {
    fn prettyprint(&self, db: &TestDatabase) -> String {
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
        errs[..],
        [
            NameResolutionError::NotFound {
                name: y,
                context_module: None,
                ..
            },
            NameResolutionError::NotFound {
                name: z,
                context_module: None,
                ..
            }
        ] => {
            assert_eq!(y.text(&db), "y");
            assert_eq!(z.text(&db), "z");
        }
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
        errs[..],
        [
            NameResolutionError::NotFound {
                name: h,
                context_module: None,
                ..
            },
            NameResolutionError::NotFound {
                name: x,
                context_module: None,
                ..
            }
        ] => {
            assert_eq!(h.text(&db), "h");
            assert_eq!(x.text(&db), "x");
        }
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
        errs[..],
        [
            NameResolutionError::NotFound {
                name: f,
                context_module: None,
                ..
            },
            NameResolutionError::NotFound {
                name: x,
                context_module: None,
                ..
            }
        ] => {
            assert_eq!(f.text(&db), "f");
            assert_eq!(x.text(&db), "x");
        }
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
        errs[..],
        [
            NameResolutionError::NotFound {
                name: y,
                context_module: None,
                ..
            },
            NameResolutionError::NotFound {
                name: x,
                context_module: None,
                ..
            }
        ] => {
            assert_eq!(y.text(&db), "y");
            assert_eq!(x.text(&db), "x");
        }
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
        errs[..],
        [NameResolutionError::NotFound {
            name: x,
            context_module: None,
            ..
        }] => {
            assert_eq!(x.text(&db), "x");
        }
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
        errs[..],
        [NameResolutionError::NotFound {
            name: x,
            context_module: None,
            ..
        }] => {
            assert_eq!(x.text(&db), "x");
        }
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
        errs[..],
        [
            NameResolutionError::NotFound {
                name: f,
                context_module: None,
                ..
            },
            NameResolutionError::NotFound {
                name: z,
                context_module: None,
                ..
            }
        ] => {
            assert_eq!(f.text(&db), "f");
            assert_eq!(z.text(&db), "z");
        }
    );
  }

  #[test]
  fn test_match_error_duplicate_name() {
    let db = TestDatabase::default();
    let (_, names, errs) = parse_resolve_term(&db, "match <{a = x, b = x} => x(x)>");
    assert_matches!(
        errs[..],
        [NameResolutionError::Duplicate {
            name: x,
            kind: NameKind::Var,
            original: Range { end, .. },
            duplicate: Range { start, ..},
        }] => {
            assert!(end < start, "{} < {}", end, start);
            assert_eq!(x.text(&db), "x");
        }
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
        errs[..],
        [
            NameResolutionError::NotFound {
                name: x,
                context_module: None,
                ..
            },
            NameResolutionError::NotFound {
                name: y,
                context_module: None,
                ..
            }
        ] => {
            assert_eq!(x.text(&db), "x");
            assert_eq!(y.text(&db), "y");
        }
    );
  }
}
