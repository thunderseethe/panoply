use base::{
  id::{IdSupply, MedIrVarId},
  modules::Module,
  pretty::{PrettyPrint, PrettyWithCtx},
};
use reducir::ReducIrTermName;

mod pretty;

#[salsa::jar(db = Db)]
pub struct Jar(MedIrItem, MedIrModule, MedIrTy);
pub trait Db: salsa::DbWithJar<Jar> + reducir::Db {
  fn as_medir_db(&self) -> &dyn crate::Db {
    <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
  }

  fn mk_medir_ty(&self, kind: MedIrTyKind) -> MedIrTy {
    MedIrTy::new(self.as_medir_db(), kind)
  }
}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + reducir::Db {}

pub trait MedIrFoldInPlace {
  fn fold_atom(&mut self, _atom: &mut Atom) {}
  fn fold_medir(&mut self, _kind: &mut MedIrKind) {}
}
pub trait MedIrVisit {
  fn visit_atom(&mut self, _atom: &Atom) {}
  fn visit_medir(&mut self, _kind: &MedIrKind) {}
}
pub trait MedIrTraversal {
  fn fold<F: ?Sized + MedIrFoldInPlace>(&mut self, fold: &mut F);
  fn visit<V: ?Sized + MedIrVisit>(&self, visitor: &mut V);
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct PartialAppArity {
  pub arity: usize,
  pub provided_args: usize,
}

#[derive(Default)]
pub struct CallAritys {
  calls: usize,
}
impl CallAritys {
  pub fn new() -> Self {
    Self::default()
  }
}

impl CallAritys {
  fn add_closure_arity_if_present(&mut self, kind: &MedIrKind) {
    if let MedIrKind::Call(Call::Unknown(_), args) = kind {
      self.calls = max(self.calls, args.len());
    }
  }

  pub fn into_calls(self) -> usize {
    self.calls
  }
}
impl MedIrFoldInPlace for CallAritys {
  fn fold_medir(&mut self, kind: &mut MedIrKind) {
    self.add_closure_arity_if_present(kind);
  }
}
impl MedIrVisit for CallAritys {
  fn visit_medir(&mut self, kind: &MedIrKind) {
    self.add_closure_arity_if_present(kind);
  }
}

#[salsa::tracked]
pub struct MedIrItem {
  #[id]
  pub name: MedIrItemName,
  #[return_ref]
  pub item: Defn,
  #[return_ref]
  pub var_supply: IdSupply<MedIrVarId>,
}

#[salsa::tracked]
pub struct MedIrModule {
  #[id]
  pub module: Module,
  #[return_ref]
  pub items: Vec<MedIrItem>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MedIrVar {
  pub id: MedIrVarId,
  pub ty: MedIrTy,
}

impl From<(MedIrVarId, MedIrTy)> for MedIrVar {
  fn from(value: (MedIrVarId, MedIrTy)) -> Self {
    Self::new(value.0, value.1)
  }
}

impl MedIrVar {
  pub fn new(id: MedIrVarId, ty: MedIrTy) -> Self {
    Self { id, ty }
  }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, PartialOrd, Ord, Hash)]
pub struct MedIrItemName(pub ReducIrTermName);

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct MedIrTypedItem {
  pub name: MedIrItemName,
  pub ty: MedIrTy,
}

impl MedIrTypedItem {
  pub fn new(name: MedIrItemName, ty: MedIrTy) -> Self {
    Self { name, ty }
  }
}

impl MedIrItemName {
  pub fn new(name: ReducIrTermName) -> Self {
    Self(name)
  }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum Atom {
  Var(MedIrVar),
  Int(usize),
}
impl Atom {
  pub fn type_of<DB: ?Sized + crate::Db>(&self, db: &DB) -> MedIrTy {
    match self {
      Atom::Var(v) => v.ty,
      Atom::Int(_) => db.mk_medir_ty(MedIrTyKind::IntTy),
    }
  }
}

/// TODO: Document how this differs from ReducIr
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum MedIrKind {
  Atom(Atom),
  /// A flat sequence of values
  Blocks(Vec<Atom>),
  /// Index into a block producing the value stored
  BlockAccess(MedIrVar, usize),
  /// Switch on an int dispatching control to one of the branches based on the int's value.
  Switch(Atom, Vec<Locals>),
  /// Function call
  Call(Call, Vec<Atom>),
  /// Allocate a closure capturing the listed vars
  Closure(MedIrTypedItem, Vec<MedIrVar>),
  /// Cast a medirterm as a specific type.
  /// This is erased at runtime and just exists for typechecking.
  Typecast(MedIrTy, Box<MedIr>),
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum MedIrTyKind {
  IntTy,
  BlockTy(Vec<MedIrTy>),
  FunTy(Vec<MedIrTy>, MedIrTy),
}

#[salsa::interned]
pub struct MedIrTy {
  #[return_ref]
  kind_: MedIrTyKind,
}

impl MedIrTy {
  // Convenience wrapper to cast DB for us
  pub fn kind<DB: ?Sized + crate::Db>(self, db: &DB) -> &MedIrTyKind {
    self.kind_(db.as_medir_db())
  }

  pub fn try_as_fun_ty<DB: ?Sized + crate::Db>(self, db: &DB) -> Option<(&[MedIrTy], MedIrTy)> {
    match self.kind_(db.as_medir_db()) {
      MedIrTyKind::IntTy | MedIrTyKind::BlockTy(_) => None,
      MedIrTyKind::FunTy(args, ret) => Some((args, *ret)),
    }
  }
}

impl MedIrTy {
  pub fn block_size<DB: ?Sized + crate::Db>(&self, db: &DB) -> usize {
    match self.kind(db.as_medir_db()) {
      MedIrTyKind::IntTy => 1,
      // Represented as a function reference so fits in one block
      MedIrTyKind::FunTy(_, _) => 1,
      MedIrTyKind::BlockTy(elems) => elems.len(),
    }
  }
}

#[derive(PartialEq, Eq, Clone)]
pub struct MedIr {
  pub kind: MedIrKind,
}
impl MedIr {
  pub fn type_of<DB: ?Sized + crate::Db>(&self, db: &DB) -> MedIrTy {
    match &self.kind {
      MedIrKind::Atom(atom) => atom.type_of(db),
      MedIrKind::Blocks(elems) => db.mk_medir_ty(MedIrTyKind::BlockTy(
        elems.iter().map(|atom| atom.type_of(db)).collect(),
      )),
      MedIrKind::BlockAccess(v, indx) => match v.ty.kind(db.as_medir_db()) {
        MedIrTyKind::BlockTy(blocks) => blocks[*indx],
        _ => todo!(),
      },
      MedIrKind::Switch(_, branches) => branches
        .iter()
        .map(|branch| branch.type_of(db))
        .reduce(|a, b| {
          (a == b).then_some(a).unwrap_or_else(|| {
            panic!(
              "All types of switch must be the same:\na: {}\nb: {}",
              a.pretty_with(db).pprint().pretty(80),
              b.pretty_with(db).pprint().pretty(80)
            )
          })
        })
        .unwrap_or_else(|| db.mk_medir_ty(MedIrTyKind::BlockTy(vec![]))),
      MedIrKind::Call(fun, _) => match fun {
        Call::Known(item) => match item.ty.kind(db.as_medir_db()) {
          MedIrTyKind::FunTy(_, ret_ty) => *ret_ty,
          _ => todo!(),
        },
        Call::Unknown(var) => match var.ty.kind(db.as_medir_db()) {
          MedIrTyKind::FunTy(_, ret_ty) => *ret_ty,
          kind => {
            todo!("{}", kind.pretty_with(db).pprint().pretty(80))
          }
        },
      },
      MedIrKind::Closure(item, args) => {
        let (args_ty, ret) = match item.ty.kind(db) {
          MedIrTyKind::FunTy(args, ret) => (args, ret),
          kind => unreachable!(
            "{} {:?}",
            item.name.pretty_with(db).pprint().pretty(80),
            kind
          ),
        };
        if args_ty.len() == args.len() {
          *ret
        } else {
          db.mk_medir_ty(MedIrTyKind::FunTy(
            args_ty.iter().skip(args.len()).copied().collect(),
            *ret,
          ))
        }
      }
      MedIrKind::Typecast(ty, _) => *ty,
    }
  }
}
impl MedIrTraversal for MedIr {
  fn fold<F: ?Sized + MedIrFoldInPlace>(&mut self, fold: &mut F) {
    self.default_traverse_mut(fold)
  }

  fn visit<F: ?Sized + MedIrVisit>(&self, fold: &mut F) {
    self.default_traverse(fold)
  }
}

use std::cmp::max;
use std::fmt;
impl fmt::Debug for MedIr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.kind.fmt(f)
  }
}
impl MedIr {
  pub fn new(kind: MedIrKind) -> Self {
    MedIr { kind }
  }

  pub fn int(int: usize) -> Self {
    MedIr::new(MedIrKind::Atom(Atom::Int(int)))
  }

  pub fn var(var: MedIrVar) -> Self {
    MedIr::new(MedIrKind::Atom(Atom::Var(var)))
  }

  fn default_traverse_mut<F: ?Sized + MedIrFoldInPlace>(&mut self, fold: &mut F) {
    match &mut self.kind {
      MedIrKind::Atom(atom) => {
        fold.fold_atom(atom);
      }
      MedIrKind::Blocks(atoms) => {
        for atom in atoms.iter_mut() {
          fold.fold_atom(atom);
        }
      }
      MedIrKind::BlockAccess(_, _) => {}
      MedIrKind::Switch(atom, locals) => {
        fold.fold_atom(atom);
        locals.iter_mut().for_each(|local| local.fold(fold))
      }
      MedIrKind::Call(_, atoms) => {
        atoms.iter_mut().for_each(|atom| {
          fold.fold_atom(atom);
        });
      }
      MedIrKind::Closure(_, _) => {}
      MedIrKind::Typecast(_, term) => fold.fold_medir(&mut term.kind),
    }
    fold.fold_medir(&mut self.kind);
  }

  fn default_traverse<V: ?Sized + MedIrVisit>(&self, visit: &mut V) {
    match &self.kind {
      MedIrKind::Atom(atom) => {
        visit.visit_atom(atom);
      }
      MedIrKind::Blocks(atoms) => {
        for atom in atoms.iter() {
          visit.visit_atom(atom);
        }
      }
      MedIrKind::BlockAccess(_, _) => {}
      MedIrKind::Switch(atom, locals) => {
        visit.visit_atom(atom);
        for local in locals.iter() {
          local.visit(visit)
        }
      }
      MedIrKind::Call(_, atoms) => {
        for atom in atoms.iter() {
          visit.visit_atom(atom);
        }
      }
      MedIrKind::Closure(_, _) => {}
      MedIrKind::Typecast(_, term) => visit.visit_medir(&term.kind),
    }
    visit.visit_medir(&self.kind);
  }
}

impl MedIrTraversal for Locals {
  fn fold<F: ?Sized + MedIrFoldInPlace>(&mut self, fold: &mut F) {
    for (_, defn) in self.binds.iter_mut() {
      defn.fold(fold)
    }
    self.body.fold(fold)
  }

  fn visit<V: ?Sized + MedIrVisit>(&self, visitor: &mut V) {
    for (_, defn) in self.binds.iter() {
      defn.visit(visitor)
    }
    self.body.visit(visitor)
  }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Call {
  /// A known function call we can dispatch statically
  Known(MedIrTypedItem),
  /// An unknown function call that we have to apply
  Unknown(MedIrVar),
}

/// A series of binding that are scoped locally to body.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Locals {
  pub binds: Vec<(MedIrVar, MedIr)>,
  pub body: MedIr,
}

impl Locals {
  pub fn type_of<DB: ?Sized + crate::Db>(&self, db: &DB) -> MedIrTy {
    self.body.type_of(db)
  }
}

/// A top level definition is composed of a name, its parameters, and a Locals acting as the body
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Defn {
  pub name: MedIrItemName,
  pub params: Vec<MedIrVar>,
  pub body: Locals,
}

impl Defn {
  pub fn type_of<DB: ?Sized + crate::Db>(&self, db: &DB) -> MedIrTy {
    db.mk_medir_ty(MedIrTyKind::FunTy(
      self.params.iter().map(|var| var.ty).collect(),
      self.body.type_of(db),
    ))
  }
}
