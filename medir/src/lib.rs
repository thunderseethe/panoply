use aiahr_core::id::{IdSupply, MedIrVarId};
use aiahr_core::modules::Module;
use aiahr_core::pretty::{PrettyPrint, PrettyWithCtx};
use aiahr_reducir::ReducIrTermName;

mod pretty;

#[salsa::jar(db = Db)]
pub struct Jar(MedIrItem, MedIrModule, MedIrTy);
pub trait Db: salsa::DbWithJar<Jar> + aiahr_reducir::Db {
    fn as_medir_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
    }

    fn mk_medir_ty(&self, kind: MedIrTyKind) -> MedIrTy {
        MedIrTy::new(self.as_medir_db(), kind)
    }
}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + aiahr_reducir::Db {}

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
pub struct WIPItem {
    pub name: MedIrItemName,
    pub ty: MedIrTy,
}

impl WIPItem {
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
    Closure(WIPItem, Vec<MedIrVar>),
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
                    (a == b)
                        .then_some(a)
                        .expect("All types of switch must be the same")
                })
                .expect("Switch should have atleast one branch"),
            MedIrKind::Call(fun, _) => match fun {
                Call::Known(item) => item.ty,
                Call::Unknown(var) => match var.ty.kind(db.as_medir_db()) {
                    MedIrTyKind::FunTy(_, ret_ty) => *ret_ty,
                    _ => todo!(),
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
        }
    }
}

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
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Call {
    /// A known function call we can dispatch statically
    Known(WIPItem),
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
