use aiahr_core::id::{IdSupply, MedIrVarId};
use aiahr_core::modules::Module;
use aiahr_reducir::ReducIrTermName;

mod pretty;

#[salsa::jar(db = Db)]
pub struct Jar(MedIrItem, MedIrModule);
pub trait Db: salsa::DbWithJar<Jar> {
    fn as_medir_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
    }
}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> {}

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
}

impl MedIrVar {
    pub fn new(id: MedIrVarId) -> Self {
        Self { id }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, PartialOrd, Ord, Hash)]
pub struct MedIrItemName(pub ReducIrTermName);

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
    Closure(MedIrItemName, Vec<MedIrVar>),
}

#[derive(PartialEq, Eq, Clone)]
pub struct MedIr {
    pub kind: MedIrKind,
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
    Known(MedIrItemName),
    /// An unknown function call that we have to apply
    Unknown(MedIrVar),
}

/// A series of binding that are scoped locally to body.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Locals {
    pub binds: Vec<(MedIrVar, MedIr)>,
    pub body: MedIr,
}

/// A top level definition is composed of a name, its parameters, and a Locals acting as the body
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Defn {
    pub name: MedIrItemName,
    pub params: Vec<MedIrVar>,
    pub body: Locals,
}
