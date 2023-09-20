use aiahr_core::id::MedIrVarId;
use aiahr_reducir::ReducIrTermName;

pub struct MedIrVar {
    pub id: MedIrVarId,
}

pub struct MedIrItem(ReducIrTermName);

pub enum Atom {
    Var(MedIrVar),
    Int(usize),
}
/// TODO: Document how this differs from ReducIr
pub enum MedIrKind {
    /// A flat sequence of values
    Blocks(Vec<Atom>),
    /// Index into a block producing the value stored
    BlockAccess(MedIrVar, Atom),
    /// Switch on an int dispatching control to one of the branches based on the int's value.
    Switch(Atom, Vec<Locals>),
    /// Function call
    Call(Call, Vec<Atom>),
    /// Allocate a closure capturing the listed vars
    Closure(MedIrItem, Vec<MedIrVar>),
}

pub struct MedIr {
    pub kind: MedIrKind,
}

pub enum Call {
    /// A known function call we can dispatch statically
    Known(MedIrItem),
    /// An unknown function call that we have to apply
    Unknown(MedIrVar),
}

/// A series of binding that are scoped locally to body.
pub struct Locals {
    pub binds: Vec<(MedIrVar, MedIr)>,
    pub body: MedIr,
}

/// A top level definition is composed of a name, its parameters, and a Locals acting as the body
pub struct Defn {
    pub name: MedIrItem,
    pub params: Vec<MedIrVar>,
    pub body: Locals,
}
