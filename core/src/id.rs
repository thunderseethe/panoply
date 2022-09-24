/// An ID for a module. Unique within a compilation session.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct ModuleId(pub usize);

/// An ID for a top-level item in a module. Unique within a module.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct ItemId(pub usize);

/// An ID for a local variable. Unique within a module.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct VarId(pub usize);
