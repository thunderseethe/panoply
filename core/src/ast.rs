use crate::id::{EffectId, EffectOpId, ItemId, ModuleId, TyVarId};
use crate::memory::handle::RefHandle;
use crate::span::Span;
use rustc_hash::FxHashMap;

/// Abstract Syntax Tree (AST)
pub struct Ast<'a, Var> {
    // We store spans of the Ast out of band because we won't need them for most operations
    spans: FxHashMap<&'a Term<'a, Var>, Span>,
    pub tree: &'a Term<'a, Var>,
}

impl<'a, Var> Ast<'a, Var> {
    pub fn new(spans: FxHashMap<&'a Term<'a, Var>, Span>, tree: &'a Term<'a, Var>) -> Self {
        Self { spans, tree }
    }

    /// Get the root node of this Ast
    pub fn root(&self) -> &'a Term<'a, Var> {
        self.tree
    }

    pub fn vars(&self) -> impl Iterator<Item = &'a Var> {
        self.tree.vars()
    }
}

impl<'a, Var: Eq + std::hash::Hash> Ast<'a, Var> {
    /// Lookup the span of a node within this Ast
    pub fn span_of(&self, node: &'a Term<'a, Var>) -> Option<&Span> {
        self.spans.get(node)
    }
}

/// Direction of a row operation.
/// When operating on a row we often split the row into a left and right row (concatenation,
/// branching, injecting, etc.). When row combination is not commutative it matters whether an
/// operation is using the left row, or right row. So we track it with this enum
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Direction {
    Left,
    Right,
}

/// A Term of the AST
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Term<'a, Var> {
    // Function abstraction, a closure, a lambda etc.
    Abstraction {
        arg: Var,
        body: &'a Term<'a, Var>,
    },
    // Function application
    Application {
        func: &'a Term<'a, Var>,
        arg: &'a Term<'a, Var>,
    },
    // A local variable binding
    Variable(Var),
    Int(usize),
    // A global variable binding
    Item((ModuleId, ItemId)),
    // A unit value
    // Because all products are represented in terms of concat, we don't actually have a way to
    // represent unit at this level
    Unit,
    // Label a term, used in construction of Product and Sum types.
    Label {
        label: RefHandle<'a, str>,
        term: &'a Term<'a, Var>,
    },
    // Unlabel a term, this is used to project a product into one of it's fields.
    Unlabel {
        label: RefHandle<'a, str>,
        term: &'a Term<'a, Var>,
    },
    // Concat two rows into a larger row
    Concat {
        left: &'a Term<'a, Var>,
        right: &'a Term<'a, Var>,
    },
    // Project a product out into a subproduct
    Project {
        direction: Direction,
        term: &'a Term<'a, Var>,
    },
    Branch {
        left: &'a Term<'a, Var>,
        right: &'a Term<'a, Var>,
    },
    Inject {
        direction: Direction,
        term: &'a Term<'a, Var>,
    },
    // An effect operation
    Operation(EffectOpId),
    Handle {
        eff: EffectId,
        handler: &'a Term<'a, Var>,
        body: &'a Term<'a, Var>,
    },
}

impl<'a, Var> Term<'a, Var> {
    fn vars(&'a self) -> impl Iterator<Item = &'a Var> {
        TermVars { stack: vec![self] }
    }
}

struct TermVars<'a, Var> {
    stack: Vec<&'a Term<'a, Var>>,
}
impl<'a, Var> Iterator for TermVars<'a, Var> {
    type Item = &'a Var;

    fn next(&mut self) -> Option<Self::Item> {
        self.stack.pop().and_then(|term| match term {
            Term::Abstraction { arg, body } => {
                self.stack.push(body);
                Some(arg)
            }
            Term::Application { func, arg } => {
                self.stack.extend([func, arg]);
                self.next()
            }
            Term::Variable(var) => Some(var),
            Term::Operation(_) => self.next(),
            Term::Item(_) => self.next(),
            Term::Unit => self.next(),
            Term::Concat { left, right } => {
                self.stack.extend([left, right]);
                self.next()
            }
            Term::Project { term, .. } => {
                self.stack.push(term);
                self.next()
            }
            Term::Branch { left, right } => {
                self.stack.extend([left, right]);
                self.next()
            }
            Term::Inject { term, .. } => {
                self.stack.push(term);
                self.next()
            }
            Term::Label { term, .. } => {
                self.stack.push(term);
                self.next()
            }
            Term::Unlabel { term, .. } => {
                self.stack.push(term);
                self.next()
            }
            Term::Handle { handler, body, .. } => {
                self.stack.extend([handler, body]);
                self.next()
            }
            Term::Int(_) => self.next(),
        })
    }
}
