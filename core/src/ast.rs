use crate::id::{EffectId, EffectOpId, ItemId, ModuleId};
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
    /// Return an iterator over all instances of variables in this term.
    pub fn vars(&'a self) -> impl Iterator<Item = &'a Var> {
        TermTraverse::new(self).filter_map(|term| match term {
            Term::Abstraction { arg, .. } => Some(arg),
            Term::Variable(var) => Some(var),
            _ => None,
        })
    }

    /// Return an iterator over all terms that require row evidence in this term.
    pub fn row_ev_terms(&'a self) -> impl Iterator<Item = RowTermView<'a, Var>> {
        TermTraverse::new(self).filter_map(|term| RowTermView::try_from(term).ok())
    }

    fn children(&'a self) -> ZeroOneOrTwo<&'a Term<'a, Var>> {
        match self {
            Term::Abstraction { body, .. } => ZeroOneOrTwo::One(body),
            Term::Application { func, arg } => ZeroOneOrTwo::Two(func, arg),
            Term::Label { term, .. } | Term::Unlabel { term, .. } => ZeroOneOrTwo::One(term),
            Term::Concat { left, right } | Term::Branch { left, right } => {
                ZeroOneOrTwo::Two(left, right)
            }
            Term::Project { term, .. } | Term::Inject { term, .. } => ZeroOneOrTwo::One(term),
            Term::Handle { handler, body, .. } => ZeroOneOrTwo::Two(handler, body),
            Term::Variable(_) | Term::Int(_) | Term::Item(_) | Term::Unit | Term::Operation(_) => {
                ZeroOneOrTwo::Zero
            }
        }
    }
}

/// Convenience wrapper around a term to encode that is must be a row term (Concat, Branch, etc.) and not any other kind
/// of term.
pub struct RowTermView<'a, Var> {
    pub parent: &'a Term<'a, Var>,
    pub view: RowTerm<'a, Var>,
}
pub enum RowTerm<'a, Var> {
    Concat {
        left: &'a Term<'a, Var>,
        right: &'a Term<'a, Var>,
    },
    Branch {
        left: &'a Term<'a, Var>,
        right: &'a Term<'a, Var>,
    },
    Project {
        direction: Direction,
        term: &'a Term<'a, Var>,
    },
    Inject {
        direction: Direction,
        term: &'a Term<'a, Var>,
    },
}
impl<'a, Var> From<RowTermView<'a, Var>> for &'a Term<'a, Var> {
    fn from(value: RowTermView<'a, Var>) -> Self {
        value.parent
    }
}
impl<'a, Var> TryFrom<&'a Term<'a, Var>> for RowTermView<'a, Var> {
    type Error = &'a Term<'a, Var>;

    fn try_from(parent: &'a Term<'a, Var>) -> Result<Self, Self::Error> {
        let view = match parent {
            Term::Concat { left, right } => RowTerm::Concat { left, right },
            Term::Project { direction, term } => RowTerm::Project {
                direction: *direction,
                term,
            },
            Term::Branch { left, right } => RowTerm::Branch { left, right },
            Term::Inject { direction, term } => RowTerm::Inject {
                direction: *direction,
                term,
            },
            _ => return Err(parent),
        };
        Ok(RowTermView { parent, view })
    }
}

/// Pass 0, 1, or 2 Ts on the stack as an iterator
enum ZeroOneOrTwo<T> {
    Zero,
    One(T),
    Two(T, T),
}
impl<T> Iterator for ZeroOneOrTwo<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let zot = std::mem::replace(self, ZeroOneOrTwo::Zero);
        match zot {
            ZeroOneOrTwo::Zero => None,
            ZeroOneOrTwo::One(fst) => Some(fst),
            ZeroOneOrTwo::Two(fst, snd) => {
                *self = ZeroOneOrTwo::One(snd);
                Some(fst)
            }
        }
    }
}

/// Traverse a term as an iterator, producing each child term.
struct TermTraverse<'a, Var> {
    stack: Vec<&'a Term<'a, Var>>,
}

impl<'a, Var> TermTraverse<'a, Var> {
    fn new(root: &'a Term<'a, Var>) -> Self {
        Self { stack: vec![root] }
    }
}
impl<'a, Var> Iterator for TermTraverse<'a, Var> {
    type Item = &'a Term<'a, Var>;

    fn next(&mut self) -> Option<Self::Item> {
        self.stack.pop().and_then(|term| {
            self.stack.extend(term.children());
            Some(term)
        })
    }
}
