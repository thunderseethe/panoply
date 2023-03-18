use crate::id::{EffectOpId, ItemId, ModuleId};
use crate::ident::Ident;
use crate::span::Span;
use rustc_hash::FxHashMap;

pub mod indexed {
    use std::hash::Hash;

    use la_arena::{Arena, Idx};
    use rustc_hash::FxHashMap;

    use crate::id::{EffectOpId, ItemId, ModuleId};
    use crate::ident::Ident;
    use crate::indexed::{HasArena, IndexedAllocate};
    use crate::span::Span;

    use super::Direction;

    pub struct AstIndxAlloc<'b, 'a, Var> {
        terms: Arena<Term<Var>>,
        ref_spans: &'b FxHashMap<&'a super::Term<'a, Var>, Span>,
        idx_spans: FxHashMap<Idx<Term<Var>>, Span>,
    }
    impl<Var> HasArena<Term<Var>> for AstIndxAlloc<'_, '_, Var> {
        fn arena(&mut self) -> &mut Arena<Term<Var>> {
            &mut self.terms
        }
    }

    /// A Term of the AST
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub enum Term<Var> {
        // Function abstraction, a closure, a lambda etc.
        Abstraction {
            arg: Var,
            body: Idx<Self>,
        },
        // Function application
        Application {
            func: Idx<Self>,
            arg: Idx<Self>,
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
            label: Ident,
            term: Idx<Self>,
        },
        // Unlabel a term, this is used to project a product into one of it's fields.
        Unlabel {
            label: Ident,
            term: Idx<Self>,
        },
        // Concat two rows into a larger row
        Concat {
            left: Idx<Self>,
            right: Idx<Self>,
        },
        // Project a product out into a subproduct
        Project {
            direction: Direction,
            term: Idx<Self>,
        },
        Branch {
            left: Idx<Self>,
            right: Idx<Self>,
        },
        Inject {
            direction: Direction,
            term: Idx<Self>,
        },
        // An effect operation
        Operation(EffectOpId),
        Handle {
            handler: Idx<Self>,
            body: Idx<Self>,
        },
    }

    impl<'a, Var: Copy + Eq + Hash> IndexedAllocate<AstIndxAlloc<'_, 'a, Var>>
        for &'a super::Term<'a, Var>
    {
        type Out = Idx<Term<Var>>;

        fn alloc(&self, alloc: &mut AstIndxAlloc<'_, 'a, Var>) -> Self::Out {
            let span = alloc.ref_spans[*self];
            let term = match self {
                super::Term::Abstraction { arg, body } => Term::Abstraction {
                    arg: *arg,
                    body: body.alloc(alloc),
                },
                super::Term::Application { func, arg } => Term::Application {
                    func: func.alloc(alloc),
                    arg: arg.alloc(alloc),
                },
                super::Term::Variable(var) => Term::Variable(*var),
                super::Term::Int(i) => Term::Int(*i),
                super::Term::Item(ids) => Term::Item(*ids),
                super::Term::Unit => Term::Unit,
                super::Term::Label { label, term } => Term::Label {
                    label: *label,
                    term: term.alloc(alloc),
                },
                super::Term::Unlabel { label, term } => Term::Unlabel {
                    label: *label,
                    term: term.alloc(alloc),
                },
                super::Term::Concat { left, right } => Term::Concat {
                    left: left.alloc(alloc),
                    right: right.alloc(alloc),
                },
                super::Term::Project { direction, term } => Term::Project {
                    direction: *direction,
                    term: term.alloc(alloc),
                },
                super::Term::Branch { left, right } => Term::Branch {
                    left: left.alloc(alloc),
                    right: right.alloc(alloc),
                },
                super::Term::Inject { direction, term } => Term::Inject {
                    direction: *direction,
                    term: term.alloc(alloc),
                },
                super::Term::Operation(ids) => Term::Operation(*ids),
                super::Term::Handle { handler, body } => Term::Handle {
                    handler: handler.alloc(alloc),
                    body: body.alloc(alloc),
                },
            };
            let idx = alloc.arena().alloc(term);
            alloc.idx_spans.insert(idx, span);
            idx
        }
    }

    /// Abstract Syntax Tree (AST)
    pub struct Ast<Var> {
        // We store spans of the Ast out of band because we won't need them for most operations.
        pub spans: FxHashMap<Idx<Term<Var>>, Span>,
        pub tree: Idx<Term<Var>>,
    }

    impl<'a, Var: Copy + Eq + Hash> From<&super::Ast<'a, Var>> for Ast<Var> {
        fn from(value: &super::Ast<'a, Var>) -> Self {
            let mut alloc = AstIndxAlloc {
                terms: Arena::default(),
                ref_spans: &value.spans,
                idx_spans: FxHashMap::default(),
            };
            let tree = value.tree.alloc(&mut alloc);
            Ast {
                spans: alloc.idx_spans,
                tree,
            }
        }
    }
}

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
        label: Ident,
        term: &'a Term<'a, Var>,
    },
    // Unlabel a term, this is used to project a product into one of it's fields.
    Unlabel {
        label: Ident,
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
        self.stack.pop().map(|term| {
            self.stack.extend(term.children());
            term
        })
    }
}
