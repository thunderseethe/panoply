use crate::id::{EffectId, EffectOpId, ItemId, ModuleId};
use crate::ident::Ident;
use crate::modules::Module;
use crate::span::Span;
use crate::ty::{Ty, TyScheme};
use rustc_hash::FxHashMap;

pub mod indexed {
    use std::fmt::Debug;
    use std::hash::Hash;

    use bumpalo::Bump;
    use la_arena::{Arena, Idx};
    use rustc_hash::FxHashMap;

    use crate::id::{EffectId, EffectOpId, ItemId, ModuleId, VarId};
    use crate::ident::Ident;
    use crate::indexed::{
        HasArenaMut, HasArenaRef, HasRefArena, IndexedAllocate, ReferenceAllocate,
    };
    use crate::span::Span;
    use crate::ty::{Ty, TyScheme};

    use super::Direction;

    pub struct AstIndxAlloc<'b, 'a, Var> {
        terms: Arena<Term<Var>>,
        ref_spans: &'b FxHashMap<&'a super::Term<'a, Var>, Span>,
        idx_spans: FxHashMap<Idx<Term<Var>>, Span>,
    }
    impl<Var> HasArenaRef<Term<Var>> for AstIndxAlloc<'_, '_, Var> {
        fn arena(&self) -> &Arena<Term<Var>> {
            &self.terms
        }
    }
    impl<Var> HasArenaMut<Term<Var>> for AstIndxAlloc<'_, '_, Var> {
        fn arena_mut(&mut self) -> &mut Arena<Term<Var>> {
            &mut self.terms
        }
    }

    pub struct AstRefAlloc<'b, 'a, Var> {
        arena: &'a Bump,
        terms: &'b Arena<Term<Var>>,
        ref_spans: FxHashMap<&'a super::Term<'a, Var>, Span>,
        idx_spans: &'b FxHashMap<Idx<Term<Var>>, Span>,
        mapping: FxHashMap<&'a super::Term<'a, Var>, Idx<Term<Var>>>,
    }
    impl<'a, Var> HasRefArena<'a> for AstRefAlloc<'_, 'a, Var> {
        fn ref_arena(&self) -> &'a Bump {
            self.arena
        }
    }
    impl<Var> HasArenaRef<Term<Var>> for AstRefAlloc<'_, '_, Var> {
        fn arena(&self) -> &Arena<Term<Var>> {
            self.terms
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
        Operation((ModuleId, EffectId, EffectOpId)),
        Handle {
            handler: Idx<Self>,
            body: Idx<Self>,
        },
        Annotated {
            ty: Ty,
            term: Idx<Term<Var>>,
        },
    }

    use super::ZeroOneOrTwo;
    impl<Var: Eq> Term<Var> {
        /// Return an iterator over all terms that require row evidence in this term.
        pub fn row_ev_terms<'a>(
            &self,
            arena: &'a Arena<Term<Var>>,
        ) -> impl Iterator<Item = RowTermView<Var>> + 'a {
            let index = arena
                .iter()
                .position(|(_, term)| self == term)
                .unwrap_or_else(|| panic!("Expected arena to contain term but it did not"))
                as u32;
            let root_idx = Idx::from_raw(index.into());
            TermTraverse::new(root_idx, arena).filter_map(|term| RowTermView::try_from(term).ok())
        }
    }

    impl<Var> Term<Var> {
        fn children(&self) -> ZeroOneOrTwo<Idx<Term<Var>>> {
            match self {
                Term::Abstraction { body, .. } => ZeroOneOrTwo::One(*body),
                Term::Application { func, arg } => ZeroOneOrTwo::Two(*func, *arg),
                Term::Label { term, .. } | Term::Unlabel { term, .. } => ZeroOneOrTwo::One(*term),
                Term::Concat { left, right } | Term::Branch { left, right } => {
                    ZeroOneOrTwo::Two(*left, *right)
                }
                Term::Project { term, .. } | Term::Inject { term, .. } => ZeroOneOrTwo::One(*term),
                Term::Handle { handler, body, .. } => ZeroOneOrTwo::Two(*handler, *body),
                Term::Variable(_)
                | Term::Int(_)
                | Term::Item(_)
                | Term::Unit
                | Term::Operation(_) => ZeroOneOrTwo::Zero,
                Term::Annotated { term, .. } => ZeroOneOrTwo::One(*term),
            }
        }
    }

    /// Convenience wrapper around a term to encode that is must be a row term (Concat, Branch, etc.) and not any other kind
    /// of term.
    pub struct RowTermView<Var> {
        pub parent: Idx<Term<Var>>,
        pub view: RowTerm<Var>,
    }
    pub enum RowTerm<Var> {
        Concat {
            left: Idx<Term<Var>>,
            right: Idx<Term<Var>>,
        },
        Branch {
            left: Idx<Term<Var>>,
            right: Idx<Term<Var>>,
        },
        Project {
            direction: Direction,
            term: Idx<Term<Var>>,
        },
        Inject {
            direction: Direction,
            term: Idx<Term<Var>>,
        },
    }

    impl<Var> TryFrom<(Idx<Term<Var>>, &Term<Var>)> for RowTermView<Var> {
        type Error = Idx<Term<Var>>;

        fn try_from((parent, view): (Idx<Term<Var>>, &Term<Var>)) -> Result<Self, Self::Error> {
            let view = match view {
                Term::Concat { left, right } => RowTerm::Concat {
                    left: *left,
                    right: *right,
                },
                Term::Project { direction, term } => RowTerm::Project {
                    direction: *direction,
                    term: *term,
                },
                Term::Branch { left, right } => RowTerm::Branch {
                    left: *left,
                    right: *right,
                },
                Term::Inject { direction, term } => RowTerm::Inject {
                    direction: *direction,
                    term: *term,
                },
                _ => return Err(parent),
            };
            Ok(RowTermView { parent, view })
        }
    }

    /// Traverse a term as an iterator, producing each child term.
    struct TermTraverse<'a, Var> {
        arena: &'a Arena<Term<Var>>,
        stack: Vec<Idx<Term<Var>>>,
    }

    impl<'a, Var> TermTraverse<'a, Var> {
        fn new(root: Idx<Term<Var>>, arena: &'a Arena<Term<Var>>) -> Self {
            Self {
                stack: vec![root],
                arena,
            }
        }
    }
    impl<'a, Var> Iterator for TermTraverse<'a, Var> {
        type Item = (Idx<Term<Var>>, &'a Term<Var>);

        fn next(&mut self) -> Option<Self::Item> {
            self.stack.pop().map(|idx| {
                let view = &self.arena[idx];
                self.stack.extend(view.children());
                (idx, view)
            })
        }
    }

    impl<'a, Var: Copy + Eq + Hash + std::fmt::Debug> IndexedAllocate<AstIndxAlloc<'_, 'a, Var>>
        for &'a super::Term<'a, Var>
    {
        type Out = Idx<Term<Var>>;

        fn alloc(&self, alloc: &mut AstIndxAlloc<'_, 'a, Var>) -> Self::Out {
            let span = alloc
                .ref_spans
                .get(self)
                .unwrap_or_else(|| panic!("No span for {:?}", self));
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
                super::Term::Annotated { ty, term } => Term::Annotated {
                    ty: *ty,
                    term: term.alloc(alloc),
                },
            };
            let idx = alloc.arena_mut().alloc(term);
            alloc.idx_spans.insert(idx, *span);
            idx
        }
    }

    impl<'a, Var: 'a + Copy + Eq + Hash> ReferenceAllocate<'a, AstRefAlloc<'_, 'a, Var>>
        for Idx<Term<Var>>
    {
        type Out = &'a super::Term<'a, Var>;

        fn ref_alloc(&self, alloc: &mut AstRefAlloc<'_, 'a, Var>) -> Self::Out {
            let span = alloc.idx_spans[self];
            let term = match alloc.arena()[*self] {
                Term::Abstraction { arg, body } => super::Term::Abstraction {
                    arg,
                    body: body.ref_alloc(alloc),
                },
                Term::Application { func, arg } => super::Term::Application {
                    func: func.ref_alloc(alloc),
                    arg: arg.ref_alloc(alloc),
                },
                Term::Variable(var) => super::Term::Variable(var),
                Term::Int(i) => super::Term::Int(i),
                Term::Item(ids) => super::Term::Item(ids),
                Term::Unit => super::Term::Unit,
                Term::Label { label, term } => super::Term::Label {
                    label,
                    term: term.ref_alloc(alloc),
                },
                Term::Unlabel { label, term } => super::Term::Unlabel {
                    label,
                    term: term.ref_alloc(alloc),
                },
                Term::Concat { left, right } => super::Term::Concat {
                    left: left.ref_alloc(alloc),
                    right: right.ref_alloc(alloc),
                },
                Term::Project { direction, term } => super::Term::Project {
                    direction,
                    term: term.ref_alloc(alloc),
                },
                Term::Branch { left, right } => super::Term::Branch {
                    left: left.ref_alloc(alloc),
                    right: right.ref_alloc(alloc),
                },
                Term::Inject { direction, term } => super::Term::Inject {
                    direction,
                    term: term.ref_alloc(alloc),
                },
                Term::Operation(ids) => super::Term::Operation(ids),
                Term::Handle { handler, body } => super::Term::Handle {
                    handler: handler.ref_alloc(alloc),
                    body: body.ref_alloc(alloc),
                },
                Term::Annotated { ty, term } => super::Term::Annotated {
                    ty,
                    term: term.ref_alloc(alloc),
                },
            };
            let term_ref = alloc.ref_arena().alloc(term) as &_;
            alloc.ref_spans.insert(term_ref, span);
            alloc.mapping.insert(term_ref, *self);
            term_ref
        }
    }

    /// Abstract Syntax Tree (AST)
    #[derive(Debug, Clone, Eq)]
    pub struct Ast<Var> {
        pub name: ItemId,
        // We store spans of the Ast out of band because we won't need them for most operations.
        spans: FxHashMap<Idx<Term<Var>>, Span>,
        pub annotation: Option<TyScheme>,
        terms: Arena<Term<Var>>,
        pub tree: Idx<Term<Var>>,
    }
    impl<Var> Ast<Var> {
        pub fn new(
            name: ItemId,
            spans: FxHashMap<Idx<Term<Var>>, Span>,
            annotation: TyScheme,
            terms: Arena<Term<Var>>,
            tree: Idx<Term<Var>>,
        ) -> Self {
            Self {
                name,
                spans,
                annotation: Some(annotation),
                terms,
                tree,
            }
        }
        pub fn with_untyped(
            name: ItemId,
            spans: FxHashMap<Idx<Term<Var>>, Span>,
            terms: Arena<Term<Var>>,
            tree: Idx<Term<Var>>,
        ) -> Self {
            Self {
                name,
                spans,
                annotation: None,
                terms,
                tree,
            }
        }

        pub fn root(&self) -> Idx<Term<Var>> {
            self.tree
        }

        pub fn arena(&self) -> &Arena<Term<Var>> {
            &self.terms
        }

        pub fn view(&self, term: Idx<Term<Var>>) -> &Term<Var> {
            &self.terms[term]
        }

        pub fn span_of(&self, node: Idx<Term<Var>>) -> Option<&Span> {
            self.spans.get(&node)
        }
    }
    impl<Var: PartialEq> PartialEq for Ast<Var> {
        fn eq(&self, other: &Self) -> bool {
            self.name == other.name
                && self.tree == other.tree
                && self.annotation == other.annotation
                && self.spans == other.spans
                && self.terms == other.terms
        }
    }
    impl<Var: Hash> std::hash::Hash for Ast<Var> {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            self.name.hash(state);
            for (idx, span) in self.spans.iter() {
                idx.hash(state);
                span.hash(state);
            }
            self.terms.hash(state);
            self.tree.hash(state);
        }
    }

    impl<Var: Copy + Eq + Hash> Ast<Var> {
        pub fn ref_alloc<'a>(
            &self,
            arena: &'a Bump,
        ) -> (
            super::Ast<'a, Var>,
            FxHashMap<&'a super::Term<'a, Var>, Idx<Term<Var>>>,
        ) {
            let mut alloc = AstRefAlloc {
                arena,
                terms: &self.terms,
                ref_spans: FxHashMap::default(),
                mapping: FxHashMap::default(),
                idx_spans: &self.spans,
            };
            let tree = self.tree.ref_alloc(&mut alloc);
            (
                super::Ast {
                    name: self.name,
                    spans: alloc.ref_spans,
                    annotation: self.annotation.clone(),
                    tree,
                },
                alloc.mapping,
            )
        }
    }

    impl<'a, Var: Copy + Eq + Hash + Debug> From<&super::Ast<'a, Var>> for Ast<Var> {
        fn from(value: &super::Ast<'a, Var>) -> Self {
            let mut alloc = AstIndxAlloc {
                terms: Arena::default(),
                ref_spans: &value.spans,
                idx_spans: FxHashMap::default(),
            };
            let tree = value.tree.alloc(&mut alloc);
            Ast {
                name: value.name,
                spans: alloc.idx_spans,
                annotation: value.annotation.clone(),
                terms: alloc.terms,
                tree,
            }
        }
    }

    #[salsa::tracked]
    pub struct SalsaItem {
        #[id]
        pub item: Item<VarId>,
    }

    /// A top-level item in an Aiahr source file.
    /// This is desugared from an NST item and all of it's spans are moved out of band to make working
    /// with the semantic information of the tree easier.
    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub enum Item<Var> {
        Effect(super::EffectItem),
        Function(Ast<Var>),
    }

    impl<'a, Var: Copy + Eq + Hash + Debug> From<&super::Item<'a, Var>> for Item<Var> {
        fn from(value: &super::Item<'a, Var>) -> Self {
            match value {
                super::Item::Effect(eff) => Item::Effect(eff.clone()),
                super::Item::Function(ast) => Item::Function(Ast::from(ast)),
            }
        }
    }
}

#[salsa::tracked]
pub struct AstModule {
    #[id]
    pub module: Module,
    #[return_ref]
    pub items: Vec<indexed::SalsaItem>,
}

/// An ast definition of an effect
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EffectItem {
    pub name: EffectId,
    pub ops: Vec<Option<(EffectOpId, TyScheme)>>,
}

/// A top-level item in an Aiahr source file.
/// This is desugared from an NST item and all of it's spans are moved out of band to make working
/// with the semantic information of the tree easier.
#[derive(Clone, Debug)]
pub enum Item<'a, Var> {
    Effect(EffectItem),
    Function(Ast<'a, Var>),
}
impl<'a, Var> Item<'a, Var> {
    pub fn unwrap_func(self) -> Ast<'a, Var> {
        match self {
            Item::Effect(_) => panic!("Expected Function but got Effect"),
            Item::Function(ast) => ast,
        }
    }
}

/// Abstract Syntax Tree (AST)
#[derive(Clone, Debug)]
pub struct Ast<'a, Var> {
    name: ItemId,
    // We store spans of the Ast out of band because we won't need them for most operations
    spans: FxHashMap<&'a Term<'a, Var>, Span>,
    annotation: Option<TyScheme>,
    pub tree: &'a Term<'a, Var>,
}

impl<'a, Var> Ast<'a, Var> {
    pub fn new(
        name: ItemId,
        spans: FxHashMap<&'a Term<'a, Var>, Span>,
        tree: &'a Term<'a, Var>,
    ) -> Self {
        Self {
            name,
            spans,
            annotation: None,
            tree,
        }
    }

    pub fn with_ann(
        name: ItemId,
        spans: FxHashMap<&'a Term<'a, Var>, Span>,
        annotation: TyScheme,
        tree: &'a Term<'a, Var>,
    ) -> Self {
        Self {
            name,
            spans,
            annotation: Some(annotation),
            tree,
        }
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
    Operation((ModuleId, EffectId, EffectOpId)),
    Handle {
        handler: &'a Term<'a, Var>,
        body: &'a Term<'a, Var>,
    },
    Annotated {
        ty: Ty,
        term: &'a Term<'a, Var>,
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
            Term::Annotated { term, .. } => ZeroOneOrTwo::One(term),
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
