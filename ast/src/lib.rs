use std::fmt::Debug;
use std::hash::Hash;

use la_arena::{Arena, Idx};
use pretty::{docs, DocAllocator, Pretty};
use rustc_hash::FxHashMap;

use aiahr_core::{
    id::{EffectId, EffectOpId, ItemId, VarId},
    ident::Ident,
    modules::Module,
    span::Span,
};
use aiahr_ty::{Ty, TyScheme};

#[salsa::jar(db = Db)]
pub struct Jar(AstModule, SalsaItem);
pub trait Db: salsa::DbWithJar<Jar> + aiahr_core::Db {
    fn as_ast_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
    }
}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + aiahr_core::Db {}
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
    Item((Module, ItemId)),
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
    Operation((Module, EffectId, EffectOpId)),
    Handle {
        handler: Idx<Self>,
        body: Idx<Self>,
    },
    Annotated {
        ty: Ty,
        term: Idx<Term<Var>>,
    },
}

struct PrettyTerm<'a, Var> {
    root: Idx<Term<Var>>,
    arena: &'a Arena<Term<Var>>,
    db: &'a dyn crate::Db,
}

impl<'a, Var> PrettyTerm<'a, Var> {
    fn with_root(&self, root: Idx<Term<Var>>) -> Self {
        PrettyTerm {
            root,
            arena: self.arena,
            db: self.db,
        }
    }
}

impl<'a, Var, D, A: 'a> Pretty<'a, D, A> for PrettyTerm<'_, Var>
where
    D: DocAllocator<'a, A>,
    Var: Clone + Pretty<'a, D, A>,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, A> {
        match &self.arena[self.root] {
            Term::Variable(var) => var.clone().pretty(alloc),
            Term::Item((module, item_id)) => docs![
                alloc,
                "module",
                alloc.text(format!("{:?}", module)).angles(),
                ".item",
                alloc.as_string(item_id.0).angles()
            ],
            Term::Operation((module, eff_id, op_id)) => docs![
                alloc,
                "module",
                alloc.text(format!("{:?}", module)).angles(),
                ".effect",
                alloc.as_string(eff_id.0).angles(),
                ".op",
                alloc.as_string(op_id.0).angles()
            ],
            Term::Int(i) => alloc.as_string(i),
            Term::Unit => alloc.text("{}"),
            Term::Abstraction { arg, body } => docs![
                alloc,
                "|",
                arg.clone(),
                "|",
                alloc.softline(),
                self.with_root(*body)
            ]
            .group()
            .parens(),
            Term::Application { func, arg } => self
                .with_root(*func)
                .pretty(alloc)
                .append(self.with_root(*arg).pretty(alloc).parens()),
            Term::Label { label, term } => docs![
                alloc,
                label.text(self.db.as_core_db()).clone(),
                alloc.softline(),
                "=",
                alloc.softline(),
                self.with_root(*term)
            ],
            Term::Unlabel { label, term } => docs![
                alloc,
                self.with_root(*term),
                ".",
                label.text(self.db.as_core_db()).clone()
            ],
            Term::Concat { left, right } => docs![
                alloc,
                self.with_root(*left),
                alloc.softline(),
                "***",
                alloc.softline(),
                self.with_root(*right),
            ]
            .parens(),
            Term::Project { direction, term } => docs![
                alloc,
                "prj",
                direction.pretty(alloc).angles(),
                self.with_root(*term).pretty(alloc).parens()
            ],
            Term::Branch { left, right } => docs![
                alloc,
                self.with_root(*left),
                alloc.softline(),
                "+++",
                alloc.softline(),
                self.with_root(*right)
            ]
            .parens(),
            Term::Inject { direction, term } => docs![
                alloc,
                "inj",
                direction.pretty(alloc).angles(),
                self.with_root(*term).pretty(alloc).parens()
            ],
            Term::Handle { handler, body } => docs![
                alloc,
                "handle",
                alloc.softline(),
                self.with_root(*body).pretty(alloc).nest(2),
                alloc.softline(),
                "with",
                alloc.softline(),
                self.with_root(*handler).pretty(alloc).nest(2)
            ],
            // TODO: Pretty print types.
            Term::Annotated { term, .. } => self.with_root(*term).pretty(alloc),
        }
    }
}

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
            Term::Variable(_) | Term::Int(_) | Term::Item(_) | Term::Unit | Term::Operation(_) => {
                ZeroOneOrTwo::Zero
            }
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
impl<Var> Ast<Var> {
    pub fn pretty<'a, 'b, D>(
        &'a self,
        db: &'a dyn crate::Db,
        alloc: &'b D,
    ) -> pretty::DocBuilder<'b, D>
    where
        D: DocAllocator<'b>,
        Var: Clone + Pretty<'b, D>,
    {
        PrettyTerm {
            root: self.root(),
            arena: &self.terms,
            db,
        }
        .pretty(alloc)
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
    Effect(EffectItem),
    Function(Ast<Var>),
}

impl<Var> Item<Var> {
    pub fn unwrap_func(self) -> Ast<Var> {
        match self {
            Item::Effect(_) => panic!("Expected Function but got Effect"),
            Item::Function(ast) => ast,
        }
    }
}

#[salsa::tracked]
pub struct AstModule {
    #[id]
    pub module: Module,
    #[return_ref]
    pub items: Vec<SalsaItem>,
}

/// An ast definition of an effect
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EffectItem {
    pub name: EffectId,
    pub ops: Vec<Option<(EffectOpId, TyScheme)>>,
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
impl<'a, D, A: 'a> Pretty<'a, D, A> for Direction
where
    D: DocAllocator<'a, A>,
{
    fn pretty(self, allocator: &'a D) -> pretty::DocBuilder<'a, D, A> {
        match self {
            Direction::Left => allocator.text("L"),
            Direction::Right => allocator.text("R"),
        }
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
