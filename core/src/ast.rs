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

/// A Term of the AST
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Term<'a, Var> {
    Abstraction {
        arg: Var,
        body: &'a Term<'a, Var>,
    },
    Application {
        func: &'a Term<'a, Var>,
        arg: &'a Term<'a, Var>,
    },
    Variable(Var),
}
