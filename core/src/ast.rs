use crate::span::Span;
use rustc_hash::FxHashMap;

/// Abstract Syntax Tree (AST)
pub struct Ast<'t, Var> {
    // We store spans of the Ast out of band because we won't need them for most operations
    spans: FxHashMap<&'t Term<'t, Var>, Span>,
    pub tree: &'t Term<'t, Var>,
}

impl<'t, Var> Ast<'t, Var> {
    pub fn new(spans: FxHashMap<&'t Term<'t, Var>, Span>, tree: &'t Term<'t, Var>) -> Self {
        Self { spans, tree }
    }
}

impl<'t, Var: Eq + std::hash::Hash> Ast<'t, Var> {
    /// Lookup the span of a node within this Ast
    pub fn span_of(&self, node: &'t Term<'t, Var>) -> Option<&Span> {
        self.spans.get(node)
    }
}

/// A Term of the AST
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Term<'t, Var> {
    Abstraction {
        arg: Var,
        body: &'t Term<'t, Var>,
    },
    Application {
        func: &'t Term<'t, Var>,
        arg: &'t Term<'t, Var>,
    },
    Variable(Var),
}
