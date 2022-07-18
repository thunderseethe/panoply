use crate::{
    loc::Loc,
    span::{Span, Spanned, WithSpan},
};
use std::fmt::Debug;

#[derive(Debug)]
pub enum Term<'i> {
    Binding {
        var: WithSpan<&'i str>,
        eq: Span,
        value: Box<Term<'i>>,
        semi: Span,
        expr: Box<Term<'i>>,
    },
    Abstraction {
        lbar: Span,
        arg: WithSpan<&'i str>,
        rbar: Span,
        body: Box<Term<'i>>,
    },
    Application {
        func: Box<Term<'i>>,
        lpar: Span,
        arg: Box<Term<'i>>,
        rpar: Span,
    },
    VariableRef(WithSpan<&'i str>),
    Parenthesized {
        lpar: Span,
        term: Box<Term<'i>>,
        rpar: Span,
    },
}

impl<'i> Spanned for Term<'i> {
    fn start(&self) -> Loc {
        match self {
            Term::Binding { var, .. } => var.start(),
            Term::Abstraction { lbar, .. } => lbar.start(),
            Term::Application { func, .. } => func.start(),
            Term::VariableRef(v) => v.start(),
            Term::Parenthesized { lpar, .. } => lpar.start(),
        }
    }

    fn end(&self) -> Loc {
        match self {
            Term::Binding { expr, .. } => expr.end(),
            Term::Abstraction { body, .. } => body.end(),
            Term::Application { rpar, .. } => rpar.end(),
            Term::VariableRef(v) => v.end(),
            Term::Parenthesized { rpar, .. } => rpar.end(),
        }
    }
}

// #[derive(Debug)]
// pub enum FunctionArg<'i> {
//     Single(Box<Term<'i>>),
//     // Product(Vec<(Label, Term)>),
// }

// #[derive(Debug)]
// pub struct Label<'i>(&'i str);
