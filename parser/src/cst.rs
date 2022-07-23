use crate::{
    loc::Loc,
    span::{Span, Spanned, WithSpan},
};
use std::fmt::Debug;

#[derive(Clone, Copy, Debug)]
pub enum Term<'a, 'i> {
    Binding {
        var: WithSpan<&'i str>,
        eq: Span,
        value: &'a Term<'a, 'i>,
        semi: Span,
        expr: &'a Term<'a, 'i>,
    },
    Abstraction {
        lbar: Span,
        arg: WithSpan<&'i str>,
        rbar: Span,
        body: &'a Term<'a, 'i>,
    },
    Application {
        func: &'a Term<'a, 'i>,
        lpar: Span,
        arg: &'a Term<'a, 'i>,
        rpar: Span,
    },
    VariableRef(WithSpan<&'i str>),
    Parenthesized {
        lpar: Span,
        term: &'a Term<'a, 'i>,
        rpar: Span,
    },
}

impl<'a, 'i> Spanned for Term<'a, 'i> {
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
