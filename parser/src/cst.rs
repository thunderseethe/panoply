use crate::span::Span;
use std::fmt::Debug;

#[derive(Debug)]
pub enum Term<'i> {
    Abstraction {
        lbar: Span<()>,
        arg: Span<&'i str>,
        rbar: Span<()>,
        body: Box<Term<'i>>,
    },
    Application {
        func: Box<Term<'i>>,
        lpar: Span<()>,
        arg: Box<Term<'i>>,
        rpar: Span<()>,
    },
    VariableRef(Span<&'i str>),
    Parenthesized {
        lpar: Span<()>,
        term: Box<Term<'i>>,
        rpar: Span<()>,
    },
}

// #[derive(Debug)]
// pub enum FunctionArg<'i> {
//     Single(Box<Term<'i>>),
//     // Product(Vec<(Label, Term)>),
// }

// #[derive(Debug)]
// pub struct Label<'i>(&'i str);
