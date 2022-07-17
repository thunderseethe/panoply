use std::fmt::Debug;

/// A token in AIAHR.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Token<'i> {
    Identifier(&'i str),
    VerticalBar,
    LParen,
    RParen,
}
