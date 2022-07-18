use std::fmt::Debug;

/// A token in AIAHR.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Token<'i> {
    Identifier(&'i str),
    Equal,
    VerticalBar,
    LParen,
    RParen,
    Semicolon,
}
