use std::fmt::Debug;

/// A token in AIAHR.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Token<'i> {
    KwMatch,
    KwEnd,
    Identifier(&'i str),
    Equal,
    VerticalBar,
    BigArrow,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LAngle,
    RAngle,
    Semicolon,
    Comma,
    Dot,
}
