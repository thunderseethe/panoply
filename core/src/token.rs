use std::fmt::Debug;

use crate::ident::Ident;

/// A token in Aiahr.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Token {
    KwForall,
    KwEffect,
    KwMatch,
    KwWith,
    KwDo,
    Identifier(Ident),
    Plus,
    Equal,
    VerticalBar,
    SmallArrow,
    BigArrow,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    LAngle,
    RAngle,
    Colon,
    Semicolon,
    Comma,
    Dot,
}

impl Token {
    /// Returns a human-friendly name for this kind of token. If the token corresponds to a specific
    /// literal string, then that string is its name.
    pub fn name(&self) -> &'static str {
        match self {
            Token::KwForall => "forall",
            Token::KwMatch => "match",
            Token::KwEffect => "effect",
            Token::KwWith => "with",
            Token::KwDo => "do",
            Token::Identifier(..) => "<identifier>",
            Token::Plus => "+",
            Token::Equal => "=",
            Token::VerticalBar => "|",
            Token::SmallArrow => "->",
            Token::BigArrow => "=>",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::LBracket => "[",
            Token::RBracket => "]",
            Token::LBrace => "{",
            Token::RBrace => "}",
            Token::LAngle => "<",
            Token::RAngle => ">",
            Token::Colon => ":",
            Token::Semicolon => ";",
            Token::Comma => ",",
            Token::Dot => ".",
        }
    }
}
