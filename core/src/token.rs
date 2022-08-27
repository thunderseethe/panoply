use std::fmt::Debug;

/// A token in Aiahr.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Token<'i> {
    KwMatch,
    KwWith,
    KwDo,
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

impl<'i> Token<'i> {
    /// Returns a human-friendly name for this kind of token. If the token corresponds to a specific
    /// literal string, then that string is its name.
    pub fn name(&self) -> &'static str {
        match self {
            Token::KwMatch => "match",
            Token::KwWith => "with",
            Token::KwDo => "do",
            Token::Identifier(..) => "<identifier>",
            Token::Equal => "=",
            Token::VerticalBar => "|",
            Token::BigArrow => "=>",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::LBrace => "{",
            Token::RBrace => "}",
            Token::LAngle => "<",
            Token::RAngle => ">",
            Token::Semicolon => ";",
            Token::Comma => ",",
            Token::Dot => ".",
        }
    }
}
