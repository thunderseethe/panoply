use std::cmp::Reverse;

use aiahr_core::{
    diagnostic::lexer::LexError,
    id::ModuleId,
    ident::Ident,
    loc::{Loc, Locator},
    span::SpanOf,
    //token::Token,
};
use regex::{Captures, Regex, RegexSet};

use std::fmt::Debug;

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
/// A function that produces a token from a regex match using a string interner.
type TokenFactory = Box<dyn Fn(Captures, &(dyn crate::Db + '_)) -> Token>;

/// A `Lexer` turns input text into a sequence of `Token`s based on input regexes.
///
/// When multiple regexes match a given piece of text, the regex with the longest match takes
/// precedence. In the case of a tie in length, regexes earlier in the list provided at construction
/// take precedence over later ones.
pub struct Lexer<'s> {
    union: RegexSet, // The set of all regexes in `tokens`.
    tokens: Vec<(Regex, Option<TokenFactory>)>,
    db: &'s dyn crate::Db,
}

impl<'s> Lexer<'s> {
    /// Returns a new lexer which maps each of the given regexes to the corresponding token factory
    /// function. Regexes paired with `None` indicate text that should be ignored.
    ///
    /// Interns all strings using the given interner.
    pub fn new(
        tokens: Vec<(String, Option<TokenFactory>)>,
        db: &'s dyn crate::Db,
    ) -> Result<Lexer<'s>, regex::Error> {
        let anchored = tokens
            .into_iter()
            .map(|(p, f)| (format!("^{}", p), f))
            .collect::<Vec<_>>();
        Ok(Lexer {
            union: RegexSet::new(anchored.iter().map(|(p, _)| p))?,
            tokens: anchored
                .into_iter()
                .map(|(p, f)| Ok((Regex::new(&p)?, f)))
                .collect::<Result<Vec<_>, _>>()?,
            db,
        })
    }

    /// Splits `text` into a sequence of tokens.
    pub fn lex(&self, module: ModuleId, text: &str) -> Result<(Vec<SpanOf<Token>>, Loc), LexError> {
        let locator = Locator::new(module, text);
        let mut idx = 0;
        let mut tokens = Vec::new();

        while idx < text.len() {
            let curr = &text[idx..];

            // Use `min_by_key()` with `Reverse` to select the first element from a tie.
            if let Some((caps, f)) = self
                .union
                .matches(curr)
                .into_iter()
                .map(|i| (self.tokens[i].0.captures(curr).unwrap(), &self.tokens[i].1))
                .min_by_key(|(caps, ..)| Reverse(caps[0].len()))
            {
                let len = caps[0].len();
                if let Some(f) = f {
                    tokens.push(SpanOf {
                        start: locator.locate(idx).unwrap(),
                        value: f(caps, self.db),
                        end: locator.locate(idx + len).unwrap(),
                    });
                }
                idx += len;
            } else {
                return Err(LexError::NotAToken(locator.locate(idx).unwrap()));
            }
        }
        Ok((tokens, locator.eoi()))
    }
}

// Maps the literal text to the given token.
fn literal(text: &'static str, t: Token) -> (String, Option<TokenFactory>) {
    (regex::escape(text), Some(Box::new(move |_, _| t)))
}

// Calls `f` on the entire match. Use this if you don't care about capture groups.
fn whole<F>(f: F) -> Option<TokenFactory>
where
    F: Fn(Ident) -> Token + 'static,
{
    Some(Box::new(move |c, db| {
        f(db.ident_str(c.get(0).unwrap().as_str()))
    }))
}

/// Returns a lexer for the Aiahr language that uses the given interner.
pub fn aiahr_lexer<'s>(db: &'s (dyn crate::Db + '_)) -> Lexer<'s> {
    // TODO: Do something with comments, or at least doc comments.
    Lexer::new(
        vec![
            // Keywords
            literal("forall", Token::KwForall),
            literal("match", Token::KwMatch),
            literal("effect", Token::KwEffect),
            literal("with", Token::KwWith),
            literal("do", Token::KwDo),
            // Identifier
            (
                r"[a-zA-Z][a-zA-Z0-9_]*".to_string(),
                whole(Token::Identifier),
            ),
            // Punctuation
            literal("+", Token::Plus),
            literal("=", Token::Equal),
            literal("|", Token::VerticalBar),
            literal("->", Token::SmallArrow),
            literal("=>", Token::BigArrow),
            literal("(", Token::LParen),
            literal(")", Token::RParen),
            literal("[", Token::LBracket),
            literal("]", Token::RBracket),
            literal("{", Token::LBrace),
            literal("}", Token::RBrace),
            literal("<", Token::LAngle),
            literal(">", Token::RAngle),
            literal(":", Token::Colon),
            literal(";", Token::Semicolon),
            literal(",", Token::Comma),
            literal(".", Token::Dot),
            // Comments
            (r"//.*".to_string(), None),
            (r"(?s)/\*.*\*/".to_string(), None),
            // Whitespace
            (r"\s+".to_string(), None),
        ],
        db,
    )
    .unwrap()
}
