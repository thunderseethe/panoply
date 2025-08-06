use std::cmp::Reverse;
use std::ops::Range;

use crate::locator::Locator;
use base::{diagnostic::lexer::LexError, loc::Loc, span::SpanOf};
use regex::{Captures, Regex, RegexSet};
use logos::Logos;

use std::fmt::Debug;

/// A function that produces a token from a regex match using a string interner.
type TokenFactory = Box<dyn Fn(Captures, &(dyn crate::Db + '_)) -> SyntaxKind>;

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
  pub fn lex(&self, locator: &Locator, text: &str) -> Result<(Vec<(SyntaxKind, Range<usize>)>, Loc), LexError> {   
    let tokens = SyntaxKind::lexer(text)
        .spanned()
        .map(|(tok, span)| (tok.unwrap_or(SyntaxKind::Error), span))
        .collect();
    return Ok((tokens, todo!()));
  }
}

// Maps the literal text to the given token.
fn literal(text: &'static str, t: SyntaxKind) -> (String, Option<TokenFactory>) {
  (regex::escape(text), Some(Box::new(move |_, _| t)))
}

// Calls `f` on the entire match. Use this if you don't care about capture groups.
fn whole<F>(f: F) -> Option<TokenFactory>
where
  F: Fn(&dyn crate::Db, &str) -> SyntaxKind + 'static,
{
  Some(Box::new(move |c, db| f(db, c.get(0).unwrap().as_str())))
}

//// Returns a lexer for the language that uses the given interner.
//pub fn lexer<'s>(db: &'s (dyn crate::Db + '_)) -> Lexer<'s> {
//  // TODO: Do something with comments, or at least doc comments.
//  Lexer::new(
//    vec![
//      // Keywords
//      literal("forall", SyntaxKind::KwForall),
//      literal("match", SyntaxKind::KwMatch),
//      literal("effect", SyntaxKind::KwEffect),
//      literal("defn", SyntaxKind::KwDefn),
//      literal("with", SyntaxKind::KwWith),
//      literal("do", SyntaxKind::KwDo),
//      literal("let", SyntaxKind::KwLet),
//      // Identifier
//      (
//        r"[a-zA-Z][a-zA-Z0-9_]*".to_string(),
//        whole(|db, s| SyntaxKind::Identifier),
//      ),
//      (
//        r"\d+".to_string(),
//        whole(|_, s| SyntaxKind::Int),
//      ),
//      // Punctuation
//      literal("+", SyntaxKind::Plus),
//      literal("=", SyntaxKind::Equal),
//      literal("|", SyntaxKind::VerticalBar),
//      literal("->", SyntaxKind::SmallArrow),
//      literal("=>", SyntaxKind::BigArrow),
//      literal("(", SyntaxKind::LParen),
//      literal(")", SyntaxKind::RParen),
//      literal("[", SyntaxKind::LBracket),
//      literal("]", SyntaxKind::RBracket),
//      literal("{", SyntaxKind::LBrace),
//      literal("}", SyntaxKind::RBrace),
//      literal("<", SyntaxKind::LAngle),
//      literal(">", SyntaxKind::RAngle),
//      literal(":", SyntaxKind::Colon),
//      literal(";", SyntaxKind::Semicolon),
//      literal(",,", SyntaxKind::Concat),
//      literal(",", SyntaxKind::Comma),
//      literal(".", SyntaxKind::Dot),
//      // Comments
//      (r"//.*".to_string(), None),
//      (r"(?s)/\*.*\*/".to_string(), None),
//      // Whitespace
//      (r"\s+".to_string(), None),
//    ],
//    db,
//  )
//  .unwrap()
//}
