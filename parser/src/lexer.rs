use std::cmp::Reverse;

use aiahr_core::{
    diagnostic::lexer::LexError,
    loc::{Loc, Locator},
    span::SpanOf,
    token::Token,
};
use regex::{Captures, Regex, RegexSet};

/// A function that produces a token from a regex match.
type TokenFactory = Box<dyn Fn(Captures) -> Token>;

/// A `Lexer` turns input text into a sequence of `Token`s based on input regexes.
///
/// When multiple regexes match a given piece of text, the regex with the longest match takes
/// precedence. In the case of a tie in length, regexes earlier in the list provided at construction
/// take precedence over later ones.
pub struct Lexer {
    union: RegexSet, // The set of all regexes in `tokens`.
    tokens: Vec<(Regex, Option<TokenFactory>)>,
}

impl Lexer {
    /// Returns a new lexer which maps each of the given regexes to the corresponding token factory
    /// function. Regexes paired with `None` indicate text that should be ignored.
    pub fn new(tokens: Vec<(String, Option<TokenFactory>)>) -> Result<Lexer, regex::Error> {
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
        })
    }

    /// Splits `text` into a sequence of tokens.
    pub fn lex<'i>(&self, text: &'i str) -> Result<(Vec<SpanOf<Token<'i>>>, Loc), LexError> {
        let locator = Locator::new(text);
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
                        value: f(caps),
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
fn literal(text: &'static str, t: Token<'static>) -> (String, Option<TokenFactory>) {
    (regex::escape(text), Some(Box::new(move |_| t.clone())))
}

// Calls `f` on the entire match. Use this if you don't care about capture groups.
fn whole<F: 'static + Fn(&str) -> Token>(f: F) -> Option<TokenFactory> {
    Some(Box::new(move |c| f(c.get(0).unwrap().as_str())))
}

/// Returns a lexer for the Aiahr language.
pub fn aiahr_lexer() -> Lexer {
    // TODO: Do something with comments, or at least doc comments.
    Lexer::new(vec![
        // Keywords
        literal("match", Token::KwMatch),
        literal("with", Token::KwWith),
        literal("do", Token::KwDo),
        // Identifier
        (
            r"[a-zA-Z][a-zA-Z0-9_]*".to_string(),
            whole(|s| Token::Identifier(s)),
        ),
        // Punctuation
        literal("=", Token::Equal),
        literal("|", Token::VerticalBar),
        literal("=>", Token::BigArrow),
        literal("(", Token::LParen),
        literal(")", Token::RParen),
        literal("{", Token::LBrace),
        literal("}", Token::RBrace),
        literal("<", Token::LAngle),
        literal(">", Token::RAngle),
        literal(";", Token::Semicolon),
        literal(",", Token::Comma),
        literal(".", Token::Dot),
        // Comments
        (r"//.*".to_string(), None),
        (r"(?s)/\*.*\*/".to_string(), None),
        // Whitespace
        (r"\s+".to_string(), None),
    ])
    .unwrap()
}
