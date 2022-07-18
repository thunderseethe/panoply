use crate::{
    loc::{Loc, Locator},
    span::{Span, WithSpan},
    token::Token,
};
use regex::{Captures, Regex, RegexSet};

/// A function that produces a token from a regex match.
type TokenFactory = Box<dyn Fn(Captures) -> Token>;

/// A `Lexer` turns input text into a sequence of `Token`s.
pub struct Lexer {
    union: RegexSet, // The set of all regexes in `tokens`.
    tokens: Vec<(Regex, Option<TokenFactory>)>,
}

/// Indicates that the text at the given location could not be parsed as a token.
#[derive(Debug)]
pub struct NotATokenError {
    pub loc: Loc,
}

impl Lexer {
    /// Returns a new lexer which maps each of the given regexes to the corresponding token factory
    /// function. Earlier token patterns take precedence over later ones. Regexes paired with `None`
    /// indicate text that should be ignored.
    pub fn new(tokens: Vec<(&'static str, Option<TokenFactory>)>) -> Result<Lexer, regex::Error> {
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
    pub fn lex<'i>(&self, text: &'i str) -> Result<Vec<WithSpan<Token<'i>>>, NotATokenError> {
        let locator = Locator::new(text);
        let mut idx = 0;
        let mut tokens = Vec::new();

        while idx < text.len() {
            let curr = &text[idx..];

            // Just take the first match, since earlier regexes take precedence over later ones.
            if let Some(i) = self.union.matches(curr).into_iter().next() {
                let (ref re, ref f) = self.tokens[i];
                let caps = re.captures(curr).unwrap();
                let len = caps[0].len();
                if let Some(f) = f {
                    tokens.push((
                        f(caps),
                        Span {
                            start: locator.locate(idx),
                            end: locator.locate(idx + len),
                        },
                    ));
                }
                idx += len
            } else {
                return Err(NotATokenError {
                    loc: locator.locate(idx),
                });
            }
        }

        Ok(tokens)
    }
}

// Returns `t` for all matches.
fn lit(t: Token<'static>) -> Option<TokenFactory> {
    Some(Box::new(move |_| t.clone()))
}

// Calls `f` on the entire match. Use this if you don't care about capture groups.
fn whole<F: 'static + Fn(&str) -> Token>(f: F) -> Option<TokenFactory> {
    Some(Box::new(move |c| f(c.get(0).unwrap().as_str())))
}

/// Returns a lexer for the Aiahr language.
pub fn aiahr_lexer() -> Lexer {
    // TODO: Do something with comments, or at least doc comments.
    Lexer::new(vec![
        (r"[a-zA-Z][a-zA-Z0-9_]*", whole(|s| Token::Identifier(s))),
        // Delimiters
        (r"\|", lit(Token::VerticalBar)),
        (r"\(", lit(Token::LParen)),
        (r"\)", lit(Token::RParen)),
        // Comments
        (r"//.*", None),
        (r"(?s)/\*.*\*/", None),
        // Whitespace
        (r"\s+", None),
    ])
    .unwrap()
}
