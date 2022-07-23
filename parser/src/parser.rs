use bumpalo::Bump;
use chumsky::{
    prelude::{choice, end, just, recursive},
    select, Parser, Stream,
};

use crate::{
    cst::Term,
    error::ParseErrors,
    loc::Loc,
    span::{Span, WithSpan},
    token::Token,
};

// Returns a spanned parser that matches just the given token and returns ().
fn lit<'i>(token: Token<'i>) -> impl Clone + Parser<Token<'i>, Span, Error = ParseErrors<'i>> {
    just(token).map_with_span(|_, span| span)
}

// Returns a spanned parser that matches any `Token::Identifier` and unwraps it to the contained
// `&str`.
fn ident<'i>() -> impl Clone + Parser<Token<'i>, WithSpan<&'i str>, Error = ParseErrors<'i>> {
    select! {
        Token::Identifier(id) => id,
    }
    .map_with_span(|s, span| (s, span))
}

enum TermPrefix<'a, 'i> {
    Binding {
        var: WithSpan<&'i str>,
        eq: Span,
        value: &'a Term<'a, 'i>,
        semi: Span,
    },
    Abstraction {
        lbar: Span,
        arg: WithSpan<&'i str>,
        rbar: Span,
    },
}

impl<'a, 'i> TermPrefix<'a, 'i> {
    fn apply(self, arena: &'a Bump, t: &'a Term<'a, 'i>) -> &'a Term<'a, 'i> {
        match self {
            TermPrefix::Binding {
                var,
                eq,
                value,
                semi,
            } => arena.alloc(Term::Binding {
                var,
                eq,
                value,
                semi,
                expr: t,
            }),
            TermPrefix::Abstraction { lbar, arg, rbar } => arena.alloc(Term::Abstraction {
                lbar,
                arg,
                rbar,
                body: t,
            }),
        }
    }
}

type Output<'a, 'i> = &'a Term<'a, 'i>;

/// Returns a parser for the Aiahr language, using the given arena to allocate CST nodes.
pub fn aiahr_parser<'a, 'i: 'a>(
    arena: &'a Bump,
) -> impl Parser<Token<'i>, Output<'a, 'i>, Error = ParseErrors<'i>> {
    recursive(|term| {
        // intermediary we use in atom and app
        let paren_term = lit(Token::LParen)
            .then(term.clone())
            .then(lit(Token::RParen));

        let atom = choice((
            // variable
            ident().map(|s| arena.alloc(Term::VariableRef(s)) as &Term),
            // explicit term precedence
            paren_term.clone().map(|((lpar, t), rpar)| {
                arena.alloc(Term::Parenthesized {
                    lpar,
                    term: t,
                    rpar,
                }) as &Term
            }),
        ));

        // Function application
        let app = atom
            .clone()
            .then(paren_term.repeated())
            .map(|(func, args)| {
                args.into_iter().fold(func, |t, ((lpar, arg), rpar)| {
                    arena.alloc(Term::Application {
                        func: t,
                        lpar,
                        arg,
                        rpar,
                    })
                })
            });

        // Local variable binding
        let local_bind = ident()
            .then(lit(Token::Equal))
            .then(term)
            .then(lit(Token::Semicolon))
            .map(|(((var, eq), val), semi)| TermPrefix::Binding {
                var,
                eq,
                value: val,
                semi,
            });

        // Lambda abstraction
        let closure = lit(Token::VerticalBar)
            .then(ident())
            .then(lit(Token::VerticalBar))
            .map(|((lbar, var), rbar)| TermPrefix::Abstraction {
                lbar,
                arg: var,
                rbar,
            });

        // Term parser
        // We need to construct our parse tree here bottom to get associativity of bindings and
        // closures correct. However we're recursive descent, so we only go top-down. To remedy
        // this we construct a series of closures top-down that are applied to the final expression
        // in right associative order.
        choice((local_bind, closure))
            .repeated()
            .then(app)
            .map(|(binds, expr)| {
                binds
                    .into_iter()
                    .rfold(expr, |t, prefix| prefix.apply(arena, t))
            })
    })
    .then_ignore(end())
}

/// Converts lexer output to a stream readable by a Chumsky parser.
pub fn to_stream<'i, I: IntoIterator<Item = WithSpan<Token<'i>>>>(
    tokens: I,
    end_of_input: Loc,
) -> Stream<'i, Token<'i>, Span, I::IntoIter> {
    // TODO: figure out what the `eoi` parameter is actually used for.
    Stream::from_iter(
        Span {
            start: end_of_input,
            end: end_of_input.next(),
        },
        tokens.into_iter(),
    )
}

#[cfg(test)]
mod tests {
    use bumpalo::Bump;
    use chumsky::Parser;

    use crate::{cst::Term, lexer::aiahr_lexer};

    use super::{aiahr_parser, to_stream};

    // CST pattern macros. Used to construct patterns that ignore spans.
    macro_rules! local {
        ($var:pat, $value:pat, $expr:pat) => {
            &Term::Binding {
                var: ($var, ..),
                value: $value,
                expr: $expr,
                ..
            }
        };
    }
    macro_rules! abs {
        ($arg:pat, $body:pat) => {
            &Term::Abstraction {
                arg: ($arg, ..),
                body: $body,
                ..
            }
        };
    }
    macro_rules! app {
        ($func:pat, $arg:pat) => {
            &Term::Application {
                func: $func,
                arg: $arg,
                ..
            }
        };
    }
    macro_rules! var {
        ($var:pat) => {
            &Term::VariableRef(($var, ..))
        };
    }
    macro_rules! paren {
        ($term:pat) => {
            &Term::Parenthesized { term: $term, .. }
        };
    }

    fn parse_unwrap<'a, 'i: 'a>(arena: &'a Bump, input: &'i str) -> &'a Term<'a, 'i> {
        let (tokens, eoi) = aiahr_lexer().lex(input).unwrap();
        aiahr_parser(arena).parse(to_stream(tokens, eoi)).unwrap()
    }

    #[test]
    fn test_undelimted_closure_fails() {
        let arena = Bump::new();
        let (tokens, eoi) = aiahr_lexer().lex("|x whoops(x)").unwrap();
        assert_matches!(aiahr_parser(&arena).parse(to_stream(tokens, eoi)), Err(..));
    }

    #[test]
    fn test_app_precedence() {
        assert_matches!(
            parse_unwrap(&Bump::new(), "(|x| |w| w)(y)(z)"),
            app!(
                app!(paren!(abs!("x", abs!("w", var!("w")))), var!("y")),
                var!("z")
            )
        );
    }

    #[test]
    fn test_mixing_prefixes() {
        assert_matches!(
            parse_unwrap(&Bump::new(), "|x| y = |z| y(z); w = x(y); w"),
            abs!(
                "x",
                local!(
                    "y",
                    abs!("z", app!(var!("y"), var!("z"))),
                    local!("w", app!(var!("x"), var!("y")), var!("w"))
                )
            )
        );
    }

    #[test]
    fn test_basic_lambdas() {
        assert_matches!(
            parse_unwrap(&Bump::new(), "|x| |y| z = x(y); z"),
            abs!(
                "x",
                abs!("y", local!("z", app!(var!("x"), var!("y")), var!("z")))
            )
        );
    }
}
