use chumsky::{
    prelude::{end, just, Recursive},
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
fn lit<'i>(token: Token<'i>) -> impl Parser<Token<'i>, Span, Error = ParseErrors<'i>> {
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

type Output<'i> = Term<'i>;

/// Returns a parser for the Aiahr language.
pub fn aiahr_parser<'i>() -> impl Parser<Token<'i>, Output<'i>, Error = ParseErrors<'i>> {
    // Each level of precedence should have its own `Recursive` instance declared here in reverse
    // precedence order. `term` is reserved for the top-level instance.
    let mut term = Recursive::declare();
    let mut prefixed = Recursive::declare();
    let mut app = Recursive::declare();
    let mut atom = Recursive::declare();

    // Define parsers here in the order declared above.
    term.define(prefixed.clone());
    prefixed.define(
        ident()
            .then(lit(Token::Equal))
            .then(term.clone())
            .then(lit(Token::Semicolon))
            .map(|(((var, eq), val), semi)| {
                Box::new(move |t| Term::Binding {
                    var,
                    eq,
                    value: Box::new(val),
                    semi,
                    expr: Box::new(t),
                }) as Box<dyn FnOnce(Term<'i>) -> Term<'i>>
            })
            .or(lit(Token::VerticalBar)
                .then(ident())
                .then(lit(Token::VerticalBar))
                .map(|((lbar, var), rbar)| {
                    Box::new(move |t| Term::Abstraction {
                        lbar,
                        arg: var,
                        rbar,
                        body: Box::new(t),
                    }) as Box<dyn FnOnce(Term<'i>) -> Term<'i>>
                }))
            .repeated()
            .then(app.clone())
            .map(|(binds, expr)| binds.into_iter().rfold(expr, |t, f| f(t))),
    );
    app.define(
        atom.clone()
            .then(
                lit(Token::LParen)
                    .then(term.clone())
                    .then(lit(Token::RParen))
                    .repeated(),
            )
            .map(|(func, args)| {
                args.into_iter()
                    .rfold(func, |t, ((lpar, arg), rpar)| Term::Application {
                        func: Box::new(t),
                        lpar,
                        arg: Box::new(arg),
                        rpar,
                    })
            }),
    );
    atom.define(
        ident().map(Term::VariableRef).or(lit(Token::LParen)
            .then(term.clone())
            .then(lit(Token::RParen))
            .map(|((lpar, t), rpar)| Term::Parenthesized {
                lpar,
                term: Box::new(t),
                rpar,
            })),
    );
    term.then_ignore(end())
}

/// Converts lexer output to a stream readable by a Chumsky parser.
pub fn to_stream<'i, I: IntoIterator<Item = WithSpan<Token<'i>>>>(
    tokens: I,
) -> Stream<'i, Token<'i>, Span, I::IntoIter> {
    const EOI: Loc = Loc {
        byte: !0usize,
        line: !0usize,
        col: !0usize,
    };

    // TODO: figure out what the `eoi` parameter is actually used for.
    Stream::from_iter(
        Span {
            start: EOI,
            end: EOI,
        },
        tokens.into_iter(),
    )
}

#[cfg(test)]
mod tests {
    use chumsky::Parser;

    use crate::{cst::Term, lexer::aiahr_lexer};

    use super::{aiahr_parser, to_stream};

    #[test]
    fn test_basic_lambdas() {
        const PROGRAM: &'static str = "|x| |y| z = x(y); z";

        let lexer = aiahr_lexer();
        let parser = aiahr_parser();

        let tokens = lexer.lex(PROGRAM).unwrap();
        let cst = parser.parse(to_stream(tokens)).unwrap();

        if let Term::Abstraction {
            arg: ("x", ..),
            body: ref b,
            ..
        } = cst
        {
            if let Term::Abstraction {
                arg: ("y", ..),
                body: ref c,
                ..
            } = &**b
            {
                if let Term::Binding {
                    var: ("z", ..),
                    value: ref v,
                    expr: ref e,
                    ..
                } = &**c
                {
                    if let Term::Application {
                        func: ref f,
                        arg: ref a,
                        ..
                    } = &**v
                    {
                        if let Term::VariableRef(("x", ..)) = &**f {
                            if let Term::VariableRef(("y", ..)) = &**a {
                                if let Term::VariableRef(("z", ..)) = &**e {
                                    return;
                                }
                            }
                        }
                    }
                }
            }
        }
        panic!("Wrong CST: {:?}", cst);
    }
}
