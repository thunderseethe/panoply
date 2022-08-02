use bumpalo::Bump;
use chumsky::{
    prelude::{choice, empty, end, just, recursive},
    select, Parser, Stream,
};

use crate::{
    cst::{CommaSep, Field, Term},
    error::ParseErrors,
    loc::Loc,
    span::{Span, SpanOf},
    token::Token,
};

// Returns a spanned parser that matches just the given token and returns ().
fn lit<'i>(token: Token<'i>) -> impl Clone + Parser<Token<'i>, Span, Error = ParseErrors<'i>> {
    just(token).map_with_span(|_, span| span)
}

// Returns a spanned parser that matches any `Token::Identifier` and unwraps it to the contained
// `&str`.
fn ident<'i>() -> impl Clone + Parser<Token<'i>, SpanOf<&'i str>, Error = ParseErrors<'i>> {
    select! {
        Token::Identifier(id) => id,
    }
    .map_with_span(|s, span| (s, span))
}

// Returns a parser for comma-separated `T` values, represented by `Option<CommaSep<T>>`. `None`
// indicates an empty list.
fn comma_sep<'a, 'i, T: 'a>(
    arena: &'a Bump,
    elem: impl Clone + Parser<Token<'i>, T, Error = ParseErrors<'i>>,
) -> impl Clone + Parser<Token<'i>, Option<CommaSep<'a, T>>, Error = ParseErrors<'i>> {
    choice((
        elem.clone()
            .then(lit(Token::Comma).then(elem).repeated())
            .then(choice((lit(Token::Comma).map(Some), empty().map(|_| None))))
            .map(|((first, elems), comma)| {
                Some(CommaSep {
                    first,
                    elems: arena.alloc_slice_fill_iter(elems.into_iter()),
                    comma,
                })
            }),
        empty().map(|_| None),
    ))
}

// Returns a parser for a field with a label, separator, and target.
fn field<'i, T>(
    sep: Token<'i>,
    target: impl Clone + Parser<Token<'i>, T, Error = ParseErrors<'i>>,
) -> impl Clone + Parser<Token<'i>, Field<'i, T>, Error = ParseErrors<'i>> {
    ident()
        .then(lit(sep))
        .then(target)
        .map(|((label, sep), target)| Field { label, sep, target })
}

enum TermPrefix<'a, 'i> {
    Binding {
        var: SpanOf<&'i str>,
        eq: Span,
        value: &'a Term<'a, 'i>,
        semi: Span,
    },
    Abstraction {
        lbar: Span,
        arg: SpanOf<&'i str>,
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

enum TermSuffix<'a, 'i> {
    Application {
        lpar: Span,
        arg: &'a Term<'a, 'i>,
        rpar: Span,
    },
    FieldAccess {
        dot: Span,
        field: SpanOf<&'i str>,
    },
}

impl<'a, 'i> TermSuffix<'a, 'i> {
    fn apply(self, arena: &'a Bump, t: &'a Term<'a, 'i>) -> &'a Term<'a, 'i> {
        match self {
            TermSuffix::Application { lpar, arg, rpar } => arena.alloc(Term::Application {
                func: t,
                lpar,
                arg,
                rpar,
            }),
            TermSuffix::FieldAccess { dot, field } => arena.alloc(Term::FieldAccess {
                product: t,
                dot,
                field,
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

        // Product row
        let prod = lit(Token::LBrace)
            .then(comma_sep(arena, field(Token::Equal, term.clone())))
            .then(lit(Token::RBrace))
            .map(|((lbrace, fields), rbrace)| {
                arena.alloc(Term::ProductRow {
                    lbrace,
                    fields,
                    rbrace,
                }) as &Term
            });

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
            prod,
        ));

        // Function application
        let app = paren_term.map(|((lpar, arg), rpar)| TermSuffix::Application { lpar, arg, rpar });

        // Field access
        let access = lit(Token::Dot)
            .then(ident())
            .map(|(dot, field)| TermSuffix::FieldAccess { dot, field });

        let suffixed =
            atom.clone()
                .then(choice((app, access)).repeated())
                .map(|(expr, suffixes)| {
                    suffixes
                        .into_iter()
                        .fold(expr, |t, suffix| suffix.apply(arena, t))
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
        // this we construct a series of prefixes top-down that are applied to the final expression
        // in right associative order.
        let prefixed =
            choice((local_bind, closure))
                .repeated()
                .then(suffixed)
                .map(|(binds, expr)| {
                    binds
                        .into_iter()
                        .rfold(expr, |t, prefix| prefix.apply(arena, t))
                });
        prefixed
    })
    .then_ignore(end())
}

/// Converts lexer output to a stream readable by a Chumsky parser.
pub fn to_stream<'i, I: IntoIterator<Item = SpanOf<Token<'i>>>>(
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

    use crate::{
        cst::{CommaSep, Field, Term},
        lexer::aiahr_lexer,
    };

    use super::{aiahr_parser, to_stream};

    // CST pattern macros. Used to construct patterns that ignore spans.
    macro_rules! comma_sep {
        ($first:pat, $($elems:pat),*) => {
            Some(CommaSep {
                first: $first,
                elems: &[$((.., $elems)),*],
                ..
            })
        };
        () => {
            None
        };
    }
    macro_rules! field {
        ($label:pat, $target:pat) => {
            Field {
                label: ($label, ..),
                target: $target,
                ..
            }
        };
    }
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
    macro_rules! prod {
        ($fields:pat) => {
            &Term::ProductRow {
                fields: $fields,
                ..
            }
        };
    }
    macro_rules! get {
        ($product:pat, $field:pat) => {
            &Term::FieldAccess {
                product: $product,
                field: ($field, ..),
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

    #[test]
    fn test_product_rows() {
        assert_matches!(parse_unwrap(&Bump::new(), "{}"), prod!(comma_sep!()));
        assert_matches!(
            parse_unwrap(&Bump::new(), "{x = a, y = |t| t}"),
            prod!(comma_sep!(
                field!("x", var!("a")),
                field!("y", abs!("t", var!("t")))
            ))
        );
    }

    #[test]
    fn test_product_rows_precedence() {
        assert_matches!(
            parse_unwrap(&Bump::new(), "{x = |t| t}({y = |t| u})"),
            app!(
                prod!(comma_sep!(field!("x", abs!("t", var!("t"))),)),
                prod!(comma_sep!(field!("y", abs!("t", var!("u"))),))
            )
        );
    }

    #[test]
    fn test_field_access() {
        assert_matches!(
            parse_unwrap(&Bump::new(), "{x = a, y = b}.x"),
            get!(
                prod!(comma_sep!(field!("x", var!("a")), field!("y", var!("b")))),
                "x"
            )
        );
    }

    #[test]
    fn test_combined_suffixes() {
        assert_matches!(
            parse_unwrap(&Bump::new(), "a.x(b)"),
            app!(get!(var!("a"), "x"), var!("b"))
        );
    }
}
