use aiahr_core::{
    cst::{CommaSep, Field, IdField, Item, Pattern, ProductRow, SumRow, Term},
    diagnostic::parser::ParseErrors,
    loc::Loc,
    span::{Span, SpanOf, Spanned},
    token::Token,
};
use bumpalo::Bump;
use chumsky::{
    prelude::{choice, empty, end, just, recursive},
    select, Parser, Stream,
};

use crate::expr::{postfix, prefix};

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
    .map_with_span(|s, span: Span| span.of(s))
}

// Returns a parser for either `parser` or the empty string.
fn option<'i, T>(
    parser: impl Clone + Parser<Token<'i>, T, Error = ParseErrors<'i>>,
) -> impl Clone + Parser<Token<'i>, Option<T>, Error = ParseErrors<'i>> {
    choice((parser.map(Some), empty().map(|_| None)))
}

// Returns a parser for one or more comma-separated `T` values, represented by `CommaSep<T>`.
fn comma_sep<'a, 'i, T: 'a>(
    arena: &'a Bump,
    elem: impl Clone + Parser<Token<'i>, T, Error = ParseErrors<'i>>,
) -> impl Clone + Parser<Token<'i>, CommaSep<'a, T>, Error = ParseErrors<'i>> {
    elem.clone()
        .then(lit(Token::Comma).then(elem).repeated())
        .then(choice((lit(Token::Comma).map(Some), empty().map(|_| None))))
        .map(|((first, elems), comma)| CommaSep {
            first,
            elems: arena.alloc_slice_fill_iter(elems.into_iter()),
            comma,
        })
}

// Returns a parser for a field with a label, separator, and target.
fn field<'i, L, T>(
    label: impl Clone + Parser<Token<'i>, L, Error = ParseErrors<'i>>,
    sep: Token<'i>,
    target: impl Clone + Parser<Token<'i>, T, Error = ParseErrors<'i>>,
) -> impl Clone + Parser<Token<'i>, Field<L, T>, Error = ParseErrors<'i>> {
    label
        .then(lit(sep))
        .then(target)
        .map(|((label, sep), target)| Field { label, sep, target })
}

// Returns a parser for a field with an identifier label, separator, and target.
fn id_field<'i, T>(
    sep: Token<'i>,
    target: impl Clone + Parser<Token<'i>, T, Error = ParseErrors<'i>>,
) -> impl Clone + Parser<Token<'i>, IdField<'i, T>, Error = ParseErrors<'i>> {
    field(ident(), sep, target)
}

// Returns a parser for a product row with terms in `term`.
fn product_row<'a, 'i: 'a, T: 'a>(
    arena: &'a Bump,
    term: impl Clone + Parser<Token<'i>, T, Error = ParseErrors<'i>>,
) -> impl Clone + Parser<Token<'i>, ProductRow<'a, 'i, T>, Error = ParseErrors<'i>> {
    lit(Token::LBrace)
        .then(option(comma_sep(
            arena,
            id_field(Token::Equal, term.clone()),
        )))
        .then(lit(Token::RBrace))
        .map(|((lbrace, fields), rbrace)| ProductRow {
            lbrace,
            fields,
            rbrace,
        })
}

// Returns a parser for a sum row with terms in `term`.
fn sum_row<'i, T>(
    term: impl Clone + Parser<Token<'i>, T, Error = ParseErrors<'i>>,
) -> impl Clone + Parser<Token<'i>, SumRow<'i, T>, Error = ParseErrors<'i>> {
    lit(Token::LAngle)
        .then(id_field(Token::Equal, term.clone()))
        .then(lit(Token::RAngle))
        .map(|((langle, field), rangle)| SumRow {
            langle,
            field,
            rangle,
        })
}

// Returns a parser for a pattern.
fn pattern<'a, 'i: 'a>(
    arena: &'a Bump,
) -> impl Clone + Parser<Token<'i>, &'a Pattern<'a, 'i, &'i str>, Error = ParseErrors<'i>> {
    recursive(|pattern| {
        choice((
            product_row(arena, pattern.clone()).map(|p| arena.alloc(Pattern::ProductRow(p)) as &_),
            sum_row(pattern.clone()).map(|s| arena.alloc(Pattern::SumRow(s)) as &_),
            ident().map(|v| arena.alloc(Pattern::Whole(v)) as &_),
        ))
    })
}

enum TermPrefix<'a, 'i, N> {
    Binding {
        var: SpanOf<N>,
        eq: Span,
        value: &'a Term<'a, 'i, N>,
        semi: Span,
    },
    Handle {
        with: Span,
        handler: &'a Term<'a, 'i, N>,
        do_: Span,
    },
    Abstraction {
        lbar: Span,
        arg: SpanOf<N>,
        rbar: Span,
    },
}

impl<'a, 'i, N> TermPrefix<'a, 'i, N> {
    fn apply(self, arena: &'a Bump, t: &'a Term<'a, 'i, N>) -> &'a Term<'a, 'i, N> {
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
            TermPrefix::Handle { with, handler, do_ } => arena.alloc(Term::Handle {
                with,
                handler,
                do_,
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

enum TermPostfix<'a, 'i, N> {
    Application {
        lpar: Span,
        arg: &'a Term<'a, 'i, N>,
        rpar: Span,
    },
    DotAccess {
        dot: Span,
        field: SpanOf<&'i str>,
    },
}

impl<'a, 'i, N> TermPostfix<'a, 'i, N> {
    fn apply(self, arena: &'a Bump, t: &'a Term<'a, 'i, N>) -> &'a Term<'a, 'i, N> {
        match self {
            TermPostfix::Application { lpar, arg, rpar } => arena.alloc(Term::Application {
                func: t,
                lpar,
                arg,
                rpar,
            }),
            TermPostfix::DotAccess { dot, field } => arena.alloc(Term::DotAccess {
                base: t,
                dot,
                field,
            }),
        }
    }
}

/// Returns a parser for terms.
pub fn term<'a, 'i: 'a>(
    arena: &'a Bump,
) -> impl Parser<Token<'i>, &'a Term<'a, 'i, &'i str>, Error = ParseErrors<'i>> {
    recursive(|term| {
        // intermediary we use in atom and app
        let paren_term = lit(Token::LParen)
            .then(term.clone())
            .then(lit(Token::RParen));

        // Product row
        let prod = product_row(arena, term.clone()).map(|p| arena.alloc(Term::ProductRow(p)) as &_);

        let sum = sum_row(term.clone()).map(|s| arena.alloc(Term::SumRow(s)) as &_);

        let match_ = lit(Token::KwMatch)
            .then(lit(Token::LAngle))
            .then(comma_sep(
                arena,
                field(pattern(arena), Token::BigArrow, term.clone()),
            ))
            .then(lit(Token::RAngle))
            .map(|(((match_, langle), cases), rangle)| {
                arena.alloc(Term::Match {
                    match_,
                    langle,
                    cases,
                    rangle,
                }) as &_
            });

        let atom = choice((
            // variable
            ident().map(|s| arena.alloc(Term::SymbolRef(s)) as &_),
            // explicit term precedence
            paren_term.clone().map(|((lpar, t), rpar)| {
                arena.alloc(Term::Parenthesized {
                    lpar,
                    term: t,
                    rpar,
                }) as &_
            }),
            prod,
            sum,
            match_,
        ));

        // Function application
        let app =
            paren_term.map(|((lpar, arg), rpar)| TermPostfix::Application { lpar, arg, rpar });

        // Field access
        let access = lit(Token::Dot)
            .then(ident())
            .map(|(dot, field)| TermPostfix::DotAccess { dot, field });

        let app_access = postfix(atom.clone(), choice((app, access)), |t, p| {
            p.apply(arena, t)
        });

        // Local variable binding
        let local_bind = ident()
            .then(lit(Token::Equal))
            .then(term.clone())
            .then(lit(Token::Semicolon))
            .map(|(((var, eq), val), semi)| TermPrefix::Binding {
                var,
                eq,
                value: val,
                semi,
            });

        let handle = lit(Token::KwWith)
            .then(term)
            .then(lit(Token::KwDo))
            .map(|((with, handler), do_)| TermPrefix::Handle { with, handler, do_ });

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
        let bound = prefix(choice((local_bind, handle, closure)), app_access, |p, t| {
            p.apply(arena, t)
        });
        bound
    })
}

/// Returns a parser for the Aiahr language, using the given arena to allocate CST nodes.
pub fn aiahr_parser<'a, 'i: 'a>(
    arena: &'a Bump,
) -> impl Parser<Token<'i>, &'a [Item<'a, 'i, &'i str>], Error = ParseErrors<'i>> {
    let term = ident()
        .then(lit(Token::Equal))
        .then(term(arena))
        .map(|((name, eq), value)| Item::Term { name, eq, value });

    choice((term,))
        .repeated()
        .map(|items| arena.alloc_slice_fill_iter(items.into_iter()) as &[_])
        .then_ignore(end())
}

/// Converts lexer output to a stream readable by a Chumsky parser.
pub fn to_stream<'i, I>(
    tokens: I,
    end_of_input: Loc,
) -> Stream<'i, Token<'i>, Span, Box<dyn Iterator<Item = (Token<'i>, Span)> + 'i>>
where
    I: IntoIterator<Item = SpanOf<Token<'i>>>,
    I::IntoIter: 'i,
{
    // TODO: figure out what the `eoi` parameter is actually used for.
    Stream::from_iter(
        Span::zero(end_of_input),
        Box::new(tokens.into_iter().map(|token| (token.value, token.span()))),
    )
}

#[cfg(test)]
mod tests {
    use aiahr_core::cst::{Item, Term};
    use bumpalo::Bump;
    use chumsky::{prelude::end, Parser};

    use crate::lexer::aiahr_lexer;

    use super::{aiahr_parser, term, to_stream};

    fn parse_term_unwrap<'a, 'i: 'a>(arena: &'a Bump, input: &'i str) -> &'a Term<'a, 'i, &'i str> {
        let (tokens, eoi) = aiahr_lexer().lex(input).unwrap();
        term(arena)
            .then_ignore(end())
            .parse(to_stream(tokens, eoi))
            .unwrap()
    }

    fn parse_file_unwrap<'a, 'i: 'a>(
        arena: &'a Bump,
        input: &'i str,
    ) -> &'a [Item<'a, 'i, &'i str>] {
        let (tokens, eoi) = aiahr_lexer().lex(input).unwrap();
        aiahr_parser(arena).parse(to_stream(tokens, eoi)).unwrap()
    }

    #[test]
    fn test_undelimted_closure_fails() {
        let arena = Bump::new();
        let (tokens, eoi) = aiahr_lexer().lex("|x whoops(x)").unwrap();
        assert_matches!(term(&arena).parse(to_stream(tokens, eoi)), Err(..));
    }

    #[test]
    fn test_app_precedence() {
        assert_matches!(
            parse_term_unwrap(&Bump::new(), "(|x| |w| w)(y)(z)"),
            term_app!(
                term_app!(
                    term_paren!(term_abs!("x", term_abs!("w", term_sym!("w")))),
                    term_sym!("y")
                ),
                term_sym!("z")
            )
        );
    }

    #[test]
    fn test_with_do() {
        assert_matches!(
            parse_term_unwrap(&Bump::new(), "with h do a(b)"),
            term_with!(term_sym!("h"), term_app!(term_sym!("a"), term_sym!("b")))
        );
    }

    #[test]
    fn test_mixing_prefixes() {
        assert_matches!(
            parse_term_unwrap(&Bump::new(), "|x| y = |z| y(z); w = x(y); w"),
            term_abs!(
                "x",
                term_local!(
                    "y",
                    term_abs!("z", term_app!(term_sym!("y"), term_sym!("z"))),
                    term_local!(
                        "w",
                        term_app!(term_sym!("x"), term_sym!("y")),
                        term_sym!("w")
                    )
                )
            )
        );
    }

    #[test]
    fn test_basic_lambdas() {
        assert_matches!(
            parse_term_unwrap(&Bump::new(), "|x| |y| z = x(y); z"),
            term_abs!(
                "x",
                term_abs!(
                    "y",
                    term_local!(
                        "z",
                        term_app!(term_sym!("x"), term_sym!("y")),
                        term_sym!("z")
                    )
                )
            )
        );
    }

    #[test]
    fn test_product_rows() {
        assert_matches!(parse_term_unwrap(&Bump::new(), "{}"), term_prod!());
        assert_matches!(
            parse_term_unwrap(&Bump::new(), "{x = a, y = |t| t}"),
            term_prod!(
                id_field!("x", term_sym!("a")),
                id_field!("y", term_abs!("t", term_sym!("t"))),
            )
        );
    }

    #[test]
    fn test_product_rows_precedence() {
        assert_matches!(
            parse_term_unwrap(&Bump::new(), "{x = |t| t}({y = |t| u})"),
            term_app!(
                term_prod!(id_field!("x", term_abs!("t", term_sym!("t")))),
                term_prod!(id_field!("y", term_abs!("t", term_sym!("u"))))
            )
        );
    }

    #[test]
    fn test_field_access() {
        assert_matches!(
            parse_term_unwrap(&Bump::new(), "{x = a, y = b}.x"),
            term_dot!(
                term_prod!(
                    id_field!("x", term_sym!("a")),
                    id_field!("y", term_sym!("b")),
                ),
                "x"
            )
        );
    }

    #[test]
    fn test_combined_postfixes() {
        assert_matches!(
            parse_term_unwrap(&Bump::new(), "a.x(b)"),
            term_app!(term_dot!(term_sym!("a"), "x"), term_sym!("b"))
        );
    }

    #[test]
    fn test_sum_rows() {
        assert_matches!(
            parse_term_unwrap(&Bump::new(), "<x = |t| t>"),
            term_sum!(id_field!("x", term_abs!("t", term_sym!("t"))))
        );
    }

    #[test]
    fn test_matches() {
        assert_matches!(
            parse_term_unwrap(&Bump::new(), "match < {x = a} => a, <y = b> => b, c => c >"),
            term_match!(
                field!(pat_prod!(id_field!("x", pat_var!("a"))), term_sym!("a")),
                field!(pat_sum!(id_field!("y", pat_var!("b"))), term_sym!("b")),
                field!(pat_var!("c"), term_sym!("c")),
            )
        );
    }

    #[test]
    fn test_term_items() {
        assert_matches!(
            parse_file_unwrap(&Bump::new(), "x = a\ny = |b| b\nz = t = x; t"),
            &[
                item_term!("x", term_sym!("a")),
                item_term!("y", term_abs!("b", term_sym!("b"))),
                item_term!("z", term_local!("t", term_sym!("x"), term_sym!("t"))),
            ]
        );
    }
}
