use aiahr_core::{
    cst::{
        Annotation, Constraint, EffectOp, Field, IdField, Item, Pattern, ProductRow, Qualifiers,
        Quantifier, Row, RowAtom, Scheme, Separated, SumRow, Term, Type, TypeAnnotation,
    },
    diagnostic::parser::ParseErrors,
    loc::Loc,
    memory::handle::RefHandle,
    span::{Span, SpanOf, Spanned},
    token::Token,
};
use bumpalo::Bump;
use chumsky::{
    prelude::{choice, empty, end, just, recursive},
    select, Parser, Stream,
};

use crate::expr::{infixr1, postfix, prefix};

/// A trait alias for a cloneable parser for Aiahr syntax.
pub trait AiahrParser<'s, T>: Clone + Parser<Token<'s>, T, Error = ParseErrors<'s>> {}
impl<'s, T, A> AiahrParser<'s, T> for A where
    A: Clone + Parser<Token<'s>, T, Error = ParseErrors<'s>>
{
}

// Returns a spanned parser that matches just the given token and returns ().
fn lit(token: Token<'_>) -> impl AiahrParser<'_, Span> {
    just(token).map_with_span(|_, span| span)
}

// Returns a spanned parser that matches any `Token::Identifier` and unwraps it to the contained
// `&str`.
fn ident<'s>() -> impl AiahrParser<'s, SpanOf<RefHandle<'s, str>>> {
    select! {
        Token::Identifier(id) => id,
    }
    .map_with_span(|s, span: Span| span.of(s))
}

// Returns a parser for either `parser` or the empty string.
fn option<'s, T>(parser: impl AiahrParser<'s, T>) -> impl AiahrParser<'s, Option<T>> {
    choice((parser.map(Some), empty().map(|_| None)))
}

// Returns a parser for one or more `T` values separated by `sep`, representing the sequence with
// `Separated<T>`.
fn separated<'a, 's, T: 'a>(
    arena: &'a Bump,
    elem: impl AiahrParser<'s, T>,
    sep: impl AiahrParser<'s, Span>,
) -> impl AiahrParser<'s, Separated<'a, T>> {
    elem.clone()
        .then(sep.clone().then(elem).repeated())
        .then(choice((sep.map(Some), empty().map(|_| None))))
        .map(|((first, elems), comma)| Separated {
            first,
            elems: arena.alloc_slice_fill_iter(elems.into_iter()),
            comma,
        })
}

// Returns a parser for a field with a label, separator, and target.
fn field<'s, L, T>(
    label: impl AiahrParser<'s, L>,
    sep: Token<'s>,
    target: impl AiahrParser<'s, T>,
) -> impl AiahrParser<'s, Field<L, T>> {
    label
        .then(lit(sep))
        .then(target)
        .map(|((label, sep), target)| Field { label, sep, target })
}

// Returns a parser for a field with an identifier label, separator, and target.
fn id_field<'s, T>(
    sep: Token<'s>,
    target: impl AiahrParser<'s, T>,
) -> impl AiahrParser<'s, IdField<'s, T>> {
    field(ident(), sep, target)
}

// Returns a parser for a product row with terms in `term`.
fn product_row<'a, 's: 'a, T: 'a>(
    arena: &'a Bump,
    term: impl AiahrParser<'s, T>,
) -> impl AiahrParser<'s, ProductRow<'a, 's, T>> {
    lit(Token::LBrace)
        .then(option(separated(
            arena,
            id_field(Token::Equal, term),
            lit(Token::Comma),
        )))
        .then(lit(Token::RBrace))
        .map(|((lbrace, fields), rbrace)| ProductRow {
            lbrace,
            fields,
            rbrace,
        })
}

// Returns a parser for a sum row with terms in `term`.
fn sum_row<'s, T>(term: impl AiahrParser<'s, T>) -> impl AiahrParser<'s, SumRow<'s, T>> {
    lit(Token::LAngle)
        .then(id_field(Token::Equal, term))
        .then(lit(Token::RAngle))
        .map(|((langle, field), rangle)| SumRow {
            langle,
            field,
            rangle,
        })
}

// Returns a parser for a non-empty row of `C` in an explicit type.
fn row<'a, 's: 'a, C: 'a>(
    arena: &'a Bump,
    field: impl AiahrParser<'s, C>,
) -> impl AiahrParser<'s, Row<'a, RefHandle<'s, str>, C>> {
    let concrete = separated(arena, field, lit(Token::Comma));
    let variables = separated(arena, ident(), lit(Token::Plus));
    choice((
        concrete
            .clone()
            .then(option(lit(Token::VerticalBar).then(variables.clone())))
            .map(|(concrete, maybe_vars)| match maybe_vars {
                Some((vbar, variables)) => Row::Mixed {
                    concrete,
                    vbar,
                    variables,
                },
                None => Row::Concrete(concrete),
            }),
        variables.map(Row::Variable),
    ))
}

/// Returns a parser for an Aiahr (mono-)type.
pub fn type_<'a, 's: 'a>(
    arena: &'a Bump,
) -> impl AiahrParser<'s, &'a Type<'a, 's, RefHandle<'s, str>>> {
    recursive(|type_| {
        let type_row = row(arena, id_field(Token::Colon, type_.clone()));

        // Named type.
        let named = ident().map(|n| arena.alloc(Type::Named(n)) as &_);

        // Sum type.
        let sum = lit(Token::LAngle)
            .then(type_row.clone())
            .then(lit(Token::RAngle))
            .map(|((langle, variants), rangle)| {
                arena.alloc(Type::Sum {
                    langle,
                    variants,
                    rangle,
                }) as &_
            });

        // Product type.
        let prod = lit(Token::LBrace)
            .then(option(type_row))
            .then(lit(Token::RBrace))
            .map(|((lbrace, fields), rbrace)| {
                arena.alloc(Type::Product {
                    lbrace,
                    fields,
                    rbrace,
                }) as &_
            });

        // Parenthesized type.
        let paren =
            lit(Token::LParen)
                .then(type_)
                .then(lit(Token::RParen))
                .map(|((lpar, type_), rpar)| {
                    arena.alloc(Type::Parenthesized { lpar, type_, rpar }) as &_
                });

        // Function type.
        infixr1(
            choice((named, sum, prod, paren)),
            lit(Token::SmallArrow),
            |domain, arrow, codomain| {
                arena.alloc(Type::Function {
                    domain,
                    arrow,
                    codomain,
                }) as &_
            },
        )
    })
}

// Returns a parser for a row type expression.
fn row_atom<'a, 's: 'a>(
    arena: &'a Bump,
) -> impl AiahrParser<'s, RowAtom<'a, 's, RefHandle<'s, str>>> {
    choice((
        lit(Token::LParen)
            .then(separated(
                arena,
                id_field(Token::Colon, type_(arena)),
                lit(Token::Comma),
            ))
            .then(lit(Token::RParen))
            .map(|((lpar, fields), rpar)| RowAtom::Concrete { lpar, fields, rpar }),
        ident().map(RowAtom::Variable),
    ))
}

// Returns a parser for a type constraint.
fn constraint<'a, 's: 'a>(
    arena: &'a Bump,
) -> impl AiahrParser<'s, Constraint<'a, 's, RefHandle<'s, str>>> {
    let row = row_atom(arena);
    row.clone()
        .then(lit(Token::Plus))
        .then(row.clone())
        .then(lit(Token::Equal))
        .then(row)
        .map(|((((lhs, plus), rhs), eq), goal)| Constraint::RowSum {
            lhs,
            plus,
            rhs,
            eq,
            goal,
        })
}

/// Returns a parser for a scheme (polymorphic type).
pub fn scheme<'a, 's: 'a>(
    arena: &'a Bump,
) -> impl AiahrParser<'s, &'a Scheme<'a, 's, RefHandle<'s, str>>> {
    lit(Token::KwForall)
        .then(ident())
        .then(lit(Token::Dot))
        .map(|((forall, var), dot)| Quantifier { forall, var, dot })
        .repeated()
        .map(|qs| arena.alloc_slice_fill_iter(qs.into_iter()) as &[_])
        .then(option(
            separated(arena, constraint(arena), lit(Token::Comma))
                .then(lit(Token::BigArrow))
                .map(|(constraints, arrow)| Qualifiers { constraints, arrow }),
        ))
        .then(type_(arena))
        .map(|((quantifiers, qualifiers), type_)| {
            arena.alloc(Scheme {
                quantifiers,
                qualifiers,
                type_,
            }) as &_
        })
}

// Returns a parser for an effect operation.
fn effect_op<'a, 's: 'a>(
    arena: &'a Bump,
) -> impl AiahrParser<'s, EffectOp<'a, 's, RefHandle<'s, str>, RefHandle<'s, str>>> {
    ident()
        .then(lit(Token::Colon))
        .then(type_(arena))
        .map(|((name, colon), type_)| EffectOp { name, colon, type_ })
}

/// Returns a parser for an annotation with a given type syntax.
pub fn annotation<'s, T>(ty: impl AiahrParser<'s, T>) -> impl AiahrParser<'s, Annotation<T>> {
    lit(Token::Colon)
        .then(ty)
        .map(|(colon, type_)| Annotation { colon, type_ })
}

/// Returns a parser for a pattern.
pub fn pattern<'a, 's: 'a>(arena: &'a Bump) -> impl AiahrParser<'s, &'a Pattern<'a, 's>> {
    recursive(|pattern| {
        choice((
            product_row(arena, pattern.clone()).map(|p| arena.alloc(Pattern::ProductRow(p)) as &_),
            sum_row(pattern.clone()).map(|s| arena.alloc(Pattern::SumRow(s)) as &_),
            ident().map(|v| arena.alloc(Pattern::Whole(v)) as &_),
        ))
    })
}

enum TermPrefix<'a, 's> {
    Binding {
        var: SpanOf<RefHandle<'s, str>>,
        annotation: Option<TypeAnnotation<'a, 's, RefHandle<'s, str>>>,
        eq: Span,
        value: &'a Term<'a, 's>,
        semi: Span,
    },
    Handle {
        with: Span,
        handler: &'a Term<'a, 's>,
        do_: Span,
    },
    Abstraction {
        lbar: Span,
        arg: SpanOf<RefHandle<'s, str>>,
        annotation: Option<TypeAnnotation<'a, 's, RefHandle<'s, str>>>,
        rbar: Span,
    },
}

impl<'a, 's> TermPrefix<'a, 's> {
    fn apply(self, arena: &'a Bump, t: &'a Term<'a, 's>) -> &'a Term<'a, 's> {
        match self {
            TermPrefix::Binding {
                var,
                annotation,
                eq,
                value,
                semi,
            } => arena.alloc(Term::Binding {
                var,
                annotation,
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
            TermPrefix::Abstraction {
                lbar,
                arg,
                annotation,
                rbar,
            } => arena.alloc(Term::Abstraction {
                lbar,
                arg,
                annotation,
                rbar,
                body: t,
            }),
        }
    }
}

enum TermPostfix<'a, 's> {
    Application {
        lpar: Span,
        arg: &'a Term<'a, 's>,
        rpar: Span,
    },
    DotAccess {
        dot: Span,
        field: SpanOf<RefHandle<'s, str>>,
    },
}

impl<'a, 's> TermPostfix<'a, 's> {
    fn apply(self, arena: &'a Bump, t: &'a Term<'a, 's>) -> &'a Term<'a, 's> {
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
pub fn term<'a, 's: 'a>(
    arena: &'a Bump,
) -> impl Parser<Token<'s>, &'a Term<'a, 's>, Error = ParseErrors<'s>> {
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
            .then(separated(
                arena,
                field(pattern(arena), Token::BigArrow, term.clone()),
                lit(Token::Comma),
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
            .then(option(annotation(type_(arena))))
            .then(lit(Token::Equal))
            .then(term.clone())
            .then(lit(Token::Semicolon))
            .map(
                |((((var, annotation), eq), val), semi)| TermPrefix::Binding {
                    var,
                    annotation,
                    eq,
                    value: val,
                    semi,
                },
            );

        let handle = lit(Token::KwWith)
            .then(term)
            .then(lit(Token::KwDo))
            .map(|((with, handler), do_)| TermPrefix::Handle { with, handler, do_ });

        // Lambda abstraction
        let closure = lit(Token::VerticalBar)
            .then(ident())
            .then(option(annotation(type_(arena))))
            .then(lit(Token::VerticalBar))
            .map(
                |(((lbar, var), annotation), rbar)| TermPrefix::Abstraction {
                    lbar,
                    arg: var,
                    annotation,
                    rbar,
                },
            );

        // Term parser
        // We need to construct our parse tree here bottom to get associativity of bindings and
        // closures correct. However we're recursive descent, so we only go top-down. To remedy
        // this we construct a series of prefixes top-down that are applied to the final expression
        // in right associative order.

        prefix(choice((local_bind, handle, closure)), app_access, |p, t| {
            p.apply(arena, t)
        })
    })
}

/// Returns a parser for the Aiahr language, using the given arena to allocate CST nodes.
pub fn aiahr_parser<'a, 's: 'a>(
    arena: &'a Bump,
) -> impl Parser<Token<'s>, &'a [Item<'a, 's>], Error = ParseErrors<'s>> {
    let effect = lit(Token::KwEffect)
        .then(ident())
        .then(lit(Token::LBrace))
        .then(effect_op(arena).repeated())
        .then(lit(Token::RBrace))
        .map(|((((effect, name), lbrace), ops), rbrace)| Item::Effect {
            effect,
            name,
            lbrace,
            ops: arena.alloc_slice_fill_iter(ops),
            rbrace,
        });

    let term = ident()
        .then(option(annotation(scheme(arena))))
        .then(lit(Token::Equal))
        .then(term(arena))
        .map(|(((name, annotation), eq), value)| Item::Term {
            name,
            annotation,
            eq,
            value,
        });

    choice((effect, term))
        .repeated()
        .map(|items| arena.alloc_slice_fill_iter(items.into_iter()) as &[_])
        .then_ignore(end())
}

/// Converts lexer output to a stream readable by a Chumsky parser.
pub fn to_stream<'s, I>(
    tokens: I,
    end_of_input: Loc,
) -> Stream<'s, Token<'s>, Span, Box<dyn Iterator<Item = (Token<'s>, Span)> + 's>>
where
    I: IntoIterator<Item = SpanOf<Token<'s>>>,
    I::IntoIter: 's,
{
    // TODO: figure out what the `eoi` parameter is actually used for.
    Stream::from_iter(
        Span::zero(end_of_input),
        Box::new(tokens.into_iter().map(|token| (token.value, token.span()))),
    )
}

pub mod test_utils {
    use super::*;
    use crate::lexer::aiahr_lexer;
    use aiahr_core::{id::ModuleId, memory::intern::InternerByRef};

    const MOD: ModuleId = ModuleId(0);

    pub fn parse_term<'a, S: InternerByRef<'a, str>>(
        arena: &'a Bump,
        interner: &'a S,
        input: &str,
    ) -> &'a Term<'a, 'a> {
        let (tokens, eoi) = aiahr_lexer(interner)
            .lex(MOD, input)
            .expect("Lexing input failed");
        term(arena)
            .parse(to_stream(tokens, eoi))
            .expect("Parsing input failed")
    }
}

#[cfg(test)]
mod tests {
    use aiahr_core::{
        cst::{Item, Scheme, Term, Type},
        ct_rowsum, eff_op, field,
        id::ModuleId,
        id_field, item_effect, item_term,
        memory::{
            handle::RefHandle,
            intern::{InternerByRef, SyncInterner},
        },
        pat_prod, pat_sum, pat_var, qual, quant, row_concrete, row_mixed, row_variable,
        rwx_concrete, rwx_variable, scheme, term_abs, term_app, term_dot, term_local, term_match,
        term_paren, term_prod, term_sum, term_sym, term_with, type_func, type_named, type_par,
        type_prod, type_sum,
    };
    use assert_matches::assert_matches;
    use bumpalo::Bump;
    use chumsky::{prelude::end, Parser};

    use crate::lexer::aiahr_lexer;

    use super::{aiahr_parser, scheme, term, to_stream, type_};

    const MOD: ModuleId = ModuleId(0);

    fn parse_type_unwrap<'a, 's: 'a, S: InternerByRef<'s, str>>(
        arena: &'a Bump,
        interner: &'s S,
        input: &str,
    ) -> &'a Type<'a, 's, RefHandle<'s, str>> {
        let (tokens, eoi) = aiahr_lexer(interner).lex(MOD, input).unwrap();
        type_(arena)
            .then_ignore(end())
            .parse(to_stream(tokens, eoi))
            .unwrap()
    }

    fn parse_scheme_unwrap<'a, 's: 'a, S: InternerByRef<'s, str>>(
        arena: &'a Bump,
        interner: &'s S,
        input: &str,
    ) -> &'a Scheme<'a, 's, RefHandle<'s, str>> {
        let (tokens, eoi) = aiahr_lexer(interner).lex(MOD, input).unwrap();
        scheme(arena)
            .then_ignore(end())
            .parse(to_stream(tokens, eoi))
            .unwrap()
    }

    fn parse_term_unwrap<'a, 's: 'a, S: InternerByRef<'s, str>>(
        arena: &'a Bump,
        interner: &'s S,
        input: &str,
    ) -> &'a Term<'a, 's> {
        let (tokens, eoi) = aiahr_lexer(interner).lex(MOD, input).unwrap();
        term(arena)
            .then_ignore(end())
            .parse(to_stream(tokens, eoi))
            .unwrap()
    }

    fn parse_file_unwrap<'a, 's: 'a, S: InternerByRef<'s, str>>(
        arena: &'a Bump,
        interner: &'s S,
        input: &str,
    ) -> &'a [Item<'a, 's>] {
        let (tokens, eoi) = aiahr_lexer(interner).lex(MOD, input).unwrap();
        aiahr_parser(arena).parse(to_stream(tokens, eoi)).unwrap()
    }

    #[test]
    fn test_sum_types() {
        assert_matches!(
            parse_type_unwrap(
                &Bump::new(),
                &SyncInterner::new(&Bump::new()),
                "<x: a, y: b>"
            ),
            type_sum!(row_concrete!(
                id_field!("x", type_named!("a")),
                id_field!("y", type_named!("b"))
            ))
        );
        assert_matches!(
            parse_type_unwrap(&Bump::new(), &SyncInterner::new(&Bump::new()), "<r + s>"),
            type_sum!(row_variable!("r", "s"))
        );
        assert_matches!(
            parse_type_unwrap(&Bump::new(), &SyncInterner::new(&Bump::new()), "<x: a | r>"),
            type_sum!(row_mixed!((id_field!("x", type_named!("a"))), ("r")))
        );
    }

    #[test]
    fn test_product_types() {
        assert_matches!(
            parse_type_unwrap(&Bump::new(), &SyncInterner::new(&Bump::new()), "{}"),
            type_prod!()
        );
        assert_matches!(
            parse_type_unwrap(
                &Bump::new(),
                &SyncInterner::new(&Bump::new()),
                "{x: a, y: b}"
            ),
            type_prod!(row_concrete!(
                id_field!("x", type_named!("a")),
                id_field!("y", type_named!("b"))
            ))
        );
        assert_matches!(
            parse_type_unwrap(&Bump::new(), &SyncInterner::new(&Bump::new()), "{r + s}"),
            type_prod!(row_variable!("r", "s"))
        );
        assert_matches!(
            parse_type_unwrap(&Bump::new(), &SyncInterner::new(&Bump::new()), "{x: a | r}"),
            type_prod!(row_mixed!((id_field!("x", type_named!("a"))), ("r")))
        );
    }

    #[test]
    fn test_function_types() {
        // Make sure this example tests right-associativity.
        assert_matches!(
            parse_type_unwrap(
                &Bump::new(),
                &SyncInterner::new(&Bump::new()),
                "(a -> b) -> a -> (b -> c)"
            ),
            type_func!(
                type_par!(type_func!(type_named!("a"), type_named!("b"))),
                type_func!(
                    type_named!("a"),
                    type_par!(type_func!(type_named!("b"), type_named!("c")))
                )
            )
        );
    }

    #[test]
    fn test_mixed_types() {
        assert_matches!(
            parse_type_unwrap(
                &Bump::new(),
                &SyncInterner::new(&Bump::new()),
                "{x: a} -> <f: b -> a | r>"
            ),
            type_func!(
                type_prod!(row_concrete!(id_field!("x", type_named!("a")))),
                type_sum!(row_mixed!(
                    (id_field!("f", type_func!(type_named!("b"), type_named!("a")))),
                    ("r")
                ))
            )
        );
    }

    #[test]
    fn test_unqualified_schemes() {
        assert_matches!(
            parse_scheme_unwrap(
                &Bump::new(),
                &SyncInterner::new(&Bump::new()),
                "{x: a} -> <f: b -> a | r>"
            ),
            scheme!(type_func!(
                type_prod!(row_concrete!(id_field!("x", type_named!("a")))),
                type_sum!(row_mixed!(
                    (id_field!("f", type_func!(type_named!("b"), type_named!("a")))),
                    ("r")
                ))
            ))
        );
    }

    #[test]
    fn test_equals_schemes() {
        assert_matches!(
            parse_scheme_unwrap(
                &Bump::new(),
                &SyncInterner::new(&Bump::new()),
                "r + (y: a) = s => {r} -> a -> {s}"
            ),
            scheme!(
                qual!(ct_rowsum!(
                    rwx_variable!("r"),
                    rwx_concrete!(id_field!("y", type_named!("a"))),
                    rwx_variable!("s")
                )),
                type_func!(
                    type_prod!(row_variable!("r")),
                    type_func!(type_named!("a"), type_prod!(row_variable!("s")))
                )
            )
        );
    }

    #[test]
    fn test_undelimted_closure_fails() {
        let arena = Bump::new();
        let interner = SyncInterner::new(&arena);
        let (tokens, eoi) = aiahr_lexer(&interner).lex(MOD, "|x whoops(x)").unwrap();
        assert_matches!(term(&arena).parse(to_stream(tokens, eoi)), Err(..));
    }

    #[test]
    fn test_annotated_bindings() {
        assert_matches!(
            parse_term_unwrap(
                &Bump::new(),
                &SyncInterner::new(&Bump::new()),
                "x: a = {}; y: {} = {}; x"
            ),
            term_local!(
                "x",
                type_named!("a"),
                term_prod!(),
                term_local!("y", type_prod!(), term_prod!(), term_sym!("x"))
            )
        );
    }

    #[test]
    fn test_app_precedence() {
        assert_matches!(
            parse_term_unwrap(
                &Bump::new(),
                &SyncInterner::new(&Bump::new()),
                "(|x| |w| w)(y)(z)"
            ),
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
            parse_term_unwrap(
                &Bump::new(),
                &SyncInterner::new(&Bump::new()),
                "with h do a(b)"
            ),
            term_with!(term_sym!("h"), term_app!(term_sym!("a"), term_sym!("b")))
        );
    }

    #[test]
    fn test_mixing_prefixes() {
        assert_matches!(
            parse_term_unwrap(
                &Bump::new(),
                &SyncInterner::new(&Bump::new()),
                "|x| y = |z| y(z); w = x(y); w"
            ),
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
            parse_term_unwrap(
                &Bump::new(),
                &SyncInterner::new(&Bump::new()),
                "|x| |y: a| z = x(y); z"
            ),
            term_abs!(
                "x",
                term_abs!(
                    "y",
                    type_named!("a"),
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
        assert_matches!(
            parse_term_unwrap(&Bump::new(), &SyncInterner::new(&Bump::new()), "{}"),
            term_prod!()
        );
        assert_matches!(
            parse_term_unwrap(
                &Bump::new(),
                &SyncInterner::new(&Bump::new()),
                "{x = a, y = |t| t}"
            ),
            term_prod!(
                id_field!("x", term_sym!("a")),
                id_field!("y", term_abs!("t", term_sym!("t"))),
            )
        );
    }

    #[test]
    fn test_product_rows_precedence() {
        assert_matches!(
            parse_term_unwrap(
                &Bump::new(),
                &SyncInterner::new(&Bump::new()),
                "{x = |t| t}({y = |t| u})"
            ),
            term_app!(
                term_prod!(id_field!("x", term_abs!("t", term_sym!("t")))),
                term_prod!(id_field!("y", term_abs!("t", term_sym!("u"))))
            )
        );
    }

    #[test]
    fn test_field_access() {
        assert_matches!(
            parse_term_unwrap(
                &Bump::new(),
                &SyncInterner::new(&Bump::new()),
                "{x = a, y = b}.x"
            ),
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
            parse_term_unwrap(&Bump::new(), &SyncInterner::new(&Bump::new()), "a.x(b)"),
            term_app!(term_dot!(term_sym!("a"), "x"), term_sym!("b"))
        );
    }

    #[test]
    fn test_sum_rows() {
        assert_matches!(
            parse_term_unwrap(
                &Bump::new(),
                &SyncInterner::new(&Bump::new()),
                "<x = |t| t>"
            ),
            term_sum!(id_field!("x", term_abs!("t", term_sym!("t"))))
        );
    }

    #[test]
    fn test_matches() {
        assert_matches!(
            parse_term_unwrap(
                &Bump::new(),
                &SyncInterner::new(&Bump::new()),
                "match < {x = a} => a, <y = b> => b, c => c >"
            ),
            term_match!(
                field!(pat_prod!(id_field!("x", pat_var!("a"))), term_sym!("a")),
                field!(pat_sum!(id_field!("y", pat_var!("b"))), term_sym!("b")),
                field!(pat_var!("c"), term_sym!("c")),
            )
        );
    }

    #[test]
    fn test_effect_items() {
        assert_matches!(
            parse_file_unwrap(
                &Bump::new(),
                &SyncInterner::new(&Bump::new()),
                "effect foo { foo: a -> a }"
            ),
            &[item_effect!(
                "foo",
                eff_op!("foo", type_func!(type_named!("a"), type_named!("a")))
            )]
        );
    }

    #[test]
    fn test_term_items() {
        assert_matches!(
            parse_file_unwrap(
                &Bump::new(),
                &SyncInterner::new(&Bump::new()),
                "x = a\ny = |b| b\nz = t = x; t"
            ),
            &[
                item_term!("x", term_sym!("a")),
                item_term!("y", term_abs!("b", term_sym!("b"))),
                item_term!("z", term_local!("t", term_sym!("x"), term_sym!("t"))),
            ]
        );
        assert_matches!(
            parse_file_unwrap(
                &Bump::new(),
                &SyncInterner::new(&Bump::new()),
                "x: a = a\ny: forall b. b -> b = |b| b"
            ),
            &[
                item_term!("x", scheme!(type_named!("a")), term_sym!("a")),
                item_term!(
                    "y",
                    scheme!(
                        quant!("b"),
                        None,
                        type_func!(type_named!("b"), type_named!("b"))
                    ),
                    term_abs!("b", term_sym!("b"))
                ),
            ]
        );
    }
}
