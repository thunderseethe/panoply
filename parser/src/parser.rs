use aiahr_core::{
    cst::{
        Annotation, Constraint, EffectOp, Field, IdField, Item, Module, Pattern, ProductRow,
        Qualifiers, Quantifier, Row, RowAtom, Scheme, Separated, SumRow, Term, Type,
        TypeAnnotation,
    },
    diagnostic::parser::ParseErrors,
    ident::Ident,
    loc::Loc,
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
pub trait AiahrParser<T>: Clone + Parser<Token, T, Error = ParseErrors> {}
impl<T, A> AiahrParser<T> for A where A: Clone + Parser<Token, T, Error = ParseErrors> {}

// Returns a spanned parser that matches just the given token and returns ().
fn lit(token: Token) -> impl AiahrParser<Span> {
    just(token).map_with_span(|_, span| span)
}

// Returns a spanned parser that matches any `Token::Identifier` and unwraps it to the contained
// `&str`.
fn ident() -> impl AiahrParser<SpanOf<Ident>> {
    select! {
        Token::Identifier(id) => id,
    }
    .map_with_span(|s, span: Span| span.of(s))
}

// Returns a parser for either `parser` or the empty string.
fn option<T>(parser: impl AiahrParser<T>) -> impl AiahrParser<Option<T>> {
    choice((parser.map(Some), empty().map(|_| None)))
}

// Returns a parser for one or more `T` values separated by `sep`, representing the sequence with
// `Separated<T>`.
fn separated<'a, T: 'a>(
    arena: &'a Bump,
    elem: impl AiahrParser<T>,
    sep: impl AiahrParser<Span>,
) -> impl AiahrParser<Separated<'a, T>> {
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
fn field<L, T>(
    label: impl AiahrParser<L>,
    sep: Token,
    target: impl AiahrParser<T>,
) -> impl AiahrParser<Field<L, T>> {
    label
        .then(lit(sep))
        .then(target)
        .map(|((label, sep), target)| Field { label, sep, target })
}

// Returns a parser for a field with an identifier label, separator, and target.
fn id_field<T>(sep: Token, target: impl AiahrParser<T>) -> impl AiahrParser<IdField<T>> {
    field(ident(), sep, target)
}

// Returns a parser for a product row with terms in `term`.
fn product_row<'a, T: 'a>(
    arena: &'a Bump,
    term: impl AiahrParser<T>,
) -> impl AiahrParser<ProductRow<'a, T>> {
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
fn sum_row<T>(term: impl AiahrParser<T>) -> impl AiahrParser<SumRow<T>> {
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
fn row<'a, C: 'a>(
    arena: &'a Bump,
    field: impl AiahrParser<C>,
) -> impl AiahrParser<Row<'a, Ident, C>> {
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
pub fn type_(arena: &Bump) -> impl AiahrParser<&Type<'_, Ident>> {
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
fn row_atom(arena: &Bump) -> impl AiahrParser<RowAtom<'_, Ident>> {
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
fn constraint(arena: &Bump) -> impl AiahrParser<Constraint<'_, Ident>> {
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
pub fn scheme(arena: &Bump) -> impl AiahrParser<&Scheme<'_, Ident>> {
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
fn effect_op(arena: &Bump) -> impl AiahrParser<EffectOp<'_, Ident, Ident>> {
    ident()
        .then(lit(Token::Colon))
        .then(type_(arena))
        .map(|((name, colon), type_)| EffectOp { name, colon, type_ })
}

/// Returns a parser for an annotation with a given type syntax.
pub fn annotation<T>(ty: impl AiahrParser<T>) -> impl AiahrParser<Annotation<T>> {
    lit(Token::Colon)
        .then(ty)
        .map(|(colon, type_)| Annotation { colon, type_ })
}

/// Returns a parser for a pattern.
pub fn pattern(arena: &Bump) -> impl AiahrParser<&Pattern<'_>> {
    recursive(|pattern| {
        choice((
            product_row(arena, pattern.clone()).map(|p| arena.alloc(Pattern::ProductRow(p)) as &_),
            sum_row(pattern.clone()).map(|s| arena.alloc(Pattern::SumRow(s)) as &_),
            ident().map(|v| arena.alloc(Pattern::Whole(v)) as &_),
        ))
    })
}

enum TermPrefix<'a> {
    Binding {
        var: SpanOf<Ident>,
        annotation: Option<TypeAnnotation<'a, Ident>>,
        eq: Span,
        value: &'a Term<'a>,
        semi: Span,
    },
    Handle {
        with: Span,
        handler: &'a Term<'a>,
        do_: Span,
    },
    Abstraction {
        lbar: Span,
        arg: SpanOf<Ident>,
        annotation: Option<TypeAnnotation<'a, Ident>>,
        rbar: Span,
    },
}

impl<'a> TermPrefix<'a> {
    fn apply(self, arena: &'a Bump, t: &'a Term<'a>) -> &'a Term<'a> {
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

enum TermPostfix<'a> {
    Application {
        lpar: Span,
        arg: &'a Term<'a>,
        rpar: Span,
    },
    DotAccess {
        dot: Span,
        field: SpanOf<Ident>,
    },
}

impl<'a> TermPostfix<'a> {
    fn apply(self, arena: &'a Bump, t: &'a Term<'a>) -> &'a Term<'a> {
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
pub fn term(arena: &Bump) -> impl Parser<Token, &Term<'_>, Error = ParseErrors> {
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
pub fn aiahr_parser(arena: &Bump) -> impl Parser<Token, Module<'_>, Error = ParseErrors> {
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
        .map(|items| Module {
            items: arena.alloc_slice_fill_iter(items.into_iter()) as &[_],
        })
        .then_ignore(end())
}

/// Converts lexer output to a stream readable by a Chumsky parser.
pub fn to_stream<'s, I>(
    tokens: I,
    end_of_input: Loc,
) -> Stream<'s, Token, Span, Box<dyn Iterator<Item = (Token, Span)> + 's>>
where
    I: IntoIterator<Item = SpanOf<Token>>,
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
    use aiahr_core::id::ModuleId;

    const MOD: ModuleId = ModuleId(0);

    pub fn parse_term<'a>(db: &'a dyn crate::Db, arena: &'a Bump, input: &str) -> &'a Term<'a> {
        let (tokens, eoi) = aiahr_lexer(db)
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
        cst::{Module, Scheme, Term, Type},
        ct_rowsum, eff_op, field,
        id::ModuleId,
        id_field,
        ident::Ident,
        item_effect, item_term, pat_prod, pat_sum, pat_var, qual, quant, row_concrete, row_mixed,
        row_variable, rwx_concrete, rwx_variable, scheme, term_abs, term_app, term_dot, term_local,
        term_match, term_paren, term_prod, term_sum, term_sym, term_with, type_func, type_named,
        type_par, type_prod, type_sum,
    };
    use assert_matches::assert_matches;
    use bumpalo::Bump;
    use chumsky::{prelude::end, Parser};

    use aiahr_test::assert_ident_text_matches_name;

    use crate::lexer::aiahr_lexer;

    use super::{aiahr_parser, scheme, term, to_stream, type_};

    #[derive(Default)]
    #[salsa::db(crate::Jar, aiahr_core::Jar)]
    struct TestDatabase {
        storage: salsa::Storage<Self>,
    }
    impl salsa::Database for TestDatabase {}

    const MOD: ModuleId = ModuleId(0);

    fn parse_type_unwrap<'a>(
        db: &'a dyn crate::Db,
        arena: &'a Bump,
        input: &str,
    ) -> &'a Type<'a, Ident> {
        let (tokens, eoi) = aiahr_lexer(db).lex(MOD, input).unwrap();
        type_(arena)
            .then_ignore(end())
            .parse(to_stream(tokens, eoi))
            .unwrap()
    }

    fn parse_scheme_unwrap<'a>(
        db: &'a dyn crate::Db,
        arena: &'a Bump,
        input: &str,
    ) -> &'a Scheme<'a, Ident> {
        let (tokens, eoi) = aiahr_lexer(db).lex(MOD, input).unwrap();
        scheme(arena)
            .then_ignore(end())
            .parse(to_stream(tokens, eoi))
            .unwrap()
    }

    fn parse_term_unwrap<'a>(db: &'a dyn crate::Db, arena: &'a Bump, input: &str) -> &'a Term<'a> {
        let (tokens, eoi) = aiahr_lexer(db).lex(MOD, input).unwrap();
        term(arena)
            .then_ignore(end())
            .parse(to_stream(tokens, eoi))
            .unwrap()
    }

    fn parse_file_unwrap<'a>(db: &'a dyn crate::Db, arena: &'a Bump, input: &str) -> Module<'a> {
        let (tokens, eoi) = aiahr_lexer(db).lex(MOD, input).unwrap();
        aiahr_parser(arena).parse(to_stream(tokens, eoi)).unwrap()
    }

    #[test]
    fn test_sum_types() {
        let db = TestDatabase::default();
        assert_matches!(
            parse_type_unwrap(
                &db,
                &Bump::new(),
                "<x: a, y: b>"
            ),
            type_sum!(row_concrete!(
                id_field!(x, type_named!(a)),
                id_field!(y, type_named!(b))
            )) => assert_ident_text_matches_name!(db, [x, y, a, b])
        );
        assert_matches!(
            parse_type_unwrap(&db, &Bump::new(), "<r + s>"),
            type_sum!(row_variable!(r, s)) => assert_ident_text_matches_name!(db, [r, s])
        );
        assert_matches!(
            parse_type_unwrap(&db, &Bump::new(), "<x: a | r>"),
            type_sum!(row_mixed!((id_field!(x, type_named!(a))), (r))) => assert_ident_text_matches_name!(db, [x, a, r])
        );
    }

    #[test]
    fn test_product_types() {
        let db = TestDatabase::default();
        assert_matches!(parse_type_unwrap(&db, &Bump::new(), "{}"), type_prod!());
        assert_matches!(
            parse_type_unwrap(
                &db,
                &Bump::new(),
                "{x: a, y: b}"
            ),
            type_prod!(row_concrete!(
                id_field!(x, type_named!(a)),
                id_field!(y, type_named!(b))
            )) => { assert_ident_text_matches_name!(db, [x, y, a, b]); }
        );
        assert_matches!(
            parse_type_unwrap(&db, &Bump::new(), "{r + s}"),
            type_prod!(row_variable!(r, s)) => assert_ident_text_matches_name!(db, [r, s])
        );
        assert_matches!(
            parse_type_unwrap(&db, &Bump::new(), "{x: a | r}"),
            type_prod!(row_mixed!((id_field!(x, type_named!(a))), (r))) => assert_ident_text_matches_name!(db, [x, a, r])
        );
    }

    #[test]
    fn test_function_types() {
        let db = TestDatabase::default();
        // Make sure this example tests right-associativity.
        assert_matches!(
            parse_type_unwrap(
                &db,
                &Bump::new(),
                "(a -> b) -> a -> (b -> c)"
            ),
            type_func!(
                type_par!(type_func!(type_named!(a), type_named!(b))),
                type_func!(
                    type_named!(a1),
                    type_par!(type_func!(type_named!(b1), type_named!(c)))
                )
            ) => {
                assert_ident_text_matches_name!(db, [a, b, c]);
                assert_eq!(a, a1);
                assert_eq!(b, b1);
            }
        );
    }

    #[test]
    fn test_mixed_types() {
        let db = TestDatabase::default();
        assert_matches!(
            parse_type_unwrap(
                &db,
                &Bump::new(),
                "{x: a} -> <f: b -> a | r>"
            ),
            type_func!(
                type_prod!(row_concrete!(id_field!(x, type_named!(a)))),
                type_sum!(row_mixed!(
                    (id_field!(f, type_func!(type_named!(b), type_named!(a1)))),
                    (r)
                ))
            ) => {
                assert_ident_text_matches_name!(db, [x, a, f, b, r]);
                assert_eq!(a, a1);
            }
        );
    }

    #[test]
    fn test_unqualified_schemes() {
        let db = TestDatabase::default();
        assert_matches!(
            parse_scheme_unwrap(
                &db,
                &Bump::new(),
                "{x: a} -> <f: b -> a | r>"
            ),
            scheme!(type_func!(
                type_prod!(row_concrete!(id_field!(x, type_named!(a)))),
                type_sum!(row_mixed!(
                    (id_field!(f, type_func!(type_named!(b), type_named!(a1)))),
                    (r)
                ))
            )) => {
                assert_ident_text_matches_name!(db, [x, a, f, b, r]);
                assert_eq!(a, a1);
            }
        );
    }

    #[test]
    fn test_equals_schemes() {
        let db = TestDatabase::default();
        assert_matches!(
            parse_scheme_unwrap(
                &db,
                &Bump::new(),
                "r + (y: a) = s => {r} -> a -> {s}"
            ),
            scheme!(
                qual!(ct_rowsum!(
                    rwx_variable!(r),
                    rwx_concrete!(id_field!(y, type_named!(a))),
                    rwx_variable!(s)
                )),
                type_func!(
                    type_prod!(row_variable!(r1)),
                    type_func!(type_named!(a1), type_prod!(row_variable!(s1)))
                )
            ) => {
                assert_ident_text_matches_name!(db, [r, y, a, s]);
                assert_eq!(r, r1);
                assert_eq!(a, a1);
                assert_eq!(s, s1);
            }
        );
    }

    #[test]
    fn test_undelimted_closure_fails() {
        let db = TestDatabase::default();
        let arena = Bump::new();
        let (tokens, eoi) = aiahr_lexer(&db).lex(MOD, "|x whoops(x)").unwrap();
        assert_matches!(term(&arena).parse(to_stream(tokens, eoi)), Err(..));
    }

    #[test]
    fn test_annotated_bindings() {
        let db = TestDatabase::default();
        assert_matches!(
            parse_term_unwrap(
                &db,
                &Bump::new(),
                "x: a = {}; y: {} = {}; x"
            ),
            term_local!(
                x,
                type_named!(a),
                term_prod!(),
                term_local!(y, type_prod!(), term_prod!(), term_sym!(x1))
            ) => {
                assert_ident_text_matches_name!(db, [x, a, y]);
                assert_eq!(x, x1);
            }
        );
    }

    #[test]
    fn test_app_precedence() {
        let db = TestDatabase::default();
        assert_matches!(
            parse_term_unwrap(
                &db,
                &Bump::new(),
                "(|x| |w| w)(y)(z)"
            ),
            term_app!(
                term_app!(
                    term_paren!(term_abs!(x, term_abs!(w, term_sym!(w1)))),
                    term_sym!(y)
                ),
                term_sym!(z)
            ) => {
                assert_ident_text_matches_name!(db, [x, w, y, z]);
                assert_eq!(w, w1);
            }
        );
    }

    #[test]
    fn test_with_do() {
        let db = TestDatabase::default();
        assert_matches!(
            parse_term_unwrap(
                &db,
                &Bump::new(),
                "with h do a(b)"
            ),
            term_with!(term_sym!(h), term_app!(term_sym!(a), term_sym!(b))) => assert_ident_text_matches_name!(db, [h, a, b])
        );
    }

    #[test]
    fn test_mixing_prefixes() {
        let db = TestDatabase::default();
        assert_matches!(
            parse_term_unwrap(
                &db,
                &Bump::new(),
                "|x| y = |z| y(z); w = x(y); w"
            ),
            term_abs!(
                x,
                term_local!(
                    y,
                    term_abs!(z, term_app!(term_sym!(y1), term_sym!(z1))),
                    term_local!(
                        w,
                        term_app!(term_sym!(x1), term_sym!(y2)),
                        term_sym!(w1)
                    )
                )
            ) => {
                assert_ident_text_matches_name!(db, [x, y, z, w]);
                assert_eq!([x, y, z, w], [x1, y1, z1, w1]);
                assert_eq!(y, y2);
            }
        );
    }

    #[test]
    fn test_basic_lambdas() {
        let db = TestDatabase::default();
        assert_matches!(
            parse_term_unwrap(
                &db,
                &Bump::new(),
                "|x| |y: a| z = x(y); z"
            ),
            term_abs!(
                x,
                term_abs!(
                    y,
                    type_named!(a),
                    term_local!(
                        z,
                        term_app!(term_sym!(x1), term_sym!(y1)),
                        term_sym!(z1)
                    )
                )
            ) => {
                assert_ident_text_matches_name!(db, [x, y, a, z]);
                assert_eq!([x, y, z], [x1, y1, z1]);
            }
        );
    }

    #[test]
    fn test_product_rows() {
        let db = TestDatabase::default();
        assert_matches!(parse_term_unwrap(&db, &Bump::new(), "{}"), term_prod!());
        assert_matches!(
            parse_term_unwrap(
                &db,
                &Bump::new(),
                "{x = a, y = |t| t}"
            ),
            term_prod!(
                id_field!(x, term_sym!(a)),
                id_field!(y, term_abs!(t, term_sym!(t1))),
            ) => {
                assert_ident_text_matches_name!(db, [x, a, y, t]);
                assert_eq!(t, t1);
            }
        );
    }

    #[test]
    fn test_product_rows_precedence() {
        let db = TestDatabase::default();
        assert_matches!(
            parse_term_unwrap(
                &db,
                &Bump::new(),
                "{x = |t| t}({y = |t| u})"
            ),
            term_app!(
                term_prod!(id_field!(x, term_abs!(t, term_sym!(t1)))),
                term_prod!(id_field!(y, term_abs!(t2, term_sym!(u))))
            ) => {
                assert_ident_text_matches_name!(db, [x, y, t, u]);
                assert_eq!(t, t1);
                assert_eq!(t, t2);
            }
        );
    }

    #[test]
    fn test_field_access() {
        let db = TestDatabase::default();
        assert_matches!(
            parse_term_unwrap(
                &db,
                &Bump::new(),
                "{x = a, y = b}.x"
            ),
            term_dot!(
                term_prod!(
                    id_field!(x, term_sym!(a)),
                    id_field!(y, term_sym!(b)),
                ),
                x1
            ) => {
                assert_ident_text_matches_name!(db, [x, a, y, b]);
                assert_eq!(x, x1);
            }
        );
    }

    #[test]
    fn test_combined_postfixes() {
        let db = TestDatabase::default();
        assert_matches!(
            parse_term_unwrap(&db, &Bump::new(), "a.x(b)"),
            term_app!(term_dot!(term_sym!(a), x), term_sym!(b)) => assert_ident_text_matches_name!(db, [a, x, b])
        );
    }

    #[test]
    fn test_sum_rows() {
        let db = TestDatabase::default();
        assert_matches!(
            parse_term_unwrap(
                &db,
                &Bump::new(),
                "<x = |t| t>"
            ),
            term_sum!(id_field!(x, term_abs!(t, term_sym!(t1)))) => {
                assert_ident_text_matches_name!(db, [x, t]);
                assert_eq!(t, t1);
            }
        );
    }

    #[test]
    fn test_matches() {
        let db = TestDatabase::default();
        assert_matches!(
            parse_term_unwrap(
                &db,
                &Bump::new(),
                "match < {x = a} => a, <y = b> => b, c => c >"
            ),
            term_match!(
                field!(pat_prod!(id_field!(x, pat_var!(a))), term_sym!(a1)),
                field!(pat_sum!(id_field!(y, pat_var!(b))), term_sym!(b1)),
                field!(pat_var!(c), term_sym!(c1)),
            ) => {
                assert_ident_text_matches_name!(db, [x, a, y, b, c]);
                assert_eq!([a, b, c], [a1, b1, c1]);
            }
        );
    }

    #[test]
    fn test_effect_items() {
        let db = TestDatabase::default();
        assert_matches!(
            parse_file_unwrap(
                &db,
                &Bump::new(),
                "effect foo { foo: a -> a }"
            ).items,
            &[item_effect!(
                foo,
                eff_op!(foo1, type_func!(type_named!(a), type_named!(a1)))
            )] => {
                assert_ident_text_matches_name!(db, [foo, a]);
                assert_eq!([foo, a], [foo1, a1]);
            }
        );
    }

    #[test]
    fn test_term_items() {
        let db = TestDatabase::default();
        assert_matches!(
            parse_file_unwrap(
                &db,
                &Bump::new(),
                "x = a\ny = |b| b\nz = t = x; t"
            ).items,
            &[
                item_term!(x, term_sym!(a)),
                item_term!(y, term_abs!(b, term_sym!(b1))),
                item_term!(z, term_local!(t, term_sym!(x1), term_sym!(t1))),
            ] => {
                assert_ident_text_matches_name!(db, [x, y, z, a, b, t]);
                assert_eq!([x, b, t], [x1, b1, t1]);
            }
        );
        assert_matches!(
            parse_file_unwrap(
                &db,
                &Bump::new(),
                "x: a = a\ny: forall b. b -> b = |b| b"
            ).items,
            &[
                item_term!(x, scheme!(type_named!(a)), term_sym!(a1)),
                item_term!(
                    y,
                    scheme!(
                        quant!(b),
                        None,
                        type_func!(type_named!(b1), type_named!(b2))
                    ),
                    term_abs!(b3, term_sym!(b4))
                ),
            ] => {
                assert_ident_text_matches_name!(db, [x, a, y, b]);
                assert_eq!([a, b], [a1, b1]);
                assert_eq!(b, b2);
                assert_eq!(b, b3);
                assert_eq!(b, b4);
            }
        );
    }
}
