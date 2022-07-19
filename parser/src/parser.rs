use chumsky::{
    prelude::{choice, end, just, recursive, Recursive},
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

type Output<'i> = Term<'i>;

/// Returns a parser for the Aiahr language.
pub fn aiahr_parser<'i>() -> impl Parser<Token<'i>, Output<'i>, Error = ParseErrors<'i>> {
    recursive(|term| {
        // intermediary we use in atom and app
        let paren_term = lit(Token::LParen)
            .then(term.clone())
            .then(lit(Token::RParen));

        let atom = choice((
            // variable
            ident().map(Term::VariableRef),
            // explicit term precedence
            paren_term
                .clone()
                .map(|((lpar, t), rpar)| Term::Parenthesized {
                    lpar,
                    term: Box::new(t),
                    rpar,
                }),
        ));

        // Function application
        let app = atom
            .clone()
            .then(paren_term.repeated())
            .map(|(func, args)| {
                args.into_iter()
                    .fold(func, |t, ((lpar, arg), rpar)| Term::Application {
                        func: Box::new(t),
                        lpar,
                        arg: Box::new(arg),
                        rpar,
                    })
            });

        // Local variable binding
        let local_bind = ident()
            .then(lit(Token::Equal))
            .then(term)
            .then(lit(Token::Semicolon))
            .map(|(((var, eq), val), semi)| {
                Box::new(move |t| Term::Binding {
                    var,
                    eq,
                    value: Box::new(val),
                    semi,
                    expr: Box::new(t),
                }) as Box<dyn FnOnce(Term<'i>) -> Term<'i>>
            });

        // Lambda abstraction
        let closure = lit(Token::VerticalBar)
            .then(ident())
            .then(lit(Token::VerticalBar))
            .map(|((lbar, var), rbar)| {
                Box::new(move |t| Term::Abstraction {
                    lbar,
                    arg: var,
                    rbar,
                    body: Box::new(t),
                }) as Box<dyn FnOnce(Term<'i>) -> Term<'i>>
            });

        // Term parser
        // We need to construct our parse tree here bottom to get associativity of bindings and
        // closures correct. However we're recursive descent, so we only go top-down. To remedy
        // this we construct a series of closures top-down that are applied to the final expression
        // in right associative order.
        choice((local_bind, closure))
            .repeated()
            .then(app)
            .map(|(binds, expr)| binds.into_iter().rfold(expr, |t, f| f(t)))
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
    use chumsky::Parser;

    use crate::{cst::Term, lexer::aiahr_lexer};

    use super::{aiahr_parser, to_stream};

    pub enum CstMatcher {
        Binding(String, Box<CstMatcher>, Box<CstMatcher>),
        Abstraction(String, Box<CstMatcher>),
        Application(Box<CstMatcher>, Box<CstMatcher>),
        Variable(String),
        Parenthesized(Box<CstMatcher>),
    }
    impl CstMatcher {
        fn term_name(&self) -> &'static str {
            match self {
                CstMatcher::Binding(_, _, _) => "local binding",
                CstMatcher::Abstraction(_, _) => "lambda abstraction",
                CstMatcher::Application(_, _) => "function application",
                CstMatcher::Variable(_) => "variable",
                CstMatcher::Parenthesized(_) => "parenthesized term",
            }
        }

        fn matches<'i>(&self, term: &Term<'i>) -> Result<(), String> {
            use CstMatcher::*;
            match (self, term) {
                (
                    Binding(exp_var, exp_val, exp_expr),
                    Term::Binding {
                        var, value, expr, ..
                    },
                ) => {
                    if var.0 != exp_var {
                        return Err(format!(
                            "Local binding expected variable name {} but found {}",
                            exp_var, var.0
                        ));
                    }
                    exp_val.matches(value)?;
                    exp_expr.matches(expr)
                }
                (Abstraction(exp_arg, exp_body), Term::Abstraction { arg, body, .. }) => {
                    if arg.0 != exp_arg {
                        return Err(format!(
                            "Lambda abstraction expected argument name {} but found {}",
                            exp_arg, arg.0
                        ));
                    }
                    exp_body.matches(body)
                }
                (Application(exp_func, exp_arg), Term::Application { func, arg, .. }) => {
                    exp_arg.matches(arg)?;
                    exp_func.matches(func)
                }
                (Variable(exp_var), Term::VariableRef(act_var)) => {
                    if exp_var == act_var.0 {
                        Ok(())
                    } else {
                        Err(format!(
                            "Expected variable {} but found {}",
                            exp_var, act_var.0
                        ))
                    }
                }
                (Parenthesized(exp_term), Term::Parenthesized { term, .. }) => {
                    exp_term.matches(term)
                }
                (matcher, term) => Err(format!(
                    "Expected a {} but found {:?}",
                    matcher.term_name(),
                    term
                )),
            }
        }
    }
    // These have to be top level functions so we can write them without the `CstMatcher::` prefix
    fn local(var: impl ToString, value: CstMatcher, expr: CstMatcher) -> CstMatcher {
        CstMatcher::Binding(var.to_string(), Box::new(value), Box::new(expr))
    }
    fn abs(var: impl ToString, body: CstMatcher) -> CstMatcher {
        CstMatcher::Abstraction(var.to_string(), Box::new(body))
    }
    // Terrible pluralization, but we want terseness. Perhaps funs is betters?
    fn abss(vars: impl IntoIterator<IntoIter = impl DoubleEndedIterator<Item = impl ToString>>, body: CstMatcher) -> CstMatcher {
        vars.into_iter().rfold(body, |body, var| abs(var, body))
    }
    fn app(func: CstMatcher, arg: CstMatcher) -> CstMatcher {
        CstMatcher::Application(Box::new(func), Box::new(arg))
    }
    fn var(var: impl ToString) -> CstMatcher {
        CstMatcher::Variable(var.to_string())
    }
    fn paren(term: CstMatcher) -> CstMatcher {
        CstMatcher::Parenthesized(Box::new(term))
    }

    fn parse_unwrap(input: &str) -> Term<'_> {
        let (tokens, eoi) = aiahr_lexer().lex(input).unwrap();
        aiahr_parser().parse(to_stream(tokens, eoi)).unwrap()
    }

    #[test]
    fn test_undelimted_closure_fails() {
        let program = "|x whoops(x)";

        let (tokens, eoi) = aiahr_lexer().lex(program).unwrap();
        let cst = aiahr_parser().parse(to_stream(tokens, eoi));

        assert!(cst.is_err()) 
    }

    #[test]
    fn test_app_precedence() {
        let cst = parse_unwrap("(|x| |w| w)(y)(z)");

        let expected = app(app(paren(abss(["x", "w"], var("w"))), var("y")), var("z"));
        assert_eq!(expected.matches(&cst), Ok(()));
    }

    #[test]
    fn test_mixing_prefixes() {
        let cst = parse_unwrap("|x| y = |z| y(z); w = x(y); w");
        let expected = abs("x", local("y", abs("z", app(var("y"), var("z"))), local("w", app(var("x"), var("y")), var("w"))));

        assert_eq!(expected.matches(&cst), Ok(()));
    }

    #[test]
    fn test_basic_lambdas() {
        const PROGRAM: &'static str = "|x| |y| z = x(y); z";

        let lexer = aiahr_lexer();
        let parser = aiahr_parser();

        let (tokens, eoi) = lexer.lex(PROGRAM).unwrap();
        let cst = parser.parse(to_stream(tokens, eoi)).unwrap();

        let expected = abs("x", abs("y", local("z", app(var("x"), var("y")), var("z"))));
        assert_eq!(expected.matches(&cst), Ok(()));
    }
}
