use base::{
  ident::Ident,
  indexed::IdxAlloc,
  loc::Loc,
  span::{Span, SpanOf, Spanned},
};
use chumsky::{
  combinator::Map,
  prelude::{choice, end, just, recursive},
  select, Parser, Stream,
};
use cst::{Annotation, CstIndxAlloc, Field, IdField};
use la_arena::Idx;

use std::option::Option;

use super::lexer::Token;
use crate::error::ParseErrors;

/// A trait alias for a cloneable parser for syntax.
pub trait PanoplyParser<T>: Clone + Parser<Token, T, Error = ParseErrors> {}
impl<T, A> PanoplyParser<T> for A where A: Clone + Parser<Token, T, Error = ParseErrors> {}

pub trait PanoplyIdxParser<T>: PanoplyParser<IdxState<T>> {}
impl<T, A> PanoplyIdxParser<T> for A where A: PanoplyParser<IdxState<T>> {}

pub enum StateM<S, T> {
  /// A pure value that does not depend on S.
  /// We don't need the state so don't allocate a closure
  Pure(T),
  /// A statefule operation that produces T
  State(Box<dyn for<'a> FnOnce(&'a mut S) -> T>),
}

pub type IdxState<T> = StateM<CstIndxAlloc, T>;

impl<S, T> StateM<S, T> {
  pub(crate) fn apply(self, state: &mut S) -> T {
    match self {
      StateM::Pure(value) => value,
      StateM::State(f) => f(state),
    }
  }
}

pub(crate) trait PureState<State, T> {
  #[allow(clippy::type_complexity)]
  fn pure_state(self) -> Map<Self, fn(T) -> StateM<State, T>, T>
  where
    Self: Sized;
}

impl<P, State, T> PureState<State, T> for P
where
  P: Parser<Token, T, Error = ParseErrors>,
{
  fn pure_state(self) -> Map<Self, fn(T) -> StateM<State, T>, T>
  where
    Self: Sized,
  {
    fn pure_st<S, T>(value: T) -> StateM<S, T> {
      StateM::Pure(value)
    }
    self.map(pure_st)
  }
}

pub(crate) trait ParserMapState<State, Input: 'static, T> {
  #[allow(clippy::type_complexity)]
  fn map_state<F: 'static>(self, f: F) -> Map<Self, Box<dyn Fn(Input) -> StateM<State, T>>, Input>
  where
    Self: Sized,
    F: Copy + FnMut(Input, &mut State) -> T + 'static;
}
impl<P, State, Input: 'static, T> ParserMapState<State, Input, T> for P
where
  P: Parser<Token, Input, Error = ParseErrors>,
{
  fn map_state<F: 'static>(
    self,
    mut f: F,
  ) -> chumsky::combinator::Map<Self, Box<dyn Fn(Input) -> StateM<State, T>>, Input>
  where
    Self: Sized,
    F: Copy + FnMut(Input, &mut State) -> T + 'static,
  {
    self.map(Box::new(move |inp| {
      let fun: Box<dyn FnOnce(&mut State) -> T> = Box::new(move |state| f(inp, state));
      StateM::State(fun)
    }))
  }
}

// Returns a spanned parser that matches just the given token and returns ().
fn lit(token: Token) -> impl PanoplyParser<Span> {
  just(token).map_with_span(|_, span| span)
}

// Returns a spanned parser that matches any `Token::Identifier` and unwraps it to the contained
// `&str`.
fn ident() -> impl PanoplyParser<SpanOf<Ident>> {
  select! {
      Token::Identifier(id) => id,
  }
  .map_with_span(|s, span: Span| span.of(s))
}

fn int() -> impl PanoplyParser<SpanOf<usize>> {
  select! {
      Token::Int(int) => int
  }
  .map_with_span(|s, span: Span| span.of(s))
}

// Returns a parser for one or more `T` values separated by `sep`, representing the sequence with
// `Separated<T>`.
fn separated<'a, T: 'static>(
  elem: impl PanoplyIdxParser<T> + 'a,
  sep: impl PanoplyParser<Span> + 'a,
) -> impl PanoplyIdxParser<cst::Separated<T>> + 'a {
  elem
    .clone()
    .then(sep.clone().then(elem).repeated())
    .then(sep.or_not())
    .map_state(|((first, elems), comma), alloc| cst::Separated {
      first: first.apply(alloc),
      elems: elems
        .into_iter()
        .map(|(span, f)| (span, f.apply(alloc)))
        .collect::<Vec<(Span, T)>>(),
      comma,
    })
    .boxed()
}

// Returns a parser for a field with a label, separator, and target.
fn field<'a, L: 'static, T: 'static>(
  label: impl PanoplyIdxParser<L> + 'a,
  sep: Token,
  target: impl PanoplyIdxParser<T> + 'a,
) -> impl PanoplyIdxParser<Field<L, T>> + 'a {
  label
    .then(lit(sep))
    .then(target)
    .map_state(|((label, sep), target), alloc| Field {
      label: label.apply(alloc),
      sep,
      target: target.apply(alloc),
    })
    .boxed()
}

// Returns a parser for a field with an identifier label, separator, and target.
fn id_field<'a, T: 'static>(
  sep: Token,
  target: impl PanoplyIdxParser<T> + 'a,
) -> impl PanoplyIdxParser<IdField<T>> + 'a {
  field(ident().pure_state(), sep, target)
}

// Returns a parser for a product row with terms in `term`.
fn product_row<'a, T: 'static>(
  term: impl PanoplyIdxParser<T> + 'a,
) -> impl PanoplyIdxParser<cst::ProductRow<T>> + 'a {
  lit(Token::LBrace)
    .then(separated(id_field(Token::Equal, term), lit(Token::Comma)).or_not())
    .then(lit(Token::RBrace))
    .map_state(|((lbrace, fields), rbrace), alloc| cst::ProductRow {
      lbrace,
      fields: fields.map(|fields| fields.apply(alloc)),
      rbrace,
    })
    .boxed()
}

// Returns a parser for a sum row with terms in `term`.
fn sum_row<'a, T: 'static>(
  term: impl PanoplyIdxParser<T> + 'a,
) -> impl PanoplyIdxParser<cst::SumRow<T>> + 'a {
  lit(Token::LAngle)
    .then(id_field(Token::Equal, term))
    .then(lit(Token::RAngle))
    .map_state(|((langle, field), rangle), state| cst::SumRow {
      langle,
      field: field.apply(state),
      rangle,
    })
    .boxed()
}

// Returns a parser for a non-empty row of `C` in an explicit type.
fn row<'a, C: 'static>(
  field: impl PanoplyIdxParser<C> + 'a,
) -> impl PanoplyIdxParser<cst::Row<Ident, C>> + 'a {
  let concrete = separated(field, lit(Token::Comma));
  let variables = separated(ident().pure_state(), lit(Token::Plus));
  choice((
    concrete
      .then(lit(Token::VerticalBar).then(variables.clone()).or_not())
      .map_state(|(concrete, maybe_vars), alloc| match maybe_vars {
        Some((vbar, variables)) => cst::Row::Mixed {
          concrete: concrete.apply(alloc),
          vbar,
          variables: variables.apply(alloc),
        },
        None => cst::Row::Concrete(concrete.apply(alloc)),
      }),
    variables.map_state(|vars, alloc| cst::Row::Variable(vars.apply(alloc))),
  ))
  .boxed()
}

/// Returns a parser for an (mono-)type.
pub fn type_() -> impl PanoplyIdxParser<Idx<cst::Type<Ident>>> {
  recursive(|type_| {
    let type_row = row(id_field(Token::Colon, type_.clone())).boxed();

    // Named type.
    let named = ident().map_state(|n, alloc: &mut CstIndxAlloc| alloc.alloc(cst::Type::Named(n)));

    // Sum type.
    let sum = lit(Token::LAngle)
      .then(type_row.clone())
      .then(lit(Token::RAngle))
      .map_state(|((langle, variants), rangle), alloc: &mut CstIndxAlloc| {
        let variants = variants.apply(alloc);
        alloc.alloc(cst::Type::Sum {
          langle,
          variants,
          rangle,
        })
      });

    // Product type.
    let prod = lit(Token::LBrace)
      .then(type_row.or_not())
      .then(lit(Token::RBrace))
      .map_state(|((lbrace, fields), rbrace), alloc: &mut CstIndxAlloc| {
        let fields = fields.map(|fields| fields.apply(alloc));
        alloc.alloc(cst::Type::Product {
          lbrace,
          fields,
          rbrace,
        })
      });

    // Parenthesized type.
    let paren = lit(Token::LParen)
      .then(type_)
      .then(lit(Token::RParen))
      .map_state(|((lpar, type_), rpar), alloc: &mut CstIndxAlloc| {
        let type_ = type_.apply(alloc);
        alloc.alloc(cst::Type::Parenthesized { lpar, type_, rpar })
      });

    // Function type.
    let atom = choice((named, sum, prod, paren)).boxed();
    atom
      .clone()
      .then(lit(Token::SmallArrow))
      .repeated()
      .then(atom)
      .map_state(move |(los, r), alloc: &mut CstIndxAlloc| {
        los
          .into_iter()
          .rfold(r.apply(alloc), |codomain, (domain, arrow)| {
            let domain = domain.apply(alloc);
            alloc.alloc(cst::Type::Function {
              domain,
              arrow,
              codomain,
            })
          })
      })
      .boxed()
  })
}

// Returns a parser for a row type expression.
fn row_atom() -> impl PanoplyIdxParser<cst::RowAtom<Ident>> {
  choice((
    lit(Token::LParen)
      .then(separated(
        id_field(Token::Colon, type_()),
        lit(Token::Comma),
      ))
      .then(lit(Token::RParen))
      .map_state(|((lpar, fields), rpar), alloc| cst::RowAtom::Concrete {
        lpar,
        fields: fields.apply(alloc),
        rpar,
      }),
    ident().map(cst::RowAtom::Variable).pure_state(),
  ))
  .boxed()
}

// Returns a parser for a type constraint.
fn constraint() -> impl PanoplyIdxParser<cst::Constraint<Ident>> {
  row_atom()
    .then(lit(Token::Plus))
    .then(row_atom())
    .then(lit(Token::Equal))
    .then(row_atom())
    .map_state(
      |((((lhs, plus), rhs), eq), goal), alloc| cst::Constraint::RowSum {
        lhs: lhs.apply(alloc),
        plus,
        rhs: rhs.apply(alloc),
        eq,
        goal: goal.apply(alloc),
      },
    )
    .boxed()
}

/// Returns a parser for a scheme (polymorphic type).
pub fn scheme() -> impl PanoplyIdxParser<cst::Scheme<Ident>> {
  lit(Token::KwForall)
    .then(ident())
    .then(lit(Token::Dot))
    .map(|((forall, var), dot)| cst::Quantifier { forall, var, dot })
    .repeated()
    .then(
      separated(constraint(), lit(Token::Comma))
        .then(lit(Token::BigArrow))
        .map_state(|(constraints, arrow), alloc| cst::Qualifiers {
          constraints: constraints.apply(alloc),
          arrow,
        })
        .or_not(),
    )
    .then(type_())
    .map_state(|((quantifiers, qualifiers), type_), alloc| cst::Scheme {
      quantifiers,
      qualifiers: qualifiers.map(|qual| qual.apply(alloc)),
      type_: type_.apply(alloc),
    })
    .boxed()
}

// Returns a parser for an effect operation.
fn effect_op() -> impl PanoplyIdxParser<cst::EffectOp<Ident, Ident>> {
  ident()
    .then(lit(Token::Colon))
    .then(type_())
    .map_state(|((name, colon), type_), alloc| cst::EffectOp {
      name,
      colon,
      type_: type_.apply(alloc),
    })
    .boxed()
}

/// Returns a parser for an annotation with a given type syntax.
pub fn annotation<'a, T: 'static>(
  ty: impl PanoplyIdxParser<T> + 'a,
) -> impl PanoplyIdxParser<Annotation<T>> + 'a {
  lit(Token::Colon)
    .then(ty)
    .map_state(|(colon, type_), alloc| {
      let type_ = type_.apply(alloc);
      Annotation { colon, type_ }
    })
    .boxed()
}

/// Returns a parser for a pattern.
pub fn pattern() -> impl PanoplyIdxParser<Idx<cst::Pattern>> {
  recursive(|pattern| {
    choice((
      product_row(pattern.clone()).map_state(|p, alloc: &mut CstIndxAlloc| {
        let p = p.apply(alloc);
        alloc.alloc(cst::Pattern::ProductRow(p))
      }),
      sum_row(pattern.clone()).map_state(|s, alloc: &mut CstIndxAlloc| {
        let s = s.apply(alloc);
        alloc.alloc(cst::Pattern::SumRow(s))
      }),
      ident().map_state(|v, alloc: &mut CstIndxAlloc| alloc.alloc(cst::Pattern::Whole(v))),
    ))
    .boxed()
  })
}

enum TermPrefix {
  Binding {
    var: SpanOf<Ident>,
    annotation: Option<cst::TypeAnnotation<Ident>>,
    eq: Span,
    value: Idx<cst::Term>,
    semi: Span,
  },
  Handle {
    with: Span,
    handler: Idx<cst::Term>,
    do_: Span,
  },
  Abstraction {
    lbar: Span,
    arg: SpanOf<Ident>,
    annotation: Option<cst::TypeAnnotation<Ident>>,
    rbar: Span,
  },
}

impl TermPrefix {
  fn apply(self, arena: &mut CstIndxAlloc, t: Idx<cst::Term>) -> Idx<cst::Term> {
    match self {
      TermPrefix::Binding {
        var,
        annotation,
        eq,
        value,
        semi,
      } => arena.alloc(cst::Term::Binding {
        var,
        annotation,
        eq,
        value,
        semi,
        expr: t,
      }),
      TermPrefix::Handle { with, handler, do_ } => arena.alloc(cst::Term::Handle {
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
      } => arena.alloc(cst::Term::Abstraction {
        lbar,
        arg,
        annotation,
        rbar,
        body: t,
      }),
    }
  }
}

enum TermPostfix {
  Application {
    lpar: Span,
    arg: Idx<cst::Term>,
    rpar: Span,
  },
  DotAccess {
    dot: Span,
    field: SpanOf<Ident>,
  },
}

impl TermPostfix {
  fn apply(self, arena: &mut CstIndxAlloc, t: Idx<cst::Term>) -> Idx<cst::Term> {
    match self {
      TermPostfix::Application { lpar, arg, rpar } => arena.alloc(cst::Term::Application {
        func: t,
        lpar,
        arg,
        rpar,
      }),
      TermPostfix::DotAccess { dot, field } => arena.alloc(cst::Term::DotAccess {
        base: t,
        dot,
        field,
      }),
    }
  }
}

/// Returns a parser for terms.
pub fn term() -> impl PanoplyIdxParser<Idx<cst::Term>> {
  recursive(|term| {
    // intermediary we use in atom and app
    let paren_term = lit(Token::LParen)
      .then(term.clone())
      .then(lit(Token::RParen));

    // Product row
    let prod = product_row(term.clone()).map_state(|p, alloc: &mut CstIndxAlloc| {
      let p = p.apply(alloc);
      alloc.alloc(cst::Term::ProductRow(p))
    });

    let sum = sum_row(term.clone()).map_state(|s, alloc: &mut CstIndxAlloc| {
      let s = s.apply(alloc);
      alloc.alloc(cst::Term::SumRow(s))
    });

    let match_ = lit(Token::KwMatch)
      .then(lit(Token::LAngle))
      .then(separated(
        field(pattern(), Token::BigArrow, term.clone()),
        lit(Token::Comma),
      ))
      .then(lit(Token::RAngle))
      .map_state(
        |(((match_, langle), cases), rangle), alloc: &mut CstIndxAlloc| {
          let cases = cases.apply(alloc);
          alloc.alloc(cst::Term::Match {
            match_,
            langle,
            cases,
            rangle,
          })
        },
      );

    let atom = choice((
      // integer
      int().map_state(|i, alloc: &mut CstIndxAlloc| alloc.alloc(cst::Term::Int(i))),
      // variable
      ident().map_state(|s, alloc: &mut CstIndxAlloc| alloc.alloc(cst::Term::SymbolRef(s))),
      // explicit term precedence
      paren_term.clone().map_state(|((lpar, t), rpar), alloc| {
        let term = t.apply(alloc);
        alloc.alloc(cst::Term::Parenthesized { lpar, term, rpar })
      }),
      prod,
      sum,
      match_,
    ))
    .boxed();

    // Function application
    let app = paren_term.map_state(|((lpar, arg), rpar), alloc| {
      let arg = arg.apply(alloc);
      TermPostfix::Application { lpar, arg, rpar }
    });

    // Field access
    let access = lit(Token::Dot)
      .then(ident())
      .map(|(dot, field)| TermPostfix::DotAccess { dot, field })
      .pure_state();

    let app_access = atom
      .clone()
      .then(choice((app, access)).repeated())
      .map_state(|(l, rs), alloc| {
        rs.into_iter()
          .fold(l.apply(alloc), |l, r| r.apply(alloc).apply(alloc, l))
      })
      .boxed();

    let concats = separated(
      app_access.clone(),
      lit(Token::Comma)
        .then(lit(Token::Comma))
        .map_with_span(|_, span| span),
    )
    .map_state(|sep, alloc| {
      let sep = sep.apply(alloc);
      sep
        .elems
        .into_iter()
        .fold(sep.first, |left, (concat, right)| {
          alloc.alloc(cst::Term::Concat {
            left,
            concat,
            right,
          })
        })
    });

    // Local variable binding
    let local_bind = ident()
      .then(annotation(type_()).or_not())
      .then(lit(Token::Equal))
      .then(term.clone())
      .then(lit(Token::Semicolon))
      .map_state(|((((var, annotation), eq), val), semi), alloc| {
        let annotation = annotation.map(|ann| ann.apply(alloc));
        let value = val.apply(alloc);
        TermPrefix::Binding {
          var,
          annotation,
          eq,
          value,
          semi,
        }
      });

    let handle = lit(Token::KwWith)
      .then(term)
      .then(lit(Token::KwDo))
      .map_state(|((with, handler), do_), alloc| {
        let handler = handler.apply(alloc);
        TermPrefix::Handle { with, handler, do_ }
      });

    // Lambda abstraction
    let closure = lit(Token::VerticalBar)
      .then(ident())
      .then(annotation(type_()).or_not())
      .then(lit(Token::VerticalBar))
      .map_state(|(((lbar, var), annotation), rbar), alloc| {
        let annotation = annotation.map(|ann| ann.apply(alloc));
        TermPrefix::Abstraction {
          lbar,
          arg: var,
          annotation,
          rbar,
        }
      });

    // Term parser
    // We need to construct our parse tree here bottom to get associativity of bindings and
    // closures correct. However we're recursive descent, so we only go top-down. To remedy
    // this we construct a series of prefixes top-down that are applied to the final expression
    // in right associative order.
    choice((local_bind, handle, closure))
      .repeated()
      .then(concats)
      .map_state(|(ls, r), alloc| {
        ls.into_iter()
          .rfold(r.apply(alloc), |r, l| l.apply(alloc).apply(alloc, r))
      })
      .boxed()
  })
}

/// Returns a parser for the language, using the given arena to allocate CST nodes.
pub fn parser() -> impl Parser<Token, IdxState<Vec<cst::Item>>, Error = ParseErrors> {
  let effect = lit(Token::KwEffect)
    .then(ident())
    .then(lit(Token::LBrace))
    .then(effect_op().separated_by(lit(Token::Comma)))
    .then(lit(Token::RBrace))
    .map_state(|((((effect, name), lbrace), ops), rbrace), alloc| {
      cst::Item::Effect(cst::EffectDefn {
        effect,
        name,
        lbrace,
        ops: ops.into_iter().map(|op| op.apply(alloc)).collect(),
        rbrace,
      })
    });

  let term = ident()
    .then(annotation(scheme()).or_not())
    .then(lit(Token::Equal))
    .then(term())
    .map_state(|(((name, annotation), eq), value), alloc| {
      let annotation = annotation.map(|ann| ann.apply(alloc));
      let value = value.apply(alloc);
      cst::Item::Term(cst::TermDefn {
        name,
        annotation,
        eq,
        value,
      })
    });

  choice((effect, term))
    .repeated()
    .map_state(|items, alloc| items.into_iter().map(|item| item.apply(alloc)).collect())
    .then_ignore(end())
    .boxed()
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

#[cfg(test)]
mod tests {
  use std::path::PathBuf;

  use assert_matches::assert_matches;
  use base::{
    file::{FileId, SourceFile},
    ident::Ident,
    indexed::ReferenceAllocate,
    span::SpanOf,
  };
  use bumpalo::Bump;
  use chumsky::{prelude::end, Parser};
  use cst::CstIndxAlloc;
  use test_utils::{
    assert_ident_text_matches_name,
    cst::{CstRefAlloc, Scheme, Term, Type},
    ct_rowsum, field, id_field, pat_prod, pat_sum, pat_var, qual, row_concrete, row_mixed,
    row_variable, rwx_concrete, rwx_variable, scheme, term_abs, term_app, term_dot, term_local,
    term_match, term_paren, term_prod, term_sum, term_sym, term_with, type_func, type_named,
    type_par, type_prod, type_sum,
  };

  use expect_test::expect;

  use crate::error::ParseErrors;
  use crate::locator::Locator;
  use crate::{lexer::lexer, Db as ParserDb};

  use super::{term, to_stream, type_};

  #[derive(Default)]
  #[salsa::db(crate::Jar, base::Jar)]
  struct TestDatabase {
    storage: salsa::Storage<Self>,
  }
  impl salsa::Database for TestDatabase {}

  pub fn parse_term<'a>(
    db: &'a dyn crate::Db,
    arena: &'a Bump,
    input: &str,
  ) -> Result<&'a Term<'a>, Vec<ParseErrors>> {
    let file_id = FileId::new(db.as_core_db(), PathBuf::from("test"));
    let (tokens, eoi) = lexer(db)
      .lex(&Locator::new(file_id, input), input)
      .expect("Lexing input failed");

    term().parse(to_stream(tokens, eoi)).map(|idx_term| {
      let mut idx_alloc = CstIndxAlloc::default();
      let idx_term = idx_term.apply(&mut idx_alloc);
      let mut ref_alloc = CstRefAlloc::new(arena, &idx_alloc);
      idx_term.ref_alloc(&mut ref_alloc)
    })
  }

  fn parse_type_unwrap<'a>(
    db: &'a dyn crate::Db,
    arena: &'a Bump,
    input: &str,
  ) -> &'a Type<'a, Ident> {
    let (tokens, eoi) = lexer(db)
      .lex(
        &Locator::new(FileId::new(db.as_core_db(), PathBuf::from("test")), input),
        input,
      )
      .unwrap();
    let mut alloc = CstIndxAlloc::default();
    let ty = type_()
      .then_ignore(end())
      .parse(to_stream(tokens, eoi))
      .unwrap()
      .apply(&mut alloc);
    let mut ref_alloc = CstRefAlloc::new(arena, &alloc);
    ty.ref_alloc(&mut ref_alloc)
  }

  fn parse_scheme_unwrap<'a>(
    db: &'a dyn crate::Db,
    arena: &'a Bump,
    input: &str,
  ) -> &'a Scheme<'a, Ident> {
    let (tokens, eoi) = lexer(db)
      .lex(
        &Locator::new(FileId::new(db.as_core_db(), PathBuf::from("test")), input),
        input,
      )
      .unwrap();
    let mut alloc = CstIndxAlloc::default();
    let scheme = super::scheme()
      .then_ignore(end())
      .parse(to_stream(tokens, eoi))
      .unwrap()
      .apply(&mut alloc);
    let mut ref_alloc = CstRefAlloc::new(arena, &alloc);
    scheme.ref_alloc(&mut ref_alloc)
  }

  fn parse_term_unwrap<'a>(db: &'a dyn crate::Db, arena: &'a Bump, input: &str) -> &'a Term<'a> {
    parse_term(db, arena, input).unwrap()
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
    assert_matches!(parse_term(&db, &arena, "|x whoops(x)"), Err(..));
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
  fn test_unsign_int() {
    let db = TestDatabase::default();
    assert_matches!(
        parse_term_unwrap(&db, &Bump::new(), "12354"),
        &Term::Int(SpanOf { value, .. }) => {
            assert_eq!(value, 12354);
        }
    );

    assert_matches!(
        parse_term_unwrap(&db, &Bump::new(), "00101"),
        &Term::Int(SpanOf { value, .. }) => {
            assert_eq!(value, 101);
        }
    );

    assert_matches!(
        parse_term_unwrap(&db, &Bump::new(), "0"),
        &Term::Int(SpanOf { value, .. }) => {
            assert_eq!(value, 0);
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
  fn test_concat_variables() {
    let db = TestDatabase::default();
    assert_matches!(
        parse_term_unwrap(&db, &Bump::new(), "x ,, y ,, z"),
        test_utils::cst::Term::Concat {
            left: test_utils::cst::Term::Concat {
                left: term_sym!(x),
                right: term_sym!(y),
                ..
            },
            right: term_sym!(z),
            ..
        } => {
            assert_ident_text_matches_name!(db, [x, y, z]);
        }
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
    let file = SourceFile::new(
      &db,
      FileId::new(&db, PathBuf::from("eff_foo")),
      "effect foo { foo: a -> a }".to_string(),
    );
    let module = db.parse_module(file);
    let expect = expect![[r#"
        CstModule {
            indices: CstIndxAlloc {
                types: Arena {
                    len: 3,
                    data: [
                        Named(
                            SpanOf {
                                start: Loc {
                                    line: 0,
                                    col: 23,
                                    ..
                                },
                                value: Ident(
                                    Id {
                                        value: 3,
                                    },
                                ),
                                end: Loc {
                                    line: 0,
                                    col: 24,
                                    ..
                                },
                            },
                        ),
                        Named(
                            SpanOf {
                                start: Loc {
                                    line: 0,
                                    col: 18,
                                    ..
                                },
                                value: Ident(
                                    Id {
                                        value: 3,
                                    },
                                ),
                                end: Loc {
                                    line: 0,
                                    col: 19,
                                    ..
                                },
                            },
                        ),
                        Function {
                            domain: Idx::<Ident>>(1),
                            arrow: Span {
                                start: Loc {
                                    line: 0,
                                    col: 20,
                                    ..
                                },
                                end: Loc {
                                    line: 0,
                                    col: 22,
                                    ..
                                },
                            },
                            codomain: Idx::<Ident>>(0),
                        },
                    ],
                },
                terms: Arena {
                    len: 0,
                    data: [],
                },
                pats: Arena {
                    len: 0,
                    data: [],
                },
            },
            items: [
                Effect(
                    EffectDefn {
                        effect: Span {
                            start: Loc {
                                line: 0,
                                col: 0,
                                ..
                            },
                            end: Loc {
                                line: 0,
                                col: 6,
                                ..
                            },
                        },
                        name: SpanOf {
                            start: Loc {
                                line: 0,
                                col: 7,
                                ..
                            },
                            value: Ident(
                                Id {
                                    value: 2,
                                },
                            ),
                            end: Loc {
                                line: 0,
                                col: 10,
                                ..
                            },
                        },
                        lbrace: Span {
                            start: Loc {
                                line: 0,
                                col: 11,
                                ..
                            },
                            end: Loc {
                                line: 0,
                                col: 12,
                                ..
                            },
                        },
                        ops: [
                            EffectOp {
                                name: SpanOf {
                                    start: Loc {
                                        line: 0,
                                        col: 13,
                                        ..
                                    },
                                    value: Ident(
                                        Id {
                                            value: 2,
                                        },
                                    ),
                                    end: Loc {
                                        line: 0,
                                        col: 16,
                                        ..
                                    },
                                },
                                colon: Span {
                                    start: Loc {
                                        line: 0,
                                        col: 16,
                                        ..
                                    },
                                    end: Loc {
                                        line: 0,
                                        col: 17,
                                        ..
                                    },
                                },
                                type_: Idx::<Ident>>(2),
                            },
                        ],
                        rbrace: Span {
                            start: Loc {
                                line: 0,
                                col: 25,
                                ..
                            },
                            end: Loc {
                                line: 0,
                                col: 26,
                                ..
                            },
                        },
                    },
                ),
            ],
        }
    "#]];
    expect.assert_debug_eq(module.data(&db));
  }

  #[test]
  fn test_term_items() {
    let db = TestDatabase::default();
    let file = SourceFile::new(
      &db,
      FileId::new(&db, PathBuf::from("eff_foo")),
      "x = a\ny = |b| b\nz = t = x; t".to_string(),
    );
    let module = db.parse_module(file);
    let expect = expect![[r#"
        CstModule {
            indices: CstIndxAlloc {
                types: Arena {
                    len: 0,
                    data: [],
                },
                terms: Arena {
                    len: 6,
                    data: [
                        SymbolRef(
                            SpanOf {
                                start: Loc {
                                    line: 0,
                                    col: 4,
                                    ..
                                },
                                value: Ident(
                                    Id {
                                        value: 3,
                                    },
                                ),
                                end: Loc {
                                    line: 0,
                                    col: 5,
                                    ..
                                },
                            },
                        ),
                        SymbolRef(
                            SpanOf {
                                start: Loc {
                                    line: 1,
                                    col: 8,
                                    ..
                                },
                                value: Ident(
                                    Id {
                                        value: 5,
                                    },
                                ),
                                end: Loc {
                                    line: 1,
                                    col: 9,
                                    ..
                                },
                            },
                        ),
                        Abstraction {
                            lbar: Span {
                                start: Loc {
                                    line: 1,
                                    col: 4,
                                    ..
                                },
                                end: Loc {
                                    line: 1,
                                    col: 5,
                                    ..
                                },
                            },
                            arg: SpanOf {
                                start: Loc {
                                    line: 1,
                                    col: 5,
                                    ..
                                },
                                value: Ident(
                                    Id {
                                        value: 5,
                                    },
                                ),
                                end: Loc {
                                    line: 1,
                                    col: 6,
                                    ..
                                },
                            },
                            annotation: None,
                            rbar: Span {
                                start: Loc {
                                    line: 1,
                                    col: 6,
                                    ..
                                },
                                end: Loc {
                                    line: 1,
                                    col: 7,
                                    ..
                                },
                            },
                            body: Idx::<Term>(1),
                        },
                        SymbolRef(
                            SpanOf {
                                start: Loc {
                                    line: 2,
                                    col: 11,
                                    ..
                                },
                                value: Ident(
                                    Id {
                                        value: 7,
                                    },
                                ),
                                end: Loc {
                                    line: 2,
                                    col: 12,
                                    ..
                                },
                            },
                        ),
                        SymbolRef(
                            SpanOf {
                                start: Loc {
                                    line: 2,
                                    col: 8,
                                    ..
                                },
                                value: Ident(
                                    Id {
                                        value: 2,
                                    },
                                ),
                                end: Loc {
                                    line: 2,
                                    col: 9,
                                    ..
                                },
                            },
                        ),
                        Binding {
                            var: SpanOf {
                                start: Loc {
                                    line: 2,
                                    col: 4,
                                    ..
                                },
                                value: Ident(
                                    Id {
                                        value: 7,
                                    },
                                ),
                                end: Loc {
                                    line: 2,
                                    col: 5,
                                    ..
                                },
                            },
                            annotation: None,
                            eq: Span {
                                start: Loc {
                                    line: 2,
                                    col: 6,
                                    ..
                                },
                                end: Loc {
                                    line: 2,
                                    col: 7,
                                    ..
                                },
                            },
                            value: Idx::<Term>(4),
                            semi: Span {
                                start: Loc {
                                    line: 2,
                                    col: 9,
                                    ..
                                },
                                end: Loc {
                                    line: 2,
                                    col: 10,
                                    ..
                                },
                            },
                            expr: Idx::<Term>(3),
                        },
                    ],
                },
                pats: Arena {
                    len: 0,
                    data: [],
                },
            },
            items: [
                Term(
                    TermDefn {
                        name: SpanOf {
                            start: Loc {
                                line: 0,
                                col: 0,
                                ..
                            },
                            value: Ident(
                                Id {
                                    value: 2,
                                },
                            ),
                            end: Loc {
                                line: 0,
                                col: 1,
                                ..
                            },
                        },
                        annotation: None,
                        eq: Span {
                            start: Loc {
                                line: 0,
                                col: 2,
                                ..
                            },
                            end: Loc {
                                line: 0,
                                col: 3,
                                ..
                            },
                        },
                        value: Idx::<Term>(0),
                    },
                ),
                Term(
                    TermDefn {
                        name: SpanOf {
                            start: Loc {
                                line: 1,
                                col: 0,
                                ..
                            },
                            value: Ident(
                                Id {
                                    value: 4,
                                },
                            ),
                            end: Loc {
                                line: 1,
                                col: 1,
                                ..
                            },
                        },
                        annotation: None,
                        eq: Span {
                            start: Loc {
                                line: 1,
                                col: 2,
                                ..
                            },
                            end: Loc {
                                line: 1,
                                col: 3,
                                ..
                            },
                        },
                        value: Idx::<Term>(2),
                    },
                ),
                Term(
                    TermDefn {
                        name: SpanOf {
                            start: Loc {
                                line: 2,
                                col: 0,
                                ..
                            },
                            value: Ident(
                                Id {
                                    value: 6,
                                },
                            ),
                            end: Loc {
                                line: 2,
                                col: 1,
                                ..
                            },
                        },
                        annotation: None,
                        eq: Span {
                            start: Loc {
                                line: 2,
                                col: 2,
                                ..
                            },
                            end: Loc {
                                line: 2,
                                col: 3,
                                ..
                            },
                        },
                        value: Idx::<Term>(5),
                    },
                ),
            ],
        }
    "#]];
    expect.assert_debug_eq(module.data(&db));
  }

  #[test]
  fn test_annotated_term_items() {
    let db = TestDatabase::default();
    let file = SourceFile::new(
      &db,
      FileId::new(&db, PathBuf::from("eff_foo")),
      "x: a = a\ny: forall b. b -> b = |b| b".to_string(),
    );
    let module = db.parse_module(file);
    let expect = expect![[r#"
        CstModule {
            indices: CstIndxAlloc {
                types: Arena {
                    len: 4,
                    data: [
                        Named(
                            SpanOf {
                                start: Loc {
                                    line: 0,
                                    col: 3,
                                    ..
                                },
                                value: Ident(
                                    Id {
                                        value: 3,
                                    },
                                ),
                                end: Loc {
                                    line: 0,
                                    col: 4,
                                    ..
                                },
                            },
                        ),
                        Named(
                            SpanOf {
                                start: Loc {
                                    line: 1,
                                    col: 18,
                                    ..
                                },
                                value: Ident(
                                    Id {
                                        value: 5,
                                    },
                                ),
                                end: Loc {
                                    line: 1,
                                    col: 19,
                                    ..
                                },
                            },
                        ),
                        Named(
                            SpanOf {
                                start: Loc {
                                    line: 1,
                                    col: 13,
                                    ..
                                },
                                value: Ident(
                                    Id {
                                        value: 5,
                                    },
                                ),
                                end: Loc {
                                    line: 1,
                                    col: 14,
                                    ..
                                },
                            },
                        ),
                        Function {
                            domain: Idx::<Ident>>(2),
                            arrow: Span {
                                start: Loc {
                                    line: 1,
                                    col: 15,
                                    ..
                                },
                                end: Loc {
                                    line: 1,
                                    col: 17,
                                    ..
                                },
                            },
                            codomain: Idx::<Ident>>(1),
                        },
                    ],
                },
                terms: Arena {
                    len: 3,
                    data: [
                        SymbolRef(
                            SpanOf {
                                start: Loc {
                                    line: 0,
                                    col: 7,
                                    ..
                                },
                                value: Ident(
                                    Id {
                                        value: 3,
                                    },
                                ),
                                end: Loc {
                                    line: 0,
                                    col: 8,
                                    ..
                                },
                            },
                        ),
                        SymbolRef(
                            SpanOf {
                                start: Loc {
                                    line: 1,
                                    col: 26,
                                    ..
                                },
                                value: Ident(
                                    Id {
                                        value: 5,
                                    },
                                ),
                                end: Loc {
                                    line: 1,
                                    col: 27,
                                    ..
                                },
                            },
                        ),
                        Abstraction {
                            lbar: Span {
                                start: Loc {
                                    line: 1,
                                    col: 22,
                                    ..
                                },
                                end: Loc {
                                    line: 1,
                                    col: 23,
                                    ..
                                },
                            },
                            arg: SpanOf {
                                start: Loc {
                                    line: 1,
                                    col: 23,
                                    ..
                                },
                                value: Ident(
                                    Id {
                                        value: 5,
                                    },
                                ),
                                end: Loc {
                                    line: 1,
                                    col: 24,
                                    ..
                                },
                            },
                            annotation: None,
                            rbar: Span {
                                start: Loc {
                                    line: 1,
                                    col: 24,
                                    ..
                                },
                                end: Loc {
                                    line: 1,
                                    col: 25,
                                    ..
                                },
                            },
                            body: Idx::<Term>(1),
                        },
                    ],
                },
                pats: Arena {
                    len: 0,
                    data: [],
                },
            },
            items: [
                Term(
                    TermDefn {
                        name: SpanOf {
                            start: Loc {
                                line: 0,
                                col: 0,
                                ..
                            },
                            value: Ident(
                                Id {
                                    value: 2,
                                },
                            ),
                            end: Loc {
                                line: 0,
                                col: 1,
                                ..
                            },
                        },
                        annotation: Some(
                            Annotation {
                                colon: Span {
                                    start: Loc {
                                        line: 0,
                                        col: 1,
                                        ..
                                    },
                                    end: Loc {
                                        line: 0,
                                        col: 2,
                                        ..
                                    },
                                },
                                type_: Scheme {
                                    quantifiers: [],
                                    qualifiers: None,
                                    type_: Idx::<Ident>>(0),
                                },
                            },
                        ),
                        eq: Span {
                            start: Loc {
                                line: 0,
                                col: 5,
                                ..
                            },
                            end: Loc {
                                line: 0,
                                col: 6,
                                ..
                            },
                        },
                        value: Idx::<Term>(0),
                    },
                ),
                Term(
                    TermDefn {
                        name: SpanOf {
                            start: Loc {
                                line: 1,
                                col: 0,
                                ..
                            },
                            value: Ident(
                                Id {
                                    value: 4,
                                },
                            ),
                            end: Loc {
                                line: 1,
                                col: 1,
                                ..
                            },
                        },
                        annotation: Some(
                            Annotation {
                                colon: Span {
                                    start: Loc {
                                        line: 1,
                                        col: 1,
                                        ..
                                    },
                                    end: Loc {
                                        line: 1,
                                        col: 2,
                                        ..
                                    },
                                },
                                type_: Scheme {
                                    quantifiers: [
                                        Quantifier {
                                            forall: Span {
                                                start: Loc {
                                                    line: 1,
                                                    col: 3,
                                                    ..
                                                },
                                                end: Loc {
                                                    line: 1,
                                                    col: 9,
                                                    ..
                                                },
                                            },
                                            var: SpanOf {
                                                start: Loc {
                                                    line: 1,
                                                    col: 10,
                                                    ..
                                                },
                                                value: Ident(
                                                    Id {
                                                        value: 5,
                                                    },
                                                ),
                                                end: Loc {
                                                    line: 1,
                                                    col: 11,
                                                    ..
                                                },
                                            },
                                            dot: Span {
                                                start: Loc {
                                                    line: 1,
                                                    col: 11,
                                                    ..
                                                },
                                                end: Loc {
                                                    line: 1,
                                                    col: 12,
                                                    ..
                                                },
                                            },
                                        },
                                    ],
                                    qualifiers: None,
                                    type_: Idx::<Ident>>(3),
                                },
                            },
                        ),
                        eq: Span {
                            start: Loc {
                                line: 1,
                                col: 20,
                                ..
                            },
                            end: Loc {
                                line: 1,
                                col: 21,
                                ..
                            },
                        },
                        value: Idx::<Term>(2),
                    },
                ),
            ],
        }
    "#]];
    expect.assert_debug_eq(module.data(&db));
  }
}
