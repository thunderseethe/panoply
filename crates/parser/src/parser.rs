use std::iter::Peekable;

use base::diagnostic::parser::ParseError;
use base::ident::Ident;
use base::indexed::IdxAlloc;
use base::loc::Loc;
use base::span::{Span, SpanOf, Spanned};
use cst::{Annotation, CstIndxAlloc, IdField, Term, Type};
use la_arena::Idx;
use salsa::AsId;

use crate::lexer::Token;

pub struct Parser {
  tokens: Peekable<std::vec::IntoIter<SpanOf<Token>>>,
  eof: Span,
  pub alloc: CstIndxAlloc,
}
impl Parser {
  pub fn new(tokens: Vec<SpanOf<Token>>, eoi: Loc) -> Self {
    Self {
      tokens: tokens.into_iter().peekable(),
      eof: Span {
        start: eoi,
        end: Loc {
          file: eoi.file,
          byte: eoi.byte + 1,
          line: eoi.line,
          col: eoi.col + 1,
        },
      },
      alloc: CstIndxAlloc::default(),
    }
  }
}

trait NewParseError {
  fn new(got: SpanOf<Token>, want: impl IntoIterator<Item = Token>) -> Self;
}

impl NewParseError for ParseError {
  fn new(got: SpanOf<Token>, want: impl IntoIterator<Item = Token>) -> Self {
    Self::WrongToken {
      span: got.span(),
      got: got.value.name().to_string(),
      want_any: want.into_iter().map(|t| t.name().to_string()).collect(),
    }
  }
}

fn want_identifier() -> Token {
  Token::Identifier(Ident::from_id(salsa::Id::from_u32(0)))
}

impl Parser {
  fn peek(&mut self) -> Token {
    self
      .tokens
      .peek()
      .copied()
      .map(|tok| tok.value)
      .unwrap_or_else(|| Token::Eof)
  }

  fn peek_span(&mut self) -> Span {
    self
      .tokens
      .peek()
      .copied()
      .map(|span_of| span_of.span())
      .unwrap_or_else(|| self.eof)
  }

  fn consume(&mut self) -> SpanOf<Token> {
    self
      .tokens
      .next()
      .unwrap_or_else(|| self.eof.of(Token::Eof))
  }

  fn expect<T>(
    &mut self,
    cond: impl FnOnce(Token, Span) -> Result<T, ParseError>,
  ) -> Result<SpanOf<T>, ParseError> {
    cond(self.peek(), self.peek_span()).map(|t| self.consume().map(|_| t))
  }

  fn expect_lit(&mut self, expected_tok: Token) -> Result<SpanOf<Token>, ParseError> {
    self.expect(|tok, span| {
      if tok == expected_tok {
        Ok(tok)
      } else {
        Err(ParseError::new(span.of(tok), vec![expected_tok]))
      }
    })
  }

  fn ident(&mut self) -> Result<SpanOf<Ident>, ParseError> {
    self.expect(|tok, span| match tok {
      Token::Identifier(id) => Ok(id),
      t => Err(ParseError::new(span.of(t), vec![want_identifier()])),
    })
  }

  fn int(&mut self) -> Result<SpanOf<usize>, ParseError> {
    self.expect(|tok, span| match tok {
      Token::Int(int) => Ok(int),
      t => Err(ParseError::new(span.of(t), vec![Token::Int(0)])),
    })
  }

  fn type_row(&mut self) -> Result<cst::TypeRow<Ident>, ParseError> {
    let id = self.ident()?;
    match self.peek() {
      Token::Plus => {
        let mut separated = cst::Separated {
          first: id,
          elems: vec![],
          comma: None,
        };
        while let Token::Plus = self.peek() {
          let plus = self.consume();
          let id = self.ident()?;
          separated.elems.push((plus.span(), id));
        }
        Ok(cst::Row::Variable(separated))
      }
      Token::Colon => {
        let sep = self.consume().span();
        let target = self.type_()?;
        let mut separated = cst::Separated {
          first: cst::IdField {
            label: id,
            sep,
            target,
          },
          elems: vec![],
          comma: None,
        };
        while let Token::Comma = self.peek() {
          let comma = self.consume();
          let label = match self.ident() {
            Ok(id) => id,
            Err(_) => {
              separated.comma = Some(comma.span());
              break;
            }
          };
          let sep = self.expect_lit(Token::Colon)?.span();
          let target = self.type_()?;
          separated
            .elems
            .push((comma.span(), cst::IdField { label, sep, target }));
        }
        let Token::VerticalBar = self.peek() else {
          return Ok(cst::Row::Concrete(separated));
        };
        let vbar = self.consume().span();
        let id = self.ident()?;
        let mut variables = cst::Separated {
          first: id,
          elems: vec![],
          comma: None,
        };
        while let Token::Plus = self.peek() {
          let plus = self.consume();
          let id = self.ident()?;
          variables.elems.push((plus.span(), id));
        }
        Ok(cst::Row::Mixed {
          concrete: separated,
          vbar,
          variables,
        })
      }
      _ => Ok(cst::Row::Variable(cst::Separated {
        first: id,
        elems: vec![],
        comma: None,
      })),
    }
  }

  fn atom_type(&mut self) -> Result<Idx<cst::Type<Ident>>, ParseError> {
    match self.peek() {
      Token::Identifier(id) => {
        let name = self.consume().span().of(id);
        Ok(self.alloc.alloc(cst::Type::Named(name)))
      }
      Token::LAngle => {
        let langle = self.consume().span();
        let variants = self.type_row()?;
        let rangle = self.expect_lit(Token::RAngle)?.span();
        Ok(self.alloc.alloc(cst::Type::Sum {
          langle,
          variants,
          rangle,
        }))
      }
      Token::LBrace => {
        let lbrace = self.consume().span();
        let fields = self.type_row().ok();
        let rbrace = self.expect_lit(Token::RBrace)?.span();
        Ok(self.alloc.alloc(cst::Type::Product {
          lbrace,
          fields,
          rbrace,
        }))
      }
      Token::LParen => {
        let lpar = self.consume().span();
        let type_ = self.type_()?;
        let rpar = self.expect_lit(Token::RParen)?.span();
        Ok(
          self
            .alloc
            .alloc(cst::Type::Parenthesized { lpar, type_, rpar }),
        )
      }
      t => Err(ParseError::new(
        self.peek_span().of(t),
        vec![
          Token::LAngle,
          Token::LBrace,
          Token::LParen,
          want_identifier(),
        ],
      )),
    }
  }

  pub fn type_(&mut self) -> Result<Idx<cst::Type<Ident>>, ParseError> {
    let mut atom = self.atom_type()?;
    let mut arrs = vec![];
    while let Token::SmallArrow = self.peek() {
      let arrow = self.consume().span();
      let codomain = self.atom_type()?;
      arrs.push((atom, arrow));
      atom = codomain;
    }
    Ok(arrs.into_iter().rfold(atom, |codomain, (domain, arrow)| {
      self.alloc.alloc(cst::Type::Function {
        domain,
        arrow,
        codomain,
      })
    }))
  }

  fn quantifier(&mut self) -> Result<Option<cst::Quantifier<Ident>>, ParseError> {
    let Token::KwForall = self.peek() else {
      return Ok(None);
    };
    let forall = self.consume().span();
    let var = self.ident()?;
    let dot = self.expect_lit(Token::Dot)?.span();
    Ok(Some(cst::Quantifier { forall, var, dot }))
  }

  fn row_field(&mut self) -> Result<cst::IdField<Idx<Type<Ident>>>, ParseError> {
    let label = self.ident()?;
    let sep = self.expect_lit(Token::Colon)?.span();
    let target = self.type_()?;
    Ok(cst::Field { label, sep, target })
  }

  fn row_atom(&mut self) -> Result<cst::RowAtom<Ident>, ParseError> {
    match self.peek() {
      Token::LParen => {
        let lpar = self.consume().span();
        let mut fields = cst::Separated {
          first: self.row_field()?,
          elems: vec![],
          comma: None,
        };
        while let Token::Comma = self.peek() {
          let comma = self.consume().span();
          let Ok(elem) = self.row_field() else {
            fields.comma = Some(comma);
            break;
          };
          fields.elems.push((comma, elem));
        }
        let rpar = self.expect_lit(Token::RParen)?.span();
        Ok(cst::RowAtom::Concrete { lpar, fields, rpar })
      }
      Token::Identifier(id) => {
        let id = self.consume().span().of(id);
        Ok(cst::RowAtom::Variable(id))
      }
      token => Err(ParseError::new(
        self.peek_span().of(token),
        vec![Token::LParen, want_identifier()],
      )),
    }
  }

  fn constraint(&mut self) -> Result<cst::Constraint<Ident>, ParseError> {
    let _ = self.consume().span();
    let lhs = self.row_atom()?;
    let plus = self.expect_lit(Token::Plus)?.span();
    let rhs = self.row_atom()?;
    let eq = self.expect_lit(Token::Equal)?.span();
    let goal = self.row_atom()?;
    Ok(cst::Constraint::RowSum {
      lhs,
      plus,
      rhs,
      eq,
      goal,
    })
  }

  fn qualifiers(&mut self) -> Result<Option<cst::Qualifiers<Ident>>, ParseError> {
    let Token::LParen = self.peek() else {
      return Ok(None);
    };
    let mut constraints = cst::Separated {
      first: self.constraint()?,
      elems: vec![],
      comma: None,
    };
    while let Token::Comma = self.peek() {
      let comma: Span = self.consume().span();
      let Ok(constraint) = self.constraint() else {
        constraints.comma = Some(comma);
        break;
      };
      constraints.elems.push((comma, constraint));
    }
    let _ = self.expect_lit(Token::RParen)?.span();
    let arrow = self.expect_lit(Token::BigArrow)?.span();
    Ok(Some(cst::Qualifiers { constraints, arrow }))
  }

  pub fn scheme(&mut self) -> Result<cst::Scheme<Ident>, ParseError> {
    let mut quantifiers = vec![];
    while let Some(quant) = self.quantifier()? {
      quantifiers.push(quant);
    }
    let qualifiers = self.qualifiers()?;
    let type_ = self.type_()?;
    Ok(cst::Scheme {
      quantifiers,
      qualifiers,
      type_,
    })
  }

  fn pattern(&mut self) -> Result<Idx<cst::Pattern>, ParseError> {
    match self.peek() {
      Token::LBrace => {
        let lbrace = self.consume().span();
        let fields = self
          .ident()
          .and_then(|label| {
            let sep = self.expect_lit(Token::Equal)?.span();
            let target = self.pattern()?;
            let mut separated = cst::Separated {
              first: cst::IdField { label, sep, target },
              elems: vec![],
              comma: None,
            };
            while let Token::Comma = self.peek() {
              let comma = self.consume().span();
              let Ok(label) = self.ident() else {
                separated.comma = Some(comma);
                break;
              };
              let sep = self.expect_lit(Token::Equal)?.span();
              let target = self.pattern()?;
              separated
                .elems
                .push((comma.span(), IdField { label, sep, target }));
            }
            Ok(separated)
          })
          .ok();
        let rbrace = self.expect_lit(Token::RBrace)?.span();
        Ok(self.alloc.alloc(cst::Pattern::ProductRow(cst::ProductRow {
          lbrace,
          fields,
          rbrace,
        })))
      }
      Token::LAngle => {
        let langle = self.consume().span();
        let label = self.ident()?;
        let sep = self.expect_lit(Token::Equal)?.span();
        let target = self.pattern()?;
        let rangle = self.expect_lit(Token::RAngle)?.span();
        Ok(self.alloc.alloc(cst::Pattern::SumRow(cst::SumRow {
          langle,
          field: IdField { label, sep, target },
          rangle,
        })))
      }
      Token::Identifier(id) => {
        let v = self.consume().span().of(id);
        Ok(self.alloc.alloc(cst::Pattern::Whole(v)))
      }
      token => Err(ParseError::new(
        self.peek_span().of(token),
        vec![Token::LBrace, Token::LAngle, want_identifier()],
      )),
    }
  }

  fn match_arm(&mut self) -> Result<cst::Field<Idx<cst::Pattern>, Idx<cst::Term>>, ParseError> {
    let label = self.pattern()?;
    let sep = self.expect_lit(Token::BigArrow)?.span();
    let target = self.term()?;
    Ok(cst::Field { label, sep, target })
  }

  fn term_atom(&mut self) -> Result<Idx<cst::Term>, ParseError> {
    if let Ok(id) = self.ident() {
      return Ok(self.alloc.alloc(cst::Term::SymbolRef(id)));
    }
    if let Ok(int) = self.int() {
      return Ok(self.alloc.alloc(cst::Term::Int(int)));
    }
    match self.peek() {
      Token::LParen => {
        let lpar = self.consume().span();
        let term = self.term()?;
        let rpar = self.expect_lit(Token::RParen)?.span();
        Ok(
          self
            .alloc
            .alloc(cst::Term::Parenthesized { lpar, term, rpar }),
        )
      }
      Token::LBrace => {
        let lbrace = self.consume().span();
        let fields = self
          .ident()
          .and_then(|label| {
            let sep = self.expect_lit(Token::Equal)?.span();
            let target = self.term()?;
            let mut separated = cst::Separated {
              first: cst::IdField { label, sep, target },
              elems: vec![],
              comma: None,
            };
            while let Token::Comma = self.peek() {
              let comma = self.consume().span();
              let Ok(label) = self.ident() else {
                separated.comma = Some(comma);
                break;
              };
              let sep = self.expect_lit(Token::Equal)?.span();
              let target = self.term()?;
              separated
                .elems
                .push((comma.span(), IdField { label, sep, target }));
            }
            Ok(separated)
          })
          .ok();
        let rbrace = self.expect_lit(Token::RBrace)?.span();
        Ok(self.alloc.alloc(cst::Term::ProductRow(cst::ProductRow {
          lbrace,
          fields,
          rbrace,
        })))
      }
      Token::LAngle => {
        let langle = self.consume().span();
        let label = self.ident()?;
        let sep = self.expect_lit(Token::Equal)?.span();
        let target = self.term()?;
        let rangle = self.expect_lit(Token::RAngle)?.span();
        Ok(self.alloc.alloc(cst::Term::SumRow(cst::SumRow {
          langle,
          field: IdField { label, sep, target },
          rangle,
        })))
      }
      Token::KwMatch => {
        let match_ = self.consume().span();
        let langle = self.expect_lit(Token::LAngle)?.span();
        let mut cases = cst::Separated {
          first: self.match_arm()?,
          elems: vec![],
          comma: None,
        };
        while let Token::Comma = self.peek() {
          let comma = self.consume().span();
          let Ok(arm) = self.match_arm() else {
            cases.comma = Some(comma);
            break;
          };
          cases.elems.push((comma, arm));
        }
        let rangle = self.expect_lit(Token::RAngle)?.span();
        Ok(self.alloc.alloc(cst::Term::Match {
          match_,
          langle,
          cases,
          rangle,
        }))
      }
      t => Err(ParseError::new(
        self.peek_span().of(t),
        vec![Token::LParen, Token::LBrace, Token::LAngle, Token::KwMatch],
      )),
    }
  }

  fn annotation_type(&mut self) -> Result<Option<Annotation<Idx<Type<Ident>>>>, ParseError> {
    let Token::Colon = self.peek() else {
      return Ok(None);
    };
    let colon = self.consume().span();
    let type_ = self.type_()?;
    Ok(Some(Annotation { colon, type_ }))
  }

  fn annotation_scheme(&mut self) -> Result<Option<Annotation<cst::Scheme<Ident>>>, ParseError> {
    let Token::Colon = self.peek() else {
      return Ok(None);
    };
    let colon = self.consume().span();
    let scheme = self.scheme()?;
    Ok(Some(Annotation {
      colon,
      type_: scheme,
    }))
  }

  fn term_prefix(&mut self) -> Result<Option<Prefix>, ParseError> {
    match self.peek() {
      Token::KwWith => {
        let with = self.consume().span();
        let handler = self.term()?;
        let do_ = self.expect_lit(Token::KwDo)?.span();
        Ok(Some(Prefix::Handle { with, handler, do_ }))
      }
      Token::VerticalBar => {
        let lbar = self.consume().span();
        let var = self.ident()?;
        let annotation = self.annotation_type()?;
        let rbar = self.expect_lit(Token::VerticalBar)?.span();
        Ok(Some(Prefix::Abs {
          lbar,
          arg: var,
          annotation,
          rbar,
        }))
      }
      Token::KwLet => {
        let let_ = self.consume().span();
        let var = self.ident()?;
        let annotation = self.annotation_type()?;
        let eq = self.expect_lit(Token::Equal)?.span();
        let value = self.term()?;
        let semi = self.expect_lit(Token::Semicolon)?.span();
        Ok(Some(Prefix::Binding {
          let_,
          var,
          annotation,
          eq,
          value,
          semi,
        }))
      }
      _ => Ok(None),
    }
  }

  fn term_postfix(&mut self, atom: Idx<Term>) -> Result<Idx<Term>, ParseError> {
    match self.peek() {
      Token::LParen => {
        let lpar = self.consume().span();
        let arg = self.term()?;
        let rpar = self.expect_lit(Token::RParen)?.span();
        Ok(self.alloc.alloc(cst::Term::Application {
          func: atom,
          lpar,
          arg,
          rpar,
        }))
      }
      Token::Dot => {
        let dot = self.consume().span();
        let field = self.ident()?;
        Ok(self.alloc.alloc(cst::Term::DotAccess {
          base: atom,
          dot,
          field,
        }))
      }
      token => Err(ParseError::new(
        self.peek_span().of(token),
        vec![Token::LParen, Token::Dot],
      )),
    }
  }

  fn concat_op(&mut self) -> Result<Idx<cst::Term>, ParseError> {
    let mut atom = self.term_atom()?;
    while let Ok(postfix) = self.term_postfix(atom) {
      atom = postfix
    }
    Ok(atom)
  }

  pub fn term(&mut self) -> Result<Idx<cst::Term>, ParseError> {
    let mut prefixs = vec![];
    while let Some(prefix) = self.term_prefix()? {
      prefixs.push(prefix);
    }

    let mut left = self.concat_op()?;
    while let Token::Concat = self.peek() {
      let concat = self.consume().span();
      let right = self.concat_op()?;
      left = self.alloc.alloc(cst::Term::Concat {
        left,
        concat,
        right,
      })
    }

    Ok(prefixs.into_iter().rfold(left, |body, prefix| {
      self.alloc.alloc(match prefix {
        Prefix::Handle { with, handler, do_ } => cst::Term::Handle {
          with,
          handler,
          do_,
          expr: body,
        },
        Prefix::Abs {
          lbar,
          arg,
          annotation,
          rbar,
        } => cst::Term::Abstraction {
          lbar,
          arg,
          annotation,
          rbar,
          body,
        },
        Prefix::Binding {
          var,
          annotation,
          eq,
          value,
          semi,
          ..
        } => cst::Term::Binding {
          var,
          annotation,
          eq,
          value,
          semi,
          expr: body,
        },
      })
    }))
  }

  fn effect_ops(&mut self) -> Result<Vec<cst::EffectOp<Ident, Ident>>, ParseError> {
    let name = self.ident()?;
    let colon = self.expect_lit(Token::Colon)?.span();
    let type_ = self.type_()?;
    let mut ops = vec![cst::EffectOp { name, colon, type_ }];

    while let Token::Comma = self.peek() {
      let _ = self.consume();
      let name = self.ident()?;
      let colon = self.expect_lit(Token::Colon)?.span();
      let type_ = self.type_()?;
      ops.push(cst::EffectOp { name, colon, type_ });
    }

    Ok(ops)
  }

  pub fn items(&mut self) -> Result<Vec<cst::Item>, ParseError> {
    let mut items = vec![];
    loop {
      match self.peek() {
        Token::KwEffect => {
          let effect = self.consume().span();
          let name = self.ident()?;
          let lbrace = self.expect_lit(Token::LBrace)?.span();
          let ops = self.effect_ops()?;
          let rbrace = self.expect_lit(Token::RBrace)?.span();
          items.push(cst::Item::Effect(cst::EffectDefn {
            effect,
            name,
            lbrace,
            ops,
            rbrace,
          }));
        }
        Token::KwDefn => {
          let _ = self.consume().span();
          let name = self.ident()?;
          let annotation = self.annotation_scheme()?;
          let eq = self.expect_lit(Token::Equal)?.span();
          let value = self.term()?;
          items.push(cst::Item::Term(cst::TermDefn {
            name,
            annotation,
            eq,
            value,
          }));
        }
        Token::Eof => break,
        token => {
          return Err(ParseError::new(
            self.peek_span().of(token),
            vec![Token::KwEffect, want_identifier(), Token::Eof],
          ))
        }
      }
    }

    Ok(items)
  }
}

enum Prefix {
  Handle {
    with: Span,
    handler: Idx<Term>,
    do_: Span,
  },
  Abs {
    lbar: Span,
    arg: SpanOf<Ident>,
    annotation: Option<Annotation<Idx<Type<Ident>>>>,
    rbar: Span,
  },
  Binding {
    let_: Span,
    var: SpanOf<Ident>,
    annotation: Option<Annotation<Idx<Type<Ident>>>>,
    eq: Span,
    value: Idx<Term>,
    semi: Span,
  },
}

enum Postfix {
  Application {
    lpar: Span,
    arg: Idx<Term>,
    rpar: Span,
  },
  DotAccess {
    dot: Span,
    field: SpanOf<Ident>,
  },
}
