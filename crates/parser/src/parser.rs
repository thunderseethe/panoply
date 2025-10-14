use std::iter::Peekable;
use std::ops::{ControlFlow, Range};

use base::diagnostic::parser::ParseError;
use bit_set::BitSet;
use cst::Syntax;
use logos::{Logos, SpannedIter};
use rowan::{Checkpoint, GreenNode, GreenNodeBuilder};

pub struct Parser<'a> {
  input: &'a str,
  tokens: Peekable<SpannedIter<'a, Syntax>>,
  in_error: bool,
  tree: rowan::GreenNodeBuilder<'a>,
  errors: Vec<ParseError>,
}
impl<'a> Parser<'a> {
  pub fn new(input: &'a str) -> Self {
    Self {
      input,
      tokens: Syntax::lexer(input).spanned().peekable(),
      in_error: false,
      tree: GreenNodeBuilder::new(),
      errors: vec![],
    }
  }

  pub fn finish(self) -> (GreenNode, Vec<ParseError>) {
    (self.tree.finish(), self.errors)
  }
}

fn bitset(syntax: impl IntoIterator<Item = Syntax>) -> BitSet {
  let mut bit_set = BitSet::new();
  for syn in syntax {
    let kind: rowan::SyntaxKind = syn.into();
    bit_set.insert(kind.0.into());
  }
  bit_set
}

#[expect(unused)]
fn unioning(bitset: &BitSet, syntax: impl IntoIterator<Item = Syntax>) -> BitSet {
  let mut bs = bitset.clone();
  bs.extend(syntax.into_iter().map(|s| s.raw().into()));
  bs
}

impl Parser<'_> {
  fn peek(&mut self) -> Syntax {
    self
      .tokens
      .peek()
      .map(|(tok, _)| match tok {
        Ok(tok) => *tok,
        Err(_) => Syntax::Error,
      })
      .unwrap_or(Syntax::Eof)
  }

  fn at(&mut self, token: Syntax) -> bool {
    self.peek() == token
  }

  fn advance(&mut self) -> Option<Range<usize>> {
    let (_, span) = self.tokens.next()?;
    Some(span)
  }

  fn eat(&mut self, token: Syntax) -> Option<Range<usize>> {
    if self.at(token) { self.advance() } else { None }
  }

  fn at_any(&mut self, recovery_set: &BitSet) -> bool {
    recovery_set.contains(self.peek().raw().into())
  }

  fn recover_until(&mut self, anchor: &BitSet, expected: Vec<Syntax>) {
    let mut discard_toks = vec![];
    while !self.at_any(anchor) {
      let tok = self.peek();
      let Some(span) = self.advance() else {
        break;
      };

      discard_toks.push((tok, span));
    }
    // If we're already at an anchor there is nothing to do.
    if discard_toks.is_empty() {
      if !self.in_error {
        self.in_error = true;
        let got = self.peek().name().to_string();
        self.errors.push(ParseError::WrongToken {
          got,
          want_any: expected
            .into_iter()
            .map(|tok| tok.name().to_string())
            .collect::<Vec<_>>(),
          span: self
            .tokens
            .peek()
            .map(|(_, span)| span.clone())
            .unwrap_or_else(|| {
              let len = self.input.len();
              len..len
            }),
        });
      }
      return;
    }

    // This is safe because discard_toks is not empty.
    let mut err_span = discard_toks[0].1.clone();
    self.with(Syntax::Error, |this| {
      for (tok, span) in discard_toks {
        err_span.end = span.end;
        this.tree.token(tok.into(), &self.input[span]);
      }
    });
    if !self.in_error {
      self.in_error = true;
      let got = self.peek().name().to_string();
      self.errors.push(ParseError::WrongToken {
        got,
        want_any: expected
          .into_iter()
          .map(|tok| tok.name().to_string())
          .collect::<Vec<_>>(),
        span: err_span,
      });
    }
  }

  fn expect(&mut self, token: Syntax, anchor: &BitSet) -> Option<usize> {
    match self.eat(token) {
      Some(span) => {
        let count = span.len();
        self.tree.token(token.into(), &self.input[span]);
        self.in_error = false;
        Some(count)
      }
      None => {
        let mut bs = BitSet::new();
        bs.insert(token.raw().into());
        bs.union_with(anchor);
        self.recover_until(&bs, vec![token]);
        // Don't emit an error if we're already at an anchor.
        None
      }
    }
  }

  fn with<T>(&mut self, node: Syntax, body: impl FnOnce(&mut Self) -> T) -> T {
    self.tree.start_node(node.into());

    let res = body(self);

    self.tree.finish_node();

    res
  }

  fn ident(&mut self, anchor: &BitSet) -> Option<usize> {
    self.with(Syntax::Ident, |this| {
      let t = this.expect(Syntax::Identifier, anchor);
      this.whitespace();
      t
    })
  }

  fn int(&mut self, anchor: &BitSet) -> Option<usize> {
    let t = self.expect(Syntax::Int, anchor);
    self.whitespace();
    t
  }

  fn whitespace(&mut self) {
    if self.at(Syntax::Whitespace) {
      if let Some(span) = self.advance() {
        self
          .tree
          .token(Syntax::Whitespace.into(), &self.input[span]);
      }
    }
  }

  fn type_row(&mut self, anchor: &BitSet) {
    if !self.at(Syntax::Identifier) {
      // Empty row
      return;
    }
    let checkpoint = self.tree.checkpoint();
    self.ident(anchor);
    match self.peek() {
      Syntax::Plus => {
        self
          .tree
          .start_node_at(checkpoint, Syntax::VariableRow.into());
        while let Syntax::Plus = self.peek() {
          self.expect(Syntax::Plus, anchor);
          self.whitespace();
          self.ident(anchor);
        }
        self.tree.finish_node();
      }
      Syntax::Colon => {
        self.tree.start_node_at(checkpoint, Syntax::RowField.into());
        self.expect(Syntax::Colon, anchor);
        self.whitespace();
        self.type_(anchor);
        self.tree.finish_node();
        while let Syntax::Comma = self.peek() {
          self.expect(Syntax::Comma, anchor);
          self.whitespace();
          self.row_field(anchor);
        }
        let Syntax::VerticalBar = self.peek() else {
          self
            .tree
            .start_node_at(checkpoint, Syntax::ConcreteRow.into());
          self.whitespace();
          self.tree.finish_node();
          return;
        };
        self.tree.start_node_at(checkpoint, Syntax::MixedRow.into());
        self
          .tree
          .start_node_at(checkpoint, Syntax::ConcreteRow.into());
        self.whitespace();
        self.tree.finish_node();
        self.expect(Syntax::VerticalBar, anchor);
        self.whitespace();

        self.with(Syntax::VariableRow, |this| {
          this.ident(anchor);
          while let Syntax::Plus = this.peek() {
            this.expect(Syntax::Plus, anchor);
            this.whitespace();
            this.ident(anchor);
          }
        });
        self.tree.finish_node();
      }
      _ => {
        self
          .tree
          .start_node_at(checkpoint, Syntax::VariableRow.into());
        self.tree.finish_node();
      }
    }
  }

  fn atom_type(&mut self, anchor: &BitSet) -> ControlFlow<()> {
    match self.peek() {
      Syntax::Identifier => {
        self.with(Syntax::NameType, |this| {
          this.ident(anchor);
        });
      }
      Syntax::LAngle => {
        self.with(Syntax::SumType, |this| {
          this.expect(Syntax::LAngle, anchor);
          this.whitespace();
          this.type_row(anchor);
          this.whitespace();
          this.expect(Syntax::RAngle, anchor);
          this.whitespace();
        });
      }
      Syntax::LBracket => self.with(Syntax::ProductType, |this| {
        this.expect(Syntax::LBracket, anchor);
        this.whitespace();
        this.type_row(anchor);
        this.whitespace();
        this.expect(Syntax::RBracket, anchor);
        this.whitespace();
      }),
      Syntax::LParen => self.with(Syntax::ParenType, |this| {
        this.expect(Syntax::LParen, anchor);
        this.whitespace();
        this.type_(anchor);
        this.whitespace();
        this.expect(Syntax::RParen, anchor);
        this.whitespace();
      }),
      _ => return ControlFlow::Break(()),
    };
    ControlFlow::Continue(())
  }

  pub fn type_(&mut self, anchor: &BitSet) {
    let checkpoint = self.tree.checkpoint();
    let _ = self.atom_type(anchor);
    let mut is_func = false;
    if self.at(Syntax::SmallArrow) {
      is_func = true;
      self
        .tree
        .start_node_at(checkpoint, Syntax::FunctionType.into());
    }
    while let Syntax::SmallArrow = self.peek() {
      self.expect(Syntax::SmallArrow, anchor);
      self.whitespace();
      let _ = self.atom_type(anchor);
    }
    if is_func {
      self.tree.finish_node();
    }
  }

  fn quantifier(&mut self, anchor: &BitSet) -> ControlFlow<()> {
    let Syntax::KwForall = self.peek() else {
      return ControlFlow::Break(());
    };
    self.with(Syntax::ForallBinder, |this| {
      this.expect(Syntax::KwForall, anchor);
      this.whitespace();
      this.ident(anchor);
      this.expect(Syntax::Dot, anchor);
      this.whitespace();
      ControlFlow::Continue(())
    })
  }

  fn row_field(&mut self, anchor: &BitSet) {
    self.with(Syntax::RowField, |this| {
      this.ident(anchor);
      this.expect(Syntax::Colon, anchor);
      this.whitespace();
      this.type_(anchor);
    })
  }

  fn row_atom(&mut self, anchor: &BitSet) {
    match self.peek() {
      Syntax::LParen => {
        self.with(Syntax::ConcreteRowAtom, |this| {
          this.expect(Syntax::LParen, anchor);
          this.whitespace();
          if this.at(Syntax::Identifier) {
            this.row_field(anchor);
            while let Syntax::Comma = this.peek() {
              this.expect(Syntax::Comma, anchor);
              this.whitespace();
              if !this.at(Syntax::Identifier) {
                break;
              }
              this.row_field(anchor);
            }
          }
          this.whitespace();
          this.expect(Syntax::RParen, anchor);
          this.whitespace();
        });
      }
      Syntax::Identifier => {
        self.with(Syntax::VariableRowAtom, |this| this.ident(anchor));
      }
      _ => self.recover_until(anchor, vec![Syntax::Identifier, Syntax::LParen]),
    }
  }

  fn constraint(&mut self, anchor: &BitSet) {
    self.with(Syntax::RowSumConstraint, |this| {
      this.row_atom(anchor);
      this.expect(Syntax::Plus, anchor);
      this.whitespace();
      this.row_atom(anchor);
      this.expect(Syntax::Equal, anchor);
      this.whitespace();
      this.row_atom(anchor);
    });
  }

  fn qualifiers(&mut self, anchor: &BitSet) -> ControlFlow<()> {
    let Syntax::LParen = self.peek() else {
      return ControlFlow::Break(());
    };
    self.with(Syntax::Constraints, |this| {
      this.expect(Syntax::LParen, anchor);
      this.whitespace();
      this.constraint(anchor);
      while let Syntax::Comma = this.peek() {
        this.expect(Syntax::Comma, anchor);
        this.whitespace();
        if !this.at_any(&bitset([Syntax::Identifier, Syntax::LParen])) {
          break;
        }
        this.constraint(anchor);
      }
      this.expect(Syntax::RParen, anchor);
      this.whitespace();
      this.expect(Syntax::BigArrow, anchor);
      this.whitespace();
      ControlFlow::Continue(())
    })
  }

  pub fn scheme(&mut self, anchor: &BitSet) {
    self.with(Syntax::TypeScheme, |this| {
      this.with(Syntax::ForallBinders, |this| {
        while let ControlFlow::Continue(()) = this.quantifier(anchor) {}
      });
      let _ = this.qualifiers(anchor);
      this.type_(anchor);
    })
  }

  fn pattern(&mut self, anchor: &BitSet) {
    match self.peek() {
      Syntax::LBracket => {
        self.with(Syntax::ProdPattern, |this| {
          this.expect(Syntax::LBracket, anchor);
          this.whitespace();

          // Handle a unit pattern '{ }'
          if !this.at(Syntax::Identifier) {
            this.expect(Syntax::RBracket, anchor);
            this.whitespace();
            return;
          }

          this.with(Syntax::FieldPattern, |this| {
            this.ident(anchor);
            this.expect(Syntax::Equal, anchor);
            this.whitespace();
            this.pattern(anchor);
          });

          while let Syntax::Comma = this.peek() {
            this.expect(Syntax::Comma, anchor);
            this.whitespace();

            if !this.at(Syntax::Identifier) {
              break;
            }

            this.with(Syntax::FieldPattern, |this| {
              this.ident(anchor);
              this.expect(Syntax::Equal, anchor);
              this.whitespace();
              this.pattern(anchor);
            });
          }

          this.expect(Syntax::RBracket, anchor);
          this.whitespace();
        });
      }
      Syntax::LAngle => self.with(Syntax::SumPattern, |this| {
        this.expect(Syntax::LAngle, anchor);
        this.whitespace();
        this.with(Syntax::FieldPattern, |this| {
          this.ident(anchor);
          this.expect(Syntax::Equal, anchor);
          this.whitespace();
          this.pattern(anchor);
        });
        this.expect(Syntax::RAngle, anchor);
        this.whitespace();
      }),
      Syntax::Identifier => self.with(Syntax::WholePattern, |this| {
        this.ident(anchor);
      }),
      _ => self.recover_until(
        anchor,
        vec![Syntax::Identifier, Syntax::LAngle, Syntax::LBracket],
      ),
    }
  }

  fn match_arm(&mut self, anchor: &BitSet) {
    self.with(Syntax::MatchArm, |this| {
      this.pattern(anchor);
      this.expect(Syntax::BigArrow, anchor);
      this.whitespace();
      this.term(anchor);
    })
  }

  fn term_atom(&mut self, anchor: &BitSet) {
    match self.peek() {
      Syntax::Identifier => {
        self.with(Syntax::VarExpr, |this| {
          this.ident(anchor);
        });
      }
      Syntax::Int => {
        self.with(Syntax::IntExpr, |this| {
          this.int(anchor);
        });
      }
      Syntax::LParen => {
        self.with(Syntax::ParenthesizedExpr, |this| {
          this.expect(Syntax::LParen, anchor);
          this.term(anchor);
          this.expect(Syntax::RParen, anchor);
          this.whitespace();
        });
      }
      Syntax::LBracket => {
        self.with(Syntax::ProdExpr, |this| {
          this.expect(Syntax::LBracket, anchor);
          this.whitespace();

          if this.at(Syntax::Identifier) {
            this.with(Syntax::FieldExpr, |this| {
              this.ident(anchor);
              this.expect(Syntax::Equal, anchor);
              this.whitespace();
              this.term(anchor);
            });

            while let Syntax::Comma = this.peek() {
              this.expect(Syntax::Comma, anchor);
              this.whitespace();

              if !this.at(Syntax::Identifier) {
                break;
              }

              this.with(Syntax::FieldExpr, |this| {
                this.ident(anchor);
                this.expect(Syntax::Equal, anchor);
                this.whitespace();
                this.term(anchor);
              });
            }
          }

          this.expect(Syntax::RBracket, anchor);
          this.whitespace();
        });
      }
      Syntax::LAngle => {
        self.with(Syntax::SumExpr, |this| {
          this.expect(Syntax::LAngle, anchor);
          this.whitespace();

          this.ident(anchor);
          this.expect(Syntax::Equal, anchor);
          this.whitespace();

          this.term(anchor);
          this.expect(Syntax::RAngle, anchor);
          this.whitespace();
        });
      }
      Syntax::KwMatch => {
        self.with(Syntax::MatchExpr, |this| {
          this.expect(Syntax::KwMatch, anchor);
          this.whitespace();

          this.expect(Syntax::LAngle, anchor);
          this.whitespace();

          this.match_arm(anchor);

          while let Syntax::Comma = this.peek() {
            this.expect(Syntax::Comma, anchor);
            this.whitespace();

            if !this.at_any(&bitset([
              Syntax::LBracket,
              Syntax::LAngle,
              Syntax::Identifier,
            ])) {
              break;
            }
            this.match_arm(anchor);
          }

          this.expect(Syntax::RAngle, anchor);
          this.whitespace();
        });
      }
      _ => self.recover_until(
        anchor,
        vec![
          Syntax::Identifier,
          Syntax::Int,
          Syntax::LParen,
          Syntax::LBrace,
          Syntax::LAngle,
          Syntax::KwMatch,
        ],
      ),
    }
  }

  fn annotation_type(&mut self, anchor: &BitSet) -> ControlFlow<()> {
    let Syntax::Colon = self.peek() else {
      return ControlFlow::Break(());
    };
    self.with(Syntax::TypeAnnotation, |this| {
      this.expect(Syntax::Colon, anchor);
      this.whitespace();
      this.type_(anchor);
      ControlFlow::Continue(())
    })
  }

  fn annotation_scheme(&mut self, anchor: &BitSet) -> ControlFlow<()> {
    let Syntax::Colon = self.peek() else {
      return ControlFlow::Break(());
    };
    self.with(Syntax::SchemeAnnotation, |this| {
      this.expect(Syntax::Colon, anchor);
      this.whitespace();

      this.scheme(anchor);
      ControlFlow::Continue(())
    })
  }

  fn term_prefix(&mut self, anchor: &BitSet) -> ControlFlow<()> {
    match self.peek() {
      Syntax::KwWith => {
        self.with(Syntax::WithPrefix, |this| {
          this.expect(Syntax::KwWith, anchor);
          this.whitespace();

          this.term(anchor);

          this.expect(Syntax::KwDo, anchor);
          this.whitespace();
        });
      }
      Syntax::VerticalBar => {
        self.with(Syntax::ClosurePrefix, |this| {
          this.expect(Syntax::VerticalBar, anchor);
          this.whitespace();

          this.ident(anchor);
          let _ = this.annotation_type(anchor);

          this.expect(Syntax::VerticalBar, anchor);
          this.whitespace();
        });
      }
      Syntax::KwLet => {
        self.with(Syntax::LetPrefix, |this| {
          this.expect(Syntax::KwLet, anchor);
          this.whitespace();

          this.ident(anchor);

          let _ = this.annotation_type(anchor);
          this.expect(Syntax::Equal, anchor);
          this.whitespace();

          this.term(anchor);
          this.expect(Syntax::Semicolon, anchor);
          this.whitespace();
        });
      }
      _ => return ControlFlow::Break(()),
    };
    ControlFlow::Continue(())
  }

  fn term_postfix(&mut self, atom: Checkpoint, anchor: &BitSet) -> ControlFlow<()> {
    match self.peek() {
      Syntax::LParen => {
        self.tree.start_node_at(atom, Syntax::ArgPostfix.into());
        self.expect(Syntax::LParen, anchor);
        self.term(anchor);
        self.expect(Syntax::RParen, anchor);
        self.tree.finish_node();
      }
      Syntax::Dot => {
        self.tree.start_node_at(atom, Syntax::FieldPostfix.into());
        self.expect(Syntax::Dot, anchor);
        self.ident(anchor);
        self.tree.finish_node();
      }
      _ => return ControlFlow::Break(()),
    };
    ControlFlow::Continue(())
  }

  fn concat_op(&mut self, anchor: &BitSet) {
    let checkpoint = self.tree.checkpoint();
    self.term_atom(anchor);
    while let ControlFlow::Continue(()) = self.term_postfix(checkpoint, anchor) {}
  }

  pub fn term(&mut self, anchor: &BitSet) {
    self.with(Syntax::Term, |this| {
      let mut had_prefixes = false;
      if this.at_any(&bitset([
        Syntax::KwLet,
        Syntax::VerticalBar,
        Syntax::KwWith,
      ])) {
        had_prefixes = true;
        this.tree.start_node(Syntax::TermPrefix.into());
      }

      while let ControlFlow::Continue(()) = this.term_prefix(anchor) {}

      if had_prefixes {
        this.tree.finish_node();
      }

      this.with(Syntax::ConcatOps, |this| {
        this.concat_op(anchor);
        while let Syntax::Concat = this.peek() {
          this.expect(Syntax::Concat, anchor);
          this.whitespace();
          this.concat_op(anchor);
        }
      });
    });
  }

  fn effect_ops(&mut self, anchor: &BitSet) {
    self.with(Syntax::EffectOps, |this| {
      this.with(Syntax::EffectOp, |this| {
        this.ident(anchor);

        let _ = this.annotation_scheme(anchor);
        /*this.expect(Syntax::Colon, anchor);
        this.whitespace();

        this.type_(anchor);*/
      });

      while let Syntax::Comma = this.peek() {
        this.expect(Syntax::Comma, anchor);
        this.whitespace();

        if !this.at(Syntax::Identifier) {
          break;
        }
        this.with(Syntax::EffectOp, |this| {
          this.ident(anchor);

          let _ = this.annotation_scheme(anchor);
          /*this.expect(Syntax::Colon, anchor);
          this.whitespace();

          this.type_(anchor);*/
        });
      }
    });
  }

  pub fn items(&mut self) {
    let bitset = bitset([Syntax::KwEffect, Syntax::KwDefn, Syntax::Eof]);
    self.with(Syntax::Items, |this| {
      loop {
        this.whitespace();
        match this.peek() {
          Syntax::KwEffect => {
            this.with(Syntax::EffectDefn, |this| {
              this.expect(Syntax::KwEffect, &bitset);
              this.whitespace();

              this.ident(&bitset);

              this.expect(Syntax::LBracket, &bitset);
              this.whitespace();

              this.effect_ops(&bitset);

              this.expect(Syntax::RBracket, &bitset);
              this.whitespace();
            });
          }
          Syntax::KwDefn => {
            this.with(Syntax::TermDefn, |this| {
              this.expect(Syntax::KwDefn, &bitset);
              this.whitespace();

              this.ident(&bitset);
              let _ = this.annotation_scheme(&bitset);

              this.expect(Syntax::Equal, &bitset);
              this.whitespace();

              this.term(&bitset);
            });
          }
          Syntax::Eof => {
            //this.expect(Syntax::Eof, &bitset);
            break;
          }
          _ => this.recover_until(&bitset, vec![Syntax::KwDefn, Syntax::KwEffect, Syntax::Eof]),
        }
      }
    })
  }
}

#[cfg(test)]
mod tests {
  use cst::Syntax;
  use rowan::SyntaxNode;

  use expect_test::expect;

  use crate::Panoply;

  use super::bitset;

  fn parse_term(input: &str) -> SyntaxNode<Panoply> {
    let mut parser = crate::parser::Parser::new(input);
    parser.term(&bitset([Syntax::Eof]));
    let (term, _) = parser.finish();
    SyntaxNode::new_root(term)
  }

  fn parse_type(input: &str) -> SyntaxNode<Panoply> {
    let mut parser = crate::parser::Parser::new(input);
    parser.type_(&bitset([Syntax::Eof]));
    let (type_, _) = parser.finish();
    SyntaxNode::new_root(type_)
  }

  fn parse_scheme(input: &str) -> SyntaxNode<Panoply> {
    let mut parser = crate::parser::Parser::new(input);
    parser.scheme(&bitset([Syntax::Eof]));
    let (scheme, _) = parser.finish();
    SyntaxNode::new_root(scheme)
  }

  fn parse_item(input: &str) -> SyntaxNode<Panoply> {
    let mut parser = crate::parser::Parser::new(input);
    parser.items();
    let (items, _) = parser.finish();
    SyntaxNode::new_root(items)
  }

  #[test]
  fn test_sum_types() {
    let expect = expect![[r#"
        SumType@0..12
          LAngle@0..1 "<"
          ConcreteRow@1..11
            RowField@1..5
              Ident@1..2
                Identifier@1..2 "x"
              Colon@2..3 ":"
              Whitespace@3..4 " "
              NameType@4..5
                Ident@4..5
                  Identifier@4..5 "a"
            Comma@5..6 ","
            Whitespace@6..7 " "
            RowField@7..11
              Ident@7..8
                Identifier@7..8 "y"
              Colon@8..9 ":"
              Whitespace@9..10 " "
              NameType@10..11
                Ident@10..11
                  Identifier@10..11 "b"
          RAngle@11..12 ">"
    "#]];
    expect.assert_eq(&format!("{:#?}", parse_type("<x: a, y: b>")));

    let expect = expect![[r#"
        SumType@0..7
          LAngle@0..1 "<"
          VariableRow@1..6
            Ident@1..3
              Identifier@1..2 "r"
              Whitespace@2..3 " "
            Plus@3..4 "+"
            Whitespace@4..5 " "
            Ident@5..6
              Identifier@5..6 "s"
          RAngle@6..7 ">"
    "#]];
    expect.assert_eq(&format!("{:#?}", parse_type("<r + s>")));

    let expect = expect![[r#"
        SumType@0..21
          LAngle@0..1 "<"
          MixedRow@1..20
            ConcreteRow@1..13
              RowField@1..6
                Ident@1..2
                  Identifier@1..2 "x"
                Colon@2..3 ":"
                Whitespace@3..4 " "
                NameType@4..6
                  Ident@4..6
                    Identifier@4..5 "a"
                    Whitespace@5..6 " "
              Comma@6..7 ","
              Whitespace@7..8 " "
              RowField@8..13
                Ident@8..9
                  Identifier@8..9 "y"
                Colon@9..10 ":"
                Whitespace@10..11 " "
                NameType@11..13
                  Ident@11..13
                    Identifier@11..12 "b"
                    Whitespace@12..13 " "
            VerticalBar@13..14 "|"
            Whitespace@14..15 " "
            VariableRow@15..20
              Ident@15..17
                Identifier@15..16 "r"
                Whitespace@16..17 " "
              Plus@17..18 "+"
              Whitespace@18..19 " "
              Ident@19..20
                Identifier@19..20 "s"
          RAngle@20..21 ">"
    "#]];
    expect.assert_eq(&format!("{:#?}", parse_type("<x: a , y: b | r + s>")));
  }

  #[test]
  fn test_product_types() {
    let expect = expect![[r#"
        ProductType@0..2
          LBracket@0..1 "{"
          RBracket@1..2 "}"
    "#]];
    expect.assert_eq(&format!("{:#?}", parse_type("{}")));

    let expect = expect![[r#"
        ProductType@0..12
          LBracket@0..1 "{"
          ConcreteRow@1..11
            RowField@1..5
              Ident@1..2
                Identifier@1..2 "x"
              Colon@2..3 ":"
              Whitespace@3..4 " "
              NameType@4..5
                Ident@4..5
                  Identifier@4..5 "a"
            Comma@5..6 ","
            Whitespace@6..7 " "
            RowField@7..11
              Ident@7..8
                Identifier@7..8 "y"
              Colon@8..9 ":"
              Whitespace@9..10 " "
              NameType@10..11
                Ident@10..11
                  Identifier@10..11 "b"
          RBracket@11..12 "}"
    "#]];
    expect.assert_eq(&format!("{:#?}", parse_type("{x: a, y: b}")));

    let expect = expect![[r#"
        ProductType@0..7
          LBracket@0..1 "{"
          VariableRow@1..6
            Ident@1..3
              Identifier@1..2 "r"
              Whitespace@2..3 " "
            Plus@3..4 "+"
            Whitespace@4..5 " "
            Ident@5..6
              Identifier@5..6 "s"
          RBracket@6..7 "}"
    "#]];
    expect.assert_eq(&format!("{:#?}", parse_type("{r + s}")));

    let expect = expect![[r#"
        ProductType@0..13
          LBracket@0..1 "{"
          Whitespace@1..2 " "
          MixedRow@2..11
            ConcreteRow@2..7
              RowField@2..7
                Ident@2..3
                  Identifier@2..3 "x"
                Colon@3..4 ":"
                Whitespace@4..5 " "
                NameType@5..7
                  Ident@5..7
                    Identifier@5..6 "a"
                    Whitespace@6..7 " "
            VerticalBar@7..8 "|"
            Whitespace@8..9 " "
            VariableRow@9..11
              Ident@9..11
                Identifier@9..10 "r"
                Whitespace@10..11 " "
          RBracket@11..12 "}"
          Whitespace@12..13 " "
    "#]];
    expect.assert_eq(&format!("{:#?}", parse_type("{ x: a | r } ")));
  }

  #[test]
  fn test_function_types() {
    // Make sure this example tests right-associativity.
    let expect = expect![[r#"
        FunctionType@0..25
          ParenType@0..9
            LParen@0..1 "("
            FunctionType@1..7
              NameType@1..3
                Ident@1..3
                  Identifier@1..2 "a"
                  Whitespace@2..3 " "
              SmallArrow@3..5 "->"
              Whitespace@5..6 " "
              NameType@6..7
                Ident@6..7
                  Identifier@6..7 "b"
            RParen@7..8 ")"
            Whitespace@8..9 " "
          SmallArrow@9..11 "->"
          Whitespace@11..12 " "
          NameType@12..14
            Ident@12..14
              Identifier@12..13 "a"
              Whitespace@13..14 " "
          SmallArrow@14..16 "->"
          Whitespace@16..17 " "
          ParenType@17..25
            LParen@17..18 "("
            FunctionType@18..24
              NameType@18..20
                Ident@18..20
                  Identifier@18..19 "b"
                  Whitespace@19..20 " "
              SmallArrow@20..22 "->"
              Whitespace@22..23 " "
              NameType@23..24
                Ident@23..24
                  Identifier@23..24 "c"
            RParen@24..25 ")"
    "#]];
    expect.assert_eq(&format!("{:#?}", parse_type("(a -> b) -> a -> (b -> c)")));
  }

  #[test]
  fn test_mixed_types() {
    let expect = expect![[r#"
        FunctionType@0..25
          ProductType@0..7
            LBracket@0..1 "{"
            ConcreteRow@1..5
              RowField@1..5
                Ident@1..2
                  Identifier@1..2 "x"
                Colon@2..3 ":"
                Whitespace@3..4 " "
                NameType@4..5
                  Ident@4..5
                    Identifier@4..5 "a"
            RBracket@5..6 "}"
            Whitespace@6..7 " "
          SmallArrow@7..9 "->"
          Whitespace@9..10 " "
          SumType@10..25
            LAngle@10..11 "<"
            MixedRow@11..24
              ConcreteRow@11..21
                RowField@11..21
                  Ident@11..12
                    Identifier@11..12 "f"
                  Colon@12..13 ":"
                  Whitespace@13..14 " "
                  FunctionType@14..21
                    NameType@14..16
                      Ident@14..16
                        Identifier@14..15 "b"
                        Whitespace@15..16 " "
                    SmallArrow@16..18 "->"
                    Whitespace@18..19 " "
                    NameType@19..21
                      Ident@19..21
                        Identifier@19..20 "a"
                        Whitespace@20..21 " "
              VerticalBar@21..22 "|"
              Whitespace@22..23 " "
              VariableRow@23..24
                Ident@23..24
                  Identifier@23..24 "r"
            RAngle@24..25 ">"
    "#]];
    expect.assert_eq(&format!("{:#?}", parse_type("{x: a} -> <f: b -> a | r>")));
  }

  #[test]
  fn test_unqualified_schemes() {
    let expect = expect![[r#"
        TypeScheme@0..25
          ForallBinders@0..0
          FunctionType@0..25
            ProductType@0..7
              LBracket@0..1 "{"
              ConcreteRow@1..5
                RowField@1..5
                  Ident@1..2
                    Identifier@1..2 "x"
                  Colon@2..3 ":"
                  Whitespace@3..4 " "
                  NameType@4..5
                    Ident@4..5
                      Identifier@4..5 "a"
              RBracket@5..6 "}"
              Whitespace@6..7 " "
            SmallArrow@7..9 "->"
            Whitespace@9..10 " "
            SumType@10..25
              LAngle@10..11 "<"
              MixedRow@11..24
                ConcreteRow@11..21
                  RowField@11..21
                    Ident@11..12
                      Identifier@11..12 "f"
                    Colon@12..13 ":"
                    Whitespace@13..14 " "
                    FunctionType@14..21
                      NameType@14..16
                        Ident@14..16
                          Identifier@14..15 "b"
                          Whitespace@15..16 " "
                      SmallArrow@16..18 "->"
                      Whitespace@18..19 " "
                      NameType@19..21
                        Ident@19..21
                          Identifier@19..20 "a"
                          Whitespace@20..21 " "
                VerticalBar@21..22 "|"
                Whitespace@22..23 " "
                VariableRow@23..24
                  Ident@23..24
                    Identifier@23..24 "r"
              RAngle@24..25 ">"
    "#]];
    expect.assert_eq(&format!("{:#?}", parse_scheme("{x: a} -> <f: b -> a | r>")));
  }

  #[test]
  fn test_equals_schemes() {
    let expect = expect![[r#"
        TypeScheme@0..35
          ForallBinders@0..0
          Constraints@0..20
            LParen@0..1 "("
            RowSumConstraint@1..15
              VariableRowAtom@1..3
                Ident@1..3
                  Identifier@1..2 "r"
                  Whitespace@2..3 " "
              Plus@3..4 "+"
              Whitespace@4..5 " "
              ConcreteRowAtom@5..12
                LParen@5..6 "("
                RowField@6..10
                  Ident@6..7
                    Identifier@6..7 "y"
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  NameType@9..10
                    Ident@9..10
                      Identifier@9..10 "a"
                RParen@10..11 ")"
                Whitespace@11..12 " "
              Equal@12..13 "="
              Whitespace@13..14 " "
              VariableRowAtom@14..15
                Ident@14..15
                  Identifier@14..15 "s"
            RParen@15..16 ")"
            Whitespace@16..17 " "
            BigArrow@17..19 "=>"
            Whitespace@19..20 " "
          FunctionType@20..35
            ProductType@20..24
              LBracket@20..21 "{"
              VariableRow@21..22
                Ident@21..22
                  Identifier@21..22 "r"
              RBracket@22..23 "}"
              Whitespace@23..24 " "
            SmallArrow@24..26 "->"
            Whitespace@26..27 " "
            NameType@27..29
              Ident@27..29
                Identifier@27..28 "a"
                Whitespace@28..29 " "
            SmallArrow@29..31 "->"
            Whitespace@31..32 " "
            ProductType@32..35
              LBracket@32..33 "{"
              VariableRow@33..34
                Ident@33..34
                  Identifier@33..34 "s"
              RBracket@34..35 "}"
    "#]];
    expect.assert_eq(&format!(
      "{:#?}",
      parse_scheme("(r + (y: a) = s) => {r} -> a -> {s}")
    ));
  }

  #[test]
  fn test_undelimted_closure_fails() {
    let expect = expect![[r#"
        Term@0..12
          TermPrefix@0..12
            ClosurePrefix@0..12
              VerticalBar@0..1 "|"
              Ident@1..3
                Identifier@1..2 "x"
                Whitespace@2..3 " "
              Error@3..12
                Identifier@3..9 "whoops"
                LParen@9..10 "("
                Identifier@10..11 "x"
                RParen@11..12 ")"
          ConcatOps@12..12
    "#]];
    expect.assert_eq(&format!("{:#?}", parse_term("|x whoops(x)")));
  }

  #[test]
  fn test_annotated_bindings() {
    let expect = expect![[r#"
        Term@0..32
          TermPrefix@0..31
            LetPrefix@0..15
              KwLet@0..3 "let"
              Whitespace@3..4 " "
              Ident@4..5
                Identifier@4..5 "x"
              TypeAnnotation@5..9
                Colon@5..6 ":"
                Whitespace@6..7 " "
                NameType@7..9
                  Ident@7..9
                    Identifier@7..8 "a"
                    Whitespace@8..9 " "
              Equal@9..10 "="
              Whitespace@10..11 " "
              Term@11..13
                ConcatOps@11..13
                  ProdExpr@11..13
                    LBracket@11..12 "{"
                    RBracket@12..13 "}"
              Semicolon@13..14 ";"
              Whitespace@14..15 " "
            LetPrefix@15..31
              KwLet@15..18 "let"
              Whitespace@18..19 " "
              Ident@19..20
                Identifier@19..20 "y"
              TypeAnnotation@20..25
                Colon@20..21 ":"
                Whitespace@21..22 " "
                ProductType@22..25
                  LBracket@22..23 "{"
                  RBracket@23..24 "}"
                  Whitespace@24..25 " "
              Equal@25..26 "="
              Whitespace@26..27 " "
              Term@27..29
                ConcatOps@27..29
                  ProdExpr@27..29
                    LBracket@27..28 "{"
                    RBracket@28..29 "}"
              Semicolon@29..30 ";"
              Whitespace@30..31 " "
          ConcatOps@31..32
            VarExpr@31..32
              Ident@31..32
                Identifier@31..32 "x"
    "#]];
    expect.assert_eq(&format!(
      "{:#?}",
      parse_term("let x: a = {}; let y: {} = {}; x")
    ));
  }

  #[test]
  fn test_unsign_int() {
    let expect = expect![[r#"
        Term@0..5
          ConcatOps@0..5
            IntExpr@0..5
              Int@0..5 "12354"
    "#]];
    expect.assert_eq(&format!("{:#?}", parse_term("12354")));

    let expect = expect![[r#"
        Term@0..5
          ConcatOps@0..5
            IntExpr@0..5
              Int@0..5 "00101"
    "#]];
    expect.assert_eq(&format!("{:#?}", parse_term("00101")));

    let expect = expect![[r#"
        Term@0..1
          ConcatOps@0..1
            IntExpr@0..1
              Int@0..1 "0"
    "#]];
    expect.assert_eq(&format!("{:#?}", parse_term("0")));
  }

  #[test]
  fn test_app_precedence() {
    let expect = expect![[r#"
        Term@0..17
          ConcatOps@0..17
            ArgPostfix@0..17
              ArgPostfix@0..14
                ParenthesizedExpr@0..11
                  LParen@0..1 "("
                  Term@1..10
                    TermPrefix@1..9
                      ClosurePrefix@1..5
                        VerticalBar@1..2 "|"
                        Ident@2..3
                          Identifier@2..3 "x"
                        VerticalBar@3..4 "|"
                        Whitespace@4..5 " "
                      ClosurePrefix@5..9
                        VerticalBar@5..6 "|"
                        Ident@6..7
                          Identifier@6..7 "w"
                        VerticalBar@7..8 "|"
                        Whitespace@8..9 " "
                    ConcatOps@9..10
                      VarExpr@9..10
                        Ident@9..10
                          Identifier@9..10 "w"
                  RParen@10..11 ")"
                LParen@11..12 "("
                Term@12..13
                  ConcatOps@12..13
                    VarExpr@12..13
                      Ident@12..13
                        Identifier@12..13 "y"
                RParen@13..14 ")"
              LParen@14..15 "("
              Term@15..16
                ConcatOps@15..16
                  VarExpr@15..16
                    Ident@15..16
                      Identifier@15..16 "z"
              RParen@16..17 ")"
    "#]];
    expect.assert_eq(&format!("{:#?}", parse_term("(|x| |w| w)(y)(z)")));
  }

  #[test]
  fn test_with_do() {
    let expect = expect![[r#"
        Term@0..14
          TermPrefix@0..10
            WithPrefix@0..10
              KwWith@0..4 "with"
              Whitespace@4..5 " "
              Term@5..7
                ConcatOps@5..7
                  VarExpr@5..7
                    Ident@5..7
                      Identifier@5..6 "h"
                      Whitespace@6..7 " "
              KwDo@7..9 "do"
              Whitespace@9..10 " "
          ConcatOps@10..14
            ArgPostfix@10..14
              VarExpr@10..11
                Ident@10..11
                  Identifier@10..11 "a"
              LParen@11..12 "("
              Term@12..13
                ConcatOps@12..13
                  VarExpr@12..13
                    Ident@12..13
                      Identifier@12..13 "b"
              RParen@13..14 ")"
    "#]];
    expect.assert_eq(&format!("{:#?}", parse_term("with h do a(b)")));
  }

  #[test]
  fn test_mixing_prefixes() {
    let expect = expect![[r#"
        Term@0..37
          TermPrefix@0..36
            ClosurePrefix@0..4
              VerticalBar@0..1 "|"
              Ident@1..2
                Identifier@1..2 "x"
              VerticalBar@2..3 "|"
              Whitespace@3..4 " "
            LetPrefix@4..22
              KwLet@4..7 "let"
              Whitespace@7..8 " "
              Ident@8..10
                Identifier@8..9 "y"
                Whitespace@9..10 " "
              Equal@10..11 "="
              Whitespace@11..12 " "
              Term@12..20
                TermPrefix@12..16
                  ClosurePrefix@12..16
                    VerticalBar@12..13 "|"
                    Ident@13..14
                      Identifier@13..14 "z"
                    VerticalBar@14..15 "|"
                    Whitespace@15..16 " "
                ConcatOps@16..20
                  ArgPostfix@16..20
                    VarExpr@16..17
                      Ident@16..17
                        Identifier@16..17 "y"
                    LParen@17..18 "("
                    Term@18..19
                      ConcatOps@18..19
                        VarExpr@18..19
                          Ident@18..19
                            Identifier@18..19 "z"
                    RParen@19..20 ")"
              Semicolon@20..21 ";"
              Whitespace@21..22 " "
            LetPrefix@22..36
              KwLet@22..25 "let"
              Whitespace@25..26 " "
              Ident@26..28
                Identifier@26..27 "w"
                Whitespace@27..28 " "
              Equal@28..29 "="
              Whitespace@29..30 " "
              Term@30..34
                ConcatOps@30..34
                  ArgPostfix@30..34
                    VarExpr@30..31
                      Ident@30..31
                        Identifier@30..31 "x"
                    LParen@31..32 "("
                    Term@32..33
                      ConcatOps@32..33
                        VarExpr@32..33
                          Ident@32..33
                            Identifier@32..33 "y"
                    RParen@33..34 ")"
              Semicolon@34..35 ";"
              Whitespace@35..36 " "
          ConcatOps@36..37
            VarExpr@36..37
              Ident@36..37
                Identifier@36..37 "w"
    "#]];
    expect.assert_eq(&format!(
      "{:#?}",
      parse_term("|x| let y = |z| y(z); let w = x(y); w")
    ));
  }

  #[test]
  fn test_basic_lambdas() {
    let expect = expect![[r#"
        Term@0..26
          TermPrefix@0..25
            ClosurePrefix@0..4
              VerticalBar@0..1 "|"
              Ident@1..2
                Identifier@1..2 "x"
              VerticalBar@2..3 "|"
              Whitespace@3..4 " "
            ClosurePrefix@4..11
              VerticalBar@4..5 "|"
              Ident@5..6
                Identifier@5..6 "y"
              TypeAnnotation@6..9
                Colon@6..7 ":"
                Whitespace@7..8 " "
                NameType@8..9
                  Ident@8..9
                    Identifier@8..9 "a"
              VerticalBar@9..10 "|"
              Whitespace@10..11 " "
            LetPrefix@11..25
              KwLet@11..14 "let"
              Whitespace@14..15 " "
              Ident@15..17
                Identifier@15..16 "z"
                Whitespace@16..17 " "
              Equal@17..18 "="
              Whitespace@18..19 " "
              Term@19..23
                ConcatOps@19..23
                  ArgPostfix@19..23
                    VarExpr@19..20
                      Ident@19..20
                        Identifier@19..20 "x"
                    LParen@20..21 "("
                    Term@21..22
                      ConcatOps@21..22
                        VarExpr@21..22
                          Ident@21..22
                            Identifier@21..22 "y"
                    RParen@22..23 ")"
              Semicolon@23..24 ";"
              Whitespace@24..25 " "
          ConcatOps@25..26
            VarExpr@25..26
              Ident@25..26
                Identifier@25..26 "z"
    "#]];
    expect.assert_eq(&format!("{:#?}", parse_term("|x| |y: a| let z = x(y); z")));
  }

  #[test]
  fn test_product_rows() {
    let expect = expect![[r#"
        Term@0..2
          ConcatOps@0..2
            ProdExpr@0..2
              LBracket@0..1 "{"
              RBracket@1..2 "}"
    "#]];
    expect.assert_eq(&format!("{:#?}", parse_term("{}")));

    let expect = expect![[r#"
        Term@0..18
          ConcatOps@0..18
            ProdExpr@0..18
              LBracket@0..1 "{"
              FieldExpr@1..6
                Ident@1..3
                  Identifier@1..2 "x"
                  Whitespace@2..3 " "
                Equal@3..4 "="
                Whitespace@4..5 " "
                Term@5..6
                  ConcatOps@5..6
                    VarExpr@5..6
                      Ident@5..6
                        Identifier@5..6 "a"
              Comma@6..7 ","
              Whitespace@7..8 " "
              FieldExpr@8..17
                Ident@8..10
                  Identifier@8..9 "y"
                  Whitespace@9..10 " "
                Equal@10..11 "="
                Whitespace@11..12 " "
                Term@12..17
                  TermPrefix@12..16
                    ClosurePrefix@12..16
                      VerticalBar@12..13 "|"
                      Ident@13..14
                        Identifier@13..14 "t"
                      VerticalBar@14..15 "|"
                      Whitespace@15..16 " "
                  ConcatOps@16..17
                    VarExpr@16..17
                      Ident@16..17
                        Identifier@16..17 "t"
              RBracket@17..18 "}"
    "#]];
    expect.assert_eq(&format!("{:#?}", parse_term("{x = a, y = |t| t}")));
  }

  #[test]
  fn test_product_rows_precedence() {
    let expect = expect![[r#"
        Term@0..24
          ConcatOps@0..24
            ArgPostfix@0..24
              ProdExpr@0..11
                LBracket@0..1 "{"
                FieldExpr@1..10
                  Ident@1..3
                    Identifier@1..2 "x"
                    Whitespace@2..3 " "
                  Equal@3..4 "="
                  Whitespace@4..5 " "
                  Term@5..10
                    TermPrefix@5..9
                      ClosurePrefix@5..9
                        VerticalBar@5..6 "|"
                        Ident@6..7
                          Identifier@6..7 "t"
                        VerticalBar@7..8 "|"
                        Whitespace@8..9 " "
                    ConcatOps@9..10
                      VarExpr@9..10
                        Ident@9..10
                          Identifier@9..10 "t"
                RBracket@10..11 "}"
              LParen@11..12 "("
              Term@12..23
                ConcatOps@12..23
                  ProdExpr@12..23
                    LBracket@12..13 "{"
                    FieldExpr@13..22
                      Ident@13..15
                        Identifier@13..14 "y"
                        Whitespace@14..15 " "
                      Equal@15..16 "="
                      Whitespace@16..17 " "
                      Term@17..22
                        TermPrefix@17..21
                          ClosurePrefix@17..21
                            VerticalBar@17..18 "|"
                            Ident@18..19
                              Identifier@18..19 "t"
                            VerticalBar@19..20 "|"
                            Whitespace@20..21 " "
                        ConcatOps@21..22
                          VarExpr@21..22
                            Ident@21..22
                              Identifier@21..22 "u"
                    RBracket@22..23 "}"
              RParen@23..24 ")"
    "#]];
    expect.assert_eq(&format!("{:#?}", parse_term("{x = |t| t}({y = |t| u})")));
  }

  #[test]
  fn test_field_access() {
    let expect = expect![[r#"
        Term@0..16
          ConcatOps@0..16
            FieldPostfix@0..16
              ProdExpr@0..14
                LBracket@0..1 "{"
                FieldExpr@1..6
                  Ident@1..3
                    Identifier@1..2 "x"
                    Whitespace@2..3 " "
                  Equal@3..4 "="
                  Whitespace@4..5 " "
                  Term@5..6
                    ConcatOps@5..6
                      VarExpr@5..6
                        Ident@5..6
                          Identifier@5..6 "a"
                Comma@6..7 ","
                Whitespace@7..8 " "
                FieldExpr@8..13
                  Ident@8..10
                    Identifier@8..9 "y"
                    Whitespace@9..10 " "
                  Equal@10..11 "="
                  Whitespace@11..12 " "
                  Term@12..13
                    ConcatOps@12..13
                      VarExpr@12..13
                        Ident@12..13
                          Identifier@12..13 "b"
                RBracket@13..14 "}"
              Dot@14..15 "."
              Ident@15..16
                Identifier@15..16 "x"
    "#]];
    expect.assert_eq(&format!("{:#?}", parse_term("{x = a, y = b}.x")));
  }

  #[test]
  fn test_combined_postfixes() {
    let expect = expect![[r#"
        Term@0..6
          ConcatOps@0..6
            ArgPostfix@0..6
              FieldPostfix@0..3
                VarExpr@0..1
                  Ident@0..1
                    Identifier@0..1 "a"
                Dot@1..2 "."
                Ident@2..3
                  Identifier@2..3 "x"
              LParen@3..4 "("
              Term@4..5
                ConcatOps@4..5
                  VarExpr@4..5
                    Ident@4..5
                      Identifier@4..5 "b"
              RParen@5..6 ")"
    "#]];
    expect.assert_eq(&format!("{:#?}", parse_term("a.x(b)")));
  }

  #[test]
  fn test_concat_variables() {
    let expect = expect![[r#"
        Term@0..11
          ConcatOps@0..11
            VarExpr@0..2
              Ident@0..2
                Identifier@0..1 "x"
                Whitespace@1..2 " "
            Concat@2..4 ",,"
            Whitespace@4..5 " "
            VarExpr@5..7
              Ident@5..7
                Identifier@5..6 "y"
                Whitespace@6..7 " "
            Concat@7..9 ",,"
            Whitespace@9..10 " "
            VarExpr@10..11
              Ident@10..11
                Identifier@10..11 "z"
    "#]];
    expect.assert_eq(&format!("{:#?}", parse_term("x ,, y ,, z")));
  }

  #[test]
  fn test_sum_rows() {
    let expect = expect![[r#"
        Term@0..11
          ConcatOps@0..11
            SumExpr@0..11
              LAngle@0..1 "<"
              Ident@1..3
                Identifier@1..2 "x"
                Whitespace@2..3 " "
              Equal@3..4 "="
              Whitespace@4..5 " "
              Term@5..10
                TermPrefix@5..9
                  ClosurePrefix@5..9
                    VerticalBar@5..6 "|"
                    Ident@6..7
                      Identifier@6..7 "t"
                    VerticalBar@7..8 "|"
                    Whitespace@8..9 " "
                ConcatOps@9..10
                  VarExpr@9..10
                    Ident@9..10
                      Identifier@9..10 "t"
              RAngle@10..11 ">"
    "#]];
    expect.assert_eq(&format!("{:#?}", parse_term("<x = |t| t>")));
  }

  #[test]
  fn test_matches() {
    let expect = expect![[r#"
        Term@0..44
          ConcatOps@0..44
            MatchExpr@0..44
              KwMatch@0..5 "match"
              Whitespace@5..6 " "
              LAngle@6..7 "<"
              Whitespace@7..8 " "
              MatchArm@8..20
                ProdPattern@8..16
                  LBracket@8..9 "{"
                  FieldPattern@9..14
                    Ident@9..11
                      Identifier@9..10 "x"
                      Whitespace@10..11 " "
                    Equal@11..12 "="
                    Whitespace@12..13 " "
                    WholePattern@13..14
                      Ident@13..14
                        Identifier@13..14 "a"
                  RBracket@14..15 "}"
                  Whitespace@15..16 " "
                BigArrow@16..18 "=>"
                Whitespace@18..19 " "
                Term@19..20
                  ConcatOps@19..20
                    VarExpr@19..20
                      Ident@19..20
                        Identifier@19..20 "a"
              Comma@20..21 ","
              Whitespace@21..22 " "
              MatchArm@22..34
                SumPattern@22..30
                  LAngle@22..23 "<"
                  FieldPattern@23..28
                    Ident@23..25
                      Identifier@23..24 "y"
                      Whitespace@24..25 " "
                    Equal@25..26 "="
                    Whitespace@26..27 " "
                    WholePattern@27..28
                      Ident@27..28
                        Identifier@27..28 "b"
                  RAngle@28..29 ">"
                  Whitespace@29..30 " "
                BigArrow@30..32 "=>"
                Whitespace@32..33 " "
                Term@33..34
                  ConcatOps@33..34
                    VarExpr@33..34
                      Ident@33..34
                        Identifier@33..34 "b"
              Comma@34..35 ","
              Whitespace@35..36 " "
              MatchArm@36..43
                WholePattern@36..38
                  Ident@36..38
                    Identifier@36..37 "c"
                    Whitespace@37..38 " "
                BigArrow@38..40 "=>"
                Whitespace@40..41 " "
                Term@41..43
                  ConcatOps@41..43
                    VarExpr@41..43
                      Ident@41..43
                        Identifier@41..42 "c"
                        Whitespace@42..43 " "
              RAngle@43..44 ">"
    "#]];
    expect.assert_eq(&format!(
      "{:#?}",
      parse_term("match < {x = a} => a, <y = b> => b, c => c >")
    ));
  }

  #[test]
  fn test_effect_items() {
    let module = parse_item("effect foo { foo: a -> a }");
    let expect = expect![[r#"
        Items@0..26
          EffectDefn@0..26
            KwEffect@0..6 "effect"
            Whitespace@6..7 " "
            Ident@7..11
              Identifier@7..10 "foo"
              Whitespace@10..11 " "
            LBracket@11..12 "{"
            Whitespace@12..13 " "
            EffectOps@13..25
              EffectOp@13..25
                Ident@13..16
                  Identifier@13..16 "foo"
                SchemeAnnotation@16..25
                  Colon@16..17 ":"
                  Whitespace@17..18 " "
                  TypeScheme@18..25
                    ForallBinders@18..18
                    FunctionType@18..25
                      NameType@18..20
                        Ident@18..20
                          Identifier@18..19 "a"
                          Whitespace@19..20 " "
                      SmallArrow@20..22 "->"
                      Whitespace@22..23 " "
                      NameType@23..25
                        Ident@23..25
                          Identifier@23..24 "a"
                          Whitespace@24..25 " "
            RBracket@25..26 "}"
    "#]];
    expect.assert_eq(&format!("{:#?}", module));
  }

  #[test]
  fn test_term_items() {
    let module = parse_item(
      r#"
  defn x = a
  defn y = |b| b 
z = t = x; t
  "#,
    );
    let expect = expect![[r#"
        Items@0..47
          Whitespace@0..3 "\n  "
          TermDefn@3..16
            KwDefn@3..7 "defn"
            Whitespace@7..8 " "
            Ident@8..10
              Identifier@8..9 "x"
              Whitespace@9..10 " "
            Equal@10..11 "="
            Whitespace@11..12 " "
            Term@12..16
              ConcatOps@12..16
                VarExpr@12..16
                  Ident@12..16
                    Identifier@12..13 "a"
                    Whitespace@13..16 "\n  "
          TermDefn@16..32
            KwDefn@16..20 "defn"
            Whitespace@20..21 " "
            Ident@21..23
              Identifier@21..22 "y"
              Whitespace@22..23 " "
            Equal@23..24 "="
            Whitespace@24..25 " "
            Term@25..32
              TermPrefix@25..29
                ClosurePrefix@25..29
                  VerticalBar@25..26 "|"
                  Ident@26..27
                    Identifier@26..27 "b"
                  VerticalBar@27..28 "|"
                  Whitespace@28..29 " "
              ConcatOps@29..32
                VarExpr@29..32
                  Ident@29..32
                    Identifier@29..30 "b"
                    Whitespace@30..32 " \n"
          Error@32..47
            Identifier@32..33 "z"
            Whitespace@33..34 " "
            Equal@34..35 "="
            Whitespace@35..36 " "
            Identifier@36..37 "t"
            Whitespace@37..38 " "
            Equal@38..39 "="
            Whitespace@39..40 " "
            Identifier@40..41 "x"
            Semicolon@41..42 ";"
            Whitespace@42..43 " "
            Identifier@43..44 "t"
            Whitespace@44..47 "\n  "
    "#]];
    expect.assert_eq(&format!("{:#?}", module));
  }

  #[test]
  fn test_parse_wand() {
    let module = parse_item(
      r#"defn f = |m||n| (m ,, n).x

  defn g = f({ x = {} })({ y = {} })
  "#,
    );

    let expect = expect![[r#"
        Items@0..67
          TermDefn@0..30
            KwDefn@0..4 "defn"
            Whitespace@4..5 " "
            Ident@5..7
              Identifier@5..6 "f"
              Whitespace@6..7 " "
            Equal@7..8 "="
            Whitespace@8..9 " "
            Term@9..30
              TermPrefix@9..16
                ClosurePrefix@9..12
                  VerticalBar@9..10 "|"
                  Ident@10..11
                    Identifier@10..11 "m"
                  VerticalBar@11..12 "|"
                ClosurePrefix@12..16
                  VerticalBar@12..13 "|"
                  Ident@13..14
                    Identifier@13..14 "n"
                  VerticalBar@14..15 "|"
                  Whitespace@15..16 " "
              ConcatOps@16..30
                FieldPostfix@16..30
                  ParenthesizedExpr@16..24
                    LParen@16..17 "("
                    Term@17..23
                      ConcatOps@17..23
                        VarExpr@17..19
                          Ident@17..19
                            Identifier@17..18 "m"
                            Whitespace@18..19 " "
                        Concat@19..21 ",,"
                        Whitespace@21..22 " "
                        VarExpr@22..23
                          Ident@22..23
                            Identifier@22..23 "n"
                    RParen@23..24 ")"
                  Dot@24..25 "."
                  Ident@25..30
                    Identifier@25..26 "x"
                    Whitespace@26..30 "\n\n  "
          TermDefn@30..64
            KwDefn@30..34 "defn"
            Whitespace@34..35 " "
            Ident@35..37
              Identifier@35..36 "g"
              Whitespace@36..37 " "
            Equal@37..38 "="
            Whitespace@38..39 " "
            Term@39..64
              ConcatOps@39..64
                ArgPostfix@39..64
                  ArgPostfix@39..52
                    VarExpr@39..40
                      Ident@39..40
                        Identifier@39..40 "f"
                    LParen@40..41 "("
                    Term@41..51
                      ConcatOps@41..51
                        ProdExpr@41..51
                          LBracket@41..42 "{"
                          Whitespace@42..43 " "
                          FieldExpr@43..50
                            Ident@43..45
                              Identifier@43..44 "x"
                              Whitespace@44..45 " "
                            Equal@45..46 "="
                            Whitespace@46..47 " "
                            Term@47..50
                              ConcatOps@47..50
                                ProdExpr@47..50
                                  LBracket@47..48 "{"
                                  RBracket@48..49 "}"
                                  Whitespace@49..50 " "
                          RBracket@50..51 "}"
                    RParen@51..52 ")"
                  LParen@52..53 "("
                  Term@53..63
                    ConcatOps@53..63
                      ProdExpr@53..63
                        LBracket@53..54 "{"
                        Whitespace@54..55 " "
                        FieldExpr@55..62
                          Ident@55..57
                            Identifier@55..56 "y"
                            Whitespace@56..57 " "
                          Equal@57..58 "="
                          Whitespace@58..59 " "
                          Term@59..62
                            ConcatOps@59..62
                              ProdExpr@59..62
                                LBracket@59..60 "{"
                                RBracket@60..61 "}"
                                Whitespace@61..62 " "
                        RBracket@62..63 "}"
                  RParen@63..64 ")"
          Whitespace@64..67 "\n  "
    "#]];
    expect.assert_eq(&format!("{:#?}", module));
  }

  #[test]
  fn test_annotated_term_items() {
    let module = parse_item(
      r#"
  defn x: a = a
  defn y: forall b. b -> b = |b| b
  "#,
    );
    let expect = expect![[r#"
        Items@0..54
          Whitespace@0..3 "\n  "
          TermDefn@3..19
            KwDefn@3..7 "defn"
            Whitespace@7..8 " "
            Ident@8..9
              Identifier@8..9 "x"
            SchemeAnnotation@9..13
              Colon@9..10 ":"
              Whitespace@10..11 " "
              TypeScheme@11..13
                ForallBinders@11..11
                NameType@11..13
                  Ident@11..13
                    Identifier@11..12 "a"
                    Whitespace@12..13 " "
            Equal@13..14 "="
            Whitespace@14..15 " "
            Term@15..19
              ConcatOps@15..19
                VarExpr@15..19
                  Ident@15..19
                    Identifier@15..16 "a"
                    Whitespace@16..19 "\n  "
          TermDefn@19..54
            KwDefn@19..23 "defn"
            Whitespace@23..24 " "
            Ident@24..25
              Identifier@24..25 "y"
            SchemeAnnotation@25..44
              Colon@25..26 ":"
              Whitespace@26..27 " "
              TypeScheme@27..44
                ForallBinders@27..37
                  ForallBinder@27..37
                    KwForall@27..33 "forall"
                    Whitespace@33..34 " "
                    Ident@34..35
                      Identifier@34..35 "b"
                    Dot@35..36 "."
                    Whitespace@36..37 " "
                FunctionType@37..44
                  NameType@37..39
                    Ident@37..39
                      Identifier@37..38 "b"
                      Whitespace@38..39 " "
                  SmallArrow@39..41 "->"
                  Whitespace@41..42 " "
                  NameType@42..44
                    Ident@42..44
                      Identifier@42..43 "b"
                      Whitespace@43..44 " "
            Equal@44..45 "="
            Whitespace@45..46 " "
            Term@46..54
              TermPrefix@46..50
                ClosurePrefix@46..50
                  VerticalBar@46..47 "|"
                  Ident@47..48
                    Identifier@47..48 "b"
                  VerticalBar@48..49 "|"
                  Whitespace@49..50 " "
              ConcatOps@50..54
                VarExpr@50..54
                  Ident@50..54
                    Identifier@50..51 "b"
                    Whitespace@51..54 "\n  "
    "#]];
    expect.assert_eq(&format!("{:#?}", module));
  }
}
