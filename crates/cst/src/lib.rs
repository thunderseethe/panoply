use std::fmt::Debug;
use std::iter::FusedIterator;
use std::ops::Index;

use la_arena::{Arena, Idx};

use base::{
  ident::Ident,
  indexed::{HasArenaMut, HasArenaRef, IdxAlloc, IndexedAllocate, ReferenceAllocate},
  span::{Span, SpanOf, Spanned},
};

//pub mod nameres;

mod old {
  use super::*;

  #[derive(Clone, Debug, Default, Eq, PartialEq)]
  pub struct CstIndxAlloc {
    types: Arena<Type<Ident>>,
    terms: Arena<Term>,
    pats: Arena<Pattern>,
  }
  impl HasArenaRef<Pattern> for CstIndxAlloc {
    fn arena(&self) -> &Arena<Pattern> {
      &self.pats
    }
  }
  impl HasArenaMut<Pattern> for CstIndxAlloc {
    fn arena_mut(&mut self) -> &mut Arena<Pattern> {
      &mut self.pats
    }
  }
  impl HasArenaRef<Type<Ident>> for CstIndxAlloc {
    fn arena(&self) -> &Arena<Type<Ident>> {
      &self.types
    }
  }
  impl HasArenaMut<Type<Ident>> for CstIndxAlloc {
    fn arena_mut(&mut self) -> &mut Arena<Type<Ident>> {
      &mut self.types
    }
  }
  impl HasArenaRef<Term> for CstIndxAlloc {
    fn arena(&self) -> &Arena<Term> {
      &self.terms
    }
  }
  impl HasArenaMut<Term> for CstIndxAlloc {
    fn arena_mut(&mut self) -> &mut Arena<Term> {
      &mut self.terms
    }
  }
  impl<T> Index<Idx<T>> for CstIndxAlloc
  where
    Self: HasArenaRef<T>,
  {
    type Output = T;

    fn index(&self, index: Idx<T>) -> &Self::Output {
      &self.arena()[index]
    }
  }
  impl<T> IdxAlloc<T> for CstIndxAlloc
  where
    Self: HasArenaMut<T>,
  {
    fn alloc(&mut self, value: T) -> Idx<T> {
      self.arena_mut().alloc(value)
    }
  }

  /// A typing annotation for a variable.
  #[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
  pub struct Annotation<T> {
    pub colon: Span,
    pub type_: T,
  }
  impl<'a, A, T> ReferenceAllocate<'a, A> for Annotation<T>
  where
    T: ReferenceAllocate<'a, A>,
  {
    type Out = Annotation<T::Out>;

    fn ref_alloc(&self, alloc: &mut A) -> Self::Out {
      Annotation {
        colon: self.colon,
        type_: self.type_.ref_alloc(alloc),
      }
    }
  }
  /// A monotype annotation.
  pub type TypeAnnotation<V> = Annotation<Idx<Type<V>>>;

  /// A scheme annotation.
  pub type SchemeAnnotation<V> = Annotation<Scheme<V>>;

  /// A non-empty list of elements, separated by some fixed separator. To allow an empty list, wrap in
  /// `Option`.
  #[derive(Clone, Debug, Eq, PartialEq, Hash)]
  pub struct Separated<T> {
    pub first: T,
    pub elems: Vec<(Span, T)>,
    pub comma: Option<Span>,
  }
  impl<T> Separated<T> {
    pub fn span_with(&self, f: impl Fn(&T) -> Span) -> Span {
      Span::join(
        &f(&self.first),
        &self
          .comma
          .or_else(|| self.elems.last().map(|e| e.0))
          .unwrap_or_else(|| f(&self.first)),
      )
    }
  }
  impl<T: Spanned> Spanned for Separated<T> {
    fn span(&self) -> Span {
      Span::join(
        &self.first,
        &self
          .comma
          .or_else(|| self.elems.last().map(|e| e.0))
          .unwrap_or_else(|| self.first.span()),
      )
    }
  }
  impl<T> Separated<T> {
    /// An iterator over the non-separator elements.
    pub fn elements(&self) -> Elements<'_, T> {
      self.into_iter()
    }
  }

  type ElementTail<'a, T> =
    std::iter::Map<std::slice::Iter<'a, (Span, T)>, fn(&'a (Span, T)) -> &'a T>;

  /// An iterator over the elements of a `Separated`. Needed because `std::iter::Chain` does not
  /// implement `ExactSizeIterator`.
  #[derive(Debug)]
  pub struct Elements<'a, T> {
    head: Option<&'a T>,
    tail: ElementTail<'a, T>,
  }
  impl<'a, T> IntoIterator for &'a Separated<T> {
    type Item = &'a T;

    type IntoIter = Elements<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
      Elements {
        head: Some(&self.first),
        tail: self.elems.iter().map(|(_, t)| t),
      }
    }
  }

  impl<T> Clone for Elements<'_, T> {
    fn clone(&self) -> Self {
      Elements {
        head: self.head,
        tail: self.tail.clone(),
      }
    }
  }
  impl<T> ExactSizeIterator for Elements<'_, T> {}
  impl<T> FusedIterator for Elements<'_, T> {}
  impl<'a, T> Iterator for Elements<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
      self.head.take().or_else(|| self.tail.next())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
      let (lower, upper) = self.tail.size_hint();
      (lower + 1, upper.map(|u| u + 1))
    }
  }

  pub type IdField<T> = Field<SpanOf<Ident>, T>;

  /// A product row with values in `T`.
  #[derive(Clone, Debug, Eq, PartialEq)]
  pub struct ProductRow<T> {
    pub lbrace: Span,
    pub fields: Option<Separated<IdField<T>>>,
    pub rbrace: Span,
  }
  impl<T> Spanned for ProductRow<T> {
    fn span(&self) -> Span {
      Span::join(&self.lbrace, &self.rbrace)
    }
  }
  impl<'a, T> IntoIterator for &'a ProductRow<T> {
    type Item = &'a IdField<T>;

    type IntoIter = std::iter::FlatMap<
      std::option::Iter<'a, Separated<IdField<T>>>,
      Elements<'a, IdField<T>>,
      fn(&'a Separated<IdField<T>>) -> Elements<'a, IdField<T>>,
    >;

    fn into_iter(self) -> Self::IntoIter {
      self.fields.iter().flat_map(IntoIterator::into_iter)
    }
  }

  /// A sum row with value in `T`.
  #[derive(Clone, Copy, Debug, Eq, PartialEq)]
  pub struct SumRow<T> {
    pub langle: Span,
    pub field: IdField<T>,
    pub rangle: Span,
  }
  impl<T> Spanned for SumRow<T> {
    fn span(&self) -> Span {
      Span::join(&self.langle, &self.rangle)
    }
  }

  /// A non-empty row with concrete fields in `C` and variables in `V`.
  #[derive(Clone, Debug, Eq, PartialEq)]
  pub enum Row<V, C> {
    Concrete(Separated<C>),
    Variable(Separated<SpanOf<V>>),
    Mixed {
      concrete: Separated<C>,
      vbar: Span,
      variables: Separated<SpanOf<V>>,
    },
  }

  impl<V, C> Spanned for Row<V, C>
  where
    C: Spanned,
  {
    fn span(&self) -> Span {
      match self {
        Row::Concrete(closed) => closed.span(),
        Row::Variable(vars) => vars.span(),
        Row::Mixed {
          concrete,
          variables,
          ..
        } => Span::join(&concrete.span(), &variables.span()),
      }
    }
  }

  impl<V> Row<V, IdField<Idx<Type<V>>>> {
    pub fn spanned<A>(&self, arenas: &A) -> Span
    where
      A: HasArenaRef<Type<V>>,
    {
      match self {
        Row::Concrete(closed) => closed.span_with(|field| {
          field
            .label
            .span()
            .join_spans(&arenas.arena()[field.target].spanned(arenas))
        }),
        Row::Variable(vars) => vars.span(),
        Row::Mixed {
          concrete,
          variables,
          ..
        } => concrete
          .span_with(|field| {
            field
              .label
              .span()
              .join_spans(&arenas.arena()[field.target].spanned(arenas))
          })
          .join_spans(&variables.span()),
      }
    }
  }

  /// A row of types.
  pub type TypeRow<V> = Row<V, IdField<Idx<Type<V>>>>;

  /// An unqualified syntactic type.
  #[derive(Clone, Debug, Eq, PartialEq)]
  pub enum Type<V> {
    Int(Span),
    Named(SpanOf<V>),
    Sum {
      langle: Span,
      variants: TypeRow<V>,
      rangle: Span,
    },
    Product {
      lbrace: Span,
      fields: Option<TypeRow<V>>,
      rbrace: Span,
    },
    Function {
      domain: Idx<Self>,
      arrow: Span,
      codomain: Idx<Self>,
    },
    Parenthesized {
      lpar: Span,
      type_: Idx<Self>,
      rpar: Span,
    },
  }

  impl<V> Type<V> {
    pub fn spanned<A>(&self, arenas: &A) -> Span
    where
      A: HasArenaRef<Self>,
    {
      match self {
        Type::Int(span) => *span,
        Type::Named(var) => var.span(),
        Type::Sum { langle, rangle, .. } => Span::join(langle, rangle),
        Type::Product { lbrace, rbrace, .. } => Span::join(lbrace, rbrace),
        Type::Function {
          domain, codomain, ..
        } => arenas.arena()[*domain]
          .spanned(arenas)
          .join_spans(&arenas.arena()[*codomain].spanned(arenas)),
        Type::Parenthesized { lpar, rpar, .. } => Span::join(lpar, rpar),
      }
    }
  }

  /// An atomic row for use in a type constraint.
  #[derive(Clone, Debug, Eq, PartialEq, Hash)]
  pub enum RowAtom<V> {
    Concrete {
      lpar: Span,
      fields: Separated<IdField<Idx<Type<V>>>>,
      rpar: Span,
    },
    Variable(SpanOf<V>),
  }

  /// A type constraint.
  #[derive(Clone, Debug, Eq, PartialEq, Hash)]
  pub enum Constraint<V> {
    RowSum {
      lhs: RowAtom<V>,
      plus: Span,
      rhs: RowAtom<V>,
      eq: Span,
      goal: RowAtom<V>,
    },
  }

  /// A qualifiers for a type.
  #[derive(Clone, Debug, Eq, PartialEq, Hash)]
  pub struct Qualifiers<V> {
    pub constraints: Separated<Constraint<V>>,
    pub arrow: Span,
  }

  /// A polymorphic type.
  #[derive(Clone, Debug, Eq, PartialEq, Hash)]
  pub struct Scheme<V> {
    pub quantifiers: Vec<Quantifier<V>>,
    pub qualifiers: Option<Qualifiers<V>>,
    pub type_: Idx<Type<V>>,
  }

  /// An effect operation.
  #[derive(Clone, Debug, Eq, PartialEq, Hash)]
  pub struct EffectOp<O, V> {
    pub name: SpanOf<O>,
    pub colon: Span,
    pub type_: Idx<Type<V>>,
  }

  /// A pattern.
  #[derive(Clone, Debug, Eq, PartialEq)]
  pub enum Pattern {
    ProductRow(ProductRow<Idx<Self>>),
    SumRow(SumRow<Idx<Self>>),
    Whole(SpanOf<Ident>),
  }
  impl Spanned for Pattern {
    fn span(&self) -> Span {
      match self {
        Pattern::ProductRow(prod) => prod.lbrace.join_spans(&prod.rbrace),
        Pattern::SumRow(sum) => sum.langle.join_spans(&sum.rangle),
        Pattern::Whole(var) => var.span(),
      }
    }
  }

  /// A term.
  #[derive(Clone, Debug, Eq, PartialEq)]
  pub enum Term {
    Binding {
      var: SpanOf<Ident>,
      annotation: Option<TypeAnnotation<Ident>>,
      eq: Span,
      value: Idx<Self>,
      semi: Span,
      expr: Idx<Self>,
    },
    Handle {
      with: Span,
      handler: Idx<Self>,
      do_: Span,
      expr: Idx<Self>,
    },
    Abstraction {
      lbar: Span,
      arg: SpanOf<Ident>,
      annotation: Option<TypeAnnotation<Ident>>,
      rbar: Span,
      body: Idx<Self>,
    },
    Application {
      func: Idx<Self>,
      lpar: Span,
      arg: Idx<Self>,
      rpar: Span,
    },
    ProductRow(ProductRow<Idx<Self>>),
    // TODO: Roll this into product row once we figure out better syntax.
    // Consider copying over what we do for prod types where we allow `{ x, y, z }` syntax.
    Concat {
      left: Idx<Self>,
      concat: Span,
      right: Idx<Self>,
    },
    SumRow(SumRow<Idx<Self>>),
    DotAccess {
      base: Idx<Self>,
      dot: Span,
      field: SpanOf<Ident>,
    },
    Match {
      match_: Span,
      langle: Span,
      cases: Separated<Field<Idx<Pattern>, Idx<Self>>>,
      rangle: Span,
    },
    SymbolRef(SpanOf<Ident>),
    Parenthesized {
      lpar: Span,
      term: Idx<Self>,
      rpar: Span,
    },
    Int(SpanOf<usize>),
  }
  impl Term {
    pub fn spanned<'a>(&'a self, arenas: &'a CstIndxAlloc) -> SpanTerm<'a> {
      SpanTerm { term: self, arenas }
    }
  }
  pub struct SpanTerm<'a> {
    term: &'a Term,
    arenas: &'a CstIndxAlloc,
  }
  impl SpanTerm<'_> {
    fn with_term(&self, term: Idx<Term>) -> Self {
      Self {
        term: &self.arenas[term],
        arenas: self.arenas,
      }
    }
  }
  impl Spanned for SpanTerm<'_> {
    fn span(&self) -> Span {
      match self.term {
        Term::Binding { var, expr, .. } => Span::join(var, &self.with_term(*expr)),
        Term::Handle { with, expr, .. } => Span::join(with, &self.with_term(*expr)),
        Term::Abstraction { lbar, body, .. } => Span::join(lbar, &self.with_term(*body)),
        Term::Application { func, rpar, .. } => Span::join(&self.with_term(*func), rpar),
        Term::ProductRow(p) => p.span(),
        Term::Concat { left, right, .. } => {
          Span::join(&self.with_term(*left), &self.with_term(*right))
        }
        Term::SumRow(s) => s.span(),
        Term::DotAccess { base, field, .. } => Span::join(&self.with_term(*base), field),
        Term::Match { match_, rangle, .. } => Span::join(match_, rangle),
        Term::SymbolRef(v) => v.span(),
        Term::Parenthesized { lpar, rpar, .. } => Span::join(lpar, rpar),
        Term::Int(span_of_int) => span_of_int.span(),
      }
    }
  }

  /// An effect definition in the CST
  #[derive(Clone, Debug, Eq, PartialEq)]
  pub struct EffectDefn {
    pub effect: Span,
    pub name: SpanOf<Ident>,
    pub lbrace: Span,
    pub ops: Vec<EffectOp<Ident, Ident>>,
    pub rbrace: Span,
  }
  impl Spanned for EffectDefn {
    fn span(&self) -> Span {
      self.effect.join_spans(&self.rbrace)
    }
  }

  #[derive(Clone, Debug, Eq, PartialEq)]
  pub struct TermDefn {
    pub name: SpanOf<Ident>,
    pub annotation: Option<SchemeAnnotation<Ident>>,
    pub eq: Span,
    pub value: Idx<Term>,
  }

  /// A top-level item in an source file.
  #[derive(Clone, Debug, Eq, PartialEq)]
  // Clippy thinks our second largest variant is 0 bytes which is clearly wrong.
  #[allow(clippy::large_enum_variant)]
  pub enum Item {
    Effect(EffectDefn),
    Term(TermDefn),
  }

  impl Item {
    pub fn span(&self, arenas: &CstIndxAlloc) -> Span {
      match self {
        Item::Effect(eff) => Span::join(&eff.effect, &eff.rbrace),
        Item::Term(term) => Span::join(&term.name, &arenas[term.value].spanned(arenas)),
      }
    }
  }

  /// A parsed module.
  #[derive(Clone, Debug, Default, Eq, PartialEq)]
  pub struct CstModule {
    pub indices: CstIndxAlloc,
    pub items: Vec<Item>,
  }

  impl<A, L: IndexedAllocate<A>, T: IndexedAllocate<A>> IndexedAllocate<A> for Field<L, T> {
    type Out = Field<L::Out, T::Out>;

    fn alloc(&self, alloc: &mut A) -> Self::Out {
      Field {
        label: self.label.alloc(alloc),
        sep: self.sep,
        target: self.target.alloc(alloc),
      }
    }
  }

  impl<A, T: IndexedAllocate<A>> IndexedAllocate<A> for Annotation<T> {
    type Out = Annotation<T::Out>;

    fn alloc(&self, alloc: &mut A) -> Self::Out {
      Annotation {
        colon: self.colon,
        type_: self.type_.alloc(alloc),
      }
    }
  }

  /// A quantifier for a polytype.
  #[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
  pub struct Quantifier<V> {
    pub forall: Span,
    pub var: SpanOf<V>,
    pub dot: Span,
  }

  impl<V> Spanned for Quantifier<V> {
    fn span(&self) -> Span {
      Span::join(&self.forall, &self.dot)
    }
  }
  impl<'a, A, V: 'a + ReferenceAllocate<'a, A>> ReferenceAllocate<'a, A> for Quantifier<V>
  where
    V: ReferenceAllocate<'a, A>,
  {
    type Out = Quantifier<V::Out>;

    fn ref_alloc(&self, alloc: &mut A) -> Self::Out {
      Quantifier {
        forall: self.forall,
        var: self.var.ref_alloc(alloc),
        dot: self.dot,
      }
    }
  }

  /// A field with a label in `L`, separator, and target in `T`.
  #[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
  pub struct Field<L, T> {
    pub label: L,
    pub sep: Span,
    pub target: T,
  }
  impl<'a, A, L: ReferenceAllocate<'a, A>, T: ReferenceAllocate<'a, A>> ReferenceAllocate<'a, A>
    for Field<L, T>
  {
    type Out = Field<L::Out, T::Out>;

    fn ref_alloc(&self, alloc: &mut A) -> Self::Out {
      Field {
        label: self.label.ref_alloc(alloc),
        sep: self.sep,
        target: self.target.ref_alloc(alloc),
      }
    }
  }

  impl<L: Spanned, T: Spanned> Spanned for Field<L, T> {
    fn span(&self) -> Span {
      Span::join(&self.label, &self.target)
    }
  }
}

use logos::Logos;
pub use rowan::{ast::AstNode, GreenNode, SyntaxNode};
use rowan::{
  ast::{
    support::{child, children},
    AstChildren, AstPtr,
  },
  Language,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Panoply;
impl Language for Panoply {
  type Kind = Syntax;

  fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
    unsafe { std::mem::transmute(raw.0) }
  }

  fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
    rowan::SyntaxKind(kind.raw())
  }
}

/// A token
#[derive(Logos, Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[repr(u16)]
#[logos()]
pub enum Syntax {
  #[token("forall")]
  KwForall = 0,
  #[token("effect")]
  KwEffect,
  #[token("defn")]
  KwDefn,
  #[token("match")]
  KwMatch,
  #[token("with")]
  KwWith,
  #[token("do")]
  KwDo,
  #[token("let")]
  KwLet,
  #[regex("\\d+")]
  Int,
  #[regex("[\\p{alpha}_]\\w*")]
  Identifier,
  #[token("+")]
  Plus,
  #[token("=")]
  Equal,
  #[token("|")]
  VerticalBar,
  #[token("->")]
  SmallArrow,
  #[token("=>")]
  BigArrow,
  #[token("(")]
  LParen,
  #[token(")")]
  RParen,
  #[token("{")]
  LBracket,
  #[token("}")]
  RBracket,
  #[token("[")]
  LBrace,
  #[token("]")]
  RBrace,
  #[token("<")]
  LAngle,
  #[token(">")]
  RAngle,
  #[token(":")]
  Colon,
  #[token(";")]
  Semicolon,
  #[token(",")]
  Comma,
  #[token(",,")]
  Concat,
  #[token(".")]
  Dot,
  #[regex("\\s+")]
  Whitespace,
  #[end]
  Eof,
  Error,
  // Node wrapper for Identifier.
  // Tree is easier to navigate if we wrap Identifier up in a Node rather than leave it as a token.
  Ident,
  // Row Types
  RowVariable,
  RowField,
  VariableRow,
  ConcreteRow,
  MixedRow,

  // Types
  NameType,
  SumType,
  ProductType,
  ParenType,
  FunctionType,
  Type,

  // Row constraints
  ConcreteRowAtom,
  VariableRowAtom,
  RowSumConstraint,
  Constraints,

  // Type Scheme
  ForallBinder,
  ForallBinders,
  TypeScheme,

  // Patterns
  ProdPattern,
  SumPattern,
  WholePattern,
  FieldPattern,

  VarExpr,
  IntExpr,
  ParenthesizedExpr,
  ProdExpr,
  SumExpr,
  FieldExpr,

  MatchArm,
  MatchExpr,
  TypeAnnotation,
  SchemeAnnotation,

  WithPrefix,
  ClosurePrefix,
  LetPrefix,

  ArgPostfix,
  FieldPostfix,

  TermPrefix,
  ConcatOps,
  Term,

  EffectOp,
  EffectOps,

  TermDefn,
  EffectDefn,
  Items,
}
impl Syntax {
  pub fn name(&self) -> String {
    format!("{:?}", self)
  }

  pub fn raw(&self) -> u16 {
    let kind: rowan::SyntaxKind = (*self).into();
    kind.0
  }
}

impl From<Syntax> for rowan::SyntaxKind {
  fn from(kind: Syntax) -> Self {
    Self(kind as u16)
  }
}

macro_rules! ast_node {
  (enum $name:ident {
      $first_case:tt($first_ty:ty),
      $($case:tt($ty:ty),)*
  }) => {
    #[derive(Clone, Debug)]
    pub enum $name {
      $first_case($first_ty),
      $($case($ty)),*
    }
    impl HasAstPtr for $name {}
    impl AstNode for $name {
      type Language = Panoply;

      fn can_cast(kind: <Self::Language as Language>::Kind) -> bool
      where
        Self: Sized,
      {
        <$first_ty>::can_cast(kind) $(|| <$ty>::can_cast(kind))*
      }

      fn cast(node: SyntaxNode<Self::Language>) -> Option<Self>
      where
        Self: Sized,
      {
        if let Some(this) = <$first_ty>::cast(node.clone()) {
          return Some(Self::$first_case(this));
        }
        $(if let Some(this) = <$ty>::cast(node.clone()) {
          return Some(Self::$case(this));
        })*
        None
      }

      fn syntax(&self) -> &SyntaxNode<Self::Language> {
        match self {
          Self::$first_case(this) => this.syntax(),
          $(Self::$case(this) => this.syntax(),)*
        }
      }
    }
  };
  (struct $name:ident($kind:expr)) => { ast_node!{struct $name($kind) {}} };
  (struct $name:ident($kind:expr) { $($field_name:ident: $field_ty:ty),* }) => {
    #[derive(Clone, Debug)]
    pub struct $name(SyntaxNode<Panoply>);
    impl HasAstPtr for $name {}
    impl AstNode for $name {
      type Language = Panoply;

      fn can_cast(kind: <Self::Language as Language>::Kind) -> bool
      where
        Self: Sized,
      {
        kind == $kind
      }

      fn cast(node: SyntaxNode<Self::Language>) -> Option<Self>
      where
        Self: Sized,
      {
        if Self::can_cast(node.kind()) {
          Some(Self(node))
        } else {
          None
        }
      }

      fn syntax(&self) -> &SyntaxNode<Self::Language> {
        &self.0
      }
    }
    impl $name {
      $(pub fn $field_name(&self) -> Option<$field_ty> {
        child(self.syntax())
      })*
    }
  };
}

ast_node!(struct Name(Syntax::Ident));
impl Name {
  pub fn text(&self) -> String {
    let node = self
      .syntax()
      .first_token()
      .expect("TODO: Handle this error correctly");
    node.text().to_string()
  }
}

pub trait HasName: AstNode<Language = Panoply> {
  fn name(&self) -> Option<Name> {
    child(self.syntax())
  }
}

pub trait HasAstPtr: AstNode<Language = Panoply> + Sized {
  fn ast_ptr(&self) -> AstPtr<Self> {
    AstPtr::new(self)
  }
}

ast_node!(struct TermDefn(Syntax::TermDefn) {
  term: Term,
  annotation: SchemeAnnotation
});
impl HasName for TermDefn {}

ast_node!(struct SchemeAnnotation(Syntax::SchemeAnnotation) {
  scheme: TypeScheme
});

ast_node!(struct Term(Syntax::Term) {
  body: ConcatOps
});
impl Term {
  pub fn prefixes(&self) -> Option<AstChildren<TermPrefix>> {
    self
      .syntax()
      .first_child_by_kind(&|kind| kind == Syntax::TermPrefix)
      .map(|prefixes| children(&prefixes))
  }
}

ast_node!(struct ConcatOps(Syntax::ConcatOps));
impl ConcatOps {
  pub fn ops(&self) -> AstChildren<TermPostfix> {
    children(self.syntax())
  }
}

ast_node!(
  enum TermPostfix {
    Arg(ArgPostfix),
    Field(FieldPostfix),
    Atom(TermAtom),
  }
);

ast_node!(struct ArgPostfix(Syntax::ArgPostfix) {
  base: TermPostfix,
  arg: Term
});
ast_node!(struct FieldPostfix(Syntax::FieldPostfix) {
  base: TermPostfix
});
impl HasName for FieldPostfix {}

ast_node!(
  enum TermAtom {
    Var(VarExpr),
    Int(IntExpr),
    Paren(ParenthesizedExpr),
    Prod(ProdExpr),
    Sum(SumExpr),
    Match(MatchExpr),
  }
);

ast_node!(struct VarExpr(Syntax::VarExpr));
impl HasName for VarExpr {}

ast_node!(struct IntExpr(Syntax::IntExpr));
impl IntExpr {
  pub fn int(&self) -> Option<usize> {
    let token = self.syntax().first_token()?;
    let Syntax::Int = token.kind() else {
      return None;
    };
    token.text().parse().ok()
  }
}

ast_node!(struct ParenthesizedExpr(Syntax::ParenthesizedExpr) {
  term: Term
});
ast_node!(struct ProdExpr(Syntax::ProdExpr));
impl ProdExpr {
  pub fn fields(&self) -> AstChildren<FieldExpr> {
    children(self.syntax())
  }
}
ast_node!(struct SumExpr(Syntax::SumExpr) {
  term: Term
});
impl HasName for SumExpr {}

ast_node!(struct MatchExpr(Syntax::MatchExpr));
impl MatchExpr {
  pub fn arms(&self) -> AstChildren<MatchArm> {
    children(self.syntax())
  }
}

ast_node!(struct MatchArm(Syntax::MatchArm) {
  pattern: Pattern,
  term: Term
});

ast_node!(
  enum Pattern {
    Prod(ProdPattern),
    Sum(SumPattern),
    Whole(WholePattern),
  }
);

ast_node!(struct ProdPattern(Syntax::ProdPattern));
impl ProdPattern {
  pub fn fields(&self) -> AstChildren<FieldPattern> {
    children(self.syntax())
  }
}
ast_node!(struct SumPattern(Syntax::SumPattern) {
  field: FieldPattern
});
ast_node!(struct WholePattern(Syntax::WholePattern));
impl HasName for WholePattern {}

ast_node!(struct FieldPattern(Syntax::FieldPattern) {
  pattern: Pattern
});
impl HasName for FieldPattern {}

ast_node!(struct FieldExpr(Syntax::FieldExpr) {
  term: Term
});
impl HasName for FieldExpr {}

ast_node!(
  enum TermPrefix {
    With(WithPrefix),
    Closure(ClosurePrefix),
    Let(LetPrefix),
  }
);

ast_node!(struct WithPrefix(Syntax::WithPrefix) {
  term: Term
});

ast_node!(struct ClosurePrefix(Syntax::ClosurePrefix) {
 annotation: TypeAnnotation
});
impl HasName for ClosurePrefix {}

ast_node!(struct LetPrefix(Syntax::LetPrefix) {
  annotation: TypeAnnotation,
  defn: Term
});
impl HasName for LetPrefix {}

ast_node!(struct TypeAnnotation(Syntax::TypeAnnotation) {
  ty: Type
});

ast_node!(struct EffectDefn(Syntax::EffectDefn));
impl HasName for EffectDefn {}
impl EffectDefn {
  pub fn ops(&self) -> Option<AstChildren<EffectOp>> {
    self
      .syntax()
      .first_child_by_kind(&|kind| kind == Syntax::EffectOps)
      .map(|node| children(&node))
  }
}

ast_node!(struct EffectOp(Syntax::EffectOp) {
  annotation: SchemeAnnotation
});
impl HasName for EffectOp {}

ast_node!(struct TypeScheme(Syntax::TypeScheme) {
  ty: Type
});
impl TypeScheme {
  pub fn quantifiers(&self) -> Option<AstChildren<Quantifier>> {
    self
      .syntax()
      .first_child_by_kind(&|kind| kind == Syntax::ForallBinders)
      .map(|node| children(&node))
  }

  pub fn constraints(&self) -> Option<AstChildren<Constraint>> {
    self
      .syntax()
      .first_child_by_kind(&|kind| kind == Syntax::Constraints)
      .map(|node| children(&node))
  }
}

ast_node!(struct Quantifier(Syntax::ForallBinder));
impl HasName for Quantifier {}

ast_node!(struct Constraint(Syntax::RowSumConstraint) {
  left: RowAtom,
  right: RowAtom,
  goal: RowAtom
});

ast_node!(
  enum RowAtom {
    Concrete(ConcreteRowAtom),
    Variable(VariableRowAtom),
  }
);

ast_node!(struct ConcreteRowAtom(Syntax::ConcreteRowAtom));
impl ConcreteRowAtom {
  pub fn fields(&self) -> AstChildren<RowField> {
    children(self.syntax())
  }
}

ast_node!(struct RowField(Syntax::RowField) {
  ty: Type
});
impl HasName for RowField {}

ast_node!(struct VariableRowAtom(Syntax::VariableRowAtom));
impl HasName for VariableRowAtom {}

ast_node!(
  enum Type {
    Name(NameType),
    Sum(SumType),
    Product(ProductType),
    Paren(ParenType),
    Function(FunctionType),
  }
);

ast_node!(struct NameType(Syntax::NameType));
impl HasName for NameType {}

ast_node!(struct SumType(Syntax::SumType) {
  row: Row
});
/*impl SumType {
  pub fn row_fields(&self) -> AstChildren<RowField> {
    children(self.syntax())
  }
}*/

ast_node!(struct ProductType(Syntax::ProductType) {
  row: Row
});
/*impl ProductType {
  pub fn row_fields(&self) -> AstChildren<RowField> {
    children(self.syntax())
  }
}*/

ast_node!(
  enum Row {
    Variable(VariableRow),
    Concrete(ConcreteRow),
    Mixed(MixedRow),
  }
);

ast_node!(struct VariableRow(Syntax::VariableRow));
impl VariableRow {
  pub fn variables(&self) -> AstChildren<Name> {
    children(self.syntax())
  }
}

ast_node!(struct ConcreteRow(Syntax::ConcreteRow));
impl ConcreteRow {
  pub fn fields(&self) -> AstChildren<RowField> {
    children(self.syntax())
  }
}

ast_node!(struct MixedRow(Syntax::MixedRow) {
  concrete: ConcreteRow,
  variable: VariableRow
});

ast_node!(struct ParenType(Syntax::ParenType));
impl ParenType {
  pub fn ty(&self) -> Option<Type> {
    child(self.syntax())
  }
}

ast_node!(struct FunctionType(Syntax::FunctionType));
impl FunctionType {
  pub fn tys(&self) -> AstChildren<Type> {
    children(self.syntax())
  }
}

ast_node!(
  enum Item {
    Term(TermDefn),
    Effect(EffectDefn),
  }
);

ast_node!(struct Items(Syntax::Items));
impl Items {
  pub fn new(root: GreenNode) -> Self {
    Self(SyntaxNode::new_root(root))
  }

  pub fn items(&self) -> AstChildren<Item> {
    children(self.syntax())
  }
}
