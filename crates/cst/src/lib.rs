use logos::Logos;
pub use rowan::{ast::AstNode, GreenNode, SyntaxNode};
use rowan::{
  ast::{
    support::{child, children},
    AstChildren, AstPtr,
  },
  Language,
};
use std::fmt::Debug;

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
