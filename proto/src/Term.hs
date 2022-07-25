{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Term where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Control.Lens (Plated (plate), Traversal', maximumOf, cosmos)
import Data.Maybe

newtype Var = V Int
  deriving (Ord, Eq, Num, Enum, Show, Bounded)

type Label = Text

data Dir = L | R

infixr 4 :*
infix 5 :|>

data Clause = Clause {op :: Label, arg :: Var, resume :: Var, body :: Term}
instance Show Clause where
  showsPrec p (Clause op arg resume body) = showsPrec p op . (' ' :) . showsPrec p arg . (' ' :) . showsPrec p resume . (" = " ++) . showsPrec p body

data HandleClause = HandleClause { clauses :: NonEmpty Clause, ret_clause :: Clause }
instance Show HandleClause where
  showsPrec p (HandleClause clauses ret) = ("{ " ++) . foldr (\clause fn -> showsPrec p clause . (", " ++) . fn) (showsPrec p ret) clauses . (" }" ++)

{- Our lambda calc + int to allow for a meaningful base type in examples -}
data Term
  = -- Variable
    Var Var
  | -- Int literal
    Int Int
  | -- Unit literal
    Unit
  | -- Wrap a term in a label
    Label :|> Term
  | -- Unwrap a term with given label
    Term :/ Label
  | -- Record concatenation (intro)
    Term :* Term
  | -- Record projection (elim)
    Prj Dir Term
  | -- Lambda Abstraction
    Abs Var Term
  | -- Application
    Term :@ Term
  | -- Effect Handler
    Handle Label HandleClause Term
  | -- Effect Operation
    Op Label

instance Show Term where
  showsPrec p term =
    case term of
      Var v -> showsPrec p v
      Int i -> showsPrec p i
      Unit -> showsPrec p "{}"
      lbl :|> term -> showsPrec p lbl . (" |> " ++) . showsPrec p term
      term :/ lbl -> showsPrec p term . (" / " ++) . showsPrec p lbl
      left :* right -> showsPrec p left . (" :*: " ++) . showsPrec p right
      Prj _ term -> ("prj " ++) . showsPrec p term
      Abs v term -> ("λ " ++) . showsPrec p v . (" . " ++) . showsPrec p term
      fn :@ arg -> showsPrec p fn . (' ' :) . showsPrec p arg
      Handle eff handler term -> ("handle<" ++) . showsPrec p eff . ("> " ++) . showsPrec p handler . (' ' :) . showsPrec p term
      Op op -> showsPrec p op

instance Plated Term where
  plate f =
    \case
      lbl :|> term -> (:|>) lbl <$> f term
      term :/ lbl ->  (:/ lbl) <$> f term
      Prj dir term -> Prj dir <$> f term
      Abs v term -> Abs v <$> f term
      fn :@ arg -> (:@) <$> f fn <*> f arg
      Handle eff handler term -> Handle eff <$> handleClauseTerms f handler <*> f term
      t -> pure t


handleClauseTerms :: Traversal' HandleClause Term
handleClauseTerms f (HandleClause clauses ret) = HandleClause <$> traverse (clauseTerms f) clauses <*> clauseTerms f ret

clauseTerms :: Traversal' Clause Term
clauseTerms f (Clause lbl var resume body) = Clause lbl var resume <$> f body

termVar :: Traversal' Term Var
termVar f =
  \case
    Var v -> Var <$> f v
    term -> pure term

maxTermVar :: Term -> Var
maxTermVar = fromMaybe (V $ -1) . maximumOf (cosmos . termVar)
