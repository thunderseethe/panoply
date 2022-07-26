{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Term where

import Control.Lens (Plated (plate), Traversal, Traversal', cosmos, maximumOf, over)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

import Data.Functor.Classes (Show1 (liftShowsPrec))
import Data.List ((\\))
import Type

newtype Var = V Int
  deriving (Ord, Eq, Num, Enum, Show, Bounded)

data Dir = L | R
  deriving (Show)

data Clause meta = Clause {_op :: Label, _arg :: Var, _resume :: Var, _body :: Term meta}

instance Show1 Clause where
  liftShowsPrec show showList p (Clause op arg resume body) = showsPrec p op . (' ' :) . showsPrec p arg . (' ' :) . showsPrec p resume . (" = " ++) . liftShowsPrec show showList p body

instance (Show meta) => Show (Clause meta) where
  showsPrec = liftShowsPrec showsPrec showList

data HandleClause meta = HandleClause {clauses :: NonEmpty (Clause meta), ret_clause :: Clause meta}

instance Show1 HandleClause where
  liftShowsPrec show showList p (HandleClause clauses ret) = ("{ " ++) . foldr (\clause fn -> liftShowsPrec show showList p clause . (", " ++) . fn) (liftShowsPrec show showList p ret) clauses . (" }" ++)

instance (Show meta) => Show (HandleClause meta) where
  showsPrec = liftShowsPrec showsPrec showList

{- Our lambda calc + int to allow for a meaningful base type in examples -}
data Term meta
  = -- Variable
    Var meta Var
  | -- Int literal
    Int meta Int
  | -- Unit literal
    Unit meta
  | -- Wrap a term in a label
    Label meta Label (Term meta)
  | -- Unwrap a term with given label
    Unlabel meta (Term meta) Label
  | -- Record concatenation (intro)
    Concat meta (Term meta) (Term meta)
  | -- Record projection (elim)
    Prj meta Dir (Term meta)
  | -- Lambda Abstraction
    Abs meta Var (Term meta)
  | -- Application
    App meta (Term meta) (Term meta)
  | -- Effect Handler
    Handle meta Label (HandleClause meta) (Term meta)
  | -- Effect Operation
    Op meta Label
  deriving (Show)

meta :: Term meta -> meta
meta =
  \case
    Var m _ -> m
    Int m _ -> m
    Unit m -> m
    Label m _ _ -> m
    Unlabel m _ _ -> m
    Concat m _ _ -> m
    Prj m _ _ -> m
    Abs m _ _ -> m
    App m _ _ -> m
    Handle m _ _ _ -> m
    Op m _ -> m

meta' :: Traversal (Term a) (Term b) a b
meta' f =
  \case
    Var m x -> fmap (`Var` x) (f m)
    Int m i -> fmap (`Int` i) (f m)
    Unit m -> Unit <$> f m
    Label m lbl term -> Label <$> f m <*> pure lbl <*> meta' f term
    Unlabel m term lbl -> Unlabel <$> f m <*> meta' f term <*> pure lbl
    Concat m left right -> Concat <$> f m <*> meta' f left <*> meta' f right
    Prj m d term -> Prj <$> f m <*> pure d <*> meta' f term
    Abs m x body -> Abs <$> f m <*> pure x <*> meta' f body
    App m fn arg -> App <$> f m <*> meta' f fn <*> meta' f arg
    Handle m lbl clauses term -> Handle <$> f m <*> pure lbl <*> handleClauseTerms (meta' f) clauses <*> meta' f term
    Op m op -> Op <$> f m <*> pure op

voidAnn :: Term meta -> Term ()
voidAnn = over meta' (const ())

instance Show1 Term where
  liftShowsPrec show showList p = go
   where
    parens p f = if p > 10 then ('(' :) . f . (')' :) else f
    go t = parens p $
      case t of
        Var m v -> showsPrec p v . (' ' :) . show p m
        Int m i -> showsPrec p i . (' ' :) . show p m
        Unit m -> showsPrec p "{}" . (' ' :) . show p m
        Label m lbl term -> showsPrec p lbl . (" |> " ++) . go term . (' ' :) . show p m
        Unlabel m term lbl -> go term . (" / " ++) . showsPrec p lbl . (' ' :) . show p m
        Concat m left right -> go left . (" :*: " ++) . go right . (' ' :) . show p m
        Prj m _ term -> ("prj " ++) . go term . (" :: " ++) . (' ' :) . show p m
        Abs m v term -> ("λ " ++) . showsPrec p v . (" . " ++) . go term . (' ' :) . show p m
        App m fn arg -> go fn . (' ' :) . go arg . (" :: " ++) . (' ' :) . show p m
        Handle m eff handler term -> ("handle<" ++) . showsPrec p eff . ("> " ++) . liftShowsPrec show showList p handler . (' ' :) . go term . (' ' :) . show p m
        Op m op -> showsPrec p op . (" :: " ++) . (' ' :) . show p m

{-instance Show (Term ()) where
  showsPrec = liftShowsPrec (\_ _ s -> s) (\_ s -> s)-}

{-instance Show (Term Type) where
  showsPrec _ = liftShowsPrec showAnn showList 11
   where
    showAnn p ty = (" :: " ++) . showsPrec p ty-}

instance Plated (Term meta) where
  plate f =
    \case
      Label m lbl term -> Label m lbl <$> f term
      Unlabel m term lbl -> Unlabel m <$> f term <*> pure lbl
      Prj m dir term -> Prj m dir <$> f term
      Abs m v term -> Abs m v <$> f term
      App m fn arg -> App m <$> f fn <*> f arg
      Handle m eff handler term -> Handle m eff <$> handleClauseTerms f handler <*> f term
      t -> pure t

handleClauseTerms :: Traversal (HandleClause a) (HandleClause b) (Term a) (Term b)
handleClauseTerms f (HandleClause clauses ret) = HandleClause <$> traverse (clauseTerms f) clauses <*> clauseTerms f ret

clauseTerms :: Traversal (Clause a) (Clause b) (Term a) (Term b)
clauseTerms f (Clause lbl var resume body) = Clause lbl var resume <$> f body

covers :: NonEmpty (Clause m) -> Map Label a -> Bool
covers clauses ops = null (Map.keys ops \\ (_op <$> NonEmpty.toList clauses))

termVar :: Traversal' (Term m) Var
termVar f =
  \case
    Var m v -> Var m <$> f v
    term -> pure term

maxTermVar :: Term m -> Var
maxTermVar = fromMaybe (V $ -1) . maximumOf (cosmos . termVar)

{- Smart Constructors -}
abs :: (Foldable f) => f Var -> Term () -> Term ()
abs vars body = foldr (Abs ()) body vars

var :: Var -> Term ()
var = Var ()

int :: Int -> Term ()
int = Int ()

unit :: Term ()
unit = Unit ()

label = Label ()

unlabel = Unlabel ()

record [] = unit
record [term] = term
record terms = foldl1 (Concat ()) terms

prj = Prj ()

fn <@> arg = App () fn arg

handle = Handle ()
op = Op ()

f x =
  case x of
    Just y -> y
    Nothing -> Just Nothing
