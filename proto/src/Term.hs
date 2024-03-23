{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Term where

import Control.Lens (Lens', Plated (plate), Traversal, Traversal', cosmos, lens, maximumOf, over)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

import Data.List ((\\))
import Type
import Prettyprinter
import qualified Data.Foldable as Foldable
import Data.Text (Text)
import qualified Prettyprinter as Pretty
import Pretty
import Prelude hiding (abs)
import GHC.Generics

newtype Var = V Int
  deriving (Ord, Eq, Num, Enum, Show, Read, Bounded)

namedVar :: Var -> Maybe Text
namedVar (V (-1)) = Just "evv"
namedVar (V (-2)) = Just "marker"
namedVar (V (-3)) = Just "op_arg"
namedVar (V (-4)) = Just "resume"
namedVar (V (-5)) = Just "left"
namedVar (V (-6)) = Just "right"
namedVar (V (-7)) = Just "z"
namedVar (V (-8)) = Just "x"
namedVar (V (-9)) = Just "(+)"
namedVar (V (-10)) = Just "(<)"
namedVar _ = Nothing

prettyV :: Var -> Doc SyntaxHighlight
prettyV v = maybe (Pretty.parens $ pretty v) (annotate NamedVariable . pretty) (namedVar v)

instance Pretty Var where
  pretty = viaShow

data Dir = L | R
  deriving (Show)

data Clause meta = Clause {_op :: Label, _arg :: Var, _resume :: Var, _body :: Term meta}
  deriving (Show)

instance Functor Clause where
  fmap f (Clause op arg resume body) = Clause op arg resume (f <$> body)

instance Foldable Clause where
  foldr f seed (Clause _ _ _ body) = foldr f seed body

data HandleClause meta = HandleClause {clauses :: NonEmpty (Clause meta), ret_clause :: Clause meta}
  deriving (Show)

instance Functor HandleClause where
  fmap f (HandleClause clauses ret_clause) = HandleClause (fmap f <$> clauses) (f <$> ret_clause)

instance Foldable HandleClause where
  foldr f seed (HandleClause clauses ret) = foldr (flip (foldr f)) (foldr f seed ret) clauses

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
  | -- Sum branching (elim)
    Branch meta (Term meta) (Term meta)
  | -- Sum injection (intro)
    Inj meta Dir (Term meta)
  | -- Lambda Abstraction
    Abs { abs_meta :: meta, abs_args :: NonEmpty Var, abs_body :: Term meta }
  | -- Application
    App { app_meta :: meta, app_head :: Term meta, app_spine :: NonEmpty (Term meta) }
  | -- Handle Control Flow Operation
    Handle { handle_meta :: meta, handle_effect :: Label, handle_handler :: Term meta, handle_body :: Term meta }
  | -- Effect Handler
    Handler meta (HandleClause meta)
  | -- Effect Perform Operation
    Perform { perform_meta :: meta, perform_op :: Label, perform_val :: Term meta }
  deriving (Show)


meta :: Lens' (Term meta) meta
meta = lens get set
 where
  set term b = fmap (const b) term
  get =
    \case
      Var m _ -> m
      Int m _ -> m
      Unit m -> m
      Label m _ _ -> m
      Unlabel m _ _ -> m
      Concat m _ _ -> m
      Prj m _ _ -> m
      Branch m _ _ -> m
      Inj m _ _ -> m
      Abs m _ _ -> m
      App m _ _ -> m
      Handle m _ _ _ -> m
      Handler m _ -> m
      Perform m _ _ -> m

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
    Branch m left right -> Branch <$> f m <*> meta' f left <*> meta' f right
    Inj m dir term -> Inj <$> f m <*> pure dir <*> meta' f term
    Abs m x body -> Abs <$> f m <*> pure x <*> meta' f body
    App m fn arg -> App <$> f m <*> meta' f fn <*> traverse (meta' f) arg
    Handle m lbl handler term -> Handle <$> f m <*> pure lbl <*> meta' f handler <*> meta' f term
    Handler m clauses -> Handler <$> f m <*> handleClauseTerms (meta' f) clauses
    Perform m op val -> Perform <$> f m <*> pure op <*> meta' f val

voidAnn :: Term meta -> Term ()
voidAnn = over meta' (const ())

{-instance Show1 Term where
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
        App m fn arg -> go fn . (" [" ++) . foldr (\item acc -> item . (',' :) . acc) (']' :) (go <$> arg) . (" :: " ++) . (' ' :) . show p m
        Handle m eff handler term -> ("handle<" ++) . showsPrec p eff . ("> " ++) . go handler . (' ' :) . go term . (' ' :) . show p m
        Handler _ clauses -> liftShowsPrec show showList p clauses
        Perform m op val -> ("perform " ++) . showsPrec p op . (' ' :) . go val . (" :: " ++) . (' ' :) . show p m-}

instance Functor Term where
  fmap f =
    \case
      Label m lbl term -> Label (f m) lbl (f <$> term)
      Unlabel m term lbl -> Unlabel (f m) (f <$> term) lbl
      Abs m v term -> Abs (f m) v (f <$> term)
      App m fn arg -> App (f m) (f <$> fn) (fmap f <$> arg)
      Concat m left right -> Concat (f m) (f <$> left) (f <$> right)
      Prj m dir term -> Prj (f m) dir (f <$> term)
      Branch m left right -> Branch (f m) (f <$> left) (f <$> right)
      Inj m dir term -> Inj (f m) dir (f <$> term)
      Handle m eff handler term -> Handle (f m) eff (f <$> handler) (f <$> term)
      Handler m clauses -> Handler (f m) (f <$> clauses)
      Var m x -> Var (f m) x
      Int m i -> Int (f m) i
      Perform m op val -> Perform (f m) op (f <$> val)
      Unit m -> Unit (f m)

instance Foldable Term where
  foldMap f =
    \case
      Label m _ term -> f m <> foldMap f term
      Unlabel m term _ -> f m <> foldMap f term
      Abs m _ term -> f m <> foldMap f term
      App m fn arg -> f m <> foldMap f fn <> foldMap (foldMap f) arg
      Concat m left right -> f m <> foldMap f left <> foldMap f right
      Prj m _ term -> f m <> foldMap f term
      Branch m left right -> f m <> foldMap f left <> foldMap f right
      Inj m _ term -> f m <> foldMap f term
      Handle m _ handler term -> f m <> foldMap f handler <> foldMap f term
      Handler m clauses -> f m <> foldMap f clauses
      Var m _ -> f m
      Int m _ -> f m
      Perform m _ _ -> f m
      Unit m -> f m

instance Traversable Term where
  traverse f =
    \case
      Label m lbl term -> Label <$> f m <*> pure lbl <*> traverse f term
      Unlabel m term lbl -> Unlabel <$> f m <*> traverse f term <*> pure lbl
      Abs m v term -> Abs <$> f m <*> pure v <*> traverse f term
      App m fn arg -> App <$> f m <*> traverse f fn <*> traverse (traverse f) arg
      Concat m left right -> Concat <$> f m <*> traverse f left <*> traverse f right
      Prj m dir term -> Prj <$> f m <*> pure dir <*> traverse f term
      Branch m left right -> Branch <$> f m <*> traverse f left <*> traverse f right
      Inj m dir term -> Inj <$> f m <*> pure dir <*> traverse f term
      Handle m eff handler term -> Handle <$> f m <*> pure eff <*> traverse f handler <*> traverse f term
      Handler m clauses -> Handler <$> f m <*> handleClauseTerms (traverse f) clauses
      Var m x -> Var <$> f m <*> pure x
      Int m i -> Int <$> f m <*> pure i
      Perform m op val -> Perform <$> f m <*> pure op <*> traverse f val
      Unit m -> Unit <$> f m

instance Plated (Term meta) where
  plate f =
    \case
      Label m lbl term -> Label m lbl <$> f term
      Unlabel m term lbl -> Unlabel m <$> f term <*> pure lbl
      Prj m dir term -> Prj m dir <$> f term
      Inj m dir term -> Inj m dir <$> f term
      Concat m left right -> Concat m <$> f left <*> f right
      Branch m left right -> Branch m <$> f left <*> f right
      Abs m v term -> Abs m v <$> f term
      App m fn arg -> App m <$> f fn <*> traverse f arg
      Handle m eff handler term -> Handle m eff <$> f handler <*> f term
      Handler m handler -> Handler m <$> handleClauseTerms f handler
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
abs :: [Var] -> Term () -> Term ()
abs vars = Abs () (NonEmpty.fromList vars)

letChain :: (Foldable t) => t (Var, Term ()) -> Term () -> Term () 
letChain defns body = foldr (\(x, defn) body -> abs [x] body <@> defn) body defns

letTerm defn = letChain [defn]

var :: Var -> Term ()
var = Var ()

int :: Int -> Term ()
int = Int ()

unit :: Term ()
unit = Unit ()

label = Label ()

unlabel = Unlabel ()

handler clauses ret = Handler () $ HandleClause clauses ret

record [] = unit
record [term] = term
record terms = foldl1 (Concat ()) terms

prj = Prj ()

fn <@> arg = App () fn (arg :| [])

-- Panics on empty list
app fn args = App () fn (NonEmpty.fromList (Foldable.toList args))

handle = Handle ()
perform = Perform ()

f x =
  case x of
    Just y -> y
    Nothing -> Just Nothing
