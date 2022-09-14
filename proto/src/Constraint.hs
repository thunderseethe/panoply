{-# LANGUAGE LambdaCase #-}
module Constraint where

import Control.Lens
import qualified Data.Map.Strict as Map

import Type
import Subst
import Control.Applicative (Applicative(liftA2))

-- This is a helper datatype that enumerates things that might appear in an equality constraint
data Ct
  -- A mono type
  = T Type
  -- Two rows that must combine to equal their neighbor constraint
  | InternalRow :⊙ InternalRow
  deriving (Show, Eq, Ord)

ctOccurs :: TVar -> Ct -> Bool
ctOccurs tvar = anyOf (typeOf . typeVars) (== tvar)

ctTVars :: Traversal' Ct TVar
ctTVars f =
  \case
    T ty -> T <$> typeVars f ty
    left :⊙ right -> liftA2 (:⊙) (internalRowTVars f left) (internalRowTVars f right)

instance TypeOf Ct where
  typeOf f =
    \case
      T ty -> T <$> f ty
      left :⊙ right -> (:⊙) <$> typeOf f left <*> typeOf f right 

instance SubstApp Ct where
  apply subst =
    \case
      T ty -> T (apply subst ty)
      left :⊙ right -> apply subst left :⊙ apply subst right

infixl 9 :⊙
infixl 8 :<~>
infixl 8 ~
infixl 8 ~>

-- Need a better name for this
data Q
  = -- Equality constraint
    Type :<~> Ct
  deriving (Eq, Ord)

-- NB: Convention is that < and > point towards the more complex constraint
-- So :<~> can have a full constraint on either side.
-- (~) can only have a type on either side.
(~) :: Type -> Type -> Q
t1 ~ t2 = t1 :<~> T t2

-- (~>) has a type as it's left argument and a full constraint as it's right argument
(~>) :: Type -> Ct -> Q
ty ~> ct = ty :<~> ct

instance Show Q where
  showsPrec p (t1 :<~> T t2) = showsPrec p t1 . (" :~ " ++) . showsPrec p t2
  showsPrec p (t1 :<~> ct) = showsPrec p t1 . (" :~> " ++) . showsPrec p ct

instance SubstApp Q where
  apply subst (ct1 :<~> ct2) = apply subst ct1 :<~> apply subst ct2

instance TypeOf Q where
  typeOf f = 
    \case
      ct1 :<~> ct2 -> liftA2 (:<~>) (typeOf f ct1) (typeOf f ct2)

data Implication = Imp {_exists :: [TVar], _prop :: [Q], _implies :: [Constraint]}
  deriving (Eq, Show)

instance SubstApp Implication where
  apply (Subst map) (Imp exists prop impl) =
    let subst = Subst $ foldr Map.delete map exists
     in Imp exists (apply subst prop) (apply subst impl)

{- Constraints generated during type inference that must hold for the program to typecheck -}
data Constraint
  = Simp Q
  | Impl Implication
  deriving (Eq, Show)

instance SubstApp Constraint where
  apply subst (Simp q) = Simp (apply subst q)
  apply subst (Impl imp) = Impl (apply subst imp)

constraintEither :: Iso' Constraint (Either Q Implication)
constraintEither = iso to from
 where
  to (Simp q) = Left q
  to (Impl i) = Right i

  from (Left q) = Simp q
  from (Right i) = Impl i

instance TypeOf Constraint where
  typeOf f = 
    \case
      Simp q -> Simp <$> typeOf f q
      constr -> pure constr

constraintQs :: Traversal' Constraint Q
constraintQs f (Simp q) = Simp <$> f q
constraintQs _ (Impl i) = pure (Impl i)

constraintImpls :: Traversal' Constraint Implication
constraintImpls _ (Simp q) = pure (Simp q)
constraintImpls f (Impl i) = Impl <$> f i

simple :: Traversable f => Traversal' (f Constraint) Q
simple = traverse . constraintQs

impl :: Traversable f => Traversal' (f Constraint) Implication
impl = traverse . constraintImpls

data Scheme = Scheme [TVar] [Q] Type
  deriving (Show, Eq, Ord)

monoScheme :: Type -> Scheme
monoScheme = Scheme [] []
