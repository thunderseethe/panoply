{-# LANGUAGE LambdaCase #-}
module Constraint where

import Control.Lens

import Type

data Ct
  = T Type
  | InternalRow :⊙ InternalRow
  deriving (Show, Eq, Ord)

ctOccurs :: TVar -> Ct -> Bool
ctOccurs tvar = anyOf (ctTys . typeVars) (== tvar)

ctTys :: Traversal' Ct Type
ctTys f =
  \case
    T ty -> T <$> f ty
    left :⊙ right -> (:⊙) <$> internalRowTys f left <*> internalRowTys f right

-- Need a better name for this
data Q
  = -- Two types must be equal (aka unifiable)
    Ct :<~> Ct
  deriving (Eq, Ord)

instance Show Q where
  showsPrec p (T t1 :<~> T t2) = showsPrec p t1 . (" :~ " ++) . showsPrec p t2
  showsPrec p (T t1 :<~> ct) = showsPrec p t1 . (" :~> " ++) . showsPrec p ct
  showsPrec p (ct :<~> T t2) = showsPrec p ct . (" :<~ " ++) . showsPrec p t2
  showsPrec p (ct1 :<~> ct2) = showsPrec p ct1 . (" :<~> " ++) . showsPrec p ct2

infixl 9 :⊙
infixl 8 :<~>
infixl 8 ~
infixl 8 ~>
infixl 8 <~

(~) :: Type -> Type -> Q
t1 ~ t2 = T t1 :<~> T t2

(~>) :: Type -> Ct -> Q
ty ~> ct = T ty :<~> ct

(<~) :: Ct -> Type -> Q
ct <~ ty = ct :<~> T ty

qTys :: Traversal' Q Type
qTys f q =
  case q of
    -- Is doing builtin recursion like this bad?
    t1 :<~> t2 -> (:<~>) <$> ctTys f t1 <*> ctTys f t2

data Implication = Imp {_exists :: [TVar], _prop :: [Q], _implies :: [Constraint]}
  deriving (Eq, Show)

{- Constraints generated during type inference that must hold for the program to typecheck -}
data Constraint
  = Simp Q
  | Impl Implication
  deriving (Eq, Show)

constraintEither :: Iso' Constraint (Either Q Implication)
constraintEither = iso to from
 where
  to (Simp q) = Left q
  to (Impl i) = Right i

  from (Left q) = Simp q
  from (Right i) = Impl i

constraintTypes :: Traversal' Constraint Type
constraintTypes f constr =
  case constr of
    Simp q -> Simp <$> qTys f q
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
