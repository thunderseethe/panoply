{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
module Subst where

import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map

import Type
import Control.Lens (transform)
import Data.Maybe (fromMaybe)


newtype Subst = Subst (Map TVar Type)

instance Show Subst where
  -- pointfree, more like... pointless
  showsPrec p (Subst map) = ('[' :) . (\s -> Map.foldrWithKey go s map) . (']' :)
   where
    go tvar ty = showsPrec p tvar . (" := " ++) . showsPrec p ty . (", " ++)

tvar |-> ty = Subst (Map.singleton tvar ty)
member tvar (Subst map) = Map.member tvar map

instance Semigroup Subst where
  (Subst left) <> (Subst right) =
    Subst $
      Map.merge
        Map.preserveMissing
        Map.preserveMissing
        (Map.zipWithMatched (\_ _ _ -> error "You fucked up, buck"))
        left
        right

instance Monoid Subst where
  mempty = Subst Map.empty

insert :: TVar -> Type -> Subst -> Subst
insert a ty (Subst map) = Subst $ Map.insert a ty map

class SubstApp t where
  apply :: Subst -> t -> t

instance (Functor f, SubstApp t) => SubstApp (f t) where
  apply subst = fmap (apply subst)

instance SubstApp Type where
  apply (Subst map) =
    transform
      ( \case
          VarTy var -> fromMaybe (VarTy var) (Map.lookup var map)
          ty -> ty)

instance SubstApp InternalRow where
  apply subst (Closed row) = Closed $ fmap (apply subst) row
  apply (Subst map) (Open tvar) =
    case map !? tvar of
      Just (RowTy row) -> Closed row
      Just (VarTy tv) -> Open tv
      -- If we can't substitute our open row for another row don't apply at all
      _ -> Open tvar
