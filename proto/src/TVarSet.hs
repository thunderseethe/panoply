{- Thin wrapper around IntSet to expose it's API in terms of TVar -}
module TVarSet where

import qualified Data.IntSet as IntSet
import Type (TVar(..))

newtype TVarSet = TVarSet { unTVarSet :: IntSet.IntSet }

empty :: TVarSet 
empty = TVarSet IntSet.empty

fromList :: [TVar] -> TVarSet
fromList = TVarSet . IntSet.fromList . fmap unTV

toList :: TVarSet -> [TVar]
toList = fmap TV . IntSet.toList . unTVarSet

member :: TVar -> TVarSet -> Bool
member (TV tvar) (TVarSet set) = IntSet.member tvar set

insert :: TVar -> TVarSet -> TVarSet
insert (TV i) = TVarSet . IntSet.insert i . unTVarSet

union :: TVarSet -> TVarSet -> TVarSet
union (TVarSet a) (TVarSet b) = TVarSet $ IntSet.union a b

