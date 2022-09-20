module Program where

import Term
import Type
import Constraint
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)

data Eff = Eff { eff_name :: Text, eff_handler_ty :: Type, eff_ops :: Map Label Scheme }
  deriving (Show)

type SigsByEff = Map Label Eff
type EffBySigs = Map Label (Eff, Int, Scheme)

data EffCtx = EffCtx { effs :: SigsByEff, sigs :: EffBySigs }

emptyEffCtx :: EffCtx
emptyEffCtx = EffCtx Map.empty Map.empty

mkEffCtx :: (Foldable f) => f Eff -> EffCtx
mkEffCtx = foldr go (EffCtx Map.empty Map.empty)
 where
  go eff@(Eff name _ ops) (EffCtx effs sigs) =
    EffCtx (Map.insert name eff effs) (Map.foldrWithKey (\ op (i, sig) sigs -> Map.insert op (eff, i, sig) sigs) sigs (calcIndex ops))

calcIndex :: Map Label Scheme -> Map Label (Int, Scheme)
calcIndex = snd . Map.mapAccum (\i sig -> (i + 1, (i, sig))) 1 {- return always goes in 0 so we start index here at 1 -}

data Def meta = Def { def_name :: Var, def_term :: Term meta }
  deriving (Show)

data Prog meta = Prog { terms :: [Def meta], effects :: [Eff] }
  deriving (Show)

addDef def (Prog defs effs) = Prog (def:defs) effs
