module Program where

import Term
import Type
import Constraint
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)

data Eff = Eff { eff_name :: Text, eff_ops :: Map Label Scheme }
  deriving (Show)

type EffSigs = Map Label (Map Label Scheme)
type SigsEff = Map Label (Eff, Scheme)

data EffCtx = EffCtx {effs :: EffSigs, sigs :: SigsEff}

emptyEffCtx :: EffCtx
emptyEffCtx = EffCtx Map.empty Map.empty

mkEffCtx :: (Foldable f) => f Eff -> EffCtx
mkEffCtx = foldr go (EffCtx Map.empty Map.empty)
 where
  go eff@(Eff name ops) (EffCtx effs sigs) =
    EffCtx (Map.insert name ops effs) (Map.foldrWithKey (\op sig sigs -> Map.insert op (eff, sig) sigs) sigs ops)

data Def meta = Def { def_name :: Var, def_term :: Term meta }
  deriving (Show)

data Prog meta = Prog { terms :: [Def meta], effects :: [Eff] }
  deriving (Show)

addDef def (Prog defs effs) = Prog (def:defs) effs
