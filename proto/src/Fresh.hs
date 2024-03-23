{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Fresh where

import Data.Kind
import Control.Algebra
import Control.Carrier.State.Strict

data Fresh tvar (m :: Type -> Type) k where
  Fresh :: Fresh tvar m tvar

fresh :: Has (Fresh tvar) sig m => m tvar
fresh = send Fresh

newtype FreshC tvar m a = FreshC { unFresh :: StateC tvar m a }
  deriving (Applicative, Functor, Monad)

instance (Enum tvar, Algebra sig m) => Algebra (Fresh tvar :+: sig) (FreshC tvar m) where
  alg hdl sig ctx = case sig of
    L Fresh -> FreshC $ StateC $ \s -> return (succ s, s <$ ctx)
    R other -> FreshC (alg (unFresh . hdl) (R other) ctx)

runFreshDefault :: (Bounded tvar) => FreshC tvar m a -> m (tvar, a)
runFreshDefault = runState minBound . unFresh

runFresh s = runState s . unFresh
