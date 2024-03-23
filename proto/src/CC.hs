{-# LANGUAGE Rank2Types, GeneralizedNewtypeDeriving, UndecidableInstances, FunctionalDependencies, FlexibleInstances, GADTs #-}

--------------------------------------------------------------------------
-- |
-- Module      : Control.Monad.CC
-- Copyright   : (c) R. Kent Dybvig, Simon L. Peyton Jones and Amr Sabry
-- License     : MIT
--
-- Maintainer  : Dan Doel
-- Stability   : Experimental
-- Portability : Non-portable (rank-2 types, multi-parameter type classes,
--                              functional dependencies)
--
-- A monadic treatment of delimited continuations.
--
--    Adapted from the paper
--      /A Monadic Framework for Delimited Continuations/,
--    by R. Kent Dybvig, Simon Peyton Jones and Amr Sabry
--      (<http://www.cs.indiana.edu/~sabry/papers/monadicDC.pdf>)
--
-- This module implements the delimited continuation monad and transformer,
-- using the sequence-of-frames implementation from the original paper.

module Control.Monad.CC (
        -- * The CC monad
        CC(),
        runCC,
        -- * The CCT monad transformer
        CCT(),
        runCCT,
        SubCont(),
        Prompt,
        MonadDelimitedCont(..),
        -- * Assorted useful control operators
        reset,
        shift,
        control,
        shift0,
        control0,
        abort
        -- * Examples
        -- $Examples
    ) where

import Control.Applicative

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans

import CC.Seq
import CC.Prompt

data Frame m ans a b = FFrame (a -> b)
                     | MFrame (a -> CCT ans m b)

type Cont ans m a = Seq (Frame m) ans a
newtype SubCont ans m a b = SC (SubSeq (Frame m) ans a b)

-- | The CCT monad transformer allows you to layer delimited control
-- effects over an arbitrary monad.
--
-- The CCT transformer is parameterized by the following types
--
-- * ans : A region parameter, so that prompts and subcontinuations
--         may only be used in the same region they are created.
--
-- * m   : the underlying monad
--
-- * a   : The contained value. A value of type CCT ans m a can be though
--         of as a computation that calls its continuation with a value of
--         type 'a'
newtype CCT ans m a = CCT { unCCT :: Cont ans m a -> P ans m ans }

instance (Monad m) => Functor (CCT ans m) where
    fmap f (CCT e) = CCT $ \k -> e (PushSeg (FFrame f) k)

instance (Monad m) => Applicative (CCT ans m) where
    pure  = return
    (<*>) = ap

instance (Monad m) => Monad (CCT ans m) where
    return v = CCT $ \k -> appk k v
    (CCT e1) >>= e2 = CCT $ \k -> e1 (PushSeg (MFrame e2) k)

instance MonadTrans (CCT ans) where
    lift m = CCT $ \k -> lift m >>= appk k

instance (MonadReader r m) => MonadReader r (CCT ans m) where
    ask = lift ask
    local f m = CCT $ \k -> local f (unCCT m k)

instance (MonadState s m) => MonadState s (CCT ans m) where
    get = lift get
    put = lift . put

instance (MonadIO m) => MonadIO (CCT ans m) where
    liftIO = lift . liftIO

-- Applies a continuation to a value. 
appk :: Monad m => Cont ans m a -> a -> P ans m ans
appk EmptyS        a = return a
appk (PushP _ k)   a = appk k a
appk (PushSeg f k) a = appFrame f a k
 where
 appFrame (MFrame g) b l = unCCT (g b) l
 appFrame (FFrame g) b l = appk l (g b)

-- | Executes a CCT computation, yielding a value in the underlying monad
runCCT :: (Monad m) => (forall ans. CCT ans m a) -> m a
runCCT c = runP (unCCT c EmptyS)

-- | The CC monad may be used to execute computations with delimited control.
newtype CC ans a = CC { unCC :: CCT ans Identity a }
    deriving (Functor, Monad, Applicative,
                MonadDelimitedCont (Prompt ans) (SubCont ans Identity))

-- | Executes a CC computation, yielding a resulting value.
runCC  :: (forall ans. CC ans a) -> a
runCC c = runIdentity (runCCT (unCC c))

-- | A typeclass for monads that support delimited control operators.
-- The type varibles represent the following:
--
-- m : The monad itself
--
-- p : The associated type of prompts that may delimit computations in the monad
--
-- s : The associated type of sub-continuations that may be captured
class (Monad m) => MonadDelimitedCont p s m | m -> p s where
    -- | Creates a new, unique prompt.
    newPrompt   :: m (p a)
    -- | Delimits a computation with a given prompt.
    pushPrompt  :: p a -> m a -> m a
    -- | Abortively capture the sub-continuation delimited by the given
    -- prompt, and call the given function with it. The prompt does not appear
    -- delimiting the sub-continuation, nor the resulting computation.
    withSubCont :: p b -> (s a b -> m b) -> m a
    -- | Pushes a sub-continuation, reinstating it as part of the continuation.
    pushSubCont :: s a b -> m a -> m b

instance (Monad m) => MonadDelimitedCont (Prompt ans) (SubCont ans m) (CCT ans m) where
    newPrompt = CCT $ \k -> newPromptName >>= appk k
    pushPrompt p (CCT e) = CCT $ \k -> e (PushP p k)
    withSubCont p f = CCT $ \k -> let (subk, k') = splitSeq p k
                                   in unCCT (f (SC subk)) k'
    pushSubCont (SC subk) (CCT e) = CCT $ \k -> e (pushSeq subk k)

-- | An approximation of the traditional /reset/ operator. Creates a new prompt,
-- calls the given function with it, and delimits the resulting computation
-- with said prompt.
reset :: (MonadDelimitedCont p s m) => (p a -> m a) -> m a
reset e = newPrompt >>= \p -> pushPrompt p (e p)

-- -----
-- These originally had types like:
--
-- ((a -> m b) -> m b) -> m a
--
-- but I came to the conclusion that it would be convenient to be able to pass
-- in monadically typed values.
-- As a specific example, this makes the difference between
--
-- > shift q (\f -> f (dref p))
--
-- and
--
-- > join $ shift q (\f -> f (dref p))
--
-- In other words, one can expressed in terms of the other (I think), but
-- the fact that one has to insert a 'join' /outside/ the shift, and not
-- anywhere near where the sub-continuation is actually used is rather
-- odd, and difficult to remember compared to the difference between:
--
-- > shift q (\f -> f (return pureValue))
--
-- and
--
-- > shift q (\f -> f pureValue)
-- -----

-- | The traditional /shift/ counterpart to the above 'reset'. Reifies the
-- subcontinuation into a function, keeping both the subcontinuation, and
-- the resulting computation delimited by the given prompt.
shift :: (MonadDelimitedCont p s m) => p b -> ((m a -> m b) -> m b) -> m a
shift p f = withSubCont p $ \sk -> pushPrompt p $
                                f (pushPrompt p . pushSubCont sk)

-- | The /control/ operator, traditionally the counterpart of /prompt/. It does
-- not delimit the reified subcontinuation, so control effects therein can
-- escape. The corresponding prompt is performed equally well by 'reset' above.
control :: (MonadDelimitedCont p s m) => p b -> ((m a -> m b) -> m b) -> m a
control p f = withSubCont p $ \sk -> pushPrompt p $
                                f (pushSubCont sk)

-- | Abortively captures the current subcontinuation, delimiting it in a reified
-- function. The resulting computation, however, is undelimited.
shift0 :: (MonadDelimitedCont p s m) => p b -> ((m a -> m b) -> m b) -> m a
shift0 p f = withSubCont p $ \sk -> f (pushPrompt p . pushSubCont sk)

-- | Abortively captures the current subcontinuation, delimiting neither it nor
-- the resulting computation.
control0 :: (MonadDelimitedCont p s m) => p b -> ((m a -> m b) -> m b) -> m a
control0 p f = withSubCont p $ \sk -> f (pushSubCont sk)

-- | Aborts the current continuation up to the given prompt.
abort :: (MonadDelimitedCont p s m) => p b -> m b -> m a
abort p e = withSubCont p (const e)
