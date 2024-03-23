{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}

module CC.Prompt (
        P,
        Prompt(..),
        runP,
        newPromptName,
        eqPrompt,
        prettyPrompt,
        Equal(..)
    ) where

import Control.Monad.State
import Control.Monad.Reader
import Prettyprinter

import Unsafe.Coerce
import Pretty

newtype Prompt ans a = Prompt Int
  deriving (Show)

prettyPrompt :: Prompt ans a -> Doc SyntaxHighlight
prettyPrompt (Prompt int) = parens (pretty "Prompt" <+> annotate Literal (pretty int))


newtype P ans m a = P { unP :: StateT Int m a }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadState Int, MonadReader r)

runP :: (Monad m) => P ans m ans -> m ans
runP p = evalStateT (unP p) 0

newPromptName :: (Monad m) => P ans m (Prompt ans a)
newPromptName = do i <- get ; put (succ i) ; return (Prompt i)

data Equal a b where
    EQU :: Equal a a
    NEQ :: Equal a b

eqPrompt :: Prompt ans a -> Prompt ans b -> Equal a b
eqPrompt (Prompt p1) (Prompt p2)
    | p1 == p2  = unsafeCoerce EQU
    | otherwise = NEQ


