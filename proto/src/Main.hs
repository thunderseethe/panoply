module Main where

import Core
import Term
import Pretty
import OutsideIn
import Interpret
import Data.Text
import Data.Text.IO
import Debug.Trace

checkAndExec :: Term () -> Value
checkAndExec term = trace ("\n" ++ unpack (Pretty.prettyRender (prettyCore core)) ++ "\n") $ interpret core
  where
    core = {-Core.simplify-} c
    (_, c, _) = inferSingTerm term

prettyCheckAndExec = Data.Text.IO.putStrLn . Pretty.prettyRender . prettyVal . checkAndExec

main = Prelude.putStrLn "Hello, World!"
