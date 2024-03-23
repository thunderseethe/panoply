module Main where

import Core
import Term
import Pretty
import OutsideIn
import Interpret
import Data.Text
import Data.Text.IO
import Debug.Trace
import Control.Lens
import Program

checkAndExec :: Term () -> Value
checkAndExec term = trace ("\n" ++ unpack (Pretty.prettyRender (prettyCore core)) ++ "\n") $ interpret core
  where
    core = Core.simplify c
    (_, c, _) = inferSingTerm term

steps term = prettySteps core
  where
    core = Core.simplify c
    (_, c, _) = inferSingTerm term

stepsProg prog = prettySteps core
  where 
    core = Core.simplify (Core.App c (Core.Product [])) 
    (_, c:_, _) = infer prog

core = compileSingProg exampleMVPMultiEffect

compileSingProg prog = Core.simplify c
  where
    (_, c:_, _) = infer prog


prettyCheckAndExec = Data.Text.IO.putStrLn . Pretty.prettyRender . prettyVal . checkAndExec

main = Prelude.putStrLn "Hello, World!"
