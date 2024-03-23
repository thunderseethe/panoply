{-# LANGUAGE OverloadedStrings #-}
module Tests (tests) where

import Distribution.TestSuite
import OutsideIn
import Term
import Type
import Control.Lens
import qualified Data.Map.Strict as Map

import Test.QuickCheck (quickCheck, quickCheckResult)
import Test.QuickCheck.Test (Result(..))

tests :: IO [Test]
tests = return 
  [ Group "TypeCheck" True typeCheckTests
  ]

typeCheckTests :: [Test]
typeCheckTests = 
  [ Test int_tc 
  , Test unit_tc
  , Test label_tc
  , Test unlabel_label_id_tc
  ]
  where
    test_instance name run = inst
      where
        inst = TestInstance 
          { run = run
          , name = name
          , tags = ["type_checking"]
          , options = []
          , setOption = \_ _ -> Right inst
          }

    int_tc = test_instance "Int TypeChecks" $ do
      let (infer, _, _) = inferSingTerm (int 2)
      return . Finished $ case infer ^. meta . ty of
        IntTy -> Pass
        ty -> Fail ("Expected IntTy, found: " ++ show ty)

    unit_tc = test_instance "Unit TypeChecks" $ do
      let (infer, _, _) = inferSingTerm unit
      return . Finished $ case infer ^. meta . ty of
        ProdTy (RowTy m) | Map.null m -> Pass
        ty -> Fail ("Expected unit type, found: " ++ show ty)
    
    label_tc = test_instance "Label TypeChecks" $ do
      let (infer, _, _) = inferSingTerm (label "point" (int 2))
      return . Finished $ 
        case infer ^. meta . ty of
          RowTy m | m == Map.fromList [("point", IntTy)] -> Pass
          ty -> Fail ("Expected a labelled type, found:" ++ show ty)

    unlabel_label_id_tc = test_instance "Unlabel <-> Label Identity" $ do
      let lbl = "asdfgh"
      let (infer, _, _) = inferSingTerm (unlabel (label lbl (int 2)) lbl)
      return . Finished $ case infer ^. meta . ty of
        IntTy -> Pass
        ty -> Fail ("Expected IntTy, found: " ++ show ty)
