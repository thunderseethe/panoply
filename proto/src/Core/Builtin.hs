{-# LANGUAGE OverloadedStrings #-}
{- Set of builtins that exist at the core level and can only be called from term level -}
module Core.Builtin where

import Term
import Constraint
import Core
import Type (InternalRow(..), Type(FunTy, SumTy, RowTy, VarTy, ProdTy), unitTy, TVar (TV))
import qualified Type

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

intAdd :: Core
intAdd = Lam left $ Lam right (Primitive (BinOp IntAdd (PrimVar left) (PrimVar right)))
  where
    left = CoreV (V (-5)) (CoreLit IntTy)
    right = CoreV (V (-6)) (CoreLit IntTy)


intLt :: Core
intLt = Lam left $ Lam right (Primitive (BinOp IntLt (PrimVar left) (PrimVar right)))
  where
    left = CoreV (V (-5)) (CoreLit IntTy)
    right = CoreV (V (-6)) (CoreLit IntTy)

boolTy :: Type
boolTy = SumTy (RowTy (Map.fromList [("False", unitTy), ("True", unitTy)]))

builtinCtx :: Map Var Scheme
builtinCtx = Map.fromList
  [ (V (-9), Scheme [] [] (FunTy Type.IntTy (Closed Map.empty) (FunTy Type.IntTy (Closed Map.empty) Type.IntTy)))
  , (V (-10), Scheme [] [] (FunTy Type.IntTy (Closed Map.empty) (FunTy Type.IntTy (Closed Map.empty) boolTy)))
  , (Core.evv, Scheme [] [] (ProdTy (RowTy Map.empty)))
  ]
