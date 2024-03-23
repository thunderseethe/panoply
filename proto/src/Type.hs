{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Type where

import Control.Lens
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Prettyprinter

type Label = Text

class TypeOf t where
  typeOf :: Traversal' t Type

instance (Traversable t, TypeOf a) => TypeOf (t a) where
  typeOf f = traverse (typeOf f)

newtype TVar = TV { unTV :: Int }
  deriving (Read, Ord, Eq, Num, Enum, Bounded)

typeVarNames :: [String]
typeVarNames = go 1 base
 where
  base = pure <$> ['a' .. 'z']

  go n [] = go (n + 1) ((++ replicate n '\'') <$> base)
  go n (name : tail) = name : go n tail

instance Show TVar where
  show (TV i) = 
    if i >= 0
       then typeVarNames !! i
       else case i of
              (-1) -> "branch_type"
              (-2) -> "state_eff"
              (-3) -> "state_handler"
              (-4) -> "reader_eff"
              (-5) -> "reader_handler"
              i -> error $ "unnammed type var " ++ show i

instance Pretty TVar where
  pretty (TV i) = pretty (
    if i >= 0
       then typeVarNames !! i
       else case i of
              (-1) -> "branch_type"
              (-2) -> "state_eff"
              (-3) -> "state_handler"
              (-4) -> "reader_eff"
              (-5) -> "reader_handler"
              i -> error $ "unnammed type var " ++ show i)

instance Plated TVar where
  plate _ (TV i) = pure (TV i)

type Row = Map Label Type

data Type
  = VarTy TVar
  | IntTy
  | RowTy Row
  | ProdTy Type
  | SumTy Type
  | FunTy Type InternalRow Type
  deriving (Show, Read, Eq, Ord)

unitTy :: Type
unitTy = ProdTy $ RowTy Map.empty

typeEffs :: Traversal' Type InternalRow
typeEffs f =
  \case
    FunTy arg eff ret -> FunTy arg <$> f eff <*> pure ret
    ty -> pure ty

typeEffects :: Type -> [InternalRow]
typeEffects ty = universe ty >>= toListOf typeEffs

isVar :: Type -> Bool
isVar (VarTy _) = True
isVar _ = False

infix 7 |>
(|>) :: Label -> Type -> Row
lbl |> ty = Map.singleton lbl ty

showsRow :: Int -> Row -> ShowS
showsRow p row s = foldr (\f s -> f s) s $ intersperse (", " ++) $ (\(lbl, ty) -> showsPrec p lbl . (" |> " ++) . showsPrec p ty) <$> Map.toList row

instance Pretty Type where
  pretty =
    \case
      VarTy ty -> pretty ty
      IntTy -> "IntTy"
      RowTy row | Map.null row -> "()"
      RowTy row -> hcat $ punctuate ("," <> space) ((\(lbl, ty) -> pretty lbl <+> "|>" <+> pretty ty) <$> Map.toList row)
      ProdTy (RowTy row) | Map.null row -> "{}"
      ProdTy ty -> "{" <+> pretty ty <+> "}"
      SumTy (RowTy row) | Map.null row -> "<>"
      SumTy ty -> "<" <+> pretty ty <+> ">"
      FunTy arg eff ret -> pretty arg <+> "-{" <> pretty eff <> "}->" <+> pretty ret

parens p =
  if p >= 10
    then \f -> ('(' :) . f . (')' :)
    else id

instance Plated Type where
  plate f ty =
    case ty of
      -- We do not want to traverse eff looking for types.
      -- While it is a row, it doesn't actually contain types. Only the label is used.
      FunTy arg eff ret -> FunTy <$> f arg <*> pure eff <*> f ret
      RowTy row -> RowTy <$> traverse f row
      ProdTy ty -> ProdTy <$> f ty
      SumTy ty -> SumTy <$> f ty
      ty -> pure ty

instance TypeOf Type where
  typeOf = plate

typeVars :: Traversal' Type TVar
typeVars f ty =
  case ty of
    VarTy var -> VarTy <$> f var
    FunTy a (Open v) r -> FunTy a <$> (Open <$> f v) <*> pure r
    ty -> pure ty

typeVariables :: Type -> [TVar]
typeVariables ty = universe ty >>= toListOf typeVars

{- Returns true if tvar appears in type, false otherwise -}
occurs :: TVar -> Type -> Bool
occurs tvar = anyOf (plate . typeVars) (== tvar)

data InternalRow
  = Closed Row
  | Open TVar
  deriving (Show, Read, Eq, Ord)

instance Pretty InternalRow where
  pretty (Closed row) = Prettyprinter.group . align . vcat $ punctuate "," (fmap (\(lbl, ty) -> pretty lbl <+> "|>" <+> pretty ty) (Map.toAscList row))
  pretty (Open tvar) = pretty tvar

instance TypeOf InternalRow where
  typeOf f =
    \case
      Closed row -> Closed <$> traverse f row
      Open tvar -> pure (Open tvar)

internalRowTVars :: Traversal' InternalRow TVar
internalRowTVars f =
  \case
    Closed row -> Closed <$> traverse (typeVars f) row
    Open tvar -> Open <$> f tvar

rowOccurs :: TVar -> InternalRow -> Bool
rowOccurs tvar = anyOf internalRowTVars (== tvar)

rowToType :: InternalRow -> Type
rowToType (Closed row) = RowTy row
rowToType (Open tvar) = VarTy tvar
