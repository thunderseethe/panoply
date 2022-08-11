{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Core where

import qualified Data.Map.Strict as Map

import Control.Lens
import Prettyprinter
import Subst
import Term
import Type
import qualified Data.Set as Set
import GHC.Stack
import Data.Maybe (fromMaybe)

data Literal = I Int
  deriving (Show)

instance Pretty Literal where
  pretty (I i) = pretty i


data LiteralTy = IntTy
  deriving (Show, Eq, Ord)

instance Pretty LiteralTy where
  pretty Core.IntTy = "IntTy"

data Kind
  = KindType
  | KindRow
  deriving (Show, Eq, Ord)

instance Pretty Kind where
  pretty KindType = "Type"
  pretty KindRow = "Row"

data CoreVar = CoreV Var CoreType
  deriving (Show, Eq, Ord)

instance Pretty CoreVar where
  pretty (CoreV v ty) = pretty v <+> ":" <+> pretty ty

coreVarTy :: Lens' CoreVar CoreType
coreVarTy = lens
  (\(CoreV _  ty) -> ty)
  (\(CoreV v  _)  ty -> CoreV v ty)

data TypeVar = CoreTV TVar Kind
  deriving (Show, Eq, Ord)

instance Pretty TypeVar where
  pretty (CoreTV tvar kind) = viaShow tvar <+> "::" <+> pretty kind

coreTyTv tv = CoreTV tv KindType
coreRowTv tv = CoreTV tv KindRow


data CoreType
  = CoreVar TypeVar
  | CoreLit LiteralTy
  | CoreFun CoreType InternalRow CoreType
  | CoreProduct [CoreType]
  | CoreForall TypeVar CoreType
  deriving (Show, Eq, Ord)

instance Pretty CoreType where
  pretty =
    \case
      CoreVar tyvar -> pretty tyvar
      CoreLit lit -> pretty lit
      CoreFun arg eff ret -> pretty arg <+> "-{" <> pretty eff <> "}->" <+> pretty ret
      CoreProduct tys -> "{" <+> hcat (pretty <$> tys) <+> "}"
      CoreForall tyvar ty -> "∀" <+> pretty tyvar <+> "." <+> pretty ty

instance Plated CoreType where
  plate f core_ty =
    case core_ty of
      CoreFun arg eff ret -> CoreFun <$> f arg <*> pure eff <*> f ret
      CoreProduct tys -> CoreProduct <$> traverse f tys
      CoreForall tv typ -> CoreForall tv <$> f typ
      ty -> pure ty

instance SubstApp CoreType where
  apply (Subst map) =
    transform
      ( \case
            core_ty@(CoreVar (CoreTV tv _)) -> fromMaybe core_ty $ Map.lookup tv map >>= toType
            core_ty -> core_ty
      )

fromType :: HasCallStack => Type -> CoreType
fromType ty =
  case ty of
    VarTy tv -> CoreVar (coreTyTv tv)
    Type.IntTy -> CoreLit Core.IntTy
    RowTy _ -> error "Can't translate this as is"
    ProdTy (RowTy row) -> CoreProduct (fromType <$> Map.elems row)
    ProdTy (VarTy tv) -> CoreVar (coreRowTv tv)
    ProdTy _ -> error "Invalid product type"
    FunTy arg eff ret -> CoreFun (fromType arg) eff (fromType ret)

toType :: Type -> Maybe CoreType
toType ty = 
  case ty of
    VarTy tv -> Just $ CoreVar (coreTyTv tv)
    Type.IntTy -> Just $ CoreLit Core.IntTy
    RowTy _ -> Nothing
    ProdTy (RowTy row) -> Just $ CoreProduct (fromType <$> Map.elems row)
    ProdTy (VarTy tv) -> Just $ CoreVar (coreRowTv tv)
    ProdTy _ -> error "Invalid product type"
    FunTy arg eff ret -> Just $ CoreFun (fromType arg) eff (fromType ret)


data Core
  = Var CoreVar
  | Lit Literal
  | Lam CoreVar Core
  | App Core Core
  | TyLam TypeVar Core
  | TyApp Core CoreType
  | Product [Core]
  | Project Int Core
  deriving (Show)

instance Plated Core where
  plate f core =
    case core of
      Lam x body -> Lam x <$> f body
      Core.App fn arg ->  Core.App <$> f fn <*> f arg
      TyLam x body -> TyLam x <$> f body
      TyApp forall ty -> TyApp <$> f forall <*> pure ty
      Product elems -> Product <$> traverse f elems
      Project idx product -> Project idx <$> f product
      core -> pure core

instance Pretty Core where
  pretty = 
    \case
      Core.Var var -> pretty var
      Lit lit -> pretty lit
      Lam x body -> "λ" <+> pretty x <+> "." <+> pretty body
      Core.App fn arg -> pretty fn <+> pretty arg
      TyLam x body -> "Λ" <+> pretty x <+> "." <+> pretty body
      TyApp forall ty -> pretty forall <+> pretty ty
      Product elems -> "{" <+> hcat (punctuate ("," <> space) (pretty <$> elems)) <+> "}"
      Project idx core -> "π" <> pretty idx <+> pretty core

coreVars :: Traversal' Core CoreVar
coreVars f core =
    case core of
      Core.Var var -> Core.Var <$> f var
      Lam x body -> Lam <$> f x <*> pure body
      core -> pure core

coreBoundVars :: Traversal' Core CoreVar
coreBoundVars f core =
    case core of
      Lam x body -> Lam <$> f x <*> pure body
      c -> pure c

coreUnboundVars :: Core -> Set.Set CoreVar
coreUnboundVars core = allVars `Set.difference` boundVars
  where
    allVars = foldrOf (cosmos . coreVars) Set.insert Set.empty core
    boundVars = foldrOf (cosmos . coreBoundVars) Set.insert Set.empty core

