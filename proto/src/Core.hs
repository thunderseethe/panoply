module Core where

import qualified Data.Map.Strict as Map

import Control.Lens
import Subst
import Term
import Type
import Data.Maybe (fromMaybe)

data Literal = I Int
  deriving (Show)

data LiteralTy = IntTy
  deriving (Show)

data Kind
  = KindType
  | KindRow
  deriving (Show)

data CoreVar = CoreV Var CoreType
  deriving (Show)

coreVarTy :: Lens' CoreVar CoreType
coreVarTy = lens 
  (\(CoreV _  ty) -> ty)  
  (\(CoreV v  _)  ty -> CoreV v ty)

data TypeVar = CoreTV TVar Kind
  deriving (Show)

coreTyTv tv = CoreTV tv KindType
coreRowTv tv = CoreTV tv KindRow

fromType :: Type -> CoreType
fromType ty =
  case ty of
    VarTy tv -> CoreVar (coreTyTv tv)
    Type.IntTy -> CoreLit Core.IntTy
    RowTy _ -> error "Can't translate this as is"
    ProdTy (RowTy row) -> CoreProduct (fromType <$> Map.elems row)
    ProdTy (VarTy tv) -> CoreVar (coreRowTv tv)
    ProdTy _ -> error "Invalid product type"
    FunTy arg eff ret -> CoreFun (fromType arg) eff (fromType ret)

data CoreType
  = CoreVar TypeVar
  | CoreLit LiteralTy
  | CoreFun {arg :: CoreType, _eff :: InternalRow, ret :: CoreType}
  | CoreProduct [CoreType]
  | CoreForall TypeVar CoreType
  deriving (Show)

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
      ( \core_ty ->
          case core_ty of
            CoreVar (CoreTV tv _) -> fromMaybe core_ty . fmap fromType $ Map.lookup tv map
            core_ty -> core_ty
      )

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
      
coreVars :: Traversal' Core CoreVar
coreVars f core =
    case core of
      Core.Var var -> Core.Var <$> f var
      Lam x body -> Lam <$> f x <*> pure body
      core -> pure core
