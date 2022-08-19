{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Core where

import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map

import Constraint
import Control.Lens
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import GHC.Stack
import Prettyprinter
import Subst
import Term
import Type

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
  pretty (CoreV v ty) = pretty v <+> ":" <+> shortTy ty

shortVar :: CoreVar -> Doc ann
shortVar (CoreV v _) = pretty v

coreVarTy :: Lens' CoreVar CoreType
coreVarTy =
  lens
    (\(CoreV _ ty) -> ty)
    (\(CoreV v _) ty -> CoreV v ty)

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
      CoreProduct tys -> "{" <+> hcat (punctuate ("," <> space) (pretty <$> tys)) <+> "}"
      CoreForall tyvar ty -> "∀" <+> pretty tyvar <+> "." <+> pretty ty

shortTy =
  \case
    CoreVar (CoreTV tyvar _) -> pretty tyvar
    CoreLit lit -> pretty lit
    CoreFun arg eff ret -> shortTy arg <+> "-{" <> pretty eff <> "}->" <+> shortTy ret
    CoreProduct tys -> "{" <> hcat (punctuate ("," <> space) (shortTy <$> tys)) <> "}"
    CoreForall tyvar ty -> "∀" <+> pretty tyvar <+> "." <+> shortTy ty

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
      Core.App fn arg -> Core.App <$> f fn <*> f arg
      TyLam x body -> TyLam x <$> f body
      TyApp forall ty -> TyApp <$> f forall <*> pure ty
      Product elems -> Product <$> traverse f elems
      Project idx product -> Project idx <$> f product
      core -> pure core

instance Pretty Core where
  pretty =
    \case
      Core.Var var -> shortVar var
      Lit lit -> pretty lit
      Lam x body -> "λ" <+> pretty x <+> "." <+> pretty body
      Core.App fn arg -> "(" <> pretty fn <+> pretty arg <> ")"
      TyLam x body -> "Λ" <+> pretty x <+> "." <+> pretty body
      TyApp forall ty -> pretty forall <+> pretty ty
      Product elems -> "{" <> hcat (punctuate ("," <> space) (pretty <$> elems)) <> "}"
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

rowEvType :: InternalRow -> InternalRow -> InternalRow -> CoreType
rowEvType left right goal = 
  CoreProduct 
    [ CoreFun leftTy (Closed Map.empty) (CoreFun rightTy (Closed Map.empty) goalTy)
    , CoreProduct []
    , CoreProduct 
      [ CoreFun goalTy (Closed Map.empty) leftTy
      , CoreProduct []
      ]
    , CoreProduct 
      [ CoreFun goalTy (Closed Map.empty) rightTy
      , CoreProduct []
      ]
    ]
  where
    leftTy = fromType (ProdTy (rowToType left))
    rightTy = fromType (ProdTy (rowToType right))
    goalTy = fromType (ProdTy (rowToType goal))

rowEvidence :: Row -> Row -> Row -> Core
rowEvidence left right goal = Product [Core.concat left right, placeholder, Product [prjL left goal, placeholder], Product [prjR right goal, placeholder]]
 where
  -- Use unit for the sum type stuff so we still produce evidence with the right structure
  placeholder = Product []

prjR outRow inRow = Lam inVar out
 where
  inSize = Map.size inRow - 1
  outSize = Map.size outRow - 1
  out =
    case [(inSize - outSize) .. inSize] of
      -- Special case for singleton
      [_] -> Project 0 (Core.Var inVar) 
      idxs -> Product (fmap (\i -> Project i (Core.Var inVar)) idxs)
  inVar = CoreV (V 0) inTy
  inTy = fromType (ProdTy (RowTy inRow))

prjL outRow inRow = Lam inVar out
 where
  out =
    case [0..(Map.size outRow - 1)] of
      [_] -> Project 0 (Core.Var inVar) 
      idxs -> Product (fmap (\i -> Project i (Core.Var inVar)) idxs)
  inVar = CoreV (V 0) inTy
  inTy = fromType (ProdTy (RowTy inRow))

concat :: Row -> Row -> Core
concat left right = Lam m $ Lam n (Product splat)
 where
  m = CoreV (V 0) leftTy
  n = CoreV (V 1) rightTy
  splat = fmap (\i -> Project i (Core.Var m)) [0 .. (Map.size left - 1)] ++ fmap (\i -> Project i (Core.Var n)) [0 .. (Map.size right - 1)]
  leftTy = fromType (ProdTy (RowTy left))
  rightTy = fromType (ProdTy (RowTy right))

varSubst :: Map.Map Var Core.Core -> Core.Core -> Core.Core
varSubst env = transform
  (\case
    core@(Core.Var (CoreV v _)) -> fromMaybe core $ env !? v
    core -> core)
