{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Lib where

import Fresh

import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Text
import Control.Effect.State
import Control.Lens (Traversal')
import Control.Lens.Plated
import Data.Maybe


newtype Label = Lbl Text
  deriving (Eq)

data Row a tvar 
  = Mono (Map Label a)
  | Poly tvar
  deriving (Eq)

emptyRow :: Row a tvar
emptyRow = Mono Map.empty

data Effect tvar = Effect Label [Ty tvar]
  deriving (Eq)

type TyRow tvar = Row (Ty tvar) tvar
type EffRow tvar = Row (Effect tvar) tvar

data Evidence tvar
  = ConcatEquals (Ty tvar) (Ty tvar) (Ty tvar)

data Ty tvar
  = IntTy
  | VarTy tvar
  | TyApp (Ty tvar) (Ty tvar)
  | FunTy (Ty tvar) (EffRow tvar) (Ty tvar)
  | ProdTy (TyRow tvar)
  | SumTy (TyRow tvar)
  deriving (Eq)

typeVars :: Traversal' (Ty tvar) tvar
typeVars f ty = 
  case ty of
    VarTy tvar -> VarTy <$> f tvar
    ty -> pure ty

instance Plated (Ty tvar) where
  plate f ty = 
    case ty of
      TyApp fn arg -> TyApp <$> f fn <*> f arg
      FunTy arg effs ret -> FunTy <$> f arg <*> pure effs <*> f ret
      ProdTy (Mono row) -> ProdTy . Mono <$> traverse f row
      SumTy (Mono row) -> SumTy . Mono <$> traverse f row
      ty -> pure ty


data Scheme tvar = Scheme [tvar] [Evidence tvar] (Ty tvar)
  {-
  forall f a b.  (a -> b) -> f a -> f b
  forall (f :: Type -> Type) (g :: Type -> Type) . f a -> g a -> Compose f g a
  data Compopse f g a = Compose (f (g a))
  (forall a . a) b
  -}

data Term var tvar
  -- Lambda Calc Stuff
  = Var var
  | Int Int
  | Lam var (Term var tvar)
  | App (Term var tvar) (Term var tvar)
  | Ann (Term var tvar) (Ty tvar)
  -- Row term construction stuff
  -- Singleton rows
  | Label Label (Term var tvar)
  | Unlabel Label (Term var tvar)
  -- Product intro and elim
  | RecordConcat (Term var tvar) (Term var tvar)
  | RecordPrj (Term var tvar)
  -- TODO: Add terms for sum intro and elim
  -- Algebraic Effect terms
  | EffectOp { op :: Label, {- this will be a product row for multi args -} args :: Term var tvar }

-- TODO: Decl type for top level terms that can use Schemes (functions, trait(s), etc.)

-- Int -> Int -> Int
-- Functor f => Functor g => ...
-- (Functor f, Functor g) => ...
-- Is this multiple type lambdas? or is it taking a product of evidence
{-
-- | A fix-point type.
newtype Prelude.Functor f => Fix (f :: Type -> Type) = Fix { unFix :: f (Fix f) }
  --deriving (Generic)

data ListF a b = Cons a b | Nil

class Functor (f :: Type -> Type) where
    fmap :: (a -> b) -> f a -> f b
-}


------------------------------
-- **** Type inference **** --
------------------------------

newtype TVar = TV Int
  deriving (Ord, Eq, Num, Enum, Show, Bounded)

newtype Var = V Int
  deriving (Ord, Eq, Num, Enum, Show, Bounded)

data Ctx = Ctx { vars :: Map Var (Ty TVar), tys :: Map TVar (Ty TVar) }

binds :: Ctx -> Var -> Maybe (Ty TVar)
binds (Ctx vars _) x = vars !? x

bind :: Var -> Ty TVar -> Ctx -> Ctx
bind x ty (Ctx vars tys) = Ctx (Map.insert x ty vars) tys

solves :: Ctx -> TVar -> Maybe (Ty TVar)
solves (Ctx _ tys) a = tys !? a

solve :: TVar -> Ty TVar -> Ctx -> Ctx
solve a ty (Ctx vars tys) = Ctx vars (Map.insert a ty tys)

withScope :: (Has (State Ctx) sig m) => m a -> m a
withScope action = do
  ctx <- get 
  res <- action
  put ctx
  return res

newtype Subst = Subst { _subst :: Map TVar (Ty TVar) }

emptySubst :: Subst
emptySubst = Subst Map.empty

applySubst :: Subst -> Ty TVar -> Maybe (Ty TVar)
applySubst (Subst subst) ty = 
  let varsOverlap = Prelude.all (\var -> not $ Map.member var subst) (cosmosOn typeVars ty)
   in if varsOverlap
     then Nothing
     else Just $ transform 
        (\case
            VarTy a -> fromMaybe (VarTy a) (subst !? a)
            ty -> ty) ty

mergeSubst :: Subst -> Subst -> Maybe Subst
mergeSubst (Subst left) (Subst right) =
  if Map.disjoint left right
     then Just . Subst $ Map.union left right
     else Nothing

unify :: (Has (State Ctx) sig m, Has (Fresh TVar) sig m) 
       => Ty TVar -> Ty TVar -> m (Maybe Subst)
unify a b = go [(a, b)] emptySubst
  where
    go [] subst = return $ Just subst
    go (unity : work) subst = 
      case unity of
        (a, b) | a == b -> go work subst
        (FunTy a_arg a_effs a_ret, FunTy b_arg b_effs b_ret) -> 
          -- TODO: acutally do this 
          undefined


  {-  case (a, b) of 
    (a, b) | a == b -> return (Just emptySubst)
    (FunTy a_arg a_effs a_ret, FunTy b_arg b_effs b_ret) -> do
      Just arg_subst <- unify a_arg b_arg 
      Just a_ret <- return $ applySubst arg_subst a_ret
      Just b_ret <- return $ applySubst arg_subst b_ret
      Just ret_subst <- unify a_ret b_ret
      -- TODO: handle effects
      return (mergeSubst arg_subst ret_subst)

    (_, _) -> return Nothing -}

infer :: 
      (Has (State Ctx) sig m, Has (Fresh TVar) sig m, MonadFail m) 
      => Term Var TVar -> m (Ty TVar, Subst)
infer term =
  case term of
    Var x -> do
      ctx <- get
      case ctx `binds` x of
        Just ty -> return (ty, emptySubst)
        Nothing -> error ("do something better here: undefined variable " ++ show x)
    Int _ -> return (IntTy, emptySubst)
    Lam x body ->
      withScope $ do
        unify <- fresh
        let arg_var = VarTy unify
        modify (bind x arg_var)
        -- Todo this has to return the effects body uses
        (ret_ty, subst) <- infer body 
        Just arg_ty <- return $ applySubst subst arg_var
        return (FunTy arg_ty emptyRow ret_ty, subst)
    App fn arg -> do
      (fn_ty, fn_sub) <- infer fn
      (arg_ty, arg_sub) <- infer arg
      Just ty <- return $ applySubst fn_sub fn_ty
      (fn_arg, fn_effs, fn_ret) <- case ty of
        FunTy arg effs ret -> return (arg, effs, ret)
        _ -> error "do something better here: expected a fn type"
      unify fn_arg arg_ty
      -- Todo: Fully apply fn_ty and then unpack into composite pieces.
      -- Unify fn_ty argument with arg_ty
      undefined
    Ann term expect_ty -> do
      (infer_ty, sub) <- infer term
      maybe_subst <- unify infer_ty expect_ty
      let out = do 
              subst <- maybe_subst
              ty <- applySubst subst expect_ty
              return (ty, subst)
      case out of
        Just out -> return out
        Nothing -> error "do something better here: couldn't unify annotated type"


    
