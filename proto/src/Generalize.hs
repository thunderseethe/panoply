{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Generalize where

import Fresh

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map.Strict as Map

import qualified Data.List as List

import Control.Algebra
import Control.Lens
import qualified Data.Map.Merge.Strict as Map
import Data.Maybe
import Debug.Trace

newtype TVar = TV Int
  deriving (Ord, Eq, Num, Enum, Show, Bounded)

newtype Var = V Int
  deriving (Ord, Eq, Num, Enum, Show, Bounded)

{- Our lambda calc + let + int to allow for let generalization and a meaningful base type in examples -}
data Term
  = Var Var
  | Int Int
  | Abs Var Term
  | Term :@ Term
  | Let {_var :: Var, _defn :: Term, _body :: Term}

instance Plated Term where
  plate f =
    \case
      Abs arg body -> Abs arg <$> f body
      fn :@ arg -> (:@) {- oof -} <$> f fn <*> f arg
      Let var defn body -> Let var <$> f defn <*> f body
      ty -> pure ty

data Type
  = IntTy
  | VarTy TVar
  | FunTy Type Type
  deriving (Eq, Ord)

instance Show Type where
  showsPrec _ IntTy = (++) "Int"
  showsPrec p (VarTy (TV tv)) = ("VarTy " ++) . showsPrec p tv
  showsPrec p (FunTy arg ret) = showsPrec p arg . (" -> " ++) . showsPrec p ret

instance Plated Type where
  plate f =
    \case
      FunTy arg ret -> FunTy <$> f arg <*> f ret
      ty -> pure ty

typeVars :: Traversal' Type TVar
typeVars f =
  \case
    VarTy var -> VarTy <$> f var
    ty -> pure ty

{- The unbound variables that appear in a type
   Since we only bind type variables in a scheme this will be all variables in the type. -}
freeVars :: Type -> Set TVar
freeVars = Set.fromList . toListOf (plate . typeVars)

data Scheme = Scheme [TVar] Type
  deriving (Show, Eq, Ord)

{- Constraints generated during type inference that must hold for the program to typecheck -}
data Constraint
  = -- Two types must be equal (aka unifiable)
    Type :== Type
  | -- Type must be a concrete instance of the scheme for some substitution of bound type variable
    Type :<= Scheme
  | -- Implict instantiation constraint
    -- The first type must be an instance of the second type once it is generalized to a scheme.
    -- When the second type is generalized to a scheme it does not bind any of the variables in Set TVar.
    -- This set represents the set of variables that were in scope (already bound) when the constraint was generated.
    Generalize Type (Set TVar) Type
  deriving (Eq)

{- We use an explicit Ord instance on constraint to order how constraints are solved explictly. -}
instance Ord Constraint where
  compare (t1_left :== t2_left) (t1_right :== t2_right) = compare t1_left t1_right <> compare t2_left t2_right
  -- We want :== to be solved first so they are less than other constraints
  compare (_ :== _) _ = LT
  compare (Generalize t1_left _ t2_left) (Generalize t1_right _ t2_right) = compare t1_left t2_left <> compare t1_right t2_right
  -- Generalize sits between the other two constraints
  compare (Generalize{}) (_ :== _) = GT
  compare (Generalize{}) (_ :<= _) = LT
  compare (t_left :<= σ_left) (t_right :<= σ_right) = compare t_left t_right <> compare σ_left σ_right
  -- We want these to be solved last so they are greater than other constraints
  compare (_ :<= _) _ = GT

instance Show Constraint where
  showsPrec p (t1 :== t2) = showsPrec p t1 . (" ≡ " ++) . showsPrec p t2
  showsPrec p (ty :<= σ) = showsPrec p ty . (" ≦ " ++) . showsPrec p σ
  showsPrec p (Generalize t1 monos t2) = showsPrec p t1 . (" <=" ++) . fmt monos . (' ' :) . showsPrec p t2
   where
    fmt tvars s = '{' : foldr (\(TV tv) s -> showsPrec p tv (", " ++ s)) ('}' : s) tvars

constraintType :: Traversal' Constraint Type
constraintType f =
  \case
    t1 :== t2 -> (:==) <$> f t1 <*> f t2
    t1 :<= (Scheme bound ty) -> (:<=) <$> f t1 <*> (Scheme bound <$> f ty)
    Generalize t1 monos t2 -> Generalize <$> f t1 <*> pure monos <*> f t2

{- Tracks the types we've assigned to term variables during inference.
   We can assume a term variable has any number of types as long as they all end up unifying. -}
newtype Assumptions = Assumptions (Map Var (Set Type))
  deriving (Show)

singleton :: Var -> Type -> Assumptions
singleton var ty = Assumptions (Map.singleton var (Set.singleton ty))

(!) :: Assumptions -> Var -> Set Type
(Assumptions map) ! var = fromMaybe Set.empty $ Map.lookup var map

(//) :: Assumptions -> Var -> Assumptions
(Assumptions map) // var = Assumptions $ Map.delete var map

instance Semigroup Assumptions where
  (<>) (Assumptions left) (Assumptions right) =
    Assumptions $
      Map.merge
        Map.preserveMissing
        Map.preserveMissing
        (Map.zipWithMatched (\_ x y -> x <> y))
        left
        right

instance Monoid Assumptions where
  mempty = Assumptions Map.empty

{- This is the BottomUp algorithm from the paper -}
generateConstraints :: (Has (Fresh TVar) sig m) => Term -> m (Type, Set Constraint, Assumptions)
generateConstraints term = over _2 Set.fromList <$> go Set.empty term
 where
  -- mono_tvars is the set of type variables in scope (from Abs) tracked top down as we recurse.
  go mono_tvars = \case
    -- For a variable we assign it a fresh type variable and assume it has that type
    Var x -> do
      beta <- VarTy <$> fresh
      return (beta, [], singleton x beta)

    -- Int literal is IntTy
    Int _ -> return (IntTy, [], mempty)

    -- Application generates a fresh type variable for the return of the application.
    -- We then infer function type and argument type and constrain the function type to return our fresh type variable
    fn :@ arg -> do
      beta <- VarTy <$> fresh
      (fn_ty, fn_constr, fn_assume) <- go mono_tvars fn
      (arg_ty, arg_constr, arg_assume) <- go mono_tvars arg
      return (beta, fn_constr <> arg_constr <> [fn_ty :== FunTy arg_ty beta], fn_assume <> arg_assume)

    Abs x body -> do
      beta' <- fresh
      let beta = VarTy beta'
      (ty, constr, assume) <- go (Set.insert beta' mono_tvars) body
      let arg_eqs = fmap (:== beta) . Set.toList $ assume ! x
      return (FunTy beta ty, constr <> arg_eqs, assume // x)

    -- Let generalization
    Let x defn body -> do
      (defn_ty, defn_constr, defn_assume) <- go mono_tvars defn
      (body_ty, body_constr, body_assume) <- go mono_tvars body
      let generalizes x_ty = Generalize x_ty mono_tvars defn_ty
      let local_generals = fmap generalizes . Set.toList $ body_assume ! x
      return (body_ty, defn_constr <> body_constr <> local_generals, defn_assume <> body_assume // x)

newtype Subst = Subst (Map TVar Type)

instance Show Subst where
  -- pointfree, more like... pointless
  showsPrec p (Subst map) = ('[' :) . (\s -> Map.foldrWithKey go s map) . (']' :)
   where
    go (TV tvar) ty = showsPrec p tvar . (" := " ++) . showsPrec p ty . (", " ++)

instance Semigroup Subst where
  {- This turns out to be really important!
     When we merge in a new substitution (on the right) we have to apply
     all prior substitutions (on the left) to the right types. Otherwise our substitution
     won't be the fixpoint and we'd have to apply it repeatedly. -}
  l@(Subst left) <> Subst right =
    Subst $
      Map.merge
        Map.preserveMissing -- for left types use as is
        -- for any right's that aren't ovewritten apply the substitution till now to them
        (Map.mapMissing (\_ -> apply l))
        (Map.zipWithMatched (\_ _ r -> r))
        left
        right

instance Monoid Subst where
  mempty = Subst Map.empty

isEmpty :: Subst -> Bool
isEmpty (Subst map) = Map.null map

insert :: TVar -> Type -> Subst -> Subst
insert a ty (Subst map) = Subst $ Map.insert a ty map

difference :: Foldable f => Subst -> f TVar -> Subst
difference (Subst map) = Subst . foldr Map.delete map

{- Apply a substitution to a type, replacing each type variable present in the substitution by it's mapped type -}
apply :: Subst -> Type -> Type
apply (Subst map) =
  transform
    ( \case
        VarTy var -> fromMaybe (VarTy var) (Map.lookup var map)
        ty -> ty
    )

{- Used to substitute the monomorphized type variables of the implicit instantiate constraint.
   Because of this we only want to map type variables to other type variables ignoring other mappings. -}
applyTVar :: Subst -> TVar -> TVar
applyTVar (Subst map) tvar =
  case Map.lookup tvar map of
    Just (VarTy tvar') -> tvar'
    _ -> tvar

applyConstr :: Subst -> Constraint -> Constraint
applyConstr subst =
  let sub = apply subst
   in \case
        t1 :== t2 -> sub t1 :== sub t2
        t1 :<= (Scheme bound ty) ->
          let boundSubst = subst `difference` bound
           in sub t1 :<= Scheme bound (apply boundSubst ty)
        Generalize t1 monos t2 -> Generalize (sub t1) (Set.map (applyTVar subst) monos) (sub t2)

{- Given a type, create a type scheme that captures all of it's free variables -}
generalize :: Set TVar -> Type -> Scheme
generalize ctx ty = trace ("generalize(" ++ show ctx ++ ", " ++ show ty ++ ") = " ++ show (Scheme vars ty)) Scheme vars ty
 where
  vars = Set.toList (Set.difference (freeVars ty) ctx)

{- Given a scheme, create a type with fresh type variables substituted for all the scheme's bound variables -}
instantiate :: (Has (Fresh TVar) sig m) => Scheme -> m Type
instantiate (Scheme bound ty) = do
  freshVars <- mapM (\var -> (,) var . VarTy <$> fresh) bound
  let subst = foldr (\(var, ty) subst -> insert var ty subst) mempty freshVars
  return (trace (show subst) $ apply subst ty)

{- The active type variables of a constraint. These are used to ensure we don't generalize any type variables that have unsolved constraints in the worklist. -}
activeVars :: Constraint -> Set TVar
activeVars (t1 :== t2) = freeVars t1 <> freeVars t2
activeVars (Generalize t1 monos t2) = freeVars t1 <> Set.intersection monos (freeVars t2)
activeVars (t1 :<= (Scheme bound t2)) = freeVars t1 <> Set.difference (freeVars t2) (Set.fromList bound)

solveConstraints :: (Has (Fresh TVar) sig m) => Set Constraint -> m Subst
solveConstraints = solve . Set.toAscList
 where
  -- This method only works on a list to allow for processing one element at a time while modifying the worklist.
  -- Conceptually it is a set and anytime we remove a constraint we remove the minimal constraint (first in the list).
  -- To maintain this invariant anytime we insert into the list we insert the element in order, not at head.
  --
  -- If we don't do this and naively insert new cosntraints at head to be immediately solved we can produce nonsense results
  -- from solving instantiate constraints before all equivalence constraints.
  solve [] = return mempty
  solve (constr : work) =
    case constr of
      t1 :== t2 -> do
        let unifier = mgu t1 t2
        substdWork <- solve (over (traverse . constraintType) (apply unifier) work)
        return (substdWork <> unifier)
      t1 :<= σ -> do
        t2 <- instantiate σ
        -- NB: It's important we insert this in order (not at head of list)
        solve (List.insert (t1 :== t2) work)
      Generalize t1 monos t2 ->
        if Set.null $ Set.intersection (Set.difference (freeVars t2) monos) (Set.unions (activeVars <$> work))
          then -- Same here, we have to insert the fresh constraint in order
            solve (List.insert (t1 :<= generalize monos t2) work)
          else error "Our constraints aren't solvable"

{- Checks a type variable does not appear free in a type -}
occurs :: TVar -> Type -> Bool
occurs x = anyOf (plate . typeVars) (== x)

{- The most general unifier between two types.
   Will crash with an error if types can't unify or are circular -}
mgu :: Type -> Type -> Subst
-- Special case if we unify two of the same type variable don't produce a superfluous substitution
mgu (VarTy x) (VarTy y) | x == y = mempty
mgu (VarTy x) t =
  if x `occurs` t
    then error "mgu: circularity"
    else Subst (Map.singleton x t)
mgu t (VarTy y) =
  if y `occurs` t
    then error "mgu: circularity"
    else Subst (Map.singleton y t)
mgu (FunTy left_arg left_ret) (FunTy right_arg right_ret) =
  let arg_mgu = mgu left_arg right_arg
      ret_mgu = mgu left_ret right_ret
   in if isEmpty arg_mgu || isEmpty ret_mgu
        then error "Failed to unify function types"
        else arg_mgu <> ret_mgu
mgu IntTy IntTy = mempty
mgu IntTy (FunTy _ _) = error "Can't unify int and function type"
mgu (FunTy _ _) IntTy = error "Can't unify int and function type"

example :: Term
example =
  Let
    { _var = id
    , _defn = Abs x (Let y (Var x) (Var y))
    , _body = Var id :@ Var id
    }
 where
  id = V 0
  x = V 1
  y = V 2

testSet :: Set Constraint
testSet =
  Set.fromList
    [ VarTy (TV 2) :== FunTy IntTy (VarTy (TV 3))
    , Generalize (VarTy (TV 4)) (Set.singleton (TV 5)) (VarTy (TV 3))
    , Generalize (VarTy (TV 2)) (Set.singleton (TV 5)) (VarTy (TV 1))
    , VarTy (TV 5) :== VarTy (TV 1)
    ]

infer :: Term -> (Type, Subst)
infer t = snd . runIdentity . runFresh (TV 0) $ do
  (ty, constr, _) <- generateConstraints t
  -- TODO: create instance constraints for assumptions <= top level decls
  -- Or throw an undefined error if an assumption isn't at top level
  subst <- solveConstraints constr
  return (apply subst ty, subst)
