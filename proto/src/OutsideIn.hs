{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module OutsideIn where

import Fresh

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map, (!?))
import qualified Data.Map.Strict as Map

import Control.Algebra
import Control.Effect.Reader
import Control.Lens
import qualified Data.Map.Merge.Strict as Map
import Data.Maybe

import Control.Effect.State
import Control.Effect.Throw
import Data.Bifunctor
import Data.Either
import Data.List (nub, partition)
import Prelude hiding (interact)
import Control.Carrier.State.Strict (runState)
import Control.Carrier.Throw.Either
import Control.Carrier.Reader
import Debug.Trace

newtype TVar = TV Int
  deriving (Ord, Eq, Num, Enum, Show, Bounded)

newtype Var = V Int
  deriving (Ord, Eq, Num, Enum, Show, Bounded)

{- Our lambda calc + let + int to allow for let generalization and a meaningful base type in examples -}
data Term
  = -- Variable
    Var Var
  | -- Int literal
    Int Int
  | -- Lambda Abstraction
    Abs Var Term
  | -- Application
    Term :@ Term

data Type
  = VarTy TVar
  | IntTy
  | FunTy Type Type
  deriving (Eq)

instance Ord Type where
  compare (VarTy a) (VarTy b) = compare a b
  compare (VarTy _) _ = LT
  compare IntTy (VarTy _) = GT
  compare IntTy IntTy = EQ
  compare IntTy (FunTy _ _) = LT
  compare (FunTy a_arg a_ret) (FunTy b_arg b_ret) = compare a_arg b_arg <> compare a_ret b_ret
  compare (FunTy _ _) IntTy = GT
  compare (FunTy _ _) (VarTy _) = GT

instance Show Type where
  showsPrec p (VarTy (TV tv)) = ("VarTy " ++) . showsPrec p tv
  showsPrec _ IntTy = ("Int" ++)
  showsPrec p (FunTy arg ret) = parens (showsPrec 11 arg . (" -> " ++) . showsPrec 9 ret)
    where
      parens = 
        if p >= 10
           then \f -> ('(' :) . f . (')' :)
           else id

instance Plated Type where
  plate f ty =
    case ty of
      FunTy arg ret -> FunTy <$> f arg <*> f ret
      ty -> pure ty

typeVars :: Traversal' Type TVar
typeVars f ty =
  case ty of
    VarTy var -> VarTy <$> f var
    ty -> pure ty

{- The unbound variables that appear in a type
   Since we only bind type variables in a scheme this will be all variables in the type. -}
freeVars :: Type -> Set TVar
freeVars = Set.fromList . toListOf (plate . typeVars)

{- Returns true if tvar appears in type, false otherwise -}
occurs :: TVar -> Type -> Bool
occurs tvar = anyOf typeVars (== tvar)

data Scheme = Scheme [TVar] [Q] Type
  deriving (Show, Eq, Ord)

monoScheme :: Type -> Scheme
monoScheme = Scheme [] []

-- Need a better name for this
data Q
  = -- Two types must be equal (aka unifiable)
    Type :~ Type
  deriving (Show, Eq, Ord)

qTys :: Traversal' Q Type
qTys f q =
  case q of
    -- Is doing builtin recursion like this bad?
    t1 :~ t2 -> (:~) <$> f t1 <*> f t2

{- Constraints generated during type inference that must hold for the program to typecheck -}
data Constraint
  = Simp Q
  | Impl Implication
  deriving (Eq, Show)

data Implication = Imp {_exists :: [TVar], _prop :: Q, _implies :: Constraint}
  deriving (Eq, Show)

makeLenses ''Implication

constraintTypes :: Traversal' Constraint Type
constraintTypes f constr =
  case constr of
    Simp q -> Simp <$> qTys f q
    constr -> pure constr

constraintQs :: Traversal' Constraint Q
constraintQs f (Simp q) = Simp <$> f q
constraintQs _ (Impl i) = pure (Impl i)

constraintImpls :: Traversal' Constraint Implication
constraintImpls _ (Simp q) = pure (Simp q)
constraintImpls f (Impl i) = Impl <$> f i

simple :: Traversable f => Traversal' (f Constraint) Q
simple = traverse . constraintQs

impl :: Traversable f => Traversal' (f Constraint) Implication
impl = traverse . constraintImpls

type Ctx = Map Var Scheme

emptyCtx :: Ctx
emptyCtx = Map.empty

newtype Subst = Subst (Map TVar Type)

instance Show Subst where
  -- pointfree, more like... pointless
  showsPrec p (Subst map) = ('[' :) . (\s -> Map.foldrWithKey go s map) . (']' :)
   where
    go (TV tvar) ty = showsPrec p tvar . (" := " ++) . showsPrec p ty . (", " ++)

tvar |-> ty = Subst (Map.singleton tvar ty)

instance Semigroup Subst where
  (Subst left) <> (Subst right) =
    Subst $
      Map.merge
        Map.preserveMissing
        Map.preserveMissing
        (Map.zipWithMatched (\_ _ _ -> error "You fucked up, buck"))
        left
        right

instance Monoid Subst where
  mempty = Subst Map.empty

insert :: TVar -> Type -> Subst -> Subst
insert a ty (Subst map) = Subst $ Map.insert a ty map

apply :: Subst -> Type -> Type
apply (Subst map) =
  transform
    ( \ty -> case ty of
        VarTy var -> fromMaybe (VarTy var) (Map.lookup var map)
        ty -> ty
    )

generateConstraints :: (Has (Reader Ctx) sig m, Has (Fresh TVar) sig m) => Term -> m (Type, [Constraint])
generateConstraints term =
  case term of
    Var x -> do
      ctx <- ask
      case ctx !? x of
        Nothing -> error ("Undefined variable " ++ show x)
        Just (Scheme bound constr ty) -> do
          freshVars <- mapM (\var -> (,) var . VarTy <$> fresh) bound
          let subst = foldr (\(var, ty) subst -> insert var ty subst) mempty freshVars
          return (apply subst ty, Simp <$> over (traverse . qTys) (apply subst) constr)
    Int _ -> do
      return (IntTy, [])
    fn :@ arg -> do
      alpha <- fresh
      (fn_ty, fn_constr) <- generateConstraints fn
      (arg_ty, arg_constr) <- generateConstraints arg
      return (VarTy alpha, Simp (fn_ty :~ FunTy arg_ty (VarTy alpha)) : fn_constr ++ arg_constr)
    Abs x body -> do
      alpha <- fresh
      (ret_ty, constr) <- local (Map.insert x (monoScheme $ VarTy alpha)) (generateConstraints body)
      return (FunTy (VarTy alpha) ret_ty, constr)

data TyErr
  = OccursCheckFailed TVar Type

addUnifier :: TVar -> ([TVar], Subst) -> ([TVar], Subst)
addUnifier tvar (tvars, subst) = (tvar:tvars, subst)

mergeSubst :: Subst -> ([TVar], Subst) -> ([TVar], Subst)
mergeSubst new_sub (tvars, old_sub) = (tvars, new_sub <> old_sub)

{- canonicalize a wanted constraint -}
canonw ::
  ( Has (Fresh TVar) sig m
  , Has (State ([TVar], Subst)) sig m
  , Has (Throw ()) sig m
  ) =>
  Q ->
  m (Either CanonCt [Q])
canonw (VarTy tvar :~ FunTy (FunTy a b) ret) = do
  beta <- fresh
  modify (addUnifier beta)
  return $ Right [VarTy tvar :~ FunTy (VarTy beta) ret, FunTy a b :~ VarTy beta]
canonw (VarTy tvar :~ FunTy arg (FunTy a b)) = do
  beta <- fresh
  modify (addUnifier beta)
  return $ Right [VarTy tvar :~ FunTy arg (VarTy beta), FunTy a b :~ VarTy beta]
-- If not a wanted specific case delegate to generic canon
canonw c = canon c

{- canonicalize a given constraint -}
canong :: (Has (Fresh TVar) sig m, Has (State ([TVar], Subst)) sig m, Has (Throw ()) sig m) => Q -> m (Either CanonCt [Q])
canong (VarTy tvar :~ FunTy (FunTy a b) ret) = do
  beta <- fresh
  modify (mergeSubst (beta |-> FunTy a b))
  return $ Right [VarTy tvar :~ FunTy (VarTy beta) ret, FunTy a b :~ VarTy beta]
canong (VarTy tvar :~ FunTy arg (FunTy a b)) = do
  beta <- fresh
  modify (mergeSubst (beta |-> FunTy a b))
  return $ Right [VarTy tvar :~ FunTy arg (VarTy beta), FunTy a b :~ VarTy beta]
-- If not a given specific case delegate to generic canon
canong c = canon c

{- canonicalize a generic constraint. this function preforms rewrites that are the same for wanted and given constraints -}
canon :: (Has (Throw ()) sig m) => Q -> m (Either CanonCt [Q])
canon (FunTy a_arg a_ret :~ FunTy b_arg b_ret) = return $ Right [a_arg :~ b_arg, a_ret :~ b_ret]
canon (a :~ b)
  | a == b = trace ("reflexive case: deleted " ++ show (a :~ b)) $ return $ Right []
  | b < a = return $ Right [b :~ a]
-- If we fail an occurs check throw to break out of cannon
canon (VarTy tvar :~ ty)
  | occurs tvar ty = throwError ()
  -- We know this is canon because if not it would have hit the b < a case above
  | otherwise = return $ Left $ Ct tvar ty
-- These shouldn't be required but doesn't hurt
canon (FunTy arg ret :~ VarTy tvar) = return $ Right [VarTy tvar :~ FunTy arg ret]
canon (IntTy :~ VarTy tvar) = return $ Right [VarTy tvar :~ IntTy]
canon (IntTy :~ IntTy) = return $ Right []
-- These are un-unifiable tys
canon (FunTy _ _ :~ IntTy) = throwError ()
canon (IntTy :~ FunTy _ _) = throwError ()

{- A canonical constraint.
   This is a tvar lhs equal to a type that does not contain tvar -}
data CanonCt
  = Ct TVar Type
  deriving (Eq, Show)

{- binary interaction between two constraints from the same set (wanted or given) -}
interact :: Interaction -> [Q]
interact i =
  -- The paper covers other rules but they are for type classes and type families
  case i of
    -- EQSAME
    InteractEq (Ct a_tv a_ty) (Ct _ b_ty) -> [VarTy a_tv :~ a_ty, a_ty :~ b_ty]
    -- EQDIFF
    InteractOccurs (Ct a_tv a_ty) (Ct b_tv b_ty) -> [VarTy a_tv :~ a_ty, VarTy b_tv :~ apply (a_tv |-> a_ty) b_ty]


{- binary interaction between constraints from each set (wanted and given).
  Because of this order is important, and the returned constraint is a wanted (replacing the input wanted constraint) -}
simplify :: Interaction -> Q
simplify i =
    -- These are basically the same rules as interact but because they combine a given with a wanted we're allowed to simplify further
    -- Basically saturating an assumption with an inference.
    -- Like interact there are other rules for type families and type classes that we leave out for now
    case i of
      -- SEQSAME
      InteractEq (Ct _ given_ty) (Ct _ wanted_ty) -> given_ty :~ wanted_ty
      -- SEQDIFF
      InteractOccurs (Ct given_tv given_ty) (Ct wanted_tv wanted_ty) -> VarTy wanted_tv :~ apply (given_tv |-> given_ty) wanted_ty

data Interaction
  = -- The canonical constraints are for the same tvar
    InteractEq CanonCt CanonCt
  | -- The tvar of the first canonical constraint occurs in the type of the second
    InteractOccurs CanonCt CanonCt

{- Find pairs of interactable constraints -}
interactions :: [CanonCt] -> ([Interaction], [CanonCt])
interactions canons = second (nub . mconcat) . partitionEithers $ mkInteraction <$> cartesian
 where
  -- This is crazy slow but that's fine for now
  cartesian = [(a_ct, b_ct) | a_ct <- canons, b_ct <- canons, a_ct /= b_ct]
  mkInteraction (a@(Ct a_tv _), b@(Ct b_tv b_ty)) =
    if | a_tv == b_tv -> Left (InteractEq a b)
       | occurs a_tv b_ty -> Left (InteractOccurs a b)
       | otherwise -> Right [a, b]

simplifications :: [CanonCt] -> [CanonCt] -> ([Interaction], [CanonCt])
simplifications given wanted = second nub . partitionEithers $ mkInteraction <$> candidates
  where
    candidates = [(given_ct, wanted_ct) | given_ct <- given, wanted_ct <- wanted]
    mkInteraction (given@(Ct given_tv _), wanted@(Ct wanted_tv wanted_ty)) =
      if | given_tv == wanted_tv -> Left (InteractEq given wanted)
         | occurs given_tv wanted_ty -> Left (InteractOccurs given wanted)
         | otherwise -> Right wanted

-- The final set of rewrites topreact are missing in our impl
-- They deal explicitly with top level type class axioms which we don't have.
-- topreact :: (Has (Fresh TVar) sig m, Has (Throw (Maybe TyErr)) sig m) => Axioms -> Q -> m ([TVar], [Q])

canonicalize :: ( Has (Fresh TVar) sig m
                , Has (State ([TVar], Subst)) sig m
                , Has (Throw ()) sig m)
                => (Q -> m (Either CanonCt [Q])) -> [Q] -> m ([CanonCt], [Q])
canonicalize op = go []
 where
  go acc [] = return (acc, [])
  go canon work = do
    (new_canon, remaining_work) <- partitionEithers <$> mapM op work
    if null new_canon
       then return (canon, mconcat remaining_work)
       else go (new_canon <> canon) (mconcat remaining_work)


{- Single step of rewriting our constraints towards a solution -}
solve :: (Has (Fresh TVar) sig m, Has (State [Q]) sig m , Has (Throw ()) sig m) => [TVar] -> Subst -> [CanonCt] -> [CanonCt] -> m (Maybe ([TVar], Subst, [CanonCt], [CanonCt]))
solve unifiers phi given wanted
  -- Handles interactions between given constraints
  | (i : interacts, inert_canon) <- interactions given = do
    let is = foldMap interact (i : interacts)
    ((fresh_unifiers, subst), (canon, _)) <- runState ([], mempty) (canonicalize canong is)
    return $ Just (unifiers <> fresh_unifiers, subst <> phi, canon <> inert_canon, wanted)

  | (i : interacts, inert_canon) <- interactions wanted = do
    let is = foldMap interact (i : interacts)
    ((fresh_unifiers, subst), (canon, remainder)) <- runState ([], mempty) (canonicalize canonw is)
    modify (remainder ++)
    return $ Just (unifiers <> fresh_unifiers, subst <> phi, given, canon <> inert_canon) 

  | (i : interacts, inert_wanted) <- simplifications given wanted = do
    let is = simplify <$> i : interacts
    ((fresh_unifiers, subst), (canon, remainder)) <- runState ([], mempty) (canonicalize canonw is)
    modify (remainder ++)
    return $ Just (unifiers <> fresh_unifiers, subst <> phi, given, canon <> inert_wanted)
solve _ _ _ _ = return Nothing

simples :: (Has (Fresh TVar) sig m, Has (Throw ()) sig m) => [TVar] -> [Q] -> [Q] -> m ([TVar], Subst, [CanonCt], ([CanonCt], [Q]))
simples unifiers noncanon_given noncanon_wanted = do
  ((canon_unifiers, subst), ((given, _), (wanted, wanted_residue))) <- runState ([], mempty) $ do
    given <- canonicalize canong noncanon_given
    wanted <- canonicalize canonw noncanon_wanted
    return (given, wanted)
  (residue, (unifiers, subst, given, wanted)) <- runState wanted_residue $ go (unifiers <> canon_unifiers) subst given wanted
  return (unifiers, subst, given, (wanted, residue))
  where
    go uni subst given wanted = do
      res <- solve uni subst given wanted
      case res of
        Just (uni, subst, given, wanted) -> go uni subst given wanted
        Nothing -> return (uni, subst, given, wanted)

solveSimplConstraints :: (Has (Fresh TVar) sig m, Has (Throw ()) sig m) => [TVar] -> [Q] -> [Q] -> m (Subst, [Q])
solveSimplConstraints ctx_unifiers given wanted = do
  (_, flatten_subst, _, (wanted_canon, residue)) <- simples ctx_unifiers given wanted
  let q_wanted = (\(Ct tv ty) -> Ct tv (apply flatten_subst ty)) <$> wanted_canon
  let q_residue =  over (traverse . qTys) (apply flatten_subst) residue
  -- tie the knot
  let theta = 
        let knotted_theta = (\(Ct tv ty) -> (tv, apply theta ty) ) <$> q_wanted
         in Subst $ Map.fromList knotted_theta
  return (theta, over (traverse . qTys) (apply theta) q_residue)

solveImplConstraints :: (Has (Reader [Q]) sig m, Has (Fresh TVar) sig m) => [Constraint] -> m ([Q], Subst)
solveImplConstraints constrs = undefined

example :: Term
example = Abs x $ Abs y $ Abs z $ Var x :@ Var z :@ (Var y :@ Var z)
 where
  x = V 0
  y = V 1
  z = V 2

showSimpl :: [TVar] -> [Q] -> [Q] -> String
showSimpl unifiers given wanted = 
    case snd $ runIdentity $ runFresh nextTV $ runThrow (solveSimplConstraints unifiers given wanted) of
      Left () -> "Error during solving constraints"
      Right (theta, residue) -> show theta ++ "\n" ++ show residue
  where 
    nextTV = foldr max (TV (-1)) unifiers

testSet :: [Q]
testSet =
  [ VarTy (TV 4) :~ FunTy (VarTy (TV 5)) (VarTy (TV 3))
  , VarTy (TV 0) :~ FunTy (VarTy (TV 2)) (VarTy (TV 4))
  , VarTy (TV 1) :~ FunTy (VarTy (TV 2)) (VarTy (TV 5))
  ]

infer :: Term -> Type
infer term = 
    case res of
      Left () -> error "Failed to infer a type"
      Right (_, (subst, _)) -> apply subst ty
  where
    (tvar, (ty, constrs)) = runIdentity $ runReader emptyCtx $ runFresh (TV 0) $ generateConstraints term
    res = runIdentity . runThrow . runFresh tvar $ solveSimplConstraints [(TV 0)..(tvar - 1)] [] (foldMap (\constr -> case constr of { Simp q -> [q] ; _ -> [] }) constrs)

runGen :: Term -> (Type, [Constraint])
runGen term = snd . runIdentity $ runReader emptyCtx $ runFresh (TV 0) $ generateConstraints term
