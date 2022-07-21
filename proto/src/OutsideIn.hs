{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module OutsideIn where

import Fresh (Fresh, fresh, runFresh)

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Map.Lazy as LazyMap
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map

import Control.Algebra (Has)
import Control.Effect.Reader (Reader, ask, local)
import Control.Lens (
  Identity (runIdentity),
  Iso',
  Plated (..),
  Traversal',
  anyOf,
  iso,
  makeLenses,
  over,
  toListOf,
  transform,
  view,
 )
import qualified Data.Map.Merge.Strict as Map
import Data.Maybe (fromMaybe)

import Control.Carrier.Reader (runReader)
import Control.Carrier.State.Strict (runState)
import Control.Carrier.Throw.Either (runThrow)
import Control.Effect.State (State, modify)
import Control.Effect.Throw (Throw, throwError)
import Control.Monad (forM)
import Data.Bifunctor (Bifunctor (first, second))
import Data.Either (partitionEithers)
import Data.Foldable (foldrM)
import Data.List (intersperse, nub)
import Data.Text (Text)
import Debug.Trace
import Text.Show.Pretty
import Prelude hiding (interact)

newtype TVar = TV Int
  deriving (Ord, Eq, Num, Enum, Show, Bounded)

newtype Var = V Int
  deriving (Ord, Eq, Num, Enum, Show, Bounded)

type Label = Text

data Dir = L | R

infixr 4 :*
infix 5 :|>

{- Our lambda calc + let + int to allow for let generalization and a meaningful base type in examples -}
data Term
  = -- Variable
    Var Var
  | -- Int literal
    Int Int
  | -- Wrap a term in a label
    Label :|> Term
  | -- Unwrap a term with given label
    Term :/ Label
  | -- Record concatenation (intro)
    Term :* Term
  | -- Record projection (elim)
    Prj Dir Term
  | -- Lambda Abstraction
    Abs Var Term
  | -- Application
    Term :@ Term

type Row = Map Label Type

data Type
  = VarTy TVar
  | IntTy
  | RowTy Row
  | ProdTy Type
  | FunTy Type Type
  deriving (Eq, Ord)

isVar :: Type -> Bool
isVar (VarTy _) = True
isVar _ = False

infix 7 |>
(|>) :: Label -> Type -> Row
lbl |> ty = Map.singleton lbl ty

showsRow :: Int -> Row -> ShowS
showsRow p row s = foldr (\f s -> f s) s $ intersperse (", " ++) $ (\(lbl, ty) -> showsPrec p lbl . (" |> " ++) . showsPrec p ty) <$> Map.toList row

instance Show Type where
  showsPrec p (VarTy (TV tv)) = ("VarTy " ++) . showsPrec p tv
  showsPrec _ IntTy = ("Int" ++)
  showsPrec p (RowTy row) = showsRow p row
  showsPrec p (ProdTy ty) = ('{' :) . showsPrec p ty . ('}' :)
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
      RowTy row -> RowTy <$> traverse f row
      ProdTy ty -> ProdTy <$> f ty
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
occurs tvar = anyOf (plate . typeVars) (== tvar)

data Scheme = Scheme [TVar] [Q] Type
  deriving (Show, Eq, Ord)

monoScheme :: Type -> Scheme
monoScheme = Scheme [] []

data InternalRow
  = Closed Row
  | Open TVar
  deriving (Eq, Ord)

instance Show InternalRow where
  showsPrec p (Closed row) = showsRow p row
  showsPrec p (Open (TV tv)) = ("Open " ++) . showsPrec p tv

internalRowTys :: Traversal' InternalRow Type
internalRowTys f =
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

infix 6 :~

-- Need a better name for this
data Q
  = -- Two types must be equal (aka unifiable)
    Type :~ Type
  | -- _left and _right combine to equal _eq
    CombineEquals {_left :: InternalRow, _right :: InternalRow, _eq :: InternalRow}
  deriving (Show, Eq, Ord)

qTys :: Traversal' Q Type
qTys f q =
  case q of
    -- Is doing builtin recursion like this bad?
    t1 :~ t2 -> (:~) <$> f t1 <*> f t2
    CombineEquals left right eq -> CombineEquals <$> internalRowTys f left <*> internalRowTys f right <*> internalRowTys f eq

data Implication = Imp {_exists :: [TVar], _prop :: [Q], _implies :: [Constraint]}
  deriving (Eq, Show)

{- Constraints generated during type inference that must hold for the program to typecheck -}
data Constraint
  = Simp Q
  | Impl Implication
  deriving (Eq, Show)

-- This has to be below Constraint
makeLenses ''Implication

constraintEither :: Iso' Constraint (Either Q Implication)
constraintEither = iso to from
 where
  to (Simp q) = Left q
  to (Impl i) = Right i

  from (Left q) = Simp q
  from (Right i) = Impl i

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
    ( \case
        VarTy var -> fromMaybe (VarTy var) (Map.lookup var map)
        ty -> ty
    )

applyRow :: Subst -> InternalRow -> InternalRow
applyRow subst (Closed row) = Closed $ fmap (apply subst) row
applyRow (Subst map) (Open tvar) =
  case map !? tvar of
    Just (RowTy row) -> Closed row
    Just (VarTy tv) -> Open tv
    -- If we can't substitute our open row for another row don't apply at all
    _ -> Open tvar

generateConstraints :: (Has (Reader Ctx) sig m, Has (Fresh TVar) sig m) => Term -> m (Type, [Constraint])
generateConstraints term =
  case term of
    -- Literals
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
    -- Labelled Types
    lbl :|> term -> do
      (term_ty, term_constr) <- generateConstraints term
      return (RowTy $ lbl |> term_ty, term_constr)
    term :/ lbl -> do
      alpha <- fresh
      (term_ty, term_constr) <- generateConstraints term
      return (VarTy alpha, Simp (term_ty :~ RowTy (lbl |> VarTy alpha)) : term_constr)
    -- Records
    left :* right -> do
      out <- fresh
      left_var <- fresh
      right_var <- fresh
      (left_ty, left_constr) <- generateConstraints left
      (right_ty, right_constr) <- generateConstraints right
      -- We don't want product types to show up in CombineEquals, so we unwrap them into Rows before constructing our Constraint
      return (ProdTy $ VarTy out, Simp (left_ty :~ ProdTy (VarTy left_var)) : Simp (right_ty :~ ProdTy (VarTy right_var)) : Simp (CombineEquals (Open left_var) (Open right_var) (Open out)) : left_constr <> right_constr)
    Prj dir term -> do
      rest <- Open <$> fresh
      out <- fresh
      input <- fresh
      (term_ty, term_constr) <- generateConstraints term
      -- We want to unwrap a product into it's underlying row for it's constraint
      let constr =
            case dir of
              L -> CombineEquals (Open out) rest (Open input)
              R -> CombineEquals rest (Open out) (Open input)
      return (ProdTy $ VarTy out, Simp (term_ty :~ ProdTy (VarTy input)) : Simp constr : term_constr)
    -- Functions
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
  | TypeMismatch Type Type

instance Show TyErr where
  showsPrec p (OccursCheckFailed tvar ty) = ("OccursCheckFailed " ++) . showsPrec p tvar . (" (" ++) . showsPrec p ty . (')' :)
  showsPrec p (TypeMismatch a_ty b_ty) = ("TypeMismatch (" ++) . showsPrec p a_ty . (") (" ++) . showsPrec p b_ty . (')' :)

addUnifier :: TVar -> ([TVar], Subst) -> ([TVar], Subst)
addUnifier tvar (tvars, subst) = (tvar : tvars, subst)

mergeSubst :: Subst -> ([TVar], Subst) -> ([TVar], Subst)
mergeSubst new_sub (tvars, old_sub) = (tvars, new_sub <> old_sub)

data CanonResult
  = -- Sucessfully canonicalized our constraint
    Done CanonCt
  | -- Constraint cannot be made canonical
    Residual Q
  | -- Sub-constraints to process
    Work [Q]

{- canonicalize a wanted constraint -}
canonw ::
  ( Has (Fresh TVar) sig m
  , Has (State ([TVar], Subst)) sig m
  , Has (Throw TyErr) sig m
  ) =>
  Q ->
  m CanonResult
canonw c@(VarTy tvar :~ FunTy arg ret) =
  case (isVar arg, isVar ret) of
    -- This is a canoical funciton type so we delegate to generic canon
    (True, True) -> canon c
    (False, True) -> do
      beta <- fresh
      modify (addUnifier beta)
      return $ Work [VarTy tvar :~ FunTy (VarTy beta) ret, arg :~ VarTy beta]
    (True, False) -> do
      beta <- fresh
      modify (addUnifier beta)
      return $ Work [VarTy tvar :~ FunTy arg (VarTy beta), ret :~ VarTy beta]
    (False, False) -> do
      alpha <- fresh
      beta <- fresh
      modify (addUnifier beta . addUnifier alpha)
      return $ Work [VarTy tvar :~ FunTy (VarTy alpha) (VarTy beta), VarTy alpha :~ arg, VarTy beta :~ ret]
canonw c@(VarTy tvar :~ RowTy row) =
  let (canon_row, noncanon_row) = Map.partition isVar row
   in if null noncanon_row
        then canon c
        else do
          (fresh_eqs, fresh_row) <-
            runState [] $
              traverse
                ( \ty -> do
                    beta <- fresh
                    modify ((VarTy beta :~ ty) :)
                    return (VarTy beta)
                )
                noncanon_row
          return $ Work (VarTy tvar :~ RowTy (canon_row <> fresh_row) : fresh_eqs)
-- If not a wanted specific case delegate to generic canon
canonw c = canon c

{- canonicalize a given constraint -}
canong :: (Has (Fresh TVar) sig m, Has (State ([TVar], Subst)) sig m, Has (Throw TyErr) sig m) => Q -> m CanonResult
canong c@(VarTy tvar :~ FunTy arg ret) =
  case (isVar arg, isVar ret) of
    (True, True) -> canon c
    (True, False) -> do
      beta <- fresh
      modify (mergeSubst (beta |-> ret))
      return $ Work [VarTy tvar :~ FunTy arg (VarTy beta), VarTy beta :~ ret]
    (False, True) -> do
      beta <- fresh
      modify (mergeSubst (beta |-> arg))
      return $ Work [VarTy tvar :~ FunTy (VarTy beta) ret, VarTy beta :~ arg]
    (False, False) -> do
      alpha <- fresh
      beta <- fresh
      modify (mergeSubst (beta |-> ret) . mergeSubst (alpha |-> arg))
      return $ Work [VarTy tvar :~ FunTy (VarTy alpha) (VarTy beta), VarTy alpha :~ arg, VarTy beta :~ ret]
canong c@(VarTy tvar :~ RowTy row) =
  let (canon_row, noncanon_row) = Map.partition isVar row
   in if null noncanon_row
        then canon c
        else do
          (fresh_eqs, fresh_row) <-
            runState [] $
              traverse
                ( \ty -> do
                    beta <- fresh
                    modify (mergeSubst (beta |-> ty))
                    modify ((VarTy beta :~ ty) :)
                    return (VarTy beta)
                )
                noncanon_row
          return $ Work (VarTy tvar :~ RowTy (canon_row <> fresh_row) : fresh_eqs)
-- If not a given specific case delegate to generic canon
canong c = canon c

{- canonicalize a generic constraint. this function preforms rewrites that are the same for wanted and given constraints -}
canon :: (Has (Throw TyErr) sig m) => Q -> m CanonResult
-- Deconstruct types into composite equalities
canon (FunTy a_arg a_ret :~ FunTy b_arg b_ret) = return $ Work [a_arg :~ b_arg, a_ret :~ b_ret]
canon (ProdTy a_ty :~ ProdTy b_ty) = return $ Work [a_ty :~ b_ty]
canon (a@(RowTy a_row) :~ b@(RowTy b_row)) = do
  row_eqs <-
    Map.mergeA
      (Map.traverseMissing (\_ _ -> throwError (TypeMismatch a b)))
      (Map.traverseMissing (\_ _ -> throwError (TypeMismatch a b)))
      (Map.zipWithAMatched (\_ a b -> return (a :~ b)))
      a_row
      b_row
  return $ Work (Map.elems row_eqs)
canon (a :~ b)
  | a == b = return $ Work []
-- If we fail an occurs check throw to break out of cannon
canon (VarTy a_tv :~ VarTy b_tv)
  | b_tv < a_tv = return $ Done (Ct b_tv (VarTy a_tv))
canon (VarTy tvar :~ ty)
  | occurs tvar ty = throwError (OccursCheckFailed tvar ty)
  -- We know this is canon because if not it would have hit the b < a case above
  | otherwise = return . Done $ Ct tvar ty
canon (ProdTy ty :~ VarTy tvar) = return $ Work [VarTy tvar :~ ProdTy ty]
canon (RowTy row :~ VarTy tvar) = return $ Work [VarTy tvar :~ RowTy row]
canon (FunTy arg ret :~ VarTy tvar) = return $ Work [VarTy tvar :~ FunTy arg ret]
canon (IntTy :~ VarTy tvar) = return $ Work [VarTy tvar :~ IntTy]
-- Coerce rows into products as needed
-- Actually don't do this
canon (ProdTy ty :~ RowTy row) = return $ Work [ty :~ RowTy row]
canon (RowTy row :~ ProdTy ty) = return $ Work [ty :~ RowTy row]
-- Expectation is these are un-unifiable types
-- Bubble them up as residual inert constraints to be handled
canon (a :~ b) = trace ("residual : " ++ ppShow (a :~ b)) $ return $ Residual (a :~ b)
-- Row constraints
-- Trivial case where all types are known. Reduces to an equality of rows
canon (CombineEquals (Closed a_row) (Closed b_row) (Closed goal)) = return $ Work [RowTy goal :~ RowTy (a_row <> b_row)]
canon (CombineEquals (Closed a_row) (Open b_tv) (Closed goal))
  | rowOccurs b_tv (Closed goal) = throwError (OccursCheckFailed b_tv (RowTy goal))
  | otherwise =
    let (a_goal, b_goal) = Map.partitionWithKey (\k _ -> Map.member k a_row) goal
     in return $ Work [RowTy a_goal :~ RowTy a_row, VarTy b_tv :~ RowTy b_goal]
canon (CombineEquals (Open a_tv) (Closed b_row) (Closed goal))
  | rowOccurs a_tv (Closed goal) = throwError (OccursCheckFailed a_tv (RowTy goal))
  | otherwise =
    let (b_goal, a_goal) = Map.partitionWithKey (\k _ -> Map.member k b_row) goal
     in return $ Work [VarTy a_tv :~ RowTy a_goal, RowTy b_row :~ RowTy b_goal]
-- In this trivial case we can reduce to an equality
canon (CombineEquals (Closed a_row) (Closed b_row) (Open goal)) = return $ Work [VarTy goal :~ RowTy (a_row <> b_row)]
canon (CombineEquals a_ty b_ty (Open goal)) =
  return . Done $ CombineCt goal a_ty b_ty
canon c@(CombineEquals _ _ (Closed _)) =
  return $ Residual c

{- A canonical constraint.
   This is a tvar lhs equal to a type that does not contain tvar -}
data CanonCt
  = Ct TVar Type
  | CombineCt TVar InternalRow InternalRow
  deriving (Eq)

instance Show CanonCt where
  showsPrec p (Ct tv ty) = showsPrec p tv . (" ~= " ++) . showsPrec p ty
  showsPrec p (CombineCt tv left right) = showsPrec p tv . (" ~= " ++) . showsPrec p left . (" ⊙ " ++) . showsPrec p right

{- binary interaction between two constraints from the same set (wanted or given) -}
interact :: Interaction -> [Q]
interact i =
  -- The paper covers other rules but they are for type classes and type families
  case i of
    -- EQSAME
    InteractEq a_tv a_ty _ b_ty -> [VarTy a_tv :~ a_ty, a_ty :~ b_ty]
    -- EQDIFF
    InteractOccurs a_tv a_ty b_tv b_ty -> [VarTy a_tv :~ a_ty, VarTy b_tv :~ apply (a_tv |-> a_ty) b_ty]
    -- Row stuff
    InteractRowEq a_tv a_row _ b_left b_right -> [VarTy a_tv :~ rowToType a_row, CombineEquals b_left b_right a_row]
    InteractRowOccurLeft a_tv a_ty b_goal b_left b_right -> [VarTy a_tv :~ a_ty, CombineEquals (applyRow (a_tv |-> a_ty) b_left) b_right (Open b_goal)]
    InteractRowOccurRight a_tv a_ty b_goal b_left b_right -> [VarTy a_tv :~ a_ty, CombineEquals b_left (applyRow (a_tv |-> a_ty) b_right) (Open b_goal)]
    InteractRowSolve a_goal a_left a_right b_goal b_left b_right ->
      case (a_left, a_right, b_left, b_right) of
        -- All closed, reduces to an equality on RowTys
        (Closed a_left, Closed a_right, Closed b_left, Closed b_right) -> [VarTy a_goal :~ RowTy (a_left <> a_right), RowTy (a_left <> a_right) :~ RowTy (b_left <> b_right)]
        -- Each row is partially open and partially closed
        -- These are the trickiest cases as we have to handle all permutations of possible equalities
        -- a_row and b_row could be disjoint, total overlap, or in a subset relationship
        (Closed a_row, Open a_tv, Closed b_row, Open b_tv) -> CombineEquals a_left a_right (Open a_goal) : equatePartiallyOpenRows a_row a_tv b_row b_tv
        (Open a_tv, Closed a_row, Closed b_row, Open b_tv) -> CombineEquals a_left a_right (Open a_goal) : equatePartiallyOpenRows a_row a_tv b_row b_tv
        (Open a_tv, Closed a_row, Open b_tv, Closed b_row) -> CombineEquals a_left a_right (Open a_goal) : equatePartiallyOpenRows a_row a_tv b_row b_tv
        (Closed a_row, Open a_tv, Open b_tv, Closed b_row) -> CombineEquals a_left a_right (Open a_goal) : equatePartiallyOpenRows a_row a_tv b_row b_tv
        -- Only one variable present
        (Open tv, Closed row, Closed sub_left, Closed sub_right) -> equateOneOpenVariable sub_left sub_right row tv a_goal
        (Closed row, Open tv, Closed sub_left, Closed sub_right) -> equateOneOpenVariable sub_left sub_right row tv a_goal
        (Closed sub_left, Closed sub_right, Open tv, Closed row) -> equateOneOpenVariable sub_left sub_right row tv b_goal
        (Closed sub_left, Closed sub_right, Closed row, Open tv) -> equateOneOpenVariable sub_left sub_right row tv b_goal
        -- The rest of cases are straightforward equalities between lefts and rights
        (a_left, a_right, b_left, b_right) -> [CombineEquals a_left a_right (Open a_goal), rowToType a_left :~ rowToType b_left, rowToType a_right :~ rowToType b_right]
 where
  equateOneOpenVariable sub_left sub_right row tv goal =
    CombineEquals (Closed row) (Open tv) (Open goal) :
    if
        | Map.keys row == Map.keys sub_left -> [RowTy row :~ RowTy sub_left, VarTy tv :~ RowTy sub_right]
        | Map.keys row == Map.keys sub_right -> [RowTy row :~ RowTy sub_right, VarTy tv :~ RowTy sub_left]
        | Map.keys row == Map.keys (sub_left <> sub_right) -> [RowTy row :~ RowTy (sub_left <> sub_right), VarTy tv :~ RowTy Map.empty]
        | otherwise -> [VarTy tv :~ RowTy (sub_left <> sub_right)]

  equatePartiallyOpenRows a_row a_tv b_row b_tv =
    let (a_in_b, a_rest) = Map.partitionWithKey (\k _ -> Map.member k b_row) a_row
     in -- a_left and b_left are disjoint, so set them to each other's open records
        if
            | Map.null a_in_b -> [VarTy b_tv :~ RowTy a_rest, VarTy a_tv :~ RowTy b_row]
            -- a and b fully overlap, so should be equated to each other
            | Map.null a_rest -> [RowTy a_in_b :~ RowTy b_row, VarTy a_tv :~ VarTy b_tv]
            -- a and b only partially overlap, equate the parts that do overlap and equate the variables for the rest
            | otherwise ->
              let (b_in_a, b_rest) = Map.partitionWithKey (\k _ -> Map.member k a_in_b) b_row
               in [RowTy a_in_b :~ RowTy b_in_a, VarTy b_tv :~ RowTy a_rest, VarTy a_tv :~ RowTy b_rest]

{- binary interaction between constraints from each set (wanted and given).
  Because of this order is important, and the returned constraint is a wanted (replacing the input wanted constraint) -}
simplify :: Interaction -> Q
simplify i =
  -- These are basically the same rules as interact but because they combine a given with a wanted we're allowed to simplify further
  -- Basically saturating an assumption with an inference.
  -- Like interact there are other rules for type families and type classes that we leave out for now
  case i of
    -- SEQSAME
    InteractEq _ given_ty _ wanted_ty -> given_ty :~ wanted_ty
    -- SEQDIFF
    InteractOccurs given_tv given_ty wanted_tv wanted_ty -> VarTy wanted_tv :~ apply (given_tv |-> given_ty) wanted_ty
    -- Row stuff
    InteractRowEq _ given_ty _ wanted_left wanted_right -> CombineEquals wanted_left wanted_right given_ty
    InteractRowOccurLeft given_tv given_ty wanted_goal wanted_left wanted_right -> CombineEquals (applyRow (given_tv |-> given_ty) wanted_left) wanted_right (Open wanted_goal)
    InteractRowOccurRight given_tv given_ty wanted_goal wanted_left wanted_right -> CombineEquals wanted_left (applyRow (given_tv |-> given_ty) wanted_right) (Open wanted_goal)
    InteractRowSolve{} -> error "todo!"

data Interaction
  = -- The canonical constraints are for the same tvar
    InteractEq TVar Type TVar Type
  | -- The tvar of the first canonical constraint occurs in the type of the second
    InteractOccurs TVar Type TVar Type
  | -- TODO: Name these, this is spaghetti
    -- And in general alot of these row interactions are eerily similar to Eq and Occurs
    -- Try to find a way to express them in terms of normal Eq and Occurs plus some kind of Combine primitive
    InteractRowOccurLeft TVar Type TVar InternalRow InternalRow
  | InteractRowOccurRight TVar Type TVar InternalRow InternalRow
  | InteractRowEq TVar InternalRow TVar InternalRow InternalRow
  | InteractRowSolve TVar InternalRow InternalRow TVar InternalRow InternalRow
  deriving (Eq, Show)

{- Find pairs of interactable constraints -}
interactions :: [CanonCt] -> ([Interaction], [CanonCt])
interactions canons = selectInteractions canons
 where
  {- Iterate over list of canonical constraints and generate interactions between pairs.
    Logic is somewhat subtle here, it is important for termination that we only generate one interaction for any one constraint at a time.
    Generating two interactions for a constraint cause the constraint to duplicate and the worklist to explode. -}
  selectInteractions [] = ([], [])
  selectInteractions (ct : rest) =
    let inters = dropWhile ((== Nothing) . snd) $ mkInteraction rest . (,) ct <$> rest
     in case inters of
          (filtered_rest, Just i) : _ -> first (i :) (selectInteractions filtered_rest)
          (_, Nothing) : _ -> undefined
          [] -> second (ct :) (selectInteractions rest)

  mkInteraction canons (a, b) =
    let filteredCanons int = (filter (\c -> c /= a && c /= b) canons, Just int)
     in case (a, b) of
          (Ct a_tv a_ty, Ct b_tv b_ty)
            | a_tv == b_tv -> filteredCanons $ InteractEq a_tv a_ty b_tv b_ty
            | occurs a_tv b_ty -> filteredCanons $ InteractOccurs a_tv a_ty b_tv b_ty
            | occurs b_tv a_ty -> filteredCanons $ InteractOccurs b_tv b_ty a_tv a_ty
          -- Handle entailment cases
          (Ct a_tv (RowTy a_row), CombineCt b_tv b_left b_right)
            | a_tv == b_tv -> filteredCanons $ InteractRowEq a_tv (Closed a_row) b_tv b_left b_right
          (Ct a_tv (VarTy a_row), CombineCt b_tv b_left b_right)
            | a_tv == b_tv -> filteredCanons $ InteractRowEq a_tv (Open a_row) b_tv b_left b_right
          (Ct a_tv a_ty, CombineCt b_goal b_left b_right)
            | rowOccurs a_tv b_left -> filteredCanons $ InteractRowOccurLeft a_tv a_ty b_goal b_left b_right
            | rowOccurs a_tv b_right -> filteredCanons $ InteractRowOccurRight a_tv a_ty b_goal b_left b_right
          -- Symmetrical cases
          (CombineCt a_tv a_left a_right, Ct b_tv (RowTy b_row))
            | a_tv == b_tv -> filteredCanons $ InteractRowEq b_tv (Closed b_row) a_tv a_left a_right
          (CombineCt a_tv a_left a_right, Ct b_tv (VarTy b_row))
            | a_tv == b_tv -> filteredCanons $ InteractRowEq b_tv (Open b_row) a_tv a_left a_right
          (CombineCt a_goal a_left a_right, Ct b_tv b_ty)
            | rowOccurs b_tv a_left -> filteredCanons $ InteractRowOccurLeft b_tv b_ty a_goal a_left a_right
            | rowOccurs b_tv a_right -> filteredCanons $ InteractRowOccurRight b_tv b_ty a_goal a_left a_right
          (CombineCt a_goal a_left a_right, CombineCt b_goal b_left b_right)
            | a_goal == b_goal -> filteredCanons $ InteractRowSolve a_goal a_left a_right b_goal b_left b_right
          (_, _) -> (canons, Nothing)

simplifications :: [CanonCt] -> [CanonCt] -> ([Interaction], [CanonCt])
simplifications given wanted = second nub . partitionEithers $ mkInteraction <$> candidates
 where
  candidates = [(given_ct, wanted_ct) | given_ct <- given, wanted_ct <- wanted]
  mkInteraction (Ct given_tv given_ty, Ct wanted_tv wanted_ty)
    | given_tv == wanted_tv = Left (InteractEq given_tv given_ty wanted_tv wanted_ty)
    | occurs given_tv wanted_ty = Left (InteractOccurs given_tv given_ty wanted_tv wanted_ty)
  mkInteraction (Ct given_tv (RowTy given_ty), CombineCt wanted_tv wanted_left wanted_right)
    | given_tv == wanted_tv = Left $ InteractRowEq given_tv (Closed given_ty) wanted_tv wanted_left wanted_right
  mkInteraction (Ct given_tv (VarTy given_ty), CombineCt wanted_tv wanted_left wanted_right)
    | given_tv == wanted_tv = Left $ InteractRowEq given_tv (Open given_ty) wanted_tv wanted_left wanted_right
  mkInteraction (Ct given_tv given_ty, CombineCt wanted_goal wanted_left wanted_right)
    | rowOccurs given_tv wanted_left = Left $ InteractRowOccurLeft given_tv given_ty wanted_goal wanted_left wanted_right
    | rowOccurs given_tv wanted_right = Left $ InteractRowOccurRight given_tv given_ty wanted_goal wanted_left wanted_right
  -- Order is important here as our input is (given, wanted). Which asks does it even make sense for this case to occur?
  mkInteraction (CombineCt{}, Ct{}) = error "does this even make sense as a possible case?"
  mkInteraction (_, wanted) = Right wanted

-- The final set of rewrites topreact are missing in our impl
-- They deal explicitly with top level type class axioms which we don't have.
-- topreact :: (Has (Fresh TVar) sig m, Has (Throw (Maybe TyErr)) sig m) => Axioms -> Q -> m ([TVar], [Q])

canonicalize ::
  (Monad m) =>
  (Q -> m CanonResult) ->
  [Q] ->
  m ([CanonCt], [Q])
canonicalize op = go [] []
 where
  go canon residuals [] = return (canon, residuals)
  go canon residuals work = do
    (new_canon, new_residuals, remaining_work) <- partitionResults work
    go (new_canon <> canon) (new_residuals <> residuals) (remaining_work [])

  partitionResults = foldrM (\constr acc -> binResult acc <$> op constr) ([], [], id)

  binResult (canons, residuals, work) (Done canon) = (canon : canons, residuals, work)
  binResult (canons, residuals, work) (Residual residue) = (canons, residue : residuals, work)
  binResult (canons, residues, work) (Work w) = (canons, residues, (w ++) . work)

{- Single step of rewriting our constraints towards a solution -}
solve :: (Has (Fresh TVar) sig m, Has (State [Q]) sig m, Has (Throw TyErr) sig m) => [TVar] -> Subst -> [CanonCt] -> [CanonCt] -> m (Maybe ([TVar], Subst, [CanonCt], [CanonCt]))
solve unifiers phi given wanted
  -- Handles interactions between given constraints
  | (i : interacts, inert_canon) <- interactions given = do
    let is = foldMap interact (i : interacts)
    ((fresh_unifiers, subst), (canon, _)) <- runState ([], mempty) (canonicalize canon is)
    return $ Just (unifiers <> fresh_unifiers, subst <> phi, canon <> inert_canon, wanted)
  | (i : interacts, inert_canon) <- interactions wanted = do
    let is = nub $ foldMap interact (i : interacts)
    ((fresh_unifiers, subst), (canon, remainder)) <- runState ([], mempty) (canonicalize canon is)
    modify (remainder ++)
    return $ Just (unifiers <> fresh_unifiers, subst <> phi, given, canon <> inert_canon)
  | (i : interacts, inert_wanted) <- simplifications given wanted = do
    let is = simplify <$> i : interacts
    ((fresh_unifiers, subst), (canon, remainder)) <- runState ([], mempty) (canonicalize canon is)
    modify (remainder ++)
    return $ Just (unifiers <> fresh_unifiers, subst <> phi, given, canon <> inert_wanted)
solve _ _ _ _ = return Nothing

simples :: (Has (Fresh TVar) sig m, Has (Throw TyErr) sig m) => [TVar] -> [Q] -> [Q] -> m ([TVar], Subst, [CanonCt], ([CanonCt], [Q]))
simples unifiers noncanon_given noncanon_wanted = do
  ((canon_unifiers, subst), ((given, _), (wanted, wanted_residue))) <- runState ([], mempty) $ do
    given <- canonicalize canon noncanon_given
    wanted <- canonicalize canon noncanon_wanted
    return (given, wanted)
  (residue, (unifiers, subst, given, wanted)) <- runState wanted_residue $ go 10000 (unifiers <> canon_unifiers) subst given wanted
  return (unifiers, subst, given, (wanted, residue))
 where
  go 0 uni subst given wanted = return (uni, subst, given, wanted)
  go n uni subst given wanted = do
    res <- solve uni subst given wanted
    case res of
      Just (uni, subst, given, wanted) -> go (n - 1) uni subst given wanted
      Nothing -> return (uni, subst, given, wanted)

solveSimplConstraints :: (Has (Fresh TVar) sig m, Has (Throw TyErr) sig m) => [TVar] -> [Q] -> [Q] -> m (Subst, [Q])
solveSimplConstraints ctx_unifiers given wanted = do
  (_, flatten_subst, _, (wanted_canon, residue)) <- simples ctx_unifiers given wanted
  let q_wanted = flatten_q flatten_subst <$> wanted_canon
  let q_residue = over (traverse . qTys) (apply flatten_subst) residue
  -- tie the knot
  let theta = Subst . LazyMap.fromList . mconcat $ q_to_pairs theta <$> q_wanted
  return (theta, over (traverse . qTys) (apply theta) q_residue)
 where
  q_to_pairs subst q =
    case q of
      Ct tv ty -> [(tv, apply subst ty)]
      CombineCt{} -> [] --error ("Unexpected CombineCt in q_wanted " ++ ppShow c)
  flatten_q subst q =
    case q of
      Ct tv ty -> Ct tv (apply subst ty)
      CombineCt tv left right -> CombineCt tv (applyRow subst left) (applyRow subst right)

solveConstraints :: (Has (Reader [Q]) sig m, Has (Throw TyErr) sig m, Has (Fresh TVar) sig m) => [TVar] -> [Q] -> [Constraint] -> m (Subst, [Q])
solveConstraints unifiers given constrs = do
  (subst, residue) <- solveSimplConstraints unifiers given simpls
  impl_substs <- forM impls $ \(Imp exists prop impl) -> do
    (subst, residue) <- solveConstraints (unifiers <> exists) (given <> residue <> prop) impl
    if not (null residue)
      then error "Expected empty residue for implication constraint"
      else return subst
  return (foldr (<>) subst impl_substs, residue)
 where
  (simpls, impls) = partitionEithers (view constraintEither <$> constrs)

example :: Term
example = Abs x $ Abs y $ Abs z $ Var x :@ Var z :@ (Var y :@ Var z)
 where
  x = V 0
  y = V 1
  z = V 2

exampleInt :: Term
exampleInt = Abs f $ Abs g $ Var f :@ Int 0 :@ (Var g :@ Int 11)
 where
  f = V 0
  g = V 1

recordLit :: (Foldable t) => t Term -> Term
recordLit = foldr1 (:*)

exampleRow :: Term
exampleRow =
  Abs rho (Prj L (Var rho) :/ "x")
    :@ recordLit ["x" :|> Int 2, "w" :|> Int 0, "y" :|> Int 4]
 where
  rho = V 0

exampleProdLit :: Term
exampleProdLit = ("x" :|> f :* "w" :|> Int 0) :* ("y" :|> Int 4 :* "z" :|> Int 1)
 where
  f = Abs (V 0) (Var (V 0))

exampleUpdate :: Term
exampleUpdate = Abs r (Abs v $ ("x" :|> Var v) :* Prj R (Var r)) :@ recordLit ["x" :|> Int 10, "w" :|> Int 0, "y" :|> Int 4, "z" :|> Int 1]
 where
  v = V 0
  r = V 1

showSimpl :: [TVar] -> [Q] -> [Q] -> String
showSimpl unifiers given wanted =
  case snd $ runIdentity $ runFresh nextTV $ runThrow (solveSimplConstraints unifiers given wanted) of
    Left (err :: TyErr) -> "Error during solving constraints " ++ show err
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
    Left (err :: TyErr) -> error ("Failed to infer a type: " ++ show err)
    Right (_, (subst, [])) -> apply subst ty
    Right (_, (subst, qs)) -> error ("Remaining Qs: " ++ show qs ++ "\nSubst: " ++ show subst ++ "\nType: " ++ ppShow (apply subst ty))
 where
  (tvar, (ty, constrs)) = runIdentity $ runReader emptyCtx $ runFresh (TV 0) $ generateConstraints term
  res = runIdentity . runReader ([] :: [Q]) . runThrow . runFresh tvar $ solveConstraints [] [] constrs

run m =
  let res = runIdentity . runThrow . runState ([] :: [TVar], mempty :: Subst) . runFresh (TV 0) $ m
   in case res of
        Left (err :: TyErr) -> error (show err)
        Right a -> a

runGen :: Term -> (Type, [Constraint])
runGen term = snd . runIdentity $ runReader emptyCtx $ runFresh (TV 0) $ generateConstraints term
