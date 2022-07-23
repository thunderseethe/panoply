{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module OutsideIn where

import Fresh (Fresh, fresh, runFresh)

import qualified Data.Map.Lazy as LazyMap
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

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
import Control.Lens.Plated (cosmos, (...))
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
  | FunTy Type InternalRow Type
  deriving (Eq, Ord)

unitTy :: Type
unitTy = RowTy Map.empty

typeEffs :: Traversal' Type InternalRow
typeEffs f = 
  \case
    FunTy arg eff ret -> FunTy arg <$> f eff <*> pure ret
    ty -> pure ty

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
  showsPrec p (FunTy arg eff ret) = parens (showsPrec 11 arg . (" ->{" ++) . showsPrec p eff . ("} " ++) . showsPrec 9 ret)
   where
    parens =
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

data Ct
  = T Type
  | InternalRow :⊙ InternalRow
  deriving (Show, Eq, Ord)

ctOccurs :: TVar -> Ct -> Bool
ctOccurs tvar = anyOf (ctTys . typeVars) (== tvar)

ctTys :: Traversal' Ct Type
ctTys f =
  \case
    T ty -> T <$> f ty
    left :⊙ right -> (:⊙) <$> internalRowTys f left <*> internalRowTys f right

-- Need a better name for this
data Q
  = -- Two types must be equal (aka unifiable)
    Ct :<~> Ct
  deriving (Eq, Ord)

instance Show Q where
  showsPrec p (T t1 :<~> T t2) = showsPrec p t1 . (" :~ " ++) . showsPrec p t2
  showsPrec p (T t1 :<~> ct) = showsPrec p t1 . (" :~> " ++) . showsPrec p ct
  showsPrec p (ct :<~> T t2) = showsPrec p ct . (" :<~ " ++) . showsPrec p t2
  showsPrec p (ct1 :<~> ct2) = showsPrec p ct1 . (" :<~> " ++) . showsPrec p ct2

infixl 9 :⊙
infixl 8 :<~>
infixl 8 ~
infixl 8 ~>
infixl 8 <~

(~) :: Type -> Type -> Q
t1 ~ t2 = T t1 :<~> T t2

(~>) :: Type -> Ct -> Q
ty ~> ct = T ty :<~> ct

(<~) :: Ct -> Type -> Q
ct <~ ty = ct :<~> T ty

{-pattern (:~) :: Type -> Type -> Q
pattern t1 :~ t2 = T t1 :<~> T t2

pattern (:<~) :: Ct -> Type -> Q
pattern ct :<~ t2 = ct :<~> T t2

pattern (:~>) :: Type -> Ct -> Q
pattern t1 :~> ct = T t1 :<~> ct-}

qTys :: Traversal' Q Type
qTys f q =
  case q of
    -- Is doing builtin recursion like this bad?
    t1 :<~> t2 -> (:<~>) <$> ctTys f t1 <*> ctTys f t2

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

generateConstraints :: (Has (Reader Ctx) sig m, Has (Fresh TVar) sig m) => Term -> m (Type, InternalRow, [Constraint])
generateConstraints term =
  case term of
    -- Literals
    Var x -> do
      ctx <- ask
      case ctx !? x of
        Nothing -> error ("Undefined variable " ++ show x)
        Just (Scheme bound constr ty) -> do
          eff <- fresh
          freshVars <- mapM (\var -> (,) var . VarTy <$> fresh) bound
          let subst = foldr (\(var, ty) subst -> insert var ty subst) mempty freshVars
          return (apply subst ty, Open eff, Simp <$> over (traverse . qTys) (apply subst) constr)
    Int _ -> do
      return (IntTy, Closed Map.empty, [])
    -- Labelled Types
    lbl :|> term -> do
      (term_ty, term_eff, term_constr) <- generateConstraints term
      return (RowTy $ lbl |> term_ty, term_eff, term_constr)
    term :/ lbl -> do
      alpha <- fresh
      (term_ty, term_eff, term_constr) <- generateConstraints term
      return (VarTy alpha, term_eff, Simp (term_ty ~ RowTy (lbl |> VarTy alpha)) : term_constr)
    -- Records
    left :* right -> do
      out <- fresh
      left_var <- fresh
      right_var <- fresh
      out_eff <- fresh
      (left_ty, left_eff, left_constr) <- generateConstraints left
      (right_ty, right_eff, right_constr) <- generateConstraints right
      -- We don't want product types to show up in CombineEquals, so we unwrap them into Rows before constructing our Constraint
      return
        ( ProdTy $ VarTy out
        , Open out_eff
        , Simp (left_ty ~ ProdTy (VarTy left_var)) :
          Simp (right_ty ~ ProdTy (VarTy right_var)) :
          Simp (VarTy out ~> Open left_var :⊙ Open right_var) :
          Simp (VarTy out_eff ~> left_eff :⊙ right_eff) :
          left_constr <> right_constr
        )
    -- Todo figure out what to do with this, I think in theory we shouldn't be able to store effects in records at all?
    Prj dir term -> do
      rest <- fresh
      out <- fresh
      input <- fresh
      (term_ty, term_eff, term_constr) <- generateConstraints term
      -- We want to unwrap a product into it's underlying row for it's constraint
      let constr =
            case dir of
              L -> VarTy input ~> Open out :⊙ Open rest
              R -> VarTy input ~> Open rest :⊙ Open out
      return (ProdTy $ VarTy out, term_eff, Simp (term_ty ~ ProdTy (VarTy input)) : Simp constr : term_constr)
    -- Functions
    fn :@ arg -> do
      alpha <- fresh
      eff <- fresh
      fn_eff_open <- fresh
      arg_eff_open <- fresh
      (fn_ty, fn_eff, fn_constr) <- generateConstraints fn
      (arg_ty, arg_eff, arg_constr) <- generateConstraints arg
      return (VarTy alpha, Open eff, Simp (VarTy eff ~> fn_eff :⊙ Open fn_eff_open) : Simp (VarTy eff ~> arg_eff :⊙ Open arg_eff_open) : Simp (fn_ty ~ FunTy arg_ty fn_eff (VarTy alpha)) : fn_constr ++ arg_constr)
    Abs x body -> do
      alpha <- fresh
      eff <- fresh
      (ret_ty, body_eff, constr) <- local (Map.insert x (monoScheme $ VarTy alpha)) (generateConstraints body)
      return (FunTy (VarTy alpha) body_eff ret_ty, Open eff, constr)

data TyErr
  = OccursCheckFailed TVar Ct
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
  deriving (Show)

{- canonicalize a generic constraint. this function preforms rewrites that are the same for wanted and given constraints -}
canon :: (Has (Throw TyErr) sig m) => Q -> m CanonResult
canon q =
  case q of
    -- Check reflexive case first, if types are syntatically equal we can remove the constraint without further work
    a :<~> b | a == b -> return $ Work []
    T a :<~> T b ->
      case (a, b) of
        -- Decompose function type into arg and ret constraints
        (FunTy a_arg a_eff a_ret, FunTy b_arg b_eff b_ret) -> return $ Work [a_arg ~ b_arg, rowToType a_eff ~ rowToType b_eff, a_ret ~ b_ret]
        -- Order constraint
        (FunTy arg eff ret, VarTy tvar) -> return $ Work [VarTy tvar ~ FunTy arg eff ret]
        (a@(FunTy{}), b) -> return . Residual $ a ~ b
        -- Decompose product
        (ProdTy a_ty, ProdTy b_ty) -> return $ Work [a_ty ~ b_ty]
        -- A product equals a row type if it's underyling type equals the row
        (ProdTy ty, RowTy row) -> return $ Work [ty ~ RowTy row]
        -- Order constraint
        (ProdTy ty, VarTy tvar) -> return $ Work [VarTy tvar ~ ProdTy ty]
        (a@(ProdTy _), b) -> return . Residual $ a ~ b
        -- Decompose row types into an equality for each matching keys
        -- If the row keys don't match up it's a type error
        (a@(RowTy a_row), b@(RowTy b_row)) ->
          let row_eqs =
                Map.mergeA
                  (Map.traverseMissing (\_ _ -> Left (Residual (a ~ b))))
                  (Map.traverseMissing (\_ _ -> Left (Residual (a ~ b))))
                  (Map.zipWithAMatched (\_ a b -> pure (a ~ b)))
                  a_row
                  b_row
           in case row_eqs of
                Left res -> return res
                Right work -> return . Work . Map.elems $ work
        -- A product equals a row type if it's underyling type equals the row
        (RowTy row, ProdTy ty) -> return $ Work [ty ~ RowTy row]
        -- Order constraint
        (RowTy row, VarTy tvar) -> return $ Work [VarTy tvar ~ RowTy row]
        (a@(RowTy _), b) -> return . Residual $ a ~ b
        -- Order constraint
        (VarTy a_tv, VarTy b_tv) | b_tv < a_tv -> return $ Done (Ct b_tv (T $ VarTy a_tv))
        (VarTy tvar, ty) ->
          -- If we fail an occurs check throw to break out of cannon
          if occurs tvar ty
            then throwError (OccursCheckFailed tvar (T ty))
            else return . Done $ Ct tvar (T ty)
        -- Order constraint
        (IntTy, VarTy tvar) -> return $ Work [VarTy tvar ~ IntTy]
        (IntTy, IntTy) -> return $ Work []
        (IntTy, ty) -> return . Residual $ IntTy ~ ty
    --(a, b) -> return $ Residual (a ~ b)
    -- Trivial case where all rows are known. Reduces to an equality
    T ty :<~> Closed a_row :⊙ Closed b_row -> return $ Work [ty ~ RowTy (a_row <> b_row)]
    T (RowTy goal) :<~> Closed a_row :⊙ Open b_tv
      | rowOccurs b_tv (Closed goal) -> throwError (OccursCheckFailed b_tv (T $ RowTy goal))
      | otherwise ->
        let (a_goal, b_goal) = Map.partitionWithKey (\k _ -> Map.member k a_row) goal
         in return $ Work [RowTy a_goal ~ RowTy a_row, VarTy b_tv ~ RowTy b_goal]
    T (RowTy goal) :<~> Open a_tv :⊙ Closed b_row
      | rowOccurs a_tv (Closed goal) -> throwError (OccursCheckFailed a_tv (T $ RowTy goal))
      | otherwise ->
        let (b_goal, a_goal) = Map.partitionWithKey (\k _ -> Map.member k b_row) goal
         in return $ Work [VarTy a_tv ~ RowTy a_goal, RowTy b_row ~ RowTy b_goal]
    T (RowTy _) :<~> Open _ :⊙ Open _ -> error "Should this be allowed?"
    T IntTy :<~> ct@(_ :⊙ _) -> return $ Residual (IntTy ~> ct)
    T (ProdTy ty) :<~> ct@(_ :⊙ _) -> return $ Residual (ProdTy ty ~> ct)
    T (FunTy arg eff ret) :<~> ct@(_ :⊙ _) -> return $ Residual (FunTy arg eff ret ~> ct)
    T (VarTy tvar) :<~> ct ->
      -- If we fail an occurs check throw to break out of cannon
      if ctOccurs tvar ct
        then throwError (OccursCheckFailed tvar ct)
        else return . Done $ Ct tvar ct
    a_row :⊙ b_row :<~> T ty -> return $ Work [ty ~> (a_row :⊙ b_row)]
    _ :⊙ _ :<~> _ :⊙ _ -> error "Should this be canonicalized? I think this shouldn't even be possible"

-- Expectation is these are un-unifiable types
-- Bubble them up as residual inert constraints to be handled
--_  -> return $ Residual (a :<~> b)

{- A canonical constraint.
   This is a tvar lhs equal to a type that does not contain tvar -}
data CanonCt
  = Ct TVar Ct
  deriving (Eq)

instance Show CanonCt where
  showsPrec p (Ct tv ty) = showsPrec p tv . (" ~= " ++) . showsPrec p ty

{- binary interaction between two constraints from the same set (wanted or given) -}
interact :: Interaction -> [Q]
interact i =
  -- The paper covers other rules but they are for type classes and type families
  case i of
    -- EQSAME
    InteractEq a_tv a_ty _ b_ct -> [VarTy a_tv ~ a_ty, a_ty ~> b_ct]
    -- EQDIFF
    InteractOccurs a_tv a_ty b_occurence -> [VarTy a_tv ~ a_ty, applyOccurs (a_tv |-> a_ty) b_occurence]
    -- Row stuff
    InteractRowCombine a_goal a_left a_right b_goal b_left b_right ->
      case (a_left, a_right, b_left, b_right) of
        -- All closed, reduces to an equality on RowTys
        (Closed a_left, Closed a_right, Closed b_left, Closed b_right) -> [VarTy a_goal ~ RowTy (a_left <> a_right), RowTy (a_left <> a_right) ~ RowTy (b_left <> b_right)]
        -- Each row is partially open and partially closed
        -- These are the trickiest cases as we have to handle all permutations of possible equalities
        -- a_row and b_row could be disjoint, total overlap, or in a subset relationship
        (Closed a_row, Open a_tv, Closed b_row, Open b_tv) -> (VarTy a_goal ~> a_left :⊙ a_right) : equatePartiallyOpenRows a_row a_tv b_row b_tv
        (Open a_tv, Closed a_row, Closed b_row, Open b_tv) -> (VarTy a_goal ~> a_left :⊙ a_right) : equatePartiallyOpenRows a_row a_tv b_row b_tv
        (Open a_tv, Closed a_row, Open b_tv, Closed b_row) -> (VarTy a_goal ~> a_left :⊙ a_right) : equatePartiallyOpenRows a_row a_tv b_row b_tv
        (Closed a_row, Open a_tv, Open b_tv, Closed b_row) -> (VarTy a_goal ~> a_left :⊙ a_right) : equatePartiallyOpenRows a_row a_tv b_row b_tv
        -- Only one variable present
        (Open tv, Closed row, Closed sub_left, Closed sub_right) -> equateOneOpenVariable sub_left sub_right row tv a_goal
        (Closed row, Open tv, Closed sub_left, Closed sub_right) -> equateOneOpenVariable sub_left sub_right row tv a_goal
        (Closed sub_left, Closed sub_right, Open tv, Closed row) -> equateOneOpenVariable sub_left sub_right row tv b_goal
        (Closed sub_left, Closed sub_right, Closed row, Open tv) -> equateOneOpenVariable sub_left sub_right row tv b_goal
        -- The rest of cases are straightforward equalities between lefts and rights
        (a_left, a_right, b_left, b_right) -> [VarTy a_goal ~> a_left :⊙ a_right, rowToType a_left ~ rowToType b_left, rowToType a_right ~ rowToType b_right]
 where
  equateOneOpenVariable sub_left sub_right row tv goal =
    VarTy goal ~> Closed row :⊙ Open tv :
    if
        | Map.keys row == Map.keys sub_left -> [RowTy row ~ RowTy sub_left, VarTy tv ~ RowTy sub_right]
        | Map.keys row == Map.keys sub_right -> [RowTy row ~ RowTy sub_right, VarTy tv ~ RowTy sub_left]
        | Map.keys row == Map.keys (sub_left <> sub_right) -> [RowTy row ~ RowTy (sub_left <> sub_right), VarTy tv ~ RowTy Map.empty]
        | otherwise -> [VarTy tv ~ RowTy (sub_left <> sub_right)]

  equatePartiallyOpenRows a_row a_tv b_row b_tv =
    let (a_in_b, a_rest) = Map.partitionWithKey (\k _ -> Map.member k b_row) a_row
     in -- a_left and b_left are disjoint, so set them to each other's open records
        if
            | Map.null a_in_b -> [VarTy b_tv ~ RowTy a_rest, VarTy a_tv ~ RowTy b_row]
            -- a and b fully overlap, so should be equated to each other
            | Map.null a_rest -> [RowTy a_in_b ~ RowTy b_row, VarTy a_tv ~ VarTy b_tv]
            -- a and b only partially overlap, equate the parts that do overlap and equate the variables for the rest
            | otherwise ->
              let (b_in_a, b_rest) = Map.partitionWithKey (\k _ -> Map.member k a_in_b) b_row
               in [RowTy a_in_b ~ RowTy b_in_a, VarTy b_tv ~ RowTy a_rest, VarTy a_tv ~ RowTy b_rest]

{- binary interaction between constraints from each set (wanted and given).
  Because of this order is important, and the returned constraint is a wanted (replacing the input wanted constraint) -}
simplify :: Interaction -> Q
simplify i =
  -- These are basically the same rules as interact but because they combine a given with a wanted we're allowed to simplify further
  -- Basically saturating an assumption with an inference.
  -- Like interact there are other rules for type families and type classes that we leave out for now
  case i of
    -- SEQSAME
    InteractEq _ given_ty _ (T wanted_ty) -> given_ty ~ wanted_ty
    InteractEq _ given_ty _ (wanted_left :⊙ wanted_right) -> given_ty ~> wanted_left :⊙ wanted_right
    -- SEQDIFF
    InteractOccurs given_tv given_ty wanted_occurence -> applyOccurs (given_tv |-> given_ty) wanted_occurence
    -- Row stuff
    InteractRowCombine{} -> error "todo!"

data Occurs
  = Within TVar Type
  | RowLeft TVar InternalRow InternalRow
  | RowRight TVar InternalRow InternalRow
  deriving (Eq, Show)

applyOccurs :: Subst -> Occurs -> Q
applyOccurs subst =
  \case
    Within tv ty -> VarTy tv ~ apply subst ty
    RowLeft ty left right -> VarTy ty ~> applyRow subst left :⊙ right
    RowRight tv left right -> VarTy tv ~> left :⊙ applyRow subst right

data Interaction
  = -- The canonical constraints are for the same tvar
    InteractEq TVar Type TVar Ct
  | -- The tvar of the first canonical constraint occurs in the type of the second
    InteractOccurs TVar Type Occurs
  | -- Two combine predicates equal each other (because they equate to the same tvar)
    InteractRowCombine TVar InternalRow InternalRow TVar InternalRow InternalRow
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
    let inters = dropWhile ((== Nothing) . snd) $ mkInteraction rest ct <$> rest
     in case inters of
          (filtered_rest, Just i) : _ -> first (i :) (selectInteractions filtered_rest)
          (_, Nothing) : _ -> undefined
          [] -> second (ct :) (selectInteractions rest)

  mkInteraction canons a b =
    let filteredCanons int = (filter (\c -> c /= a && c /= b) canons, Just int)
     in case (a, b) of
          (Ct a_tv (T a_ty), Ct b_tv (T b_ty))
            | a_tv == b_tv -> filteredCanons $ InteractEq a_tv a_ty b_tv (T b_ty)
            | occurs a_tv b_ty -> filteredCanons $ InteractOccurs a_tv a_ty (Within b_tv b_ty)
            | occurs b_tv a_ty -> filteredCanons $ InteractOccurs b_tv b_ty (Within a_tv a_ty)
          -- Handle entailment cases
          (Ct a_tv (T a_ty), Ct b_tv (b_left :⊙ b_right))
            | a_tv == b_tv -> filteredCanons $ InteractEq a_tv a_ty b_tv (b_left :⊙ b_right)
          (Ct a_tv (T a_ty), Ct b_goal (b_left :⊙ b_right))
            | rowOccurs a_tv b_left -> filteredCanons $ InteractOccurs a_tv a_ty (RowLeft b_goal b_left b_right)
            | rowOccurs a_tv b_right -> filteredCanons $ InteractOccurs a_tv a_ty (RowRight b_goal b_left b_right)
          -- Symmetrical cases
          (Ct a_tv (a_left :⊙ a_right), Ct b_tv (T b_ty))
            | a_tv == b_tv -> filteredCanons $ InteractEq b_tv b_ty a_tv (a_left :⊙ a_right)
          (Ct a_goal (a_left :⊙ a_right), Ct b_tv (T b_ty))
            | rowOccurs b_tv a_left -> filteredCanons $ InteractOccurs b_tv b_ty (RowLeft a_goal a_left a_right)
            | rowOccurs b_tv a_right -> filteredCanons $ InteractOccurs b_tv b_ty (RowRight a_goal a_left a_right)
          (Ct a_goal (a_left :⊙ a_right), Ct b_goal (b_left :⊙ b_right))
            | a_goal == b_goal -> filteredCanons $ InteractRowCombine a_goal a_left a_right b_goal b_left b_right
          (_, _) -> (canons, Nothing)

simplifications :: [CanonCt] -> [CanonCt] -> ([Interaction], [CanonCt])
simplifications given wanted = second nub . partitionEithers $ mkInteraction <$> candidates
 where
  candidates = [(given_ct, wanted_ct) | given_ct <- given, wanted_ct <- wanted]
  mkInteraction (Ct given_tv (T given_ty), Ct wanted_tv (T wanted_ty))
    | given_tv == wanted_tv = Left (InteractEq given_tv given_ty wanted_tv (T wanted_ty))
    | occurs given_tv wanted_ty = Left (InteractOccurs given_tv given_ty (Within wanted_tv wanted_ty))
  mkInteraction (Ct given_tv (T given_ty), Ct wanted_tv (wanted_left :⊙ wanted_right))
    | given_tv == wanted_tv = Left $ InteractEq given_tv given_ty wanted_tv (wanted_left :⊙ wanted_right)
    | rowOccurs given_tv wanted_left = Left $ InteractOccurs given_tv given_ty (RowLeft wanted_tv wanted_left wanted_right)
    | rowOccurs given_tv wanted_right = Left $ InteractOccurs given_tv given_ty (RowRight wanted_tv wanted_left wanted_right)
  mkInteraction (Ct given_tv (given_left :⊙ given_right), Ct wanted_tv (wanted_left :⊙ wanted_right))
    | given_tv == wanted_tv = Left (InteractRowCombine given_tv given_left given_right wanted_tv wanted_left wanted_right)
  -- Order is important here as our input is (given, wanted). Which asks does it even make sense for this case to occur?
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
      Ct tv (T ty) -> [(tv, apply subst ty)]
      Ct _ (_ :⊙ _) -> []
  flatten_q subst q =
    case q of
      Ct tv (T ty) -> Ct tv (T (apply subst ty))
      Ct tv (left :⊙ right) -> Ct tv (applyRow subst left :⊙ applyRow subst right)

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

infer :: Term -> (Type, InternalRow)
infer term =
  case res of
    Left (err :: TyErr) -> error ("Failed to infer a type: " ++ show err)
    Right (_, (subst, [])) -> trace (ppShow subst ++ "\n") (closeEffects $ apply subst ty, applyRow subst eff)
    Right (_, (subst, qs)) -> error ("Remaining Qs: " ++ show qs ++ "\nSubst: " ++ show subst ++ "\nType: " ++ ppShow (apply subst ty))
 where
  (tvar, (ty, eff, constrs)) = runIdentity $ runReader emptyCtx $ runFresh (TV 0) $ generateConstraints term
  res = runIdentity . runReader ([] :: [Q]) . runThrow . runFresh tvar $ solveConstraints [] [] constrs

  closeEffects = transform (over typeEffs
    (\case
        Open _ -> Closed Map.empty
        Closed row -> Closed row))

fn :: Type
fn = FunTy (VarTy 4) (Open 5) (FunTy (VarTy 16) (Open 11) (VarTy 6))

run m =
  let res = runIdentity . runThrow . runState ([] :: [TVar], mempty :: Subst) . runFresh (TV 0) $ m
   in case res of
        Left (err :: TyErr) -> error (show err)
        Right a -> a

runGen :: Term -> (Type, InternalRow, [Constraint])
runGen term = snd . runIdentity $ runReader emptyCtx $ runFresh (TV 0) $ generateConstraints term
