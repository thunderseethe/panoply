{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module OutsideIn where

import Fresh (Fresh, fresh, runFresh)

import qualified Data.IntSet as IntSet
import qualified Data.Map.Lazy as LazyMap
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map

import Control.Algebra (Has)
import Control.Effect.Reader (Reader, ask, local)
import Control.Lens (
  Identity (runIdentity),
  Plated (..),
  over,
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
import Data.List (nub, sortBy)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Debug.Trace
import Text.Show.Pretty
import Prelude hiding (abs, interact)

import Term
import Type
import Constraint
import Subst

data Eff = Eff {name :: Label, ops :: Map Label Scheme}

type EffSigs = Map Label (Map Label Scheme)
type SigsEff = Map Label (Eff, Scheme)

data EffCtx = EffCtx {eff :: EffSigs, sigs :: SigsEff}

emptyEffCtx :: EffCtx
emptyEffCtx = EffCtx Map.empty Map.empty

mkEffCtx :: (Foldable f) => f Eff -> EffCtx
mkEffCtx = foldr go (EffCtx Map.empty Map.empty)
 where
  go eff@(Eff name ops) (EffCtx effs sigs) =
    EffCtx (Map.insert name ops effs) (Map.foldrWithKey (\op sig sigs -> Map.insert op (eff, sig) sigs) sigs ops)

data Scheme = Scheme [TVar] [Q] Type
  deriving (Show, Eq, Ord)

monoScheme :: Type -> Scheme
monoScheme = Scheme [] []

instantiate :: (Has (Fresh TVar) sig m) => Scheme -> m ([Constraint], Type)
instantiate (Scheme bound qs ty) = do
    freshVars <- mapM (\var -> (,) var . VarTy <$> fresh) bound
    let subst = foldr (\(var, ty) subst -> insert var ty subst) mempty freshVars
    return (Simp <$> apply subst qs, apply subst ty)


type Ctx = Map Var Scheme

bind :: Var -> Type -> Ctx -> Ctx
bind var ty = Map.insert var (monoScheme ty)

emptyCtx :: Ctx
emptyCtx = Map.empty

generateConstraints :: (Has (Reader Ctx) sig m, Has (Reader EffCtx) sig m, Has (Fresh TVar) sig m) => Term () -> m (Term Type, InternalRow, [Constraint])
generateConstraints term =
  case term of
    -- Literals
    Var _ x -> do
      ctx <- ask
      case ctx !? x of
        Nothing -> error ("Undefined variable " ++ show x)
        Just scheme -> do
          eff <- fresh
          (constrs, ty) <- instantiate scheme
          return (Var ty x, Open eff, constrs)
    Int _ i -> do
      return (Int IntTy i, Closed Map.empty, [])
    Unit _ -> do
      return (Unit unitTy, Closed Map.empty, [])
    -- Labelled Types
    Label _ lbl term -> do
      (term_ty, term_eff, term_constr) <- generateConstraints term
      return (Label (RowTy $ lbl |> meta term_ty) lbl term_ty, term_eff, term_constr)
    Unlabel _ term lbl -> do
      alpha <- fresh
      (term_ty, term_eff, term_constr) <- generateConstraints term
      return (Unlabel (VarTy alpha) term_ty lbl, term_eff, Simp (meta term_ty ~ RowTy (lbl |> VarTy alpha)) : term_constr)
    -- Records
    Concat _ left right -> do
      out <- fresh
      left_var <- fresh
      right_var <- fresh
      out_eff <- fresh
      (left_ty, left_eff, left_constr) <- generateConstraints left
      (right_ty, right_eff, right_constr) <- generateConstraints right
      -- We don't want product types to show up in CombineEquals, so we unwrap them into Rows before constructing our Constraint
      return
        ( Concat (ProdTy $ VarTy out) left_ty right_ty
        , Open out_eff
        , Simp (meta left_ty ~ ProdTy (VarTy left_var)) :
          Simp (meta right_ty ~ ProdTy (VarTy right_var)) :
          Simp (VarTy out ~> Open left_var :⊙ Open right_var) :
          Simp (VarTy out_eff ~> left_eff :⊙ right_eff) :
          left_constr <> right_constr
        )
    -- Todo figure out what to do with this, I think in theory we shouldn't be able to store effects in records at all?
    Prj _ dir term -> do
      rest <- fresh
      out <- fresh
      input <- fresh
      (term_ty, term_eff, term_constr) <- generateConstraints term
      -- We want to unwrap a product into it's underlying row for it's constraint
      let constr =
            case dir of
              L -> VarTy input ~> Open out :⊙ Open rest
              R -> VarTy input ~> Open rest :⊙ Open out
      return (Prj (ProdTy $ VarTy out) dir term_ty, term_eff, Simp (meta term_ty ~ ProdTy (VarTy input)) : Simp constr : term_constr)
    -- Functions
    App _ fn arg -> do
      alpha <- fresh
      eff <- fresh
      --fn_eff_open <- fresh
      --arg_eff_open <- fresh
      (fn_ty, fn_eff, fn_constr) <- generateConstraints fn
      (arg_ty, arg_eff, arg_constr) <- generateConstraints arg
      return $ (\s -> trace (ppShow s) s)
        ( App (VarTy alpha) fn_ty arg_ty
        , Open eff
        , Simp (VarTy eff ~> fn_eff :⊙ arg_eff) :
          Simp (meta fn_ty ~ FunTy (meta arg_ty) fn_eff (VarTy alpha)) :
          fn_constr <> arg_constr
        )
    Abs _ x body -> do
      alpha <- fresh
      eff <- fresh
      (ret_ty, body_eff, constr) <- local (Map.insert x (monoScheme $ VarTy alpha)) (generateConstraints body)
      return $ (\s -> trace (ppShow s) s) (Abs (FunTy (VarTy alpha) body_eff (meta ret_ty)) x ret_ty, Open eff, constr)
    Op _ op -> do
      EffCtx _ sigs <- ask
      let (eff_name, scheme) =
            case sigs !? op of
              Nothing -> error $ "Undefined operator " ++ show op
              Just (Eff name _, sig) -> (name, sig)
      (constrs, ty) <- instantiate scheme
      return
        ( Op ty op
        , Closed (eff_name |> unitTy)
        , constrs
        )
    Handle _ lbl (HandleClause clauses (Clause ret_name ret_arg ret_resume ret_body)) body -> do
      EffCtx eff _ <- ask
      let ops = fromMaybe (error $ "Undefined effect " ++ show lbl) (eff !? lbl)
      let handled_eff = Closed (lbl |> unitTy)
      _ <-
        if clauses `covers` ops
          then return ()
          else error $ "Clauses " ++ show (NonEmpty.toList clauses) ++ " don't cover " ++ show (Map.keys ops)
      (body_ty, body_eff, body_constr) <- generateConstraints body
      eff <- fresh
      alpha <- fresh
      (ret_ty, ret_eff, ret_constr) <- local (Map.insert ret_arg (monoScheme $ VarTy alpha)) $ generateConstraints ret_body
      (clauses, op_constr) <-
        NonEmpty.unzip
          <$> forM
            clauses
            ( \(Clause name x resume clause_body) -> do
                -- Signatures of ops cannot call other effects and so must be total
                -- So we don't check any effects here
                (op_constr, op_ty) <-
                  case ops !? name of
                    Nothing -> error $ "Undefined operator " ++ show name
                    Just scheme -> instantiate scheme

                -- This is probably too cute
                (tin, tout, ret) <- (,,) <$> fresh <*> fresh <*> fresh
                (clause_ty, clause_eff, clause_constr) <-
                  local (bind resume (FunTy (VarTy tout) (Open eff) (VarTy ret)) . bind x (VarTy tout)) $ generateConstraints clause_body
                return
                  ( Clause name x resume clause_ty
                  , Simp (VarTy ret ~ meta ret_ty) :
                    Simp (VarTy ret ~ meta clause_ty) : -- Clause type and return type must agree
                    Simp (op_ty ~ FunTy (VarTy tin) handled_eff (VarTy tout)) : -- Operation needs to be a function that handles our effect
                    Simp (VarTy eff ~ rowToType clause_eff) : -- Each clause can have pass on unhandled effects from handler body
                    op_constr <> clause_constr -- Ambient constraints from recursive calls
                  )
            )

      return
        ( Handle (meta ret_ty) lbl (HandleClause clauses (Clause ret_name ret_arg ret_resume ret_ty)) body_ty -- Return type of the handler is the type of the return clause
        , Open eff
        , Simp (rowToType body_eff ~> Open eff :⊙ handled_eff) : -- The body eff must include l, our output will be whatever effs are leftover from l
          Simp (VarTy eff ~ rowToType ret_eff) :
          Simp (VarTy alpha ~ meta body_ty) : -- Input to the return clause should match the body type
          body_constr <> ret_constr <> mconcat (NonEmpty.toList op_constr)
        )

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
    -- An empty row combined with an open row is just that open row
    T ty :<~> Closed a_row :⊙ Open b_tv
      | Map.null a_row -> return $ Work [ty ~ VarTy b_tv]
    T ty :<~> Open a_tv :⊙ Closed b_row
      | Map.null b_row -> return $ Work [ty ~ VarTy a_tv]
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
    T (RowTy r) :<~> Open _ :⊙ Open _ -> error ("Should this be allowed? " ++ show r)
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
  showsPrec p (Ct tv (T ty)) = showsPrec p tv . (" ~= " ++) . showsPrec p ty
  showsPrec p (Ct tv ct) = showsPrec p tv . (" ~= " ++) . showsPrec p ct

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
    RowLeft ty left right -> VarTy ty ~> apply subst left :⊙ right
    RowRight tv left right -> VarTy tv ~> left :⊙ apply subst right

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
    _ <- trace ("interactions:\n" ++ ppShowList (i : interacts)) $ return ()
    let is = nub $ foldMap interact (i : interacts)
    ((fresh_unifiers, subst), (canon, remainder)) <- runState ([], mempty) (canonicalize canon is)
    modify (remainder ++)
    _ <- trace ("output:\n" ++ ppShowList (sortBy (\(Ct a _) (Ct b _) -> compare a b) (canon <> inert_canon))) $ return ()
    return $ Just (unifiers <> fresh_unifiers, subst <> phi, given, canon <> inert_canon)
  | (i : interacts, inert_wanted) <- simplifications given wanted = do
    let is = simplify <$> i : interacts
    ((fresh_unifiers, subst), (canon, remainder)) <- runState ([], mempty) (canonicalize canon is)
    modify (remainder ++)
    return $ Just (unifiers <> fresh_unifiers, subst <> phi, given, canon <> inert_wanted)
solve _ _ _ _ = return Nothing

simples :: (Has (Fresh TVar) sig m, Has (Throw TyErr) sig m) => [TVar] -> [Q] -> [Q] -> m ([TVar], Subst, [CanonCt], ([CanonCt], [Q]))
simples unifiers noncanon_given noncanon_wanted = do
  _ <- trace ("wanted:\n" ++ ppShowList noncanon_wanted) $ return ()
  ((canon_unifiers, subst), ((given, _), (wanted, wanted_residue))) <- runState ([], mempty) $ do
    given <- canonicalize canon noncanon_given
    wanted <- canonicalize canon noncanon_wanted
    return (given, wanted)
  _ <- trace ("canonical:\n" ++ ppShowList wanted) $ return ()
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
  let q_residue = over typeOf (apply flatten_subst) residue
  let referenced_tvars = IntSet.fromList (fmap (\(Ct (TV tv) _) -> tv) q_wanted)
  -- tie the knot
  let theta =
        let q_to_pairs subst q =
              case q of
                Ct tv (T ty) -> [(tv, apply subst ty)]
                -- TODO: Not sure if this works in general. Is it safe to assume a row is closed if we have an unsolved variable that's never referenced?
                Ct tvar (Closed row :⊙ Open (TV tv)) | not (IntSet.member tv referenced_tvars) -> [(tvar, RowTy row)]
                Ct tvar (Open (TV tv) :⊙ Closed row) | not (IntSet.member tv referenced_tvars) -> [(tvar, RowTy row)]
                Ct _ (_ :⊙ _) -> []
         in Subst . LazyMap.fromList . mconcat $ q_to_pairs theta <$> q_wanted
  return (theta, over typeOf (apply theta) q_residue)
 where
  flatten_q subst q =
    case q of
      Ct tv (T ty) -> Ct tv (T (apply subst ty))
      Ct tv (left :⊙ right) -> Ct tv (apply subst left :⊙ apply subst right)

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

example :: Term ()
example = abs [x, y, z] $ var x <@> var z <@> (var y <@> var z)
 where
  x = V 0
  y = V 1
  z = V 2

exampleInt :: Term ()
exampleInt = abs [f, g] $ var f <@> int 0 <@> (var g <@> int 11)
 where
  f = V 0
  g = V 1

exampleRow :: Term ()
exampleRow =
  abs [rho] (unlabel (prj L (var rho)) "x")
 where
  -- <@> record [label "x" (int 2), label "w" (int 0), label "y" (int 4)]

  rho = V 0

exampleProdLit :: Term ()
exampleProdLit = record [label "x" f, label "w" (int 0), label "y" (int 4), label "z" (int 1)]
 where
  f = abs [V 0] (var (V 0))

exampleUpdate :: Term ()
exampleUpdate = abs [r] (abs [v] $ record [label "x" $ var v, prj R (var r)]) <@> record [label "x" (int 10), label "w" (int 0), label "y" (int 4), label "z" (int 1)]
 where
  v = V 0
  r = V 1

exampleSmolEff :: Term ()
exampleSmolEff = handle "State" (HandleClause clauses ret) (op "get" <@> unit)
 where
  resume = V 1
  x = V 2
  -- All of these are in disjoint scopes so we're okay to reuse variables, although it's kinda sloppy
  clauses = Clause "get" x resume (var resume <@> int 4) :| [Clause "put" (V 3) (V 4) (var (V 4) <@> unit)]
  ret = Clause "" x resume (label "x" $ var x)

exampleEff :: Term ()
exampleEff = handle "State" (HandleClause clauses ret) $ abs [v] (op "put" <@> var v) <@> (op "get" <@> unit)
 where
  v = V 0
  resume = V 1
  x = V 2
  -- All of these are in disjoint scopes so we're okay to reuse variables, although it's kinda sloppy
  clauses = Clause "get" x resume (var resume <@> int 4) :| [Clause "put" x resume (var resume <@> unit)]
  ret = Clause "" x resume (label "x" $ var x)

exampleGet :: Term ()
exampleGet = abs [v] $ var v <@> (op "get" <@> unit)
 where
  v = V 0

-- This one should fail and it does!
exampleHandleErr :: Term ()
exampleHandleErr = handle "State" (HandleClause whoops ret) $ abs [v] (op "put" <@> var v) <@> (op "get" <@> unit)
 where
  v = V 0
  resume = V 1
  x = V 2
  -- We forgot a clause
  whoops = Clause "get" x resume (int 4) :| []
  ret = Clause "" x resume (label "x" $ var x)

defaultEffCtx :: EffCtx
defaultEffCtx =
  mkEffCtx
    [ Eff "State" (Map.fromList [("get", monoScheme (FunTy unitTy (Closed $ "State" |> unitTy) IntTy)), ("put", monoScheme (FunTy IntTy (Closed $ "State" |> unitTy) unitTy))])
    , Eff "RowEff" (Map.fromList [("add_field", Scheme [TV 0, TV 1, TV 2] [VarTy 0 ~> Closed ("x" |> IntTy) :⊙ Open 1, VarTy 2 ~> Closed ("y" |> IntTy) :⊙ Open 1] (FunTy (VarTy 0) (Closed $ "RowEff" |> unitTy) (VarTy 2)))])
    ]

instance SubstApp (Term Type) where
  apply subst = go
     where
      go = over meta' (closeEffects . apply subst) . over plate go
      closeEffects =
        transform
          ( over
              typeEffs
              ( \case
                  Open _ -> Closed Map.empty
                  Closed row -> Closed row
              )
          )

infer :: Term () -> (Term Type, InternalRow)
infer term =
  case res of
    Left (err :: TyErr) -> error ("Failed to infer a type: " ++ show err)
    Right (_, (subst, [])) -> trace (ppShow subst ++ "\n") ({-apply subst-} typed_term, apply subst eff)
    Right (_, (subst, qs)) -> error ("Remaining Qs: " ++ show qs ++ "\nSubst: " ++ show subst ++ "\nType: " ++ ppShow typed_term)
 where
  (_, (tvar, (typed_term, eff, constrs))) = runIdentity $ runReader defaultEffCtx $ runReader emptyCtx $ runFresh (maxVar + 1) $ runFresh (TV 0) $ generateConstraints term
  maxVar = maxTermVar term
  res = runIdentity . runReader ([] :: [Q]) . runThrow . runFresh tvar $ solveConstraints [] [] constrs
