{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OutsideIn where

import Fresh (Fresh, fresh, runFresh)

import qualified Data.Map.Lazy as LazyMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Algebra (Has)
import Control.Effect.Reader (Reader, ask, local)
import Control.Lens (
  Identity (runIdentity),
  Lens',
  cosmos,
  lens,
  over,
  toListOf,
  transform,
  view,
  (%~),
  (&),
  (^.),
 )
import Control.Lens.Tuple
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
import Prelude hiding (abs, interact, lookup, unzip3)

import Constraint
import Core (CoreType (..), coreRowTv, coreTyTv)
import qualified Core
import Data.Text (unpack)
import Prettyprinter
import Prettyprinter.Render.Text
import Subst
import qualified TVarSet
import Term
import Type
import Program
import Control.Effect.State (get)

data Wrapper
  = TyApp Wrapper Core.CoreType
  | EvApp Wrapper Core.CoreVar
  | Hole

fillIn :: Wrapper -> Core.Core -> Core.Core
fillIn wrap core =
  case wrap of
    Hole -> core
    TyApp hole ty -> Core.TyApp (fillIn hole core) ty
    EvApp hole ev -> Core.App (fillIn hole core) (Core.Var ev)

rowTVars :: [Q] -> TVarSet.TVarSet
rowTVars qs = TVarSet.fromList (qs >>= rowTvs)
 where
  rowTvs (c1 :<~> c2) = ct c1 ++ ct c2

  ct (T (VarTy tv)) = [tv]
  ct (T _) = []
  ct (l :⊙ r) = internalRow l ++ internalRow r

  internalRow (Open tv) = [tv]
  internalRow (Closed _) = []

schemeWrapper :: (Has (Fresh Var) sig m, Has (State (Map Var Q)) sig m) => [TVar] -> [Q] -> m Wrapper
schemeWrapper bound qs = do
  ev_wrap <- foldrM f Hole (qs >>= rowEvs)
  return (foldr wrapEv ev_wrap bound)
 where
  f rowEvTy wrapper = do
    var <- fresh
    return (EvApp wrapper (Core.CoreV var rowEvTy))

  wrapEv tv =
    if TVarSet.member tv rowSet
      then \wrap -> TyApp wrap (CoreVar $ coreRowTv tv)
      else \wrap -> TyApp wrap (CoreVar $ coreTyTv tv)

  rowSet = rowTVars qs

  rowEvs (T (VarTy out) :<~> Open left :⊙ Open right) = [Core.rowEvType (Open left) (Open right) (Open out)]
  rowEvs (Open left :⊙ Open right :<~> T (VarTy out)) = [Core.rowEvType (Open left) (Open right) (Open out)]
  rowEvs _ = []

instantiate :: (Has (Fresh TVar) sig m, Has (Fresh Var) sig m, Has (State (Map Var Q)) sig m) => Scheme -> m ([Constraint], Wrapper, Type)
instantiate (Scheme bound qs ty) = do
  freshVars <- mapM (\var -> (,) var . VarTy <$> fresh) bound
  let subst = foldr (\(var, ty) subst -> insert var ty subst) mempty freshVars
  wrapper <- schemeWrapper bound (apply subst qs)
  return (Simp <$> apply subst qs, wrapper, apply subst ty)

data Infer = Infer {_ty :: Type, _eff :: InternalRow}
  deriving (Show)

instance SubstApp Infer where
  apply subst (Infer ty eff) = Infer (apply subst ty) (apply subst eff)

ty :: Lens' Infer Type
ty = lens _ty (\(Infer _ eff) ty -> Infer ty eff)

eff :: Lens' Infer InternalRow
eff = lens OutsideIn._eff (\(Infer ty _) eff -> Infer ty eff)

type Ctx = Map Var Scheme

bind :: Var -> Type -> Ctx -> Ctx
bind var ty = Map.insert var (monoScheme ty)

emptyCtx :: Ctx
emptyCtx = Map.empty

askCtx :: Has (Reader Ctx) sig m => m Ctx
askCtx = ask

-- Global variables that can't be referenced by users
evv = V (-1)

generateConstraints ::
  ( Has (Reader Ctx) sig m
  , Has (Reader EffCtx) sig m
  , Has (State (Map Var Q)) sig m
  , Has (Fresh TVar) sig m
  , Has (Fresh Var) sig m
  ) =>
  Term () ->
  m (Term Infer, Core.Core, [Constraint])
generateConstraints term =
  case term of
    -- Literals
    Var _ x -> do
      ctx <- askCtx
      case ctx !? x of
        Nothing -> error ("Undefined variable " ++ show x)
        Just scheme -> do
          eff <- fresh
          (constrs, wrapper, ty) <- instantiate scheme
          return
            ( Var (Infer ty (Open eff)) x
            , fillIn wrapper (Core.Var (Core.CoreV x (Core.fromType ty)))
            , constrs
            )
    Int _ i -> do
      return
        ( Int (Infer Type.IntTy (Closed Map.empty)) i
        , Core.Lit (Core.I i)
        , []
        )
    Unit _ -> do
      return
        ( Unit (Infer unitTy (Closed Map.empty))
        , Core.Product []
        , []
        )
    -- Labelled Types
    Label _ lbl term -> do
      (term, term_core, term_constr) <- generateConstraints term
      let m = term ^. meta & ty %~ RowTy . (|>) lbl
      return
        ( Label m lbl term
        , term_core
        , term_constr
        )
    Unlabel _ term lbl -> do
      alpha <- fresh
      (term, term_core, term_constr) <- generateConstraints term
      return
        ( Unlabel (Infer (VarTy alpha) (term ^. meta . eff)) term lbl
        , term_core
        , Simp (term ^. meta . ty ~ RowTy (lbl |> VarTy alpha)) : term_constr
        )
    -- Records
    Concat _ left right -> do
      out <- fresh
      out_ev <- fresh
      left_var <- fresh
      right_var <- fresh
      out_eff <- fresh
      (left, left_core, left_constr) <- generateConstraints left
      (right, right_core, right_constr) <- generateConstraints right
      -- TODO: Should both functions be marked with out_eff here, or just one?
      let constr = VarTy out ~> Open left_var :⊙ Open right_var
      modify (Map.insert out_ev constr)
      return
        ( Concat (Infer (ProdTy $ VarTy out) (Open out_eff)) left right
        , Core.App (Core.App (Core.Project 0 (Core.Var $ Core.CoreV out_ev (Core.rowEvType (Open left_var) (Open right_var) (Open out)))) left_core) right_core
        , Simp (left ^. meta . ty ~ ProdTy (VarTy left_var)) :
          Simp (right ^. meta . ty ~ ProdTy (VarTy right_var)) :
          Simp constr :
          Simp (VarTy out_eff ~> (left ^. meta . eff) :⊙ (right ^. meta . eff)) :
          left_constr <> right_constr
        )
    Prj _ dir term -> do
      rest <- fresh
      out <- fresh
      out_ev <- fresh
      input <- fresh
      (term, term_core, term_constr) <- generateConstraints term
      -- We want to unwrap a product into it's underlying row for it's constraint
      let (idx, constr) =
            case dir of
              L -> (3, VarTy input ~> Open out :⊙ Open rest)
              R -> (4, VarTy input ~> Open rest :⊙ Open out)
      -- Save our constraint
      modify (Map.insert out_ev constr)
      return
        ( Prj (Infer (ProdTy $ VarTy out) (term ^. meta . eff)) dir term
        , Core.App (Core.Project 0 (Core.Project idx (Core.Var $ Core.CoreV out_ev (Core.rowEvType (Open input) (Open input) (Open out))))) term_core
        , Simp (term ^. meta . ty ~ ProdTy (VarTy input)) : Simp constr : term_constr
        )
    -- Special case application chains so we don't pass evidence vector unecessarily
    App _ fn@(App {}) arg -> do   
      alpha <- fresh
      out_eff <- fresh
      (fn, fn_core, fn_constr) <- generateConstraints fn
      (arg, arg_core, arg_constr) <- generateConstraints arg
      return
        ( App (Infer (VarTy alpha) (Open out_eff)) fn arg
        , Core.App fn_core arg_core
        , Simp (VarTy out_eff ~> (fn ^. meta . eff) :⊙ (arg ^. meta . eff)) :
          Simp (fn ^. meta . ty ~ FunTy (arg ^. meta . ty) (fn ^. meta . eff) (VarTy alpha)) :
          fn_constr <> arg_constr
        )
    -- Function application
    -- Here we're at the root of a chain so we pass the evidence vector to our function
    App _ fn arg -> do
      alpha <- fresh
      out_eff <- fresh
      (fn, fn_core, fn_constr) <- generateConstraints fn
      (arg, arg_core, arg_constr) <- generateConstraints arg
      return
        ( App (Infer (VarTy alpha) (Open out_eff)) fn arg
        -- Pass evidence vector as first argument to function
        , Core.App (Core.App fn_core (Core.Var $ Core.CoreV evv (Core.CoreVar $ Core.coreRowTv out_eff))) arg_core
        , Simp (VarTy out_eff ~> (fn ^. meta . eff) :⊙ (arg ^. meta . eff)) :
          Simp (fn ^. meta . ty ~ FunTy (arg ^. meta . ty) (fn ^. meta . eff) (VarTy alpha)) :
          fn_constr <> arg_constr
        )
    Abs _ x body -> do
      alpha <- fresh
      out_eff <- fresh
      (body, body_core, constr) <- local (Map.insert x (monoScheme $ VarTy alpha)) (generateConstraints body)
      let fn_ty = FunTy (VarTy alpha) (body ^. meta . eff) (body ^. meta . ty)
      return
        ( Abs (Infer fn_ty (Open out_eff)) x body
        -- Wrap any lambdas with an extra parameter for evidence vector
        , Core.Lam (Core.CoreV evv (CoreVar $ coreRowTv out_eff)) $ Core.Lam (Core.CoreV x (CoreVar $ coreTyTv alpha)) body_core
        , constr
        )
    Op _ op -> do
      EffCtx _ sigs <- ask
      let (eff_name, scheme) =
            case sigs !? op of
              Nothing -> error $ "Undefined operator " ++ show op
              Just (Eff name _, sig) -> (name, sig)
      (constrs, _, ty) <- instantiate scheme
      return
        ( Op (Infer ty (Closed (eff_name |> unitTy))) op
        , error "todo!"
        , constrs
        )
  {-Handle _ lbl (Handler _ (HandleClause clauses (Clause ret_name ret_arg ret_resume ret_body))) body -> do
      EffCtx effs _ <- ask
      let ops = fromMaybe (error $ "Undefined effect " ++ show lbl) (effs !? lbl)
      let handled_eff = Closed (lbl |> unitTy)
      _ <-
        if clauses `covers` ops
          then return ()
          else error $ "Clauses " ++ show (NonEmpty.toList clauses) ++ " don't cover " ++ show (Map.keys ops)
      (body, body_core, body_constr) <- generateConstraints body
      out_eff <- fresh
      alpha <- fresh
      (ret, ret_core, ret_constr) <- local (Map.insert ret_arg (monoScheme $ VarTy alpha)) $ generateConstraints ret_body
      (clauses, op_constr) <-
        NonEmpty.unzip
          <$> forM
            clauses
            ( \(Clause name x resume clause_body) -> do
                -- Signatures of ops cannot call other effects and so must be total
                -- So we don't check any effects here
                (op_constr, _, op_ty) <-
                  case ops !? name of
                    Nothing -> error $ "Undefined operator " ++ show name
                    Just scheme -> instantiate scheme

                -- This is probably too cute
                (tin, tout, out) <- (,,) <$> fresh <*> fresh <*> fresh
                (clause, clause_core, clause_constr) <-
                  local (bind resume (FunTy (VarTy tout) (Open out_eff) (VarTy out)) . bind x (VarTy tout)) $ generateConstraints clause_body
                return
                  ( Clause name x resume clause
                  , Simp (VarTy out ~ (ret ^. meta . ty)) :
                    Simp (VarTy out ~ (clause ^. meta . ty)) : -- Clause type and return type must agree
                    Simp (op_ty ~ FunTy (VarTy tin) handled_eff (VarTy tout)) : -- Operation needs to be a function that handles our effect
                    Simp (VarTy out_eff ~ rowToType (clause ^. meta . eff)) : -- Each clause can pass on unhandled effects from handler body
                    op_constr <> clause_constr -- Ambient constraints from recursive calls
                  )
            )

      return
        ( Handle (Infer (ret ^. meta . ty) (Open out_eff)) lbl (Handler (error "shit") (HandleClause clauses (Clause ret_name ret_arg ret_resume ret))) body -- Return type of the handler is the type of the return clause
        , error "todo!"
        , Simp (rowToType (body ^. meta . eff) ~> Open out_eff :⊙ handled_eff) : -- The body eff must include l, our output will be whatever effs are leftover from l
          Simp (VarTy out_eff ~ rowToType (ret ^. meta . eff)) :
          Simp (VarTy alpha ~ (body ^. meta . ty)) : -- Input to the return clause should match the body type
          body_constr <> ret_constr <> mconcat (NonEmpty.toList op_constr)
        )-}
    Handle _ lbl handler body -> do
      -- TODO: Should eff be on handler instead of handle? I lean no because it complicates the "type" that has to be passed.
      --    I lean yes because which effect is handled is a property of the handler not `handle ... with ...`.
      --    If we want to remove the handler and replace it with a record literal then it's bad to have it contain the effect
      -- Check that handler has a type compatible with eff

      EffCtx effs _ <- ask
      let ops = fromMaybe (error $ "Undefined effect " ++ show lbl) (effs !? lbl)
      let handled_eff = Closed (lbl |> unitTy) -- TODO: This should map to the type of it's handler prolly?
      out_eff <- fresh

      (body, body_core, body_constr) <- generateConstraints body
      (handler, handler_core, handler_constr) <- generateConstraints handler

      -- TODO: figure out what to do with wrappers
      (constrs, _, tys) <- unzip3 <$> forM ops instantiate
      let (expected_body_ty, handle_out_ty) = 
            case tys !? "return" of
                Just (FunTy argTy _ retTy) -> (argTy, retTy)
                _ -> error "Expected a return label with a function type"

      let eff_op_constrs = mconcat $ Map.elems constrs
      return ( Handle (Infer handle_out_ty (Open out_eff)) lbl handler body
             , error "We're getting there"
             , Simp (rowToType (handler ^. meta . eff) ~ rowToType handled_eff)
             : Simp (expected_body_ty ~ (body ^. meta . ty))
             : Simp (rowToType (body ^. meta . eff) ~> Open out_eff :⊙ handled_eff)
             : body_constr <> handler_constr <> eff_op_constrs
             )
    Handler _ (HandleClause clauses (Clause ret_name ret_arg ret_unused ret_body)) -> do
      expected_eff <- fresh
      out_eff <- fresh
      alpha <- fresh
      (ret, ret_core, ret_constr) <- local (Map.insert ret_arg (monoScheme $ VarTy alpha)) $ generateConstraints ret_body
      let ret_ty = FunTy (VarTy alpha) (Closed Map.empty) (ret ^. meta . ty)
      (clauses, prod_ty_fields, clause_cores, clause_constrs) <-
        unzip4 <$> forM
            clauses
            ( \(Clause name x resume clause_body) -> do
                (tout, out) <- (,) <$> fresh <*> fresh
                (clause, clause_core, clause_constr) <-
                  local (bind resume (FunTy (VarTy tout) (Open out_eff) (VarTy out)) . bind x (VarTy tout)) $ generateConstraints clause_body
                return
                  ( Clause name x resume clause
                  , (name, FunTy (VarTy tout) (Open expected_eff) (clause ^. meta .ty))
                  , Core.Lam (Core.CoreV x (Core.fromType (VarTy tout))) $ Core.Lam (Core.CoreV resume (Core.fromType $ FunTy (VarTy tout) (Open out_eff) (VarTy out))) clause_core
                  , Simp (VarTy out ~ (ret ^. meta . ty)) :
                    Simp (VarTy out ~ (clause ^. meta . ty)) : -- Clause type and return type must agree
                    -- Replicate this in the Handle operation
                    --Simp (op_ty ~ FunTy (VarTy tin) handled_eff (VarTy tout)) : -- Operation needs to be a function that handles our effect
                    Simp (VarTy out_eff ~ rowToType (clause ^. meta . eff)) : -- Each clause can pass on unhandled effects from handler body
                    clause_constr -- Ambient constraints from recursive calls
                  )
            )

      let handler_row = Map.fromList ((ret_name, ret_ty) : NonEmpty.toList prod_ty_fields)
      return ( Handler (Infer (ProdTy $ RowTy handler_row) (Open expected_eff)) (HandleClause clauses (Clause ret_name ret_arg ret_unused ret))
             , Core.Product (ret_core : NonEmpty.toList clause_cores)
             , ret_constr <> mconcat (NonEmpty.toList clause_constrs)
             )

unzip3 :: (Functor f) => f (a, b, c) -> (f a, f b, f c)
unzip3 abcs = (fst3 <$> abcs, snd3 <$> abcs, thd3 <$> abcs)
  where
    fst3 (a, _, _) = a
    snd3 (_, b, _) = b
    thd3 (_, _, c) = c

unzip4 :: (Functor f) => f (a, b, c, d) -> (f a, f b, f c, f d)
unzip4 abcds = (fst4 <$> abcds, snd4 <$> abcds, thd4 <$> abcds, fth4 <$> abcds)
  where
    fst4 (a, _, _, _) = a
    snd4 (_, b, _, _) = b
    thd4 (_, _, c, _) = c
    fth4 (_, _, _, d) = d

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
    T (RowTy _) :<~> Open _ :⊙ Open _ -> return $ Residual q -- error ("Should this be allowed? " ++ show r)
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
  = -- tvar ~ ct
    Ct TVar Ct
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

-- InteractOccurs a Int (Within b (a -{}-> r)) ~~> [b ~ Int -{}-> r]
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

class Lookup t k v | t -> k, t -> v where
  lookup :: Ord k => k -> t -> Maybe v

  (!?) :: Ord k => t -> k -> Maybe v
  t !? k = lookup k t

instance Lookup (Map k v) k v where
  lookup = Map.lookup

newtype PredMap = PM {unPM :: Map TVar (Set Ct)}

predMapToList :: PredMap -> [(TVar, Ct)]
predMapToList = concatMap (\(tv, cts) -> (,) tv <$> Set.toList cts) . Map.toAscList . unPM

predMapFromList :: [(TVar, Ct)] -> PredMap
predMapFromList = PM . foldr adjust Map.empty
 where
  adjust (tvar, ct) =
    Map.alter
      ( \curr ->
          Just $ case curr of
            Nothing -> Set.singleton ct
            Just cts -> Set.insert ct cts
      )
      tvar

instance Semigroup PredMap where
  (PM left) <> (PM right) =
    PM $
      Map.merge
        Map.preserveMissing
        Map.preserveMissing
        (Map.zipWithMatched (\_ l r -> l <> r))
        left
        right

instance Monoid PredMap where
  mempty = PM Map.empty

instance Lookup PredMap TVar (Set Ct) where
  lookup k = Map.lookup k . unPM

toList :: PredMap -> [(TVar, Set Ct)]
toList = Map.toList . unPM

solveSimplConstraints :: (Has (Fresh TVar) sig m, Has (Throw TyErr) sig m) => [TVar] -> [Q] -> [Q] -> m (Subst, [Q])
solveSimplConstraints ctx_unifiers given wanted = do
  (_, flatten_subst, _, (wanted_canon, residue)) <- simples ctx_unifiers given wanted
  let q_wanted = flatten_q flatten_subst <$> wanted_canon
  let q_residue = over typeOf (apply flatten_subst) residue
  -- tie the knot
  let theta =
        let q_to_pairs subst q =
              case q of
                Ct tv (T ty) -> [(tv, apply subst ty)]
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
  impl_substs <-
    forM
        impls
        ( \(Imp exists prop impl) -> do
            (subst, residue) <- solveConstraints (unifiers <> exists) (given <> residue <> prop) impl
            if not (null residue)
              then error "Expected empty residue for implication constraint"
              else return subst
        )
  _ <- trace (ppShowList residue) $ return ()
  return (foldr (<>) subst impl_substs, residue)
 where
  (simpls, impls) = partitionEithers (view constraintEither <$> constrs)

{- Applied to  top level terms after constraint solving to generalize open types into closed schemes.
  Zonking is the process of replacing each type variable by the type it was sovled to if available.
zonkOrGeneralizeTrying :: PredMap -> Type -> Scheme
zonkOrGeneralizeTrying solved ty = Scheme bound qs ty
 where
  bound = openTyVars ty
  qs = Map.foldrWithKey fold [] (unPM solved)
  fold tv cts acc = Set.foldr (\ct acc -> (T (VarTy tv) :<~> ct) : acc) acc cts

  openTyVars = toListOf (cosmos . typeVars) -}

zonk :: Subst -> Core.Core -> Core.Core
zonk subst =
  transform (over (Core.coreVars . Core.coreVarTy) (apply subst))

wrap :: Scheme -> Core.Core -> Core.Core
wrap (Scheme bound qs _) core =
  foldr Core.TyLam core $
    fmap
      ( \tv ->
          if TVarSet.member tv (rowTVars qs)
            then coreRowTv tv
            else coreTyTv tv
      )
      bound

{- Walk term and treat any unbound variables as evidence and wrap final term in a new lambda to bind evidence -}
liftEv :: Core.Core -> Core.Core
liftEv core = foldr Core.Lam core unbound
 where
  unbound = Core.coreUnboundVars core

defaultEffCtx :: EffCtx
defaultEffCtx =
  mkEffCtx
    [ Eff "State" (Map.fromList [("get", monoScheme (FunTy unitTy (Closed $ "State" |> unitTy) IntTy)), ("put", monoScheme (FunTy IntTy (Closed $ "State" |> unitTy) unitTy))])
    , Eff "RowEff" (Map.fromList [("add_field", Scheme [TV 0, TV 1, TV 2] [VarTy 0 ~> Closed ("x" |> IntTy) :⊙ Open 1, VarTy 2 ~> Closed ("y" |> IntTy) :⊙ Open 1] (FunTy (VarTy 0) (Closed $ "RowEff" |> unitTy) (VarTy 2)))])
    ]

generateEvidenceForSolved :: Map Var Q -> (Map Var Core.Core, [Q])
generateEvidenceForSolved = Map.foldrWithKey generateEvidence (Map.empty, [])
 where
  generateEvidence v q =
    case q of
      T (RowTy goal) :<~> Closed left :⊙ Closed right -> _1 %~ Map.insert v (Core.rowEvidence left right goal)
      -- TODO: We need to handle projection constraints here which will have one open type variable.
      -- Except that type variable won't be referenced in the actual type because it doesn't really matter.
      -- So we need to check if that is the case and if so solve with some idea of any row in our row evidence

      -- Anything else we can't solve and needs to be lifted as an evidence param
      _ -> _2 %~ (q :)

inferTerm :: (Has (Reader Ctx) sig m, Has (Reader EffCtx) sig m) => Term () -> m (Term Infer, Core.Core, Scheme)
inferTerm term = do
  (_, (tvar, (raw_ev_map, (typed_term, core_term, constrs)))) <- runFresh (maxVar + 1) $ runFresh (TV 0) $ runState empty_ev_map $ generateConstraints term
  res <- runReader ([] :: [Q]) . runThrow . runFresh tvar $ solveConstraints [] [] constrs
  let generalizeAndLiftEv subst =
        let term = {-closeEffects pred_map $-} apply subst typed_term
            (ev_map, residual_ev) = generateEvidenceForSolved (apply subst raw_ev_map)
            term_ty = apply subst (term ^. meta . ty)
            scheme = Scheme (nub $ toListOf (cosmos . typeVars) term_ty) residual_ev term_ty
         in trace ("Typed Term:\n" ++ ppShow typed_term ++ "\nSubst:\n" ++ ppShow subst ++ "\n")
                  (term, passEvVector (term ^. meta . eff) $ zonk subst $ wrap scheme $ liftEv (Core.varSubst ev_map core_term), scheme)
  case res of
    Left (err :: TyErr) -> error ("Failed to infer a type: " ++ show err)
    Right (_, (subst, [])) -> return $ generalizeAndLiftEv subst
    Right (_, (subst, qs)) -> error ("Remaining Qs: " ++ show qs ++ "\nSubst: " ++ show subst ++ "\nType: " ++ ppShow typed_term)
 where
  empty_ev_map = Map.empty :: Map Var Q
  maxVar = maxTermVar term

passEvVector :: InternalRow -> Core.Core -> Core.Core
passEvVector row = 
  case row of
    Open eff -> Core.Lam (Core.CoreV evv (CoreVar $ coreRowTv eff))
    Closed eff -> Core.Lam (Core.CoreV evv (Core.fromType . ProdTy . RowTy $ eff))

infer :: Prog () -> (Prog Infer, [Core.Core], [Scheme])
infer (Prog defs effs) = snd . runIdentity . runReader defaultEffCtx . runState emptyCtx $ inferDefs effs defs

inferDefs :: (Has (State Ctx) sig m, Has (Reader EffCtx) sig m) => [Eff] -> [Def ()] -> m (Prog Infer, [Core.Core], [Scheme])
inferDefs es [] = return (Prog [] es, [], [])
inferDefs es (Def name term : tail) = do
    ctx :: Ctx <- get
    eff_ctx :: EffCtx <- ask
    let (infer_term, core, scheme) = runIdentity $ runReader eff_ctx $ runReader ctx $ inferTerm term
    modify (bind name (infer_term ^. meta . ty))
    (prog, cores, schemes) <- inferDefs es tail
    return (addDef (Def name infer_term) prog, core : cores, scheme : schemes)

prettyInferTerm :: Term () -> IO ()
prettyInferTerm term = putStrLn $ ppShow infer ++ "\n\n" ++ unpack (prettyRender core) ++ "\n\n" ++ ppShow scheme ++ "\n"
 where
  (infer, core, scheme) = runIdentity $ runReader defaultEffCtx $ runReader emptyCtx $ inferTerm term
  prettyRender = renderStrict . layoutSmart defaultLayoutOptions . pretty

prettyInfer :: Prog () -> IO ()
prettyInfer prog = putStrLn $ ppShowList defs ++ "\n\n" ++ unpack (prettyRender cores) ++ "\n\n" ++ ppShowList schemes ++ "\n"
  where
    (Prog defs _, cores, schemes) = infer prog
    prettyRender = renderStrict . layoutSmart defaultLayoutOptions . pretty

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

exampleUpFn :: Term ()
exampleUpFn = abs [v] (abs [r] $ record [label "x" $ var v, prj R (var r)])
 where
  r = V 0
  v = V 1

exampleSmolEff :: Term ()
exampleSmolEff = handle "State" (handler clauses ret) (op "get" <@> unit)
 where
  resume = V 1
  x = V 2
  -- All of these are in disjoint scopes so we're okay to reuse variables, although it's kinda sloppy
  clauses = Clause "get" x resume (var resume <@> int 4) :| [Clause "put" (V 3) (V 4) (var (V 4) <@> unit)]
  ret = Clause "" x resume (label "x" $ var x)

exampleEff :: Term ()
exampleEff = handle "State" (handler clauses ret) $ abs [v] (op "put" <@> var v) <@> (op "get" <@> unit)
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

exampleWand :: Term ()
exampleWand = abs [m, n] $ unlabel (prj L $ record [var m, var n]) "x"
 where
  m = V 0
  n = V 1

exampleApWand :: Term ()
exampleApWand = abs [m, n] (unlabel (prj L $ record [var m, var n]) "x") <@> record [label "x" (int 3), label "y" (int 4)] <@> record [label "w" (int 1), label "z" (int 2)]
 where
  m = V 0
  n = V 1

exampleProgWand :: Prog ()
exampleProgWand =
  Prog {
    terms = [wand_def, ap],
    effects = [] }
  where
    wand = V (-2)
    wand_def = Def wand exampleWand
    ap = Def (V (-3)) (var wand <@> record [label "w" (int 1), label "z" (int 2)])

-- This one should fail and it does!
exampleHandleErr :: Term ()
exampleHandleErr = handle "State" (handler whoops ret) $ abs [v] (op "put" <@> var v) <@> (op "get" <@> unit)
 where
  v = V 0
  resume = V 1
  x = V 2
  -- We forgot a clause
  whoops = Clause "get" x resume (int 4) :| []
  ret = Clause "" x resume (label "x" $ var x)
