{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module OutsideIn where

import Fresh (Fresh, fresh, runFresh)

import qualified Data.Map.Lazy as LazyMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Algebra (Has, run)
import Control.Effect.Reader (Reader, ask, asks, local)
import Control.Lens (
  Identity (runIdentity),
  Lens',
  lens,
  over,
  toListOf,
  view,
  (%~),
  (&),
  (^.),
 )
import Control.Lens.Tuple
import qualified Data.Map.Merge.Strict as Map
import Data.Maybe (fromJust, fromMaybe)

import Control.Carrier.Reader (runReader)
import Control.Carrier.State.Strict (runState)
import Control.Carrier.Throw.Either (runThrow)
import Control.Effect.State (State, get, modify)
import Control.Effect.Throw (Throw, throwError)
import Control.Monad (forM, unless)
import Data.Bifunctor (Bifunctor (first, second))
import Data.Either (partitionEithers)
import Data.Foldable (Foldable (..), foldlM, foldrM)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Debug.Trace
import Text.Show.Pretty
import Prelude hiding (abs, interact, lookup, unzip3)

import Algebra.Graph hiding (simplify)
import qualified Algebra.Graph as Graph
import Algebra.Graph.Export.Dot
import Constraint
import Control.Lens.Plated
import Core (CoreType (..), coreRowTv, coreTyTv)
import qualified Core
import Core.Builtin
import Data.Functor ((<&>))
import Data.Text (unpack)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import GHC.IO (unsafePerformIO)
import GHC.Stack
import Prettyprinter
import Prettyprinter.Render.Terminal (renderStrict)
import Program
import Subst
import TVarSet (TVarSet)
import qualified TVarSet
import Term
import Type

data Wrapper
  = TyApp Wrapper Core.CoreType
  | EvApp Wrapper Core.CoreVar
  | Hole

fillIn :: Wrapper -> Core.Core -> Core.Core
fillIn wrap core =
  case wrap of
    Hole -> core
    TyApp hole ty -> Core.TyApp (fillIn hole core) ty
    EvApp hole ev -> Core.App (fillIn hole core) (Core.var ev)

rowTVars :: [Q] -> TVarSet.TVarSet
rowTVars qs = TVarSet.fromList (qs >>= rowTvs)
 where
  rowTvs (VarTy tv :<~> c2) = tv : ct c2
  rowTvs (_ :<~> c2) = ct c2

  ct (T (VarTy tv)) = [tv]
  ct (T _) = []
  ct (l :⊙ r) = internalRow l ++ internalRow r

  internalRow (Open tv) = [tv]
  internalRow (Closed _) = []

schemeWrapper :: (Has (Fresh Var) sig m {-, Has (State (Map Var Q)) sig m-}) => [TVar] -> [Q] -> m Wrapper
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

  rowEvs ((VarTy out) :<~> Open left :⊙ Open right) = [Core.rowEvType (Open left) (Open right) (Open out)]
  -- rowEvs (Open left :⊙ Open right :<~> T (VarTy out)) = [Core.rowEvType (Open left) (Open right) (Open out)]
  rowEvs _ = []

instantiate :: (Has (Fresh TVar) sig m, Has (Fresh Var) sig m {-, Has (State (Map Var Q)) sig m-}) => Scheme -> m ([Constraint], Wrapper, Type)
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

traceQ q = trace (showPretty q) q

generateConstraints ::
  ( Has (Reader Ctx) sig m
  , Has (Reader EffCtx) sig m
  , Has (Reader (Map Label Type)) sig m
  , Has (State (Map Var Q)) sig m
  , Has (Fresh TVar) sig m
  , Has (Fresh Var) sig m
  , HasCallStack
  ) =>
  Term () ->
  m (Term Infer, Core.Core, [Constraint])
generateConstraints = go True
 where
  -- Don't pass evv by default if we're genenrating code inside the compiler, the compiler will handle evv explicitly
  internal_compile = go False
  go pass_evv term =
    case term of
      -- Literals
      Var _ x -> do
        ctx <- askCtx
        case ctx !? x of
          Nothing -> error ("Undefined variable " ++ show x)
          Just scheme -> do
            (constrs, wrapper, ty) <- instantiate scheme
            let eff = fromMaybe (Closed Map.empty) (firstEffectOf ty)
            return
              ( Var (Infer ty eff) x
              , fillIn wrapper (Core.var (Core.CoreV x (Core.fromType ty)))
              , constrs
              )
      Int _ i ->
        return
          ( Int (Infer Type.IntTy (Closed Map.empty)) i
          , Core.Lit (Core.I i)
          , []
          )
      Unit _ ->
        return
          ( Unit (Infer unitTy (Closed Map.empty))
          , Core.Product []
          , []
          )
      -- Labelled Types
      Label _ lbl term -> do
        (term, term_core, term_constr) <- go pass_evv term
        let m = term ^. meta & ty %~ RowTy . (|>) lbl
        return
          ( Label m lbl term
          , term_core
          , term_constr
          )
      Unlabel _ term lbl -> do
        alpha <- fresh
        (term, term_core, term_constr) <- go pass_evv term
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
        (left, left_core, left_constr) <- go pass_evv left
        (right, right_core, right_constr) <- go pass_evv right
        -- TODO: Should both functions be marked with out_eff here, or just one?
        let constr = VarTy out ~> Open left_var :⊙ Open right_var
        modify (Map.insert out_ev constr)
        return
          ( Concat (Infer (ProdTy $ VarTy out) (Open out_eff)) left right
          , Core.App (Core.App (Core.Project 0 (Core.var $ Core.CoreV out_ev (Core.rowEvType (Open left_var) (Open right_var) (Open out)))) left_core) right_core
          , Simp (left ^. meta . ty ~ ProdTy (VarTy left_var))
              : Simp (right ^. meta . ty ~ ProdTy (VarTy right_var))
              : Simp constr
              : Simp (VarTy out_eff ~> (left ^. meta . eff) :⊙ (right ^. meta . eff))
              : left_constr <> right_constr
          )
      Branch _ left right -> do
        out <- fresh
        out_ev <- fresh
        left_var <- fresh
        right_var <- fresh
        out_eff <- fresh
        a <- fresh
        (left, left_core, left_constr) <- go pass_evv left
        (right, right_core, right_constr) <- go pass_evv right
        -- TODO: Should both functions be marked with out_eff here, or just one?
        let constr = VarTy out ~> Open left_var :⊙ Open right_var
        modify (Map.insert out_ev constr)
        let branch = Core.Project 1 (Core.var $ Core.CoreV out_ev (Core.rowEvType (Open left_var) (Open right_var) (Open out)))
        Scheme _ _ evv_ty <- asks (\(ctx :: Ctx) -> fromJust $ ctx !? Core.evv)
        let evv = Core.CoreV Core.evv (Core.fromType evv_ty)
        return
          ( Branch (Infer (FunTy (SumTy $ VarTy out) (Open out_eff) (VarTy a)) (Open out_eff)) left right
          , Core.Lam evv $ Core.App (Core.App (Core.TyApp branch (CoreVar $ Core.coreTyTv a)) (Core.App left_core (Core.var evv))) (Core.App right_core (Core.var evv))
          , Simp (left ^. meta . ty ~ FunTy (SumTy (VarTy left_var)) (left ^. meta . eff) (VarTy a))
              : Simp (right ^. meta . ty ~ FunTy (SumTy (VarTy right_var)) (right ^. meta . eff) (VarTy a))
              : Simp constr
              : Simp (VarTy out_eff ~> (left ^. meta . eff) :⊙ (right ^. meta . eff))
              : left_constr <> right_constr
          )
      Prj _ dir term -> do
        rest <- fresh
        out <- fresh
        out_ev <- fresh
        input <- fresh
        (term, term_core, term_constr) <- go pass_evv term
        -- We want to unwrap a product into it's underlying row for it's constraint
        let (idx, constr, rowEvTy) =
              case dir of
                L -> (2, VarTy input ~> Open out :⊙ Open rest, Core.rowEvType (Open out) (Open rest) (Open input))
                R -> (3, VarTy input ~> Open rest :⊙ Open out, Core.rowEvType (Open rest) (Open out) (Open input))

        unless pass_evv $ trace ("internal prj constr: " ++ showPretty constr) $ return ()

        -- Save our constraint
        modify (Map.insert out_ev constr)
        return
          ( Prj (Infer (ProdTy $ VarTy out) (term ^. meta . eff)) dir term
          , Core.App (Core.Project 0 (Core.Project idx (Core.var $ Core.CoreV out_ev rowEvTy))) term_core
          , Simp (term ^. meta . ty ~ ProdTy (VarTy input)) : Simp constr : term_constr
          )
      Inj _ dir term -> do
        rest <- fresh
        out <- fresh
        out_ev <- fresh
        input <- fresh
        (term, term_core, term_constr) <- go pass_evv term
        let (idx, constr, rowEvTy) =
              case dir of
                L -> (2, VarTy out ~> Open input :⊙ Open rest, Core.rowEvType (Open input) (Open rest) (Open out))
                R -> (3, VarTy out ~> Open rest :⊙ Open input, Core.rowEvType (Open rest) (Open input) (Open out))
        modify (Map.insert out_ev constr)
        return
          ( Inj (Infer (SumTy $ VarTy out) (term ^. meta . eff)) dir term
          , Core.App (Core.Project 1 (Core.Project idx (Core.Var $ Core.CoreV out_ev rowEvTy))) term_core
          , Simp (term ^. meta . ty ~ SumTy (VarTy input)) : Simp constr : term_constr
          )
      App _ fn arg ->
        let args_eff (arg :| []) = return (arg ^. meta . eff, [])
            args_eff (arg :| args) = do
              goal <- fresh
              (right, qs) <- args_eff (NonEmpty.fromList args) -- This call can't fail because of pattern match in args_eff
              return (Open goal, (VarTy goal ~> (arg ^. meta . eff) :⊙ right) : qs)
         in do
              alpha <- fresh
              out_eff <- fresh
              Scheme _ _ evv_ty <- asks (\(ctx :: Ctx) -> fromJust $ ctx !? Core.evv)
              (fn, fn_core, fn_constr) <- go pass_evv fn
              (args, arg_cores, arg_constrs) <- unzip3 <$> traverse (go pass_evv) arg
              let expected_arg_ty = foldr (\arg_ty ret -> FunTy arg_ty (fn ^. meta . eff) ret) (VarTy alpha) (view (meta . ty) <$> args)
              (arg_eff, expected_eff_constrs) <- args_eff args
              let init =
                    if pass_evv
                      then -- Pass evv to function, this is the default
                        Core.App fn_core (Core.var $ Core.CoreV Core.evv (Core.fromType evv_ty))
                      else -- Don't pass evv to the function
                        fn_core
              return
                ( App (Infer (VarTy alpha) (Open out_eff)) fn args
                , foldl' Core.App init arg_cores
                , Simp (VarTy out_eff ~> (fn ^. meta . eff) :⊙ arg_eff)
                    : Simp (fn ^. meta . ty ~ expected_arg_ty)
                    : (Simp <$> expected_eff_constrs) <> fn_constr <> mconcat (NonEmpty.toList arg_constrs)
                )
      Abs _ xs body -> do
        arg_tys <- traverse (\v -> fresh <&> (v,)) xs
        out_eff <- fresh
        evv_ty <- fresh
        (body, body_core, constr) <- local (foldr (\(x, alpha) f -> Map.insert x (monoScheme $ VarTy alpha) . f) (Map.insert Core.evv (monoScheme $ ProdTy (VarTy out_eff))) arg_tys) (go pass_evv body)
        let fn_ty = foldr (\(_, alpha) ty -> FunTy (VarTy alpha) (body ^. meta . eff) ty) (body ^. meta . ty) arg_tys
        let evv_lam =
              if pass_evv
                then Core.Lam (Core.CoreV Core.evv (CoreVar $ coreTyTv evv_ty))
                else id
        return
          ( Abs (Infer fn_ty (Open out_eff)) xs body
          , -- Wrap any lambdas with an extra parameter for evidence vector
            evv_lam $ foldr (\(x, alpha) body -> Core.Lam (Core.CoreV x (CoreVar $ coreTyTv alpha)) body) body_core arg_tys
          , Simp (VarTy evv_ty ~ ProdTy (VarTy out_eff)) : Simp (VarTy out_eff ~ rowToType (body ^. meta . eff)) : constr
          )
      Perform _ op val_ -> do
        EffCtx _ sigs <- ask
        let (eff_name, _, core_indx, scheme) =
              case sigs !? op of
                Nothing -> error $ "Undefined operator " ++ show op
                Just (Eff name core_ty _, i, sig) -> (name, core_ty, i, sig)
        (constrs, _, op_ty) <- instantiate scheme
        let (arg_ty, _, ret_ty) = expectFunTy op_ty
        (val, val_core, val_constrs) <- go pass_evv val_

        -- Figure out how to get the type of evv from context here
        (ctx :: Ctx) <- ask
        let evv_ty = case ctx !? Core.evv of
              Just (Scheme _ _ ty) -> trace ("evv ty: " ++ ppShow ty) ty
              Nothing -> error "evv undefined in Perform"
        handler_ty_var <- fresh
        rest_of_evv <- fresh
        let handler_ty = ProdTy (RowTy (Map.fromList [("handler", VarTy handler_ty_var), ("marker", IntTy)]))
        let lookup_evidence = evv_ty ~> Open rest_of_evv :⊙ Closed (eff_name |> handler_ty)
        let core_ev_type = Core.rowEvType (Open rest_of_evv) (Closed (eff_name |> handler_ty)) (typeToRow evv_ty)
        (lookup_evidence_term :: Var) <- fresh
        modify (Map.insert lookup_evidence_term lookup_evidence)
        let lookup_handler_in_evv = Core.App (Core.Project 0 (Core.Project 3 (Core.Var (Core.CoreV lookup_evidence_term core_ev_type)))) (Core.Var $ Core.CoreV Core.evv (Core.fromType evv_ty))

        let local_var = Core.CoreV (V (-3)) (Core.fromType handler_ty)
        -- Figure out how to type this honestly, I think it involves answer types?
        -- throwaway_var <- fresh
        let k_var = Core.CoreV (V (-4)) (Core.CoreLit Core.IntTy {-(CoreFun (Core.fromType ret_ty) (Core.CoreVar $ Core.coreTyTv k_ret_ty))-})
        let handler =
              Core.Lam k_var $
                Core.App
                  ( Core.App
                      -- (Core.App
                      (Core.Project core_indx (Core.Project 0 (Core.var local_var)))
                      -- (Core.Var $ Core.CoreV Core.evv evv_core_ty))
                      val_core
                  )
                  (Core.var k_var)

        -- let k_return_constr = case answer_types !? eff_name of
        --     Nothing -> [] -- If we don't have an answer_type simply don't constrain k so it's a TyLam --error "TODO: If handler is not in scope we need to life our answer polymorphism so it can get linked up to whoever eventually calls it"
        -- let stuff = RowTy $ Map.fromList [("marker", IntTy), ("handler", core_ty)]
        out_eff <- fresh
        return
          ( Perform (Infer ret_ty (Open out_eff)) op val
          , -- We need to look up our marker from evidence vector
            Core.App (Core.Lam local_var (Core.Yield (Core.Project 1 (Core.var local_var)) handler)) lookup_handler_in_evv
          , -- Core.App (Core.App out_core yield_fn) val_core
            Simp ((val ^. meta . ty) ~ arg_ty)
              -- : Simp (VarTy open_eff ~> Closed (eff_name |> handler_ty) :⊙ (val ^. meta . eff))
              -- : Simp (evv_ty ~ VarTy open_eff)
              -- : Simp ((\q -> trace (showPretty q) q) (lookup_term ^. meta . ty ~ stuff))
              -- : Simp (handler_ty ~ VarTy a_var)
              : Simp (VarTy out_eff ~> (val ^. meta . eff) :⊙ Closed (eff_name |> handler_ty))
              : Simp (trace ("lookup_evidence: " ++ showPretty lookup_evidence) lookup_evidence)
              : val_constrs <> constrs {-<> jts_constrs-}
          )
      Handle _ lbl handler body -> do
        -- TODO: Should eff be on handler instead of handle? I lean no because it complicates the "type" that has to be passed.
        --    I lean yes because which effect is handled is a property of the handler not `handle ... with ...`.
        --    If we want to remove the handler and replace it with a record literal then it's bad to have it contain the effect
        -- Check that handler has a type compatible with eff

        EffCtx effs _ <- ask
        let Eff _ _ {-core_ty-} ops = fromMaybe (error $ "Undefined effect " ++ show lbl) (effs !? lbl)
        a_var <- fresh
        let handled_eff = Closed (lbl |> VarTy a_var)
        out_eff <- fresh
        in_eff <- fresh

        (handler, handler_core, handler_constr) <- local (bind Core.evv (ProdTy (VarTy out_eff))) (go pass_evv handler)
        _ <- trace ("handler ty: " ++ showPretty (handler ^. meta . ty)) $ return ()
        _ <- trace ("in_eff: " ++ showPretty in_eff ++ "\nout_eff: " ++ showPretty out_eff) $ return ()

        -- TODO: figure out what to do with wrapper
        -- TODO: Check handler types against op types
        (constrs, _, _) <- unzip3 <$> forM ops instantiate

        -- In general this code is error prone, we shouldn't be unpacking types like this
        -- Maybe we need to introduce more constrs to represent what we want to talk about?
        let (ret_eff, expected_body_ty, handle_out_ty) =
              case handler ^. meta . ty of
                ProdTy (RowTy row) ->
                  case row !? "return" of
                    Just (FunTy ret_argTy eff ret_retTy) -> (eff, ret_argTy, ret_retTy)
                    Just ty -> error ("Expected return to have a function type, found: " ++ show ty)
                    Nothing -> error "Expected a return lablel"
                VarTy _ -> error "Don't know what to do here"
                _ -> error "Invalid handler type"
        {- TODO: Make this section work as intended. -}
        {-let op_constrs =
              Map.elems $ Map.merge
                (Map.mapMissing (\k _ -> error ("Expected label " ++ unpack k ++ " was not present in handler")))
                (Map.mapMissing (\k _ -> error ("Unexpected operation " ++ unpack k ++ " found in handler")))
                (Map.zipWithMatched (\_ expected_ty actual_ty -> expected_ty ~ actual_ty))
                tys
                handler_ty-}
        local_handler_var <- fresh
        let eff_op_constrs = mconcat $ Map.elems constrs
        let marker_var = Core.CoreV Core.marker (Core.CoreLit Core.IntTy)
        -- (handler, handler_core, handler_constrs) <-  $ internal_compile $ label lbl (record [label "marker" $ var Core.marker, label "handler" (var local_handler_var)])

        (_, a_core_or_smthg, handler_constrs) <-
          local (bind local_handler_var (handler ^. meta . ty) . bind Core.marker IntTy) $
            internal_compile $
              label lbl (record [label "marker" $ var Core.marker, label "handler" (var local_handler_var)])

        let handle_row = ProdTy $ RowTy (Map.fromList [("marker", IntTy), ("handler", handler ^. meta . ty)])
        let evidence_constr = VarTy in_eff ~> Open out_eff :⊙ Closed (lbl |> handle_row)
        let evidence_ty = Core.rowEvType (Open in_eff) (Closed (lbl |> handle_row)) (Open out_eff)
        evidence_term :: Var <- fresh
        modify (Map.insert evidence_term evidence_constr)

        --let prj_core = Core.App (Core.Project 0 (Core.Project 2 (Core.var $ Core.CoreV evidence_term evidence_ty))) (Core.Var $ Core.CoreV Core.evv (Core.CoreVar $ coreTyTv out_eff))
        let concat_core = Core.App (Core.App (Core.Project 0 (Core.var $ Core.CoreV evidence_term evidence_ty)) (Core.Var $ Core.CoreV Core.evv (Core.CoreVar $ coreTyTv out_eff))) a_core_or_smthg

        -- TODO: revisit this after figuring out what's up with the shit
        (lookup_ret_ty, ret_core, ret_constrs) <-
          local (bind local_handler_var (handler ^. meta . ty) . bind Core.evv (ProdTy (VarTy out_eff))) $
            internal_compile
              (unlabel (prj L (var local_handler_var)) "return")

        -- Locally bind updated evv vector in body_core
        (body, body_core, body_constr) <- local (bind Core.evv (VarTy in_eff)) $ go pass_evv body
        let handler_body =
              Core.local
                (Core.CoreV local_handler_var (Core.fromType $ handler ^. meta . ty))
                handler_core
                ( Core.App
                    (Core.Lam (Core.CoreV Core.evv (Core.CoreVar $ coreTyTv in_eff)) (Core.App ret_core body_core))
                    concat_core
                )

        (ctx :: Ctx) <- ask
        let qs = case ctx !? Core.evv of
              Just (Scheme _ _ evv_ty) -> trace ("constrain evv_ty: " ++ showPretty (VarTy out_eff ~ evv_ty)) [Simp (VarTy out_eff ~ evv_ty)]
              Nothing -> []
        let cs =
              -- Simp (rowToType (handler ^. meta . eff) ~ rowToType handled_eff)
              Simp (expected_body_ty ~ (body ^. meta . ty))
                : Simp (rowToType (body ^. meta . eff) ~> Open out_eff :⊙ handled_eff)
                : Simp (lookup_ret_ty ^. meta . ty ~ FunTy expected_body_ty ret_eff handle_out_ty)
                -- : Simp prj_constr
                : Simp evidence_constr
                : body_constr <> handler_constr <> eff_op_constrs <> ret_constrs <> qs <> handler_constrs
        return
          ( Handle (Infer handle_out_ty (Open out_eff)) lbl handler body
          , {-Core.Lam (Core.CoreV Core.evv (Core.CoreVar $ Core.coreTyTv out_eff)) $-} Core.NewPrompt marker_var (Core.Prompt (Core.var marker_var) handler_body)
          , cs
          )
      Handler _ (HandleClause clauses (Clause ret_name ret_arg ret_unused ret_body)) -> do
        expected_eff <- fresh
        out_eff <- fresh
        alpha <- fresh
        evv_core_ty <- fresh
        Scheme _ _ evv_ty <- asks (\(ctx :: Ctx) -> fromJust $ ctx !? Core.evv)
        (ret, ret_body_core, ret_constr) <- local (Map.insert ret_arg (monoScheme $ VarTy alpha)) $ {-internal_compile-} go pass_evv ret_body
        let ret_ty = FunTy (VarTy alpha) (Closed Map.empty) (ret ^. meta . ty)
        let ret_core =
              Core.lam
                [ Core.CoreV ret_arg (Core.CoreVar (Core.coreTyTv alpha))
                ]
                ret_body_core
        (clauses, prod_ty_fields, clause_cores, clause_constrs) <-
          unzip4
            <$> forM
              clauses
              ( \(Clause name x resume clause_body) -> do
                  out <- fresh
                  EffCtx _ eff_by_sigs <- ask
                  (op_constrs, _, op_sig) <-
                    case eff_by_sigs !? name of
                      Just (_, _, scheme) -> instantiate scheme
                      Nothing -> error ("Unexpected operation in handler: " ++ unpack name)
                  -- I think we need to incorporate the effect here but I'm not quite sure how
                  let (op_arg, _, op_ret) = expectFunTy op_sig
                  let resume_ty = FunTy op_ret (Open out_eff) (VarTy out)
                  -- we handle passing evv explicitly below so don't do it in clause
                  (clause, clause_core, clause_constr) <-
                    local (bind Core.evv (VarTy evv_core_ty) . bind resume resume_ty . bind x op_arg) $ {-internal_compile-} go pass_evv clause_body
                  return
                    ( Clause name x resume clause
                    , (name, FunTy op_arg (clause ^. meta . eff) $ FunTy (FunTy op_ret (clause ^. meta . eff) (VarTy out)) (clause ^. meta . eff) (VarTy out))
                    , Core.lam
                        -- [ Core.CoreV Core.evv (CoreVar (coreTyTv evv_core_ty))
                        [ Core.CoreV x (Core.fromType op_arg)
                        , Core.CoreV resume (Core.fromType resume_ty)
                        -- TODO: Figure how to handle evv semantics for core effectively
                        -- , Core.CoreV Core.evv (CoreVar (coreTyTv evv_core_ty))
                        ]
                        clause_core
                    , Simp (VarTy out ~ (ret ^. meta . ty))
                        : Simp (VarTy out ~ (clause ^. meta . ty))
                        : Simp (VarTy out_eff ~ rowToType (clause ^. meta . eff)) -- Clause type and return type must agree
                        -- Replicate this in the Handle operation
                        -- Simp (op_ty ~ FunTy (VarTy tin) handled_eff (VarTy tout)) : -- Operation needs to be a function that handles our effect
                        : clause_constr <> op_constrs -- Each clause can pass on unhandled effects from handler body
                        -- Ambient constraints from recursive calls
                    )
              )

        let handler_row = Map.fromList ((ret_name, ret_ty) : NonEmpty.toList prod_ty_fields)
        return
          ( Handler (Infer (ProdTy $ RowTy handler_row) (Open expected_eff)) (HandleClause clauses (Clause ret_name ret_arg ret_unused ret))
          , Core.Product (ret_core : NonEmpty.toList clause_cores)
          , Simp (VarTy evv_core_ty ~ evv_ty) : ret_constr <> mconcat (NonEmpty.toList clause_constrs)
          )

typeToRow :: Type -> InternalRow
typeToRow ty =
  case ty of
    VarTy tv -> Open tv
    RowTy row -> Closed row
    ProdTy ty -> typeToRow ty
    SumTy ty -> typeToRow ty
    _ -> error "typeToRow called on non row"

expectFunTy :: Type -> (Type, InternalRow, Type)
expectFunTy (FunTy arg eff ret) = (arg, eff, ret)
expectFunTy ty = error ("Expected a FunTy, found: " ++ ppShow ty)

firstEffectOf :: Type -> Maybe InternalRow
firstEffectOf (FunTy _ eff _) = Just eff
firstEffectOf _ = Nothing

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
canon :: (Has (Throw TyErr) sig m, Has (State (Graph Q)) sig m) => Q -> m CanonResult
canon q =
  let track res = do
        let q' = vertex q
        case res of
          Work qs -> modify (overlay $ connect q' (overlays (vertex <$> qs)))
          -- Residual r -> modify (overlay $ connect q' (vertex r))
          _ -> return ()
        return res
   in case q of
        -- Check reflexive case first, if types are syntatically equal we can remove the constraint without further work
        a :<~> T b | a == b -> return $ Work []
        a :<~> T b ->
          case (a, b) of
            -- Decompose function type into arg and ret constraints
            (FunTy a_arg a_eff a_ret, FunTy b_arg b_eff b_ret) -> track $ Work [a_arg ~ b_arg, rowToType a_eff ~ rowToType b_eff, a_ret ~ b_ret]
            -- Order constraint
            (FunTy arg eff ret, VarTy tvar) -> track $ Work [VarTy tvar ~ FunTy arg eff ret]
            (a@(FunTy{}), b) -> track . Residual $ a ~ b
            -- Decompose product
            (ProdTy a_ty, ProdTy b_ty) -> track $ Work [a_ty ~ b_ty]
            -- A product equals a row type if it's underyling type equals the row
            (ProdTy ty, RowTy row) -> track $ Work [ty ~ RowTy row]
            -- Order constraint
            (ProdTy ty, VarTy tvar) -> track $ Work [VarTy tvar ~ ProdTy ty]
            (a@(ProdTy _), b) -> track . Residual $ a ~ b
            (SumTy a_ty, SumTy b_ty) -> track $ Work [a_ty ~ b_ty]
            (SumTy ty, RowTy row) -> track $ Work [ty ~ RowTy row]
            (SumTy ty, VarTy tvar) -> track $ Work [VarTy tvar ~ SumTy ty]
            (a@(SumTy _), b) -> track . Residual $ a ~ b
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
                    Left res -> track res
                    Right work -> track . Work . Map.elems $ work
            -- A product equals a row type if it's underyling type equals the row
            (RowTy row, ProdTy ty) -> track $ Work [ty ~ RowTy row]
            (RowTy row, SumTy ty) -> track $ Work [ty ~ RowTy row]
            -- Order constraint
            (RowTy row, VarTy tvar) -> track $ Work [VarTy tvar ~ RowTy row]
            (a@(RowTy _), b) -> track . Residual $ a ~ b
            -- Order constraint
            (VarTy a_tv, VarTy b_tv) | b_tv < a_tv -> track $ Done (Ct b_tv (T $ VarTy a_tv))
            (VarTy tvar, ty) ->
              -- If we fail an occurs check throw to break out of cannon
              if occurs tvar ty
                then throwError (OccursCheckFailed tvar (T ty))
                else track . Done $ Ct tvar (T ty)
            -- Order constraint
            (IntTy, VarTy tvar) -> track $ Work [VarTy tvar ~ IntTy]
            (IntTy, IntTy) -> return $ Work []
            (IntTy, ty) -> track . Residual $ IntTy ~ ty
        -- (a, b) -> return $ Residual (a ~ b)
        -- Trivial case where all rows are known. Reduces to an equality
        ty :<~> Closed a_row :⊙ Closed b_row -> track $ Work [ty ~ RowTy (a_row <> b_row)]
        -- An empty row combined with an open row is just that open row
        ty :<~> Closed a_row :⊙ Open b_tv
          | Map.null a_row -> track $ Work [ty ~ VarTy b_tv]
        ty :<~> Open a_tv :⊙ Closed b_row
          | Map.null b_row -> track $ Work [ty ~ VarTy a_tv]
        RowTy goal :<~> Closed a_row :⊙ Open b_tv
          | rowOccurs b_tv (Closed goal) -> throwError (OccursCheckFailed b_tv (T $ RowTy goal))
          | otherwise ->
              let (a_goal, b_goal) = Map.partitionWithKey (\k _ -> Map.member k a_row) goal
               in track $ Work [RowTy a_goal ~ RowTy a_row, VarTy b_tv ~ RowTy b_goal]
        RowTy goal :<~> Open a_tv :⊙ Closed b_row
          | rowOccurs a_tv (Closed goal) -> throwError (OccursCheckFailed a_tv (T $ RowTy goal))
          | otherwise ->
              let (b_goal, a_goal) = Map.partitionWithKey (\k _ -> Map.member k b_row) goal
               in track $ Work [VarTy a_tv ~ RowTy a_goal, RowTy b_row ~ RowTy b_goal]
        RowTy goal :<~> Open a_tv :⊙ Open b_tv
          -- Special case, if our goal is empty it's clear how to solve two open variables
          | Map.null goal -> track $ Work [RowTy Map.empty ~ VarTy a_tv, RowTy Map.empty ~ VarTy b_tv]
          | otherwise -> track $ Done (RowCt goal a_tv b_tv)
        IntTy :<~> ct@(_ :⊙ _) -> track $ Residual (IntTy ~> ct)
        ProdTy ty :<~> ct@(_ :⊙ _) -> track $ Work [ty ~> ct] -- track $ Residual (ProdTy ty ~> ct)
        SumTy ty :<~> ct@(_ :⊙ _) -> track $ Residual (SumTy ty ~> ct)
        FunTy arg eff ret :<~> ct@(_ :⊙ _) -> track $ Residual (FunTy arg eff ret ~> ct)
        VarTy tvar :<~> ct ->
          -- If we fail an occurs check throw to break out of cannon
          if ctOccurs tvar ct
            then throwError (OccursCheckFailed tvar ct)
            else track . Done $ Ct tvar ct

-- Expectation is these are un-unifiable types
-- Bubble them up as residual inert constraints to be handled
-- _  -> return $ Residual (a :<~> b)

{- A canonical constraint.
   This is a tvar lhs equal to a type that does not contain tvar -}
data CanonCt
  = Ct TVar Ct
  | RowCt Row TVar TVar
  deriving (Eq)

instance Show CanonCt where
  showsPrec p (Ct tv (T ty)) = showsPrec p tv . (" ~= " ++) . showsPrec p ty
  showsPrec p (Ct tv ct) = showsPrec p tv . (" ~= " ++) . showsPrec p ct
  showsPrec p (RowCt row left right) = showsRow p row . (" ~= " ++) . showsPrec p left . (" :⊙ " ++) . showsPrec p right

instance Pretty CanonCt where
  pretty (Ct tv ct) = pretty tv <+> "~=" <+> pretty ct
  pretty (RowCt row left right) = prettyRow row <+> "~=" <+> pretty left <+> ":⊙" <+> pretty right
   where
    prettyRow row = group . align . braces . vcat $ punctuate comma ((\(lbl, ty) -> pretty lbl <+> "|>" <+> pretty ty) <$> Map.toList row)

prettyRender = renderStrict . layoutSmart layoutOpts
 where
  layoutOpts = LayoutOptions{layoutPageWidth = AvailablePerLine 60 1.0}

showPretty :: (Pretty a) => a -> String
showPretty = unpack . prettyRender . pretty

{- binary interaction between two constraints from the same set (wanted or given) -}
interact :: (Has (State (Graph Q)) sig m) => Interaction -> m [Q]
interact i =
  -- The paper covers other rules but they are for type classes and type families
  case i of
    -- EQSAME
    InteractEq a_tv a_ty b_tv b_ct -> do
      let a = vertex (VarTy a_tv ~ a_ty)
      let b = vertex (VarTy b_tv ~> b_ct)
      let out = vertex (a_ty ~> b_ct)
      let graph = overlay (connect a out) (connect b out)
      modify (overlay graph)
      return [VarTy a_tv ~ a_ty, a_ty ~> b_ct]

    -- link both input nodes to the result of the interaction

    -- EQDIFF
    InteractOccurs a_tv a_ty b_occurence -> do
      let a = vertex (VarTy a_tv ~ a_ty)
      let b = vertex (toQ b_occurence)
      let q = applyOccurs (a_tv |-> a_ty) b_occurence
      let graph = overlay (connect a (vertex q)) (connect b (vertex q))
      modify (overlay graph)
      return [VarTy a_tv ~ a_ty, q]
    InteractRowLeftEq a_tv a_left a_right b_tv b_left b_right -> do
      let a = vertex (VarTy a_tv ~> Closed a_left :⊙ a_right)
      let b = vertex (VarTy b_tv ~> Closed b_left :⊙ b_right)
      let qs = [RowTy a_left ~ RowTy b_left, rowToType a_right ~ rowToType b_right]
      let graph = overlays (map (connect (overlay a b) . vertex) qs)
      modify (overlay graph)
      return $ (VarTy a_tv ~> Closed a_left :⊙ a_right) : qs
    InteractRowRightEq a_tv a_left a_right b_tv b_left b_right -> do
      let a = vertex (VarTy a_tv ~> a_left :⊙ Closed a_right)
      let b = vertex (VarTy b_tv ~> b_left :⊙ Closed b_right)
      let qs = [rowToType a_left ~ rowToType b_left, RowTy a_right ~ RowTy b_right]
      let graph = overlays (map (connect (overlay a b) . vertex) qs)
      modify (overlay graph)
      return $ (VarTy a_tv ~> a_left :⊙ Closed a_right) : qs

toQ :: Occurs -> Q
toQ = \case
  Within tv ty -> VarTy tv ~ ty
  RowLeft goal left right -> rowToType goal ~> left :⊙ right
  RowRight goal left right -> rowToType goal ~> left :⊙ right

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
    _ -> error "TODO: simplify"

-- InteractOccurs a Int (Within b (a -{}-> r)) ~~> [b ~ Int -{}-> r]
data Occurs
  = Within TVar Type
  | RowLeft InternalRow InternalRow InternalRow
  | RowRight InternalRow InternalRow InternalRow
  deriving (Eq, Show)

instance Pretty Occurs where
  pretty (Within tv ty) = pretty tv <+> "~" <+> pretty ty
  pretty (RowLeft row left right) = "row left" <+> pretty row <+> "~>" <+> pretty left <+> ":⊙" <+> pretty right
  pretty (RowRight row left right) = "row right" <+> pretty row <+> "~>" <+> pretty left <+> ":⊙" <+> pretty right

applyOccurs :: Subst -> Occurs -> Q
applyOccurs subst =
  \case
    Within tv ty -> VarTy tv ~ apply subst ty
    RowLeft row left right -> rowToType row ~> apply subst left :⊙ right
    RowRight row left right -> rowToType row ~> left :⊙ apply subst right

data Interaction
  = -- The canonical constraints are for the same tvar
    InteractEq TVar Type TVar Ct
  | -- The tvar of the first canonical constraint occurs in the type of the second
    InteractOccurs TVar Type Occurs
  | InteractRowLeftEq TVar Row InternalRow TVar Row InternalRow
  | InteractRowRightEq TVar InternalRow Row TVar InternalRow Row
  deriving (Eq, Show)

instance Pretty Interaction where
  pretty (InteractEq tv ty _ ct) = pretty tv <+> "equates" <+> Prettyprinter.parens (pretty ty) <+> "=" <+> Prettyprinter.parens (pretty ct)
  pretty (InteractOccurs tv ty occurs) = pretty tv <+> "~" <+> pretty ty <+> "occurs in" <+> pretty occurs
  pretty (InteractRowLeftEq tv a_left a_right _ b_left b_right) = pretty tv <+> "equates" <+> Prettyprinter.parens (pretty $ Closed a_left :⊙ a_right) <+> "=" <+> Prettyprinter.parens (pretty $ Closed b_left :⊙ b_right)
  pretty (InteractRowRightEq tv a_left a_right _ b_left b_right) = pretty tv <+> "equates" <+> Prettyprinter.parens (pretty $ a_left :⊙ Closed a_right) <+> "=" <+> Prettyprinter.parens (pretty $ b_left :⊙ Closed b_right)

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
            | rowOccurs a_tv b_left -> filteredCanons $ InteractOccurs a_tv a_ty (RowLeft (Open b_goal) b_left b_right)
            | rowOccurs a_tv b_right -> filteredCanons $ InteractOccurs a_tv a_ty (RowRight (Open b_goal) b_left b_right)
          -- Symmetrical cases
          (Ct a_tv (a_left :⊙ a_right), Ct b_tv (T b_ty))
            | a_tv == b_tv -> filteredCanons $ InteractEq b_tv b_ty a_tv (a_left :⊙ a_right)
          (Ct a_goal (a_left :⊙ a_right), Ct b_tv (T b_ty))
            | rowOccurs b_tv a_left -> filteredCanons $ InteractOccurs b_tv b_ty (RowLeft (Open a_goal) a_left a_right)
            | rowOccurs b_tv a_right -> filteredCanons $ InteractOccurs b_tv b_ty (RowRight (Open a_goal) a_left a_right)
          (RowCt a_row a_left a_right, Ct b_tv (T b_ty))
            | b_tv == a_left -> filteredCanons $ InteractOccurs b_tv b_ty (RowLeft (Closed a_row) (Open a_left) (Open a_right))
            | b_tv == a_right -> filteredCanons $ InteractOccurs b_tv b_ty (RowRight (Closed a_row) (Open a_left) (Open a_right))
          (Ct a_tv (T a_ty), RowCt b_row b_left b_right)
            | a_tv == b_left -> filteredCanons $ InteractOccurs a_tv a_ty (RowLeft (Closed b_row) (Open b_left) (Open b_right))
            | a_tv == b_right -> filteredCanons $ InteractOccurs a_tv a_ty (RowRight (Closed b_row) (Open b_left) (Open b_right))
          (Ct a_tv (Closed a_left :⊙ a_right), Ct b_tv (Closed b_left :⊙ b_right))
            | a_tv == b_tv && Map.keys a_left == Map.keys b_left -> filteredCanons $ InteractRowLeftEq a_tv a_left a_right b_tv b_left b_right
          (Ct a_tv (a_left :⊙ Closed a_right), Ct b_tv (b_left :⊙ Closed b_right))
            | a_tv == b_tv && Map.keys a_right == Map.keys b_right -> filteredCanons $ InteractRowRightEq a_tv a_left a_right b_tv b_left b_right
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
    | rowOccurs given_tv wanted_left = Left $ InteractOccurs given_tv given_ty (RowLeft (Open wanted_tv) wanted_left wanted_right)
    | rowOccurs given_tv wanted_right = Left $ InteractOccurs given_tv given_ty (RowRight (Open wanted_tv) wanted_left wanted_right)
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
    -- _ <- trace ("canon: " ++ showPretty new_canon) $ return ()
    -- _ <- (if new_residuals /= [] then trace ("residue: " ++ showPretty new_residuals) else id) return ()
    -- _ <- (if remaining_work [] /= [] then trace ("remaining: " ++ showPretty (remaining_work [])) else id) return ()
    go (new_canon <> canon) (new_residuals <> residuals) (remaining_work [])

  partitionResults = foldrM (\constr acc -> binResult acc <$> op constr) ([], [], id)

  binResult (canons, residuals, work) (Done canon) = (canon : canons, residuals, work)
  binResult (canons, residuals, work) (Residual residue) = (canons, residue : residuals, work)
  binResult (canons, residues, work) (Work w) = (canons, residues, (w ++) . work)

foldMapM f =
  foldlM
    ( \acc a -> do
        w <- f a
        return $! mappend acc w
    )
    mempty

{- Single step of rewriting our constraints towards a solution -}
solve :: (Has (Fresh TVar) sig m, Has (State (Graph Q)) sig m, Has (State [Q]) sig m, Has (Throw TyErr) sig m) => [TVar] -> Subst -> [CanonCt] -> [CanonCt] -> m (Maybe ([TVar], Subst, [CanonCt], [CanonCt]))
solve unifiers phi given wanted
  -- Handles interactions between given constraints
  | (i : interacts, inert_canon) <- interactions given = do
      is <- foldMapM interact (i : interacts)
      ((fresh_unifiers, subst), (canon, _)) <- runState ([], mempty) (canonicalize canon is)
      return $ Just (unifiers <> fresh_unifiers, subst <> phi, canon <> inert_canon, wanted)
  | (i : interacts, inert_canon) <- interactions wanted = do
      -- _ <- trace ("interactions:\n" ++ showPretty (i : interacts)) $ return ()
      is <- nub <$> foldMapM interact (i : interacts)
      ((fresh_unifiers, subst), (canon, remainder)) <- runState ([], mempty) (canonicalize canon is)
      modify (remainder ++)
      -- _ <- trace ("output:\n" ++ showPretty (sortBy canonOrder (canon <> inert_canon))) $ return ()
      return $ Just (unifiers <> fresh_unifiers, subst <> phi, given, canon <> inert_canon)
  | (i : interacts, inert_wanted) <- simplifications given wanted = do
      let is = simplify <$> i : interacts
      ((fresh_unifiers, subst), (canon, remainder)) <- runState ([], mempty) (canonicalize canon is)
      modify (remainder ++)
      return $ Just (unifiers <> fresh_unifiers, subst <> phi, given, canon <> inert_wanted)
solve _ _ _ _ = return Nothing

canonOrder :: CanonCt -> CanonCt -> Ordering
canonOrder (RowCt{}) (Ct{}) = GT
canonOrder (Ct{}) (RowCt{}) = LT
canonOrder (Ct a _) (Ct b _) = compare a b
canonOrder (RowCt a _ _) (RowCt b _ _) = compare a b

simples :: (Has (Fresh TVar) sig m, Has (State (Graph Q)) sig m, Has (Throw TyErr) sig m) => [TVar] -> [Q] -> [Q] -> m ([TVar], Subst, [CanonCt], ([CanonCt], [Q]))
simples unifiers noncanon_given noncanon_wanted = do
  -- _ <- trace ("wanted:\n" ++ showPretty (sort noncanon_wanted)) $ return ()
  ((canon_unifiers, subst), ((given, _), (wanted, wanted_residue))) <- runState ([], mempty) $ do
    given <- canonicalize canon noncanon_given
    wanted <- canonicalize canon noncanon_wanted
    return (given, wanted)
  -- _ <- trace ("canonical:\n" ++ showPretty (sortBy canonOrder wanted) ++ "\nresidue:\n" ++ showPretty wanted_residue) $ return ()
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

solveSimplConstraints :: (Has (Fresh TVar) sig m, Has (State (Graph Q)) sig m, Has (Throw TyErr) sig m) => [TVar] -> [Q] -> [Q] -> m (Subst, [Q])
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
                RowCt{} -> []
         in Subst . LazyMap.fromList . mconcat $ q_to_pairs theta <$> q_wanted
  return (theta, over typeOf (apply theta) q_residue)
 where
  flatten_q subst q =
    case q of
      Ct tv (T ty) -> Ct tv (T (apply subst ty))
      Ct tv (left :⊙ right) -> Ct tv (apply subst left :⊙ apply subst right)
      RowCt row left right -> RowCt row left right -- canon (apply subst)
      {-case () of
        (apply subst row) (apply subst (VarTy left)) (apply subst (VarTy right))-}
      -- RowCt{} -> error "this shouldn't happen"
      -- RowCt

solveConstraints :: (Has (Reader [Q]) sig m, Has (State (Graph Q)) sig m, Has (Throw TyErr) sig m, Has (Fresh TVar) sig m) => [TVar] -> [Q] -> [Constraint] -> m (Subst, [Q])
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
  -- _ <- trace ("residue: " ++ ppShowList residue) $ return ()
  return (foldr (<>) subst impl_substs, residue)
 where
  (simpls, impls) = partitionEithers (view constraintEither <$> constrs)

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
liftEv core = foldr Core.Lam core (Core.coreUnboundVars core)

stateEff = Eff "State" effCoreTy (monoScheme <$> ops)
 where
  -- TODO: this is awkward, but we can't reference
  effCoreTy = ProdTy (RowTy (Map.fromList [("marker", IntTy), ("handler", handlerTy)]))
  handlerTy = ProdTy (RowTy (eraseCoreTy <$> ops))
  -- To break cycle erase core ty for handlers in handler ty
  eraseCoreTy (FunTy arg (Closed eff) ret) = FunTy arg (Closed (unitTy <$ eff)) ret
  eraseCoreTy _ = error "eraseCoreTy: impossible"

  ops =
    Map.fromList
      [ ("get", FunTy unitTy (Closed ("State" |> effCoreTy)) IntTy)
      , ("put", FunTy IntTy (Closed ("State" |> effCoreTy)) unitTy)
      ]

defaultEffCtx :: EffCtx
defaultEffCtx =
  mkEffCtx
    [ stateEff
    , Eff "RowEff" (ProdTy (RowTy Map.empty)) (Map.fromList [("add_field", Scheme [TV 0, TV 1, TV 2] [VarTy 0 ~> Closed ("x" |> IntTy) :⊙ Open 1, VarTy 2 ~> Closed ("y" |> IntTy) :⊙ Open 1] (FunTy (VarTy 0) (Closed $ "RowEff" |> unitTy) (VarTy 2)))])
    ]

generateEvidenceForSolved :: Map Var Q -> (Map Var Core.Core, [Q])
generateEvidenceForSolved = Map.foldrWithKey generateEvidence (Map.empty, [])
 where
  generateEvidence v q =
    case q of
      -- Solve 0 open rows
      RowTy goal :<~> Closed left :⊙ Closed right -> trace ("Solved evidence for " ++ showPretty v ++ ": " ++ showPretty q) $ _1 %~ Map.insert v (Core.rowEvidence left right goal)
      -- Sovle 1 open  rows
      RowTy goal :<~> Closed left :⊙ Open _ -> trace ("Solved partial evidence for " ++ showPretty v ++ ": " ++ showPretty q) $ _1 %~ Map.insert v (Core.rowEvidence left (solvePartialRow goal left) goal)
      RowTy goal :<~> Open _ :⊙ Closed right -> trace ("Solved partial evidence for " ++ showPretty v ++ ": " ++ showPretty q) $ _1 %~ Map.insert v (Core.rowEvidence (solvePartialRow goal right) right goal)
      -- Anything else we can't solve and needs to be lifted as an evidence param
      _ -> _2 %~ (q :)

  solvePartialRow goal sub = Map.withoutKeys goal (Set.fromList $ Map.keys sub)

inferTerm :: (Has (Reader Ctx) sig m, Has (Reader EffCtx) sig m) => Term () -> m (Term Infer, Core.Core, Scheme)
inferTerm term = do
  (_, (tvar, (raw_ev_map, (typed_term, core_term, raw_constrs)))) <- runFresh (maxVar + 1) $ runFresh (TV 0) $ runState empty_ev_map $ runReader (Map.empty :: Map Label Type) $ generateConstraints term
  let constrs =
        {-case typed_term ^. meta . ty of
        -- If we didn't solve our term, assuem it's Int
        -- TODO: Do something smarter than this
        -- VarTy tv -> Simp (VarTy tv ~ IntTy) : raw_constrs
        _ ->-} {-Simp (rowToType (typed_term ^. meta . eff) ~ unitTy) :-} raw_constrs
  res <- runReader ([] :: [Q]) . runThrow . runFresh tvar . runState (empty :: Graph Q) $ solveConstraints [] [] constrs
  let generalizeAndLiftEv subst =
        let term = {-closeEffects pred_map $-} apply subst typed_term
            (ev_map, residual_ev) = generateEvidenceForSolved (apply subst raw_ev_map)
            term_ty = apply subst (term ^. meta . ty)
            scheme = Scheme (nub $ toListOf (cosmos . typeVars) term_ty) residual_ev term_ty
         in trace
              ("\nEV Map:\n" ++ showPretty residual_ev ++ {-"Typed Term:\n" ++ ppShow typed_term ++-} "\nSubst:\n" ++ showPretty subst ++ "\n")
              (term, Core.zonk subst $ wrap scheme $ liftEv (Core.varSubst ev_map core_term), scheme)
  case res of
    Left (err :: TyErr) -> error ("Failed to infer a type: " ++ show err ++ "\n\n" ++ unpack (Core.prettyRender core_term))
    Right (_, (graph, (subst, []))) ->
      let style = Style mempty [] [] ["shape" := "box"] [] (renderStrict . layoutSmart (LayoutOptions (AvailablePerLine 100 1.0)) . pretty) (const []) (\_ _ -> []) DoubleQuotes
          dotfile = Algebra.Graph.Export.Dot.export style (Graph.simplify graph)
          x = unsafePerformIO $ TIO.writeFile "test.dot" dotfile
       in seq (trace ("Typed Term:\n" ++ unpack (Core.prettyRender core_term)) $ x) $ return $ generalizeAndLiftEv subst
    Right (_, (graph, (_, qs))) ->
      let style = Style mempty [] [] ["shape" := "box"] [] (renderStrict . layoutSmart (LayoutOptions (AvailablePerLine 100 1.0)) . pretty) (const []) (\_ _ -> []) DoubleQuotes
          dotfile = Algebra.Graph.Export.Dot.export style (Graph.simplify graph)
          x = unsafePerformIO $ TIO.writeFile "test.dot" dotfile
       in seq (trace ("Typed Term:\n" ++ unpack (Core.prettyRender core_term)) $ x) $ error ("Remaining Qs: " ++ showPretty qs ++ {-"\nSubst: " ++ ppShow subst ++-} "\n\nCore: " ++ unpack (Core.prettyRender (Core.simplify core_term)))
 where
  empty_ev_map = Map.empty :: Map Var Q
  maxVar = maxTermVar term

inferSingTerm term = run . runReader emptyEffCtx . runReader (Map.insert Core.evv (monoScheme unitTy) builtinCtx) $ inferTerm term

referenced :: Term Infer -> TVarSet
referenced term = TVarSet.fromList $ do
  t <- universe term
  row <- t ^. meta . eff : typeEffects (t ^. meta . ty)
  case row of
    Closed _ -> []
    Open eff -> [eff]

infer :: Prog () -> (Prog Infer, [Core.Core], [Scheme])
infer (Prog defs effs) = snd . runIdentity . runReader effCtx . runState builtinCtx $ inferDefs effs defs
 where
  effCtx = mkEffCtx effs

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
prettyInferTerm term = putStrLn $ ppShow infer ++ "\n\n" ++ unpack (Core.prettyRender . Core.simplify $ core {-++ "\n" ++ show (Core.simplify core) ++ "\n\n"-}) ++ ppShow scheme ++ "\n"
 where
  (infer, core, scheme) = runIdentity $ runReader defaultEffCtx $ runReader emptyCtx $ inferTerm term

prettyInfer :: Prog () -> IO ()
prettyInfer prog = putStrLn $ unpack (prettyDefs (Core.simplify <$> cores)) ++ "\n\n" ++ ppShowList schemes {- "\n" ++ ppShowList (universeOn traverse cores >>= toListOf (Core.coreVars .  Core.coreVarTy)) ++ -}
 where
  prettyDefs [core] = Text.concat [Core.prettyRender core, "\n\n" {-, Text.pack (show core)-}]
  prettyDefs cores = Text.intercalate "\n\n" (Core.prettyRender <$> cores)
  (_, cores, schemes) = infer prog

example :: Term ()
example = abs [x, y, z] $ app (var x) [var z, var y <@> var z]
 where
  x = V 0
  y = V 1
  z = V 2

exampleInt :: Term ()
exampleInt = abs [f, g] $ var f <@> int 0 <@> (var g <@> int 11)
 where
  f = V 0
  g = V 1

examplePrim :: Term ()
examplePrim = app (var Core.add) [int 10, int 5]

exampleBoolPrim :: Term ()
exampleBoolPrim =
  Branch
    ()
    (abs [x] (abs [y] (int 47) <@> unlabel (var x) "False"))
    (abs [x] (abs [y] (int 32) <@> unlabel (var x) "True"))
    <@> app (var Core.lt) [int 0, int 5]
 where
  x = V 0
  y = V 1

-- To break cycle erase core ty for handlers in handler ty
eraseCoreTy (FunTy arg (Closed eff) ret) = FunTy arg (Closed (unitTy <$ eff)) ret
eraseCoreTy _ = error "eraseCoreTy: impossible"

state :: Eff
state = Eff{eff_name = "State", eff_handler_ty = state_handlerTy, eff_ops = monoScheme <$> state_ops}
 where
  -- state_effCoreTy = ProdTy (VarTy . TV $ -2)--(RowTy (Map.fromList [("marker", IntTy), ("handler", state_handlerTy)]))
  state_handlerTy = ProdTy (RowTy state_ops) -- -2
  state_ops =
    Map.fromList
      [ ("get", FunTy unitTy (Open . TV $ -3 {-Closed ("State" |> state_effCoreTy)-}) IntTy)
      , ("put", FunTy IntTy (Open . TV $ -3 {-Closed ("State" |> state_effCoreTy)-}) unitTy)
      ]

reader :: Eff
reader = Eff{eff_name = "Reader", eff_handler_ty = reader_handlerTy, eff_ops = monoScheme <$> reader_ops}
 where
  -- reader_effCoreTy = ProdTy (VarTy . TV $ -3) --(RowTy (Map.fromList [("marker", IntTy), ("handler", reader_handlerTy)]))
  reader_handlerTy = ProdTy (RowTy reader_ops) -- -4
  reader_ops =
    Map.fromList
      [ ("ask", FunTy unitTy (Open (TV $ -5 {-Closed ("Reader" |> reader_effCoreTy)-})) IntTy)
      ]

exampleMVPMultiEffect :: Prog ()
exampleMVPMultiEffect = Prog{terms = [main], effects = [state, reader]}
 where
  main =
    Def
      { def_name = V 100
      , def_term = handle "State" state_handler (handle "Reader" reader_handler body) <@> int 0
      }

  body = perform "put" (perform "ask" unit)
  x = V 0
  resume = V 1
  s = V 2

  state_handler = handler state_clauses state_ret
  state_clauses =
    NonEmpty.fromList
      [ Clause{_op = "get", _arg = x, _resume = resume, _body = abs [s] (app (var resume) [var s, var s])}
      , Clause{_op = "put", _arg = x, _resume = resume, _body = abs [s] (app (var resume) [unit, var x])}
      ]
  state_ret = Clause{_op = "return", _arg = x, _resume = resume, _body = abs [s] (record [label "value" (var x), label "state" (var s)])}

  reader_handler = handler reader_clauses reader_ret
  reader_clauses =
    NonEmpty.fromList
      [Clause "ask" x resume (app (var resume) [int 2])]
  reader_ret = Clause "return" x resume (var x)

exampleMultipleEffects :: Prog ()
exampleMultipleEffects = Prog{terms = [main], effects = [state, reader]}
 where
  main =
    Def
      { def_name = V 100
      , def_term =
          letChain
            [(y, int 2)]
            (handle "Reader" reader_handler (handle "State" state_handler body <@> int 0))
      }

  body =
    letChain
      [ (s, perform "get" unit)
      , (e, perform "ask" unit)
      , (w, perform "put" (app (var Core.add) [var s, var e]))
      , (s, perform "get" unit)
      , (w, perform "put" (app (var Core.add) [var s, var e]))
      ]
      (perform "get" unit)

  state_handler = handler state_clauses state_ret
  state_clauses =
    NonEmpty.fromList
      [ Clause{_op = "get", _arg = x, _resume = resume, _body = abs [s] (app (var resume) [var s, var s])}
      , Clause{_op = "put", _arg = x, _resume = resume, _body = abs [s] (app (var resume) [unit, var x])}
      ]
  state_ret = Clause{_op = "return", _arg = x, _resume = resume, _body = abs [s] (record [label "value" (var x), label "state" (var s)])}

  reader_handler = handler reader_clauses reader_ret
  reader_clauses =
    NonEmpty.fromList
      [Clause "ask" x resume (app (var resume) [var y])]
  reader_ret = Clause "return" x resume (var x)

  x = V 0
  resume = V 1
  s = V 2
  e = V 3
  w = V 4
  y = V 5

exampleProperState :: Prog ()
exampleProperState = Prog{terms = [main], effects = [state]}
 where
  main = Def{def_name = V 100, def_term = body}
  body = unlabel (prj R (h <@> int 5)) "value"
  -- body = h <@> int 5
  h =
    handle
      "State"
      (handler clauses ret)
      (abs [V 3] (perform "get" unit) <@> perform "put" (int 2))
  -- (perform "get" (perform "put" (int 2)))

  x = V 0
  resume = V 1
  s = V 2

  clauses =
    NonEmpty.fromList
      [ Clause{_op = "get", _arg = x, _resume = resume, _body = abs [s] (app (var resume) [var s, var s])}
      , Clause{_op = "put", _arg = x, _resume = resume, _body = abs [s] (app (var resume) [unit, var x])}
      ]

  ret = Clause{_op = "return", _arg = x, _resume = resume, _body = abs [s] (record [label "value" (var x), label "state" (var s)])}

  state =
    Eff
      { eff_name = "State"
      , eff_handler_ty = effCoreTy
      , eff_ops = monoScheme <$> ops
      }

  effCoreTy = ProdTy (VarTy (TV $ -2 {-(RowTy (Map.fromList [("marker", IntTy), ("handler", handlerTy)]))-}))
  -- handlerTy = ProdTy (RowTy (eraseCoreTy <$> ops))
  -- To break cycle erase core ty for handlers in handler ty
  -- eraseCoreTy (FunTy arg (Closed eff) ret) = FunTy arg (Closed (unitTy <$ eff)) ret
  -- eraseCoreTy _ = error "eraseCoreTy: impossible"
  ops =
    Map.fromList
      [ ("get", FunTy unitTy (Closed ("State" |> effCoreTy)) IntTy)
      , ("put", FunTy IntTy (Closed ("State" |> effCoreTy)) unitTy)
      ]

exampleRow :: Term ()
exampleRow =
  abs [rho] (unlabel (prj L (var rho)) "x")
 where
  rho = V 0

exampleSum :: Term ()
exampleSum =
  Branch
    ()
    (abs [x] (unlabel (var x) "False"))
    (abs [x] (unlabel (var x) "True"))
    <@> Inj () R (label "True" (int 1))
 where
  x = V 0

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
exampleSmolEff = handle "State" (handler clauses ret) (perform "get" unit)
 where
  resume = V 1
  x = V 2
  -- All of these are in disjoint scopes so we're okay to reuse variables, although it's kinda sloppy
  clauses = Clause "get" x resume (var resume <@> int 4) :| [Clause "put" (V 3) (V 4) (var (V 4) <@> unit)]
  ret = Clause "return" x resume (var x)

exampleProgSmolEff :: Prog ()
exampleProgSmolEff = Prog defs [stateEff]
 where
  defs =
    [Def (V 0) $ abs [V 10] $ handle "State" (handler clauses ret) $ perform "get" unit]

  resume = V 1
  x = V 2

  clauses = Clause "get" x resume (var resume <@> int 4) :| [Clause "put" (V 3) (V 4) (var (V 4) <@> unit)]
  ret = Clause "return" x resume (var x)

exampleEff :: Term ()
exampleEff = handle "State" (handler clauses ret) $ abs [v] (perform "put" (var v)) <@> perform "get" unit
 where
  v = V 0
  resume = V 1
  x = V 2
  -- All of these are in disjoint scopes so we're okay to reuse variables, although it's kinda sloppy
  clauses = Clause "get" x resume (var resume <@> int 4) :| [Clause "put" x resume (var resume <@> unit)]
  ret = Clause "return" x resume (label "x" $ var x)

exampleGet :: Term ()
exampleGet = abs [v] $ var v <@> perform "get" unit
 where
  v = V 0

exampleWand :: Term ()
exampleWand = abs [m, n] $ unlabel (prj L $ record [var m, var n]) "x"
 where
  m = V 0
  n = V 1

exampleApWand :: Term ()
exampleApWand = app wand [record [label "x" (int 3), label "y" (int 4)], record [label "w" (int 1), label "z" (int 2)]]
 where
  wand = abs [m, n] (unlabel (prj L $ record [var m, var n]) "x")
  m = V 0
  n = V 1

exampleProgWand :: Prog ()
exampleProgWand =
  Prog
    { terms = [wand_def, ap]
    , effects = []
    }
 where
  wand = V (-2)
  wand_def = Def wand exampleWand
  ap = Def (V (-3)) (var wand <@> record [label "w" (int 1), label "z" (int 2)])

-- This one should fail and it does!
exampleHandleErr :: Term ()
exampleHandleErr = handle "State" (handler whoops ret) $ abs [v] (perform "put" (var v)) <@> perform "get" unit
 where
  v = V 0
  resume = V 1
  x = V 2
  -- We forgot a clause
  whoops = Clause "get" x resume (int 4) :| []
  ret = Clause "" x resume (label "x" $ var x)
