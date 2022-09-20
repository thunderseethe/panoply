{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Interpret where

import Term (Var)
import Core
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Control.Monad.CC
import Control.Monad.Reader
import Data.Maybe
import Control.Lens
import Subst
import Unsafe.Coerce

data Value ans
  = ValLit Core.Literal
  | ValLam CoreVar Core
  | ValStruct [Value ans]
  | ValPrompt (Prompt ans (Value ans))
  | ValCont (SubCont ans (Reader (Env ans)) (Value ans) (Value ans))


type Env ans = Map Var (Value ans)

type Machine ans a = CCT ans (Reader (Env ans)) a

interpret :: Core -> Machine ans (Value ans)
interpret core = 
  case core of
    Var (CoreV var _) -> do
      ctx <- ask
      return (fromMaybe (error $ "stuck: undefined variable " ++ show var) (ctx !? var))
    Lit lit -> return (ValLit lit)
    Lam x body -> return (ValLam x body)
    Product elems -> do
      values <- forM elems interpret
      return (ValStruct values)
    Project indx product -> do
      struct <- interpret product
      case struct of
        ValStruct elems -> return (elems !! indx)
        _ -> error "stuck: Non-product passed to project"
    NewPrompt (CoreV x _) body -> do
      p <- newPrompt
      local (Map.insert x (ValPrompt p)) $ interpret body
    Prompt marker _ body -> do
      prompt <- expectPrompt "stuck: Non-prompt passed as handle marker" <$> interpret marker
      pushPrompt prompt (interpret body)
    Yield marker value -> do
      prompt <- expectPrompt "stuck: Non-prompt passed as yield marker" <$> interpret marker
      (CoreV arg _, body) <- expectLam "stuck:  Non-lambda passed to yield" <$> interpret value
      withSubCont prompt $ \kont ->
        local (Map.insert arg (ValCont kont)) (interpret body)
    App fn arg -> do
      fn_val <- interpret fn
      case fn_val of
        ValLam (CoreV x _) body -> do
          arg_val <- interpret arg
          local (Map.insert x arg_val) (interpret body)
        ValCont kont -> pushSubCont kont $ interpret arg
        _ -> error "stuck: Non-function passed to app"
    TyApp (TyLam (CoreTV tvar _) body) ty -> interpret $ transform (over (Core.coreVars . Core.coreVarTy) (apply (tvar |-> toType ty))) body
    TyApp {} ->  error "stuck: Type application"
    TyLam {} -> error "stuck: Type lambda"


expectPrompt _ (ValPrompt p) = p
expectPrompt msg _ = error msg 

expectLam _ (ValLam x body) = (x, body)
expectLam msg _ = error msg

{-test = 
  TyLam (CoreTV a KindType) 
  (TyLam (CoreTV d' KindType) 
    (Lam (CoreV (V (-1)) (CoreProduct [])) 
    (Lam (CoreV (V 10) (CoreVar (CoreTV a KindType)))
      (NewPrompt (CoreV (V (-2)) (CoreLit IntTy)) 
        (Prompt {
          prompt_marker = Var (CoreV (V (-2)) (CoreLit IntTy)), 
          prompt_handler = Product 
            [ Lam (CoreV (V 2) (CoreVar (CoreTV d' KindType))) (Var (CoreV (V 2) (CoreVar (CoreTV d' KindType))))
            , Lam (CoreV (V (-1)) (CoreProduct [])) (Lam (CoreV (V 2) (CoreProduct [])) (Lam (CoreV (V 1) (CoreFun (CoreProduct []) Closed () (CoreFun (CoreLit IntTy) Open f (CoreVar (CoreTV d' KindType))))) (App (App (Var (CoreV (V 1) (CoreFun (CoreProduct []) Closed () (CoreFun (CoreLit IntTy) Open f (CoreVar (CoreTV d' KindType)))))) (Var (CoreV (V (-1)) (CoreProduct [])))) (Lit (I 4)))))
            , Lam (CoreV (V (-1)) (CoreProduct [])) (Lam (CoreV (V 3) (CoreLit IntTy)) (Lam (CoreV (V 4) (CoreFun (CoreProduct []) Closed () (CoreFun (CoreProduct []) Open f (CoreVar (CoreTV d' KindType))))) (App (App (Var (CoreV (V 4) (CoreFun (CoreProduct []) Closed () (CoreFun (CoreProduct []) Open f (CoreVar (CoreTV d' KindType)))))) (Var (CoreV (V (-1)) (CoreProduct [])))) (Product []))))
            ], 
          prompt_body = 
            App 
              (Lam (CoreV (V (-1)) (CoreProduct [CoreProduct [CoreFun (CoreProduct []) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreFun (CoreFun (CoreLit IntTy) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))),CoreFun (CoreLit IntTy) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreFun (CoreFun (CoreProduct []) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))),CoreFun (CoreVar (CoreTV d' KindType)) Closed () (CoreVar (CoreTV d' KindType))],CoreLit IntTy])) (App (Lam (CoreV (V (-3)) (CoreProduct [CoreProduct [CoreFun (CoreProduct []) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreFun (CoreFun (CoreLit IntTy) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))),CoreFun (CoreLit IntTy) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreFun (CoreFun (CoreProduct []) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))),CoreFun (CoreVar (CoreTV d' KindType)) Closed () (CoreVar (CoreTV d' KindType))],CoreLit IntTy])) (Yield {yield_marker = Project 0 (Var (CoreV (V (-3)) (CoreProduct [CoreProduct [CoreFun (CoreProduct []) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreFun (CoreFun (CoreLit IntTy) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))),CoreFun (CoreLit IntTy) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreFun (CoreFun (CoreProduct []) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))),CoreFun (CoreVar (CoreTV d' KindType)) Closed () (CoreVar (CoreTV d' KindType))],CoreLit IntTy]))), yield_value = Lam (CoreV (V (-4)) (CoreFun (CoreLit IntTy) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType)))) (App (App (Project 1 (Project 1 (Var (CoreV (V (-3)) (CoreProduct [CoreProduct [CoreFun (CoreProduct []) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreFun (CoreFun (CoreLit IntTy) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))),CoreFun (CoreLit IntTy) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreFun (CoreFun (CoreProduct []) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))),CoreFun (CoreVar (CoreTV d' KindType)) Closed () (CoreVar (CoreTV d' KindType))],CoreLit IntTy]))))) (Product [])) (Var (CoreV (V (-4)) (CoreFun (CoreLit IntTy) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))))))})) (App (Lam (CoreV (V 0) (CoreProduct [CoreProduct [CoreFun (CoreProduct []) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreFun (CoreFun (CoreLit IntTy) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))),CoreFun (CoreLit IntTy) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreFun (CoreFun (CoreProduct []) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))),CoreFun (CoreVar (CoreTV d' KindType)) Closed () (CoreVar (CoreTV d' KindType))],CoreLit IntTy])) (Var (CoreV (V 0) (CoreProduct [CoreProduct [CoreFun (CoreProduct []) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreFun (CoreFun (CoreLit IntTy) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))),CoreFun (CoreLit IntTy) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreFun (CoreFun (CoreProduct []) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))),CoreFun (CoreVar (CoreTV d' KindType)) Closed () (CoreVar (CoreTV d' KindType))],CoreLit IntTy])))) (Var (CoreV (V (-1)) (CoreProduct [CoreProduct [CoreFun (CoreProduct []) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreFun (CoreFun (CoreLit IntTy) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))),CoreFun (CoreLit IntTy) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreFun (CoreFun (CoreProduct []) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))),CoreFun (CoreVar (CoreTV d' KindType)) Closed () (CoreVar (CoreTV d' KindType))],CoreLit IntTy])))))) 
              (App 
                (Lam (CoreV (V 5) (CoreProduct [CoreFun (CoreProduct []) Open e (CoreFun (CoreFun (CoreLit IntTy) Open e (CoreVar (CoreTV d' KindType))) Open e (CoreVar (CoreTV d' KindType))),CoreFun (CoreLit IntTy) Open e (CoreFun (CoreFun (CoreProduct []) Open e (CoreVar (CoreTV d' KindType))) Open e (CoreVar (CoreTV d' KindType))),CoreFun (CoreVar (CoreTV d' KindType)) Closed () (CoreVar (CoreTV d' KindType))])) (App (App (Lam (CoreV (V 0) (CoreProduct [])) (Lam (CoreV (V 1) (CoreProduct [CoreProduct [CoreFun (CoreProduct []) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreFun (CoreFun (CoreLit IntTy) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))),CoreFun (CoreLit IntTy) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreFun (CoreFun (CoreProduct []) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))),CoreFun (CoreVar (CoreTV d' KindType)) Closed () (CoreVar (CoreTV d' KindType))],CoreLit IntTy])) (Var (CoreV (V 1) (CoreProduct [CoreProduct [CoreFun (CoreProduct []) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreFun (CoreFun (CoreLit IntTy) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))),CoreFun (CoreLit IntTy) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreFun (CoreFun (CoreProduct []) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))),CoreFun (CoreVar (CoreTV d' KindType)) Closed () (CoreVar (CoreTV d' KindType))],CoreLit IntTy]))))) (App (Lam (CoreV (V 0) (CoreProduct [])) (Product [])) (Var (CoreV (V (-1)) (CoreVar (CoreTV d KindRow)))))) (App (App (Lam (CoreV (V 0) (CoreLit IntTy)) (Lam (CoreV (V 1) (CoreProduct [CoreFun (CoreProduct []) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreFun (CoreFun (CoreLit IntTy) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))),CoreFun (CoreLit IntTy) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreFun (CoreFun (CoreProduct []) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))),CoreFun (CoreVar (CoreTV d' KindType)) Closed () (CoreVar (CoreTV d' KindType))])) (Product [Var (CoreV (V 0) (CoreLit IntTy)),Var (CoreV (V 1) (CoreProduct [CoreFun (CoreProduct []) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreFun (CoreFun (CoreLit IntTy) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))),CoreFun (CoreLit IntTy) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreFun (CoreFun (CoreProduct []) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))) "State" |> {"handler" |> {"get" |> ({} -{State |> {}, }-> IntTy), "put" |> (IntTy -{State |> {}, }-> {})}, "marker" |> IntTy} (CoreVar (CoreTV d' KindType))),CoreFun (CoreVar (CoreTV d' KindType)) Closed () (CoreVar (CoreTV d' KindType))]))]))) (Var (CoreV (V (-2)) (CoreLit IntTy)))) (Var (CoreV (V 5) (CoreProduct [CoreFun (CoreProduct []) Open e (CoreFun (CoreFun (CoreLit IntTy) Open e (CoreVar (CoreTV d' KindType))) Open e (CoreVar (CoreTV d' KindType))),CoreFun (CoreLit IntTy) Open e (CoreFun (CoreFun (CoreProduct []) Open e (CoreVar (CoreTV d' KindType))) Open e (CoreVar (CoreTV d' KindType))),CoreFun (CoreVar (CoreTV d' KindType)) Closed () (CoreVar (CoreTV d' KindType))])))))) 
                (Product 
                  [ Lam (CoreV (V 2) (CoreVar (CoreTV d' KindType))) (Var (CoreV (V 2) (CoreVar (CoreTV d' KindType))))
                  , Lam (CoreV (V (-1)) (CoreProduct [])) (Lam (CoreV (V 2) (CoreProduct [])) (Lam (CoreV (V 1) (CoreFun (CoreProduct []) Closed () (CoreFun (CoreLit IntTy) Open f (CoreVar (CoreTV d' KindType))))) (App (App (Var (CoreV (V 1) (CoreFun (CoreProduct []) Closed () (CoreFun (CoreLit IntTy) Open f (CoreVar (CoreTV d' KindType)))))) (Var (CoreV (V (-1)) (CoreProduct [])))) (Lit (I 4)))))
                  , Lam (CoreV (V (-1)) (CoreProduct [])) (Lam (CoreV (V 3) (CoreLit IntTy)) (Lam (CoreV (V 4) (CoreFun (CoreProduct []) Closed () (CoreFun (CoreProduct []) Open f (CoreVar (CoreTV d' KindType))))) (App (App (Var (CoreV (V 4) (CoreFun (CoreProduct []) Closed () (CoreFun (CoreProduct []) Open f (CoreVar (CoreTV d' KindType)))))) (Var (CoreV (V (-1)) (CoreProduct [])))) (Product []))))]))
        })))))-}
