{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Interpret where

import Term (Var, prettyV)
import Core
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Control.Lens
import Control.Effect.State
import qualified CC.Prompt as Prompt
import Subst
import Control.Carrier.State.Strict (runState)
import Pretty
import Prettyprinter
import qualified Prettyprinter.Render.Terminal as Terminal
import qualified Data.Text.IO
import Debug.Trace


data Value
  = ValLit Core.Literal
  | ValLam CoreVar Core
  | ValStruct [Value]
  | ValPrompt (Prompt.Prompt () Value)
  | ValCont Stack
  deriving (Show)

prettyVal :: Value -> Doc SyntaxHighlight
prettyVal val = 
  case val of
    ValLit lit -> annotate Literal (pretty lit)
    ValLam x body -> prettyCore (Lam x body)
    ValStruct vals 
      | null vals -> annotate Literal "{}"
      | otherwise -> group (align ("{" <+> vsep (punctuate "," (prettyVal <$> vals)) <> line <> "}"))
    ValPrompt prompt -> Prompt.prettyPrompt prompt
    ValCont stack -> group (prettyStack stack)

type DiffList a = [a] -> [a]

data EvalCtx
  = FnAppFrame Core
  | ArgAppFrame Value
  | EvalPromptFrame Core
  | PromptYieldFrame Value
  | ValueYieldFrame Core 
  | ProductFrame (DiffList Value) [Core]
  | ProjectFrame Int 

instance Show EvalCtx where
  showsPrec d eval_ctx = showParen (d > app_prec) $ 
      case eval_ctx of
        FnAppFrame core -> showString "FnAppFrame " . showsPrec (app_prec + 1) core
        ArgAppFrame val -> showString "ArgAppFrame " . showsPrec (app_prec + 1) val
        EvalPromptFrame core -> showString "EvalPromptFrame " . showsPrec (app_prec + 1) core
        PromptYieldFrame val -> showString "PromptYieldFrame " . showsPrec (app_prec + 1) val
        ValueYieldFrame core -> showString "ValueYieldFrame " . showsPrec (app_prec + 1) core
        ProductFrame vals cores -> showString "ProductFrame " . showsPrec (app_prec + 1) (vals []) . showsPrec (app_prec + 1) cores
        ProjectFrame indx -> showString "ProjectFrame " . showsPrec (app_prec + 1) indx
    where
      app_prec = 10

type Frame = [EvalCtx]

prettyFrame :: Frame -> Doc SyntaxHighlight
prettyFrame = foldr wrapEvalCtx (annotate Literal "☐")
  where
    wrapEvalCtx eval_ctx doc = 
      case eval_ctx of
        FnAppFrame core -> group . align . parens $ doc <> nest 4 (line <> prettyCore core)
        ArgAppFrame val -> group . align . parens $ prettyVal val <> nest 4 (line <> doc)
        EvalPromptFrame core -> "Prompt" <+> record [("prompt_marker", doc), ("prompt_body", prettyCore core)]
        PromptYieldFrame val -> "Yield" <+> record [("yield_marker", doc), ("yield_value", prettyVal val)]
        ValueYieldFrame core -> "Yield" <+> record [("yield_marker", prettyCore core), ("yield_value", doc)]
        ProductFrame vals cores -> 
          let cs = prettyCore <$> cores
              vs = prettyVal <$> vals []
           in braces . group . align . vsep$ punctuate comma (vs ++ (doc : cs))
        ProjectFrame indx -> doc <> brackets (annotate Literal $ pretty indx)

prettyCurFrame :: Frame -> Doc SyntaxHighlight
prettyCurFrame = group . align . vsep . punctuate " · " . fmap prettyEvalCtx . reverse
  where
    box = annotate Literal "☐"
    prettyEvalCtx eval_ctx =
      case eval_ctx of
        FnAppFrame core -> group . align . parens $ box <> nest 4 (line <> prettyCore core)
        ArgAppFrame val -> group . align . parens $ prettyVal val <> nest 4 (line <> box)
        EvalPromptFrame core -> "Prompt" <+> record [("prompt_marker", box), ("prompt_body", prettyCore core)]
        PromptYieldFrame val -> "Yield" <+> record [("yield_marker", box), ("yield_value", prettyVal val)]
        ValueYieldFrame core -> "Yield" <+> record [("yield_marker", prettyCore core), ("yield_value", box)]
        ProductFrame vals cores -> 
          let cs = prettyCore <$> cores
              vs = prettyVal <$> vals []
           in braces . group . align . vsep$ punctuate comma (vs ++ (box : cs))
        ProjectFrame indx -> box <> brackets (annotate Literal $ pretty indx)

-- | Push a frame "on top" of another frame.
-- | Replace the hole within the frame with this new frame
push :: EvalCtx -> Frame -> Frame
push = (:)

type Env = Map Var Value

prettyEnv :: Env -> Doc SyntaxHighlight
prettyEnv = braces . group . align . vsep . punctuate (flatAlt line comma) . fmap prettyEntry . Map.toList
  where
    prettyEntry (var, val) = prettyV var <+> ":=" <+> prettyVal val

data Stack
  = EmptyStack
  | PushPrompt (Prompt.Prompt () Value) Stack
  | PushFrame (Env, Frame) Stack
  deriving (Show)

prettyStack :: Stack -> Doc SyntaxHighlight
prettyStack EmptyStack = mempty
prettyStack (PushPrompt prompt stack) = align ("Prompt:" <+> Prompt.prettyPrompt prompt <> line <> prettyStack stack)
prettyStack (PushFrame (env, frame) stack) = group (align ("Frame: " <> nest 4 (line <> prettyEnv env <> line <> prettyFrame frame <> line <> line <> prettyStack stack)))

pushStack :: Stack -> Stack -> Stack
pushStack src dest = go src
  where
    go EmptyStack = dest
    go (PushPrompt prompt stack) = PushPrompt prompt (go stack)
    go (PushFrame frame stack) = PushFrame frame (go stack)

splitStack :: Prompt.Prompt () Value -> Stack -> (Stack, Stack)
splitStack needle_prompt = go id
  where
    go diffstack EmptyStack = (diffstack EmptyStack, EmptyStack)
    go diffstack (PushFrame frame stack) = go (PushFrame frame . diffstack) stack
    go diffstack (PushPrompt prompt stack) =
      case Prompt.eqPrompt needle_prompt prompt of
        Prompt.NEQ -> go (PushPrompt prompt . diffstack) stack
        -- Install prompt in both stacks
        -- At the bottom of the substack, but at the top of the remaining stack
        Prompt.EQU -> (diffstack . PushPrompt prompt $ EmptyStack, PushPrompt prompt stack)

data Machine = Machine { _stack :: Stack, _prompt :: Int, _cur_frame :: Frame, _cur_env :: Env }
  deriving (Show)

prettyMachine :: Machine -> Doc SyntaxHighlight
prettyMachine (Machine stack prompt cur_frame cur_env) =
  align (vsep 
    [ "Stack:"
    , prettyStack stack <> line
    , "Next Prompt:" <+> annotate Literal (pretty prompt)
    , "Current Frame:" <+> prettyCurFrame cur_frame
    , "Current Env:" <+> prettyEnv cur_env
    ])


initialMachine :: Machine
initialMachine = Machine EmptyStack 0 [] Map.empty


makeLenses ''Machine

data Res 
  = Done Value
  | Step Core
  deriving (Show)

prettyRes :: Res -> Doc SyntaxHighlight
prettyRes (Done val) = prettyVal val
prettyRes (Step core) = prettyCore core

findProductFocus :: (Has (State Machine) sig m) => [Core] -> m (DiffList Value, [Core])
findProductFocus = go id
  where
    -- All our product elements are evaluated, create a value out of them
    go vals [] = return (vals, [])
    go vals (core:cores) = do
      res <- eval (Step core)
      case res of
        Done val -> go ((val :) . vals) cores
        Step core -> return (vals, core:cores)

eval :: (Has (State Machine) sig m) => Res -> m Res
eval (Done val) = return (Done val)
eval (Step core) = 
  case core of
    Var (CoreV x _) -> do
      env <- gets (view cur_env)
      case env !? x of
        Nothing -> error ("Stuck: Undefined var " ++ show x)
        Just val -> unwind val
    -- These are actually wrong, we need to try and "pop" the stack whenever we reach this state
    Lit lit -> unwind (ValLit lit)
    Lam x body -> unwind (ValLam x body)

    Product elems -> do
      (vals, tail) <- findProductFocus elems
      case tail of
        [] -> unwind . ValStruct . vals $ []
        core:cores -> do
          -- Push the rest of the product evaluation as a frame
          modify (cur_frame %~ push (ProductFrame vals cores))
          -- Return new focal point as our next eval step
          step core

    Project indx core -> do
      modify (cur_frame %~ push (ProjectFrame indx))
      step core

    NewPrompt (CoreV x _) body -> do
     i <- gets (view prompt)
     modify (prompt +~ 1)
     modify (cur_env %~ Map.insert x (ValPrompt $ Prompt.Prompt i))
     step body

    Prompt marker body -> do
      modify (cur_frame %~ push (EvalPromptFrame body))
      step marker

    Yield marker body -> do
      modify (cur_frame %~ push (ValueYieldFrame marker))
      step body

    App fn arg -> do
      modify (cur_frame %~ push (FnAppFrame arg))
      step fn

    TyApp (TyLam (CoreTV a _) body) ty -> step $ transform (over (Core.coreVars . Core.coreVarTy) (apply (a |-> toType ty))) body
    TyApp {} -> error "Stuck: TyApp"
    TyLam {} -> error "Stuck: TyLam" 
  where
    step = return . Step

    -- Once we evaluate our current focus to a value, we unwind the current frame substituting our value into each EvalCtx until we reach a new focus
    -- This is where the meat of evaluation occurs.
    unwind val = do
      frame <- gets (view cur_frame)
      case frame of
        -- If our current frame is empty "return" to the previous frame by popping the stack
        [] -> pop_stack val
        eval:frames -> do
          modify (cur_frame .~ frames)
          case eval of
            FnAppFrame arg -> do
              modify (cur_frame %~ push (ArgAppFrame val))
              step arg
            ArgAppFrame fn -> do
              case fn of 
                ValLam (CoreV x _) body -> do
                  modify (cur_env %~ Map.insert x val)
                  step body
                ValCont kont -> do
                  -- Not sure this is correct
                  -- Currently this removes the current stack frame
                  -- We might want to save the current stack frame instead?
                  modify (stack %~ pushStack kont)
                  unwind val
                _ -> error "Stuck: non-function value applied as function"
            EvalPromptFrame body -> do
              let prompt = 
                    case val of 
                      ValPrompt prompt -> prompt
                      _ -> error "Stuck: non-prompt passed to Prompt as marker"
              -- Save current frame before the prompt in stack
              new_stack_frame
              modify (stack %~ PushPrompt prompt)
              step body
            ValueYieldFrame marker -> do
              modify (cur_frame %~ push (PromptYieldFrame val))
              step marker
            PromptYieldFrame fn -> do
              let prompt = expectPrompt "Stuck: non-prompt passed to Yield as marker" val
              new_stack_frame
              (sub_stack, new_stack) <- gets (splitStack prompt . view stack)
              case fn of 
                ValLam (CoreV kont _) body -> do
                  modify (stack .~ new_stack)
                  modify (cur_env %~ Map.insert kont (ValCont sub_stack))
                  step body
                _ -> error "Stuck: Non-lambda passed as value to Yield"
            ProductFrame vals remaining -> do
              (new_vals, cores) <- findProductFocus remaining
              case cores of 
                [] -> unwind . ValStruct . vals . new_vals $ []
                core:cores -> do
                  -- Push the rest of the product evaluation as a frame
                  modify (cur_frame %~ push (ProductFrame (vals . new_vals) cores))
                  -- Return new focal point as our next eval step
                  step core
            ProjectFrame indx -> 
              case val of 
                ValStruct elems -> unwind (elems !! indx)
                _ -> error "Stuck: non-struct value passed to Project"
                                          
    new_stack_frame = do
      frame <- swap cur_frame [] 
      -- we don't clean up the env because we want to keep variables up the stack in environment down the stack
      -- when we pop this will be cleaned up correctly either way so this saves us having to walk the stack to find things
      -- inefficent space-wise, but convenient
      env <- gets (view cur_env)
      modify (stack %~ PushFrame (env, frame))
          
    swap :: (Has (State s) sig m) => Lens' s a -> a -> m a
    swap lens new = do
      old <- gets (view lens)
      modify (lens .~ new)
      return old

    pop_stack val = do
      s <- gets (view stack)
      case s of 
        -- If the stack is empty this is our final return value, produce Done
        EmptyStack -> return (Done val)
        -- If we encounter a prompt while popping remove it and continue.
        -- We are leaving the scope where the prompt is relevant.
        PushPrompt _ new_stack -> do
          modify (stack .~ new_stack)
          pop_stack val
        PushFrame (env, frame) new_stack -> do
          modify (stack .~ new_stack)
          modify (cur_frame .~ frame)
          modify (cur_env .~ env)
          unwind val

oneStep :: Core -> (Machine, Res)
oneStep = steps 1

steps n core = run . runState initialMachine $ go n eval (Step core)
  where
    go 0 _ x = return x
    go n f x = f x >>= go (n - 1) f

interpret :: Core -> Value
interpret prog = snd . run . runState initialMachine $ go (Step prog)
  where
    go res = eval res >>= 
      \case
        Step core -> go (Step core)
        Done val -> return val
  

prettySteps n = Data.Text.IO.putStrLn . render . prettyOut . steps n
  where
    prettyOut (machine, res) = prettyMachine machine <> line <> line <> "Result:" <+> prettyRes res

    render = Terminal.renderStrict . reAnnotateS toAnsi . layoutSmart layoutOpts

    layoutOpts = LayoutOptions (AvailablePerLine 100 1.0)

    toAnsi :: SyntaxHighlight -> Terminal.AnsiStyle
    toAnsi =
      \case
        Keyword -> Terminal.color Terminal.Red
        Literal -> Terminal.color Terminal.Cyan
        NamedVariable -> Terminal.color Terminal.Green <> Terminal.underlined
        TypeVariable -> Terminal.color Terminal.Magenta <> Terminal.italicized

expectPrompt _ (ValPrompt p) = p
expectPrompt msg _ = error msg 

expectLam _ (ValLam x body) = (x, body)
expectLam msg _ = error msg
