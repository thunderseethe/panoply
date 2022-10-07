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
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Foldable
import Data.Maybe
import Core.Builtin
import Debug.Trace


data Value
  = ValLit Core.Literal
  | ValLam CoreVar Core
  | ValStruct [Value]
  | ValTag Int Value
  | ValPrompt (Prompt.Prompt () Value)
  | ValCont Stack
  deriving (Show)

falseVal :: Value
falseVal = ValTag 0 (ValStruct [])

trueVal :: Value
trueVal = ValTag 1 (ValStruct [])

prettyVal :: Value -> Doc SyntaxHighlight
prettyVal val =
  case val of
    ValLit lit -> annotate Literal (pretty lit)
    ValLam x body -> prettyCore (Lam x body)
    ValStruct vals
      | null vals -> annotate Literal "{}"
      | otherwise -> group (align ("{" <+> vsep (punctuate "," (prettyVal <$> vals)) <> line <> "}"))
    ValTag tag val -> group (align ("<" <+> annotate Literal (pretty tag) <+> ">" <> parens (prettyVal val)))
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
  | CaseScrutineeFrame (Seq Core)
  | CaseBranchFrame Value
  | TagFrame Int


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
        CaseScrutineeFrame branches -> showString "CaseScrutineeFrame " . showsPrec (app_prec + 1) branches
        CaseBranchFrame scrutinee -> showString "CaseBranchFrame " . showsPrec (app_prec + 1) scrutinee
        TagFrame tag -> showString "TagFrame " . showsPrec (app_prec + 1) tag
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
        CaseScrutineeFrame branches -> "Case" <+> doc <+> group (align (list (toList (prettyCore <$> branches))))
        CaseBranchFrame scrutinee -> group . align . parens $ doc <> nest 4 (line <> prettyVal scrutinee)
        TagFrame tag -> angles (annotate Literal $ pretty tag) <> parens doc

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
        CaseScrutineeFrame branches -> "Case" <+> box <+> group (align (list (toList (prettyCore <$> branches))))
        CaseBranchFrame scrutinee -> group . align . parens $ box <> nest 4 (line <> prettyVal scrutinee)
        TagFrame tag -> angles (annotate Literal $ pretty tag) <> parens box

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
initialMachine = Machine EmptyStack 0 [] env
  where
    env = Map.fromList
      [ (Core.evv, ValStruct [])
      , (Core.add, ValLam (CoreV Core.evv (CoreProduct [])) intAdd) -- we cheat here by ascribing evv with type {}, but we never query the type at runtime
      , (Core.lt, ValLam (CoreV Core.evv (CoreProduct [])) intLt)
      ]


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
    Case scrutinee branches -> do
      modify (cur_frame %~ push (CaseScrutineeFrame branches))
      step scrutinee
    -- We can ignore coproduct type during runtime
    Coproduct _ tag elem -> do
      modify (cur_frame %~ push (TagFrame tag))
      step elem

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

    -- Primitives shouldn't really use the stack, by the time we have a Primitive at the top of the stack both of it's arguments should already be arguments or variables with arguments in Scope
    -- So this method unwraps arguments to values and them unwinds on the result of the evaluation
    Primitive (BinOp op left right) -> do
          env <- gets (view cur_env)
          let left_val = -- trace (show b) $ fromDone (\l -> error $ "Stuck: left prim arg did not eval to Done: " ++ show l) <$> eval (Step $ toCore left)
                case left of
                  PrimVal lit -> ValLit lit
                  PrimVar (CoreV var _) -> fromMaybe (error "Stuck: undefined variable in prim left arg") (env !? var)
          let right_val = case right of
                            PrimVal lit -> ValLit lit
                            PrimVar (CoreV var _) -> fromMaybe (error "Stuck: undefined variable in prim right arg") (env !? var)
          evalPrimOp op left_val right_val



    TyApp (TyLam (CoreTV a _) body) ty -> step $ transform (over (Core.coreVars . Core.coreVarTy) (apply (a |-> toType ty))) body
    c@(TyApp body _) -> step body --error $ "Stuck: TypApp " ++ show c
    TyLam {} -> error "Stuck: TyLam"
  where
    step = return . Step

    evalPrimOp op left_val right_val =
      case (op, left_val, right_val) of
        (IntAdd, ValLit (I left), ValLit (I right)) -> unwind (ValLit (I (left + right)))
        (IntAdd, _, _) -> error "Stuck: invalid arguments passed to add primitive"

        (IntLt, ValLit (I left), ValLit (I right)) ->
          if left < right
             then unwind trueVal
             else unwind falseVal
        (IntLt, _, _) -> error "Stuck: invalid arguments passed to less than primitive"

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
            ArgAppFrame fn -> call fn val
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
                [] -> unwind . ValStruct . new_vals . vals $ []
                core:cores -> do
                  -- Push the rest of the product evaluation as a frame
                  modify (cur_frame %~ push (ProductFrame (new_vals . vals) cores))
                  -- Return new focal point as our next eval step
                  step core
            ProjectFrame indx ->
              case val of
                ValStruct elems -> unwind (elems !! indx)
                _ -> error "Stuck: non-struct value passed to Project"
            CaseScrutineeFrame branches -> do
              let (tag, x) =
                    case val of
                      ValTag tag x -> (tag, x)
                      _ -> error $ "Stuck: non-tagged value passed to Case"
              let branch = fromMaybe (error $ "Stuck: expected branch " ++ show x ++ " to exist, but it does not") $ Seq.lookup tag branches
              -- We're basically evaluating the `case x of branches` to `branch x` but shortcircuting the evaluation we've already done
              modify (cur_frame %~ push (CaseBranchFrame x))
              step branch
            -- In theory we could reuse our function applications to do this logic
            -- But it would require swapping our eval order from fn -> arg, to arg -> fn
            CaseBranchFrame scrutinee -> call val scrutinee
            TagFrame tag -> unwind (ValTag tag val)

    call fn arg =
      case fn of
        ValLam (CoreV x _) body -> do
          modify (cur_env %~ Map.insert x arg)
          step body
        ValCont kont -> do
          -- Not sure this is correct
          -- Currently this removes the current stack frame
          -- We might want to save the current stack frame instead?
          modify (stack %~ pushStack kont)
          unwind arg
        val -> error $ "Stuck: non-function value applied as function " ++ show val



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

fromDone :: (Core -> Value) -> Res -> Value
fromDone toVal (Step core) = toVal core
fromDone _ (Done val) = val

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
