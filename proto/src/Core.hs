{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Core where

import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import Pretty

import Control.Lens
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Set as Set
import GHC.Stack
import Prettyprinter
import qualified Prettyprinter as Pretty
import Subst
import Term
import Type
import Data.Bifunctor (Bifunctor(first))
import Data.Foldable
import Data.Text (Text, pack)
import Debug.Trace
import qualified Data.Map.Merge.Strict as Map

data Literal = I Int
  deriving (Show, Read)

instance Pretty Literal where
  pretty (I i) = pretty i

data LiteralTy = IntTy
  deriving (Show, Read, Eq, Ord)

instance Pretty LiteralTy where
  pretty Core.IntTy = "IntTy"

data Kind
  = KindType
  | KindRow
  deriving (Show, Read, Eq, Ord)

instance Pretty Kind where
  pretty KindType = "Type"
  pretty KindRow = "Row"

data CoreVar = CoreV Var CoreType
  deriving (Show, Read, Eq, Ord)


prettyVar :: CoreVar -> Doc SyntaxHighlight
prettyVar (CoreV v ty) = "(" <> prettyV v <+> ":" <+> prettyTy ty <> ")"

shortVar :: CoreVar -> Doc SyntaxHighlight
shortVar (CoreV v _) = prettyV v

coreVarTy :: Lens' CoreVar CoreType
coreVarTy =
  lens
    (\(CoreV _ ty) -> ty)
    (\(CoreV v _) ty -> CoreV v ty)

data TypeVar = CoreTV TVar Kind
  deriving (Show, Read, Eq, Ord)

instance Pretty TypeVar where
  pretty (CoreTV tvar kind) = viaShow tvar <+> "::" <+> pretty kind

coreTyTv tv = CoreTV tv KindType
coreRowTv tv = CoreTV tv KindRow

data CoreType
  = CoreVar TypeVar
  | CoreLit LiteralTy
  | CoreFun CoreType CoreType
  | CoreProduct [CoreType]
  | CoreCoproduct [CoreType]
  | CoreForall TypeVar CoreType
  deriving (Show, Read, Eq, Ord)

instance Pretty CoreType where
  pretty =
    \case
      CoreVar tyvar -> pretty tyvar
      CoreLit lit -> pretty lit
      CoreFun arg ret -> pretty arg <+> "->" <+> pretty ret
      CoreProduct tys -> "{" <+> hcat (punctuate ("," <> space) (pretty <$> tys)) <+> "}"
      CoreCoproduct tys -> "<" <+> hcat (punctuate ("," <> space) (pretty <$> tys)) <+> ">"
      CoreForall tyvar ty -> "∀" <+> pretty tyvar <+> "." <+> pretty ty

prettyTy :: CoreType -> Doc SyntaxHighlight
prettyTy = go
  where
    go = 
      \case
        CoreVar (CoreTV tyvar _) -> annotate TypeVariable (pretty tyvar)
        CoreLit lit -> annotate Literal (pretty lit)
        CoreFun arg ret -> go arg <+> "->" <+> go ret
        CoreProduct tys -> braces . hcat . punctuate ("," <> space) $ go <$> tys
        CoreCoproduct tys -> "<" <> hcat (punctuate ("," <> space) (go <$> tys)) <> ">"
        CoreForall tyvar ty -> "∀" <+> pretty tyvar <+> "." <+> go ty

instance Plated CoreType where
  plate f core_ty =
    case core_ty of
      CoreFun arg ret -> CoreFun <$> f arg <*> f ret
      CoreProduct tys -> CoreProduct <$> traverse f tys
      CoreCoproduct tys -> CoreCoproduct <$> traverse f tys
      CoreForall tv ty -> CoreForall tv <$> f ty
      ty -> pure ty

instance SubstApp CoreType where
  apply (Subst map) =
    transform
      ( \case
          core_ty@(CoreVar (CoreTV tv _)) -> fromMaybe core_ty (Map.lookup tv map >>= fromTypeSafe)
          core_ty -> core_ty
      )

fromType :: HasCallStack => Type -> CoreType
fromType = fromJust . fromTypeSafe

fromTypeSafe :: Type -> Maybe CoreType
fromTypeSafe ty =
  case ty of
    VarTy tv -> Just $ CoreVar (coreTyTv tv)
    Type.IntTy -> Just $ CoreLit Core.IntTy
    RowTy row
      -- If this is a map of length 1, unwrap it
      | Map.size row == 1 -> fromTypeSafe . head $ Map.elems row
      | otherwise -> Nothing
    ProdTy (RowTy row) -> Just $ CoreProduct (fromType <$> Map.elems row)
    ProdTy (VarTy tv) -> Just $ CoreVar (coreRowTv tv)
    ProdTy _ -> Nothing 
    SumTy (RowTy row) -> Just $ CoreCoproduct (fromType . snd <$> Map.toAscList row)
    SumTy (VarTy tv) -> Just $ CoreVar (coreRowTv tv)
    SumTy _ -> Nothing
    FunTy arg _ ret -> Just $ CoreFun (fromType arg) (fromType ret)

toType :: CoreType -> Type
toType ty = 
  case ty of
    CoreVar (CoreTV tv KindType) -> VarTy tv
    CoreVar (CoreTV _ KindRow) -> error "idk what to do this yet"
    CoreLit Core.IntTy -> Type.IntTy
    CoreFun arg ret -> FunTy (toType arg) (Closed Map.empty) (toType ret)
    -- We make up names for our row type that will put them in the same order when we go back to  core
    CoreProduct elems -> ProdTy (RowTy (Map.fromList $ zip (pack . show <$> [0..]) (toType <$> elems)))
    CoreCoproduct elems -> SumTy (RowTy (Map.fromList $ zip (pack .show <$> [0..]) (toType <$> elems)))
    CoreForall _ _ -> error "idk what to do  here"

-- We do not want to allow arbitrary computation within primitives
-- We'll wrap primitives in a helper function that will handle evaluating expressions to values before constructing a primitive out of them
-- So primitives should take variables (which eval to values at runtime), or literals if we can constant fold a call to the helper function
data PrimArg
  = PrimVar CoreVar
  | PrimVal Literal
  deriving (Show, Read)

toCore :: PrimArg -> Core
toCore (PrimVar var) = Core.Var var
toCore (PrimVal lit) = Lit lit

data BinOp
  = IntAdd
  | IntLt
  deriving (Show, Read)

data PrimOp
  = BinOp BinOp PrimArg PrimArg
  deriving (Show, Read)

prettyPrimitive :: PrimOp -> Doc SyntaxHighlight
prettyPrimitive prim = 
  case prim of
    BinOp op arg_left arg_right -> prettyPrimArg arg_left <+> prettyBinOp op <+> prettyPrimArg arg_right

prettyBinOp :: BinOp -> Doc SyntaxHighlight
prettyBinOp IntAdd = annotate Keyword "+"
prettyBinOp IntLt = annotate Keyword "<"

prettyPrimArg :: PrimArg -> Doc SyntaxHighlight
prettyPrimArg (PrimVar var) = shortVar var
prettyPrimArg (PrimVal lit) = annotate Literal (pretty lit)

data Core
  = Var CoreVar
  | Lit Literal
  | Lam CoreVar Core
  | App Core Core
  | TyLam TypeVar Core
  | TyApp Core CoreType
  | Product [Core]
  | Project Int Core
  | Coproduct CoreType Int Core
  | Case {- scrutinee -} Core {- branches -}(Seq Core)
  | NewPrompt CoreVar Core
  | Prompt { prompt_marker :: Core, prompt_body :: Core }
  | Yield { yield_marker :: Core, yield_value :: Core }
  | Primitive PrimOp
  deriving (Show, Read)

instance Plated Core where
  plate f core =
    case core of
      Lam x body -> Lam x <$> f body
      NewPrompt x body -> NewPrompt x <$> f body
      Core.App fn arg -> Core.App <$> f fn <*> f arg
      TyLam x body -> TyLam x <$> f body
      TyApp forall ty -> TyApp <$> f forall <*> pure ty
      Product elems -> Product <$> traverse f elems
      Project idx product -> Project idx <$> f product
      Case scrutinee branches -> Case <$> f scrutinee <*> traverse f branches
      Coproduct ty tag elem -> Coproduct ty tag <$> f elem
      Prompt marker body -> Prompt <$> f marker <*> f body
      Yield marker value -> Yield <$> f marker <*> f value
      Core.Var var -> pure (Core.Var var)
      Lit lit -> pure (Lit lit)
      Primitive op -> pure (Primitive op)

-- Global core specific variables that users won't see
evv = V (-1)
marker = V (-2)
add = V (-9)
lt = V (-10)

isSingleDoc :: Core -> Bool
-- Var technically isn't a single doc but we treat it as such because parens are handled by var printing logic
isSingleDoc (Core.Var _) = True
isSingleDoc (Lit _) = True
isSingleDoc (Product elems) 
  | Prelude.null elems = True
--isSingleDoc NewPrompt = True
isSingleDoc (Project _ _) = True
isSingleDoc _ = False

data Typed = Typed | Typeless 

prettyCore = prettyCore' Typed
prettyCoreUntyped = prettyCore' Typeless

prettyCore' :: Typed -> Core -> Doc SyntaxHighlight
prettyCore' typed = go
  where
    go core = 
      case core of
        Core.Var var -> shortVar var -- shortVar var 
        Lit lit -> annotate Literal (pretty lit)
        Lam x (Core.Var y) | x == y -> annotate Keyword "id"
        Lam x core -> annotate Keyword "fun" <+> align (group (lamVar x <> line <> go core))
        Core.App (Lam x lets) defn ->
          let (defns_tail, body) = collectLets lets
              defns = (x, defn) : defns_tail
           in align (annotate Keyword "let" <+> (align (vcat $ punctuate line (prettyDefn <$> defns)) <> line <> group (annotate Keyword " in" <+> go body)))
        Core.App fn arg -> 
          let (head, spine) = collectSpine [arg] fn
           in parens head <> align (group (line <> vsep (parens <$> spine)))
        TyLam x core -> annotate Keyword "ty_fun" <+> align (group (pretty x <> line <> go core))
        TyApp forall ty -> Pretty.parens (go forall) <> "⌈" <> pretty ty <> "⌉"
        Product elems
          | Prelude.null elems -> annotate Literal "Unit"
          | otherwise -> "Product" <+> align (list (go <$> elems))
        Project indx core -> go core <> "[" <> pretty indx <> "]"
        Case scrutinee branches -> 
          "Case" <+> go scrutinee <+> align (list (toList $ go <$> branches))
        Coproduct _ tag core -> "Coproduct" <+> annotate Literal (pretty tag) <+> go core
        NewPrompt x body -> align (annotate Keyword "let" <+> group (prettyVar x <+> "=" <+> annotate Literal "NewPrompt") <> line <> group (annotate Keyword " in" <+> go body))
        Prompt marker body -> "Prompt" <+> Pretty.record [("prompt_marker", go marker), ("prompt_body", go body)]
        Yield marker value -> "Yield" <+> Pretty.record [("yield_marker", go marker), ("yield_value", go value)]
        Primitive op -> prettyPrimitive op

    prettyDefn (x, defn) = group (align (lamVar x <+> "=" <> nest 4 (line <> go defn)))

    parens core = 
      if isSingleDoc core
         then go core
         else Pretty.parens (go core)

    collectLets (Core.App (Lam x body) defn) = first ((x, defn) :) (collectLets body)
    collectLets core = ([], core)

    collectSpine spine (Core.App fn arg) = collectSpine (arg:spine) fn
    collectSpine spine head = (head, spine)

    lamVar = case typed of { Typed -> prettyVar; Typeless -> shortVar } 
  

prettyRender :: Core -> Text
prettyRender = Pretty.prettyRender . prettyCore

coreVars :: Traversal' Core CoreVar
coreVars f core =
  case core of
    Core.Var var -> Core.Var <$> f var
    Lam x body -> Lam <$> f x <*> pure body
    core -> pure core

coreBoundVars :: Traversal' Core CoreVar
coreBoundVars f core =
  case core of
    Lam x body -> Lam <$> f x <*> pure body
    c -> pure c

coreUnboundVars :: Core -> Set.Set CoreVar
coreUnboundVars core = Set.fromList . Map.elems $ Map.withoutKeys allVars boundVars
 where
  allVars = foldrOf (cosmos . coreVars) (\core_var@(CoreV v _) -> Map.insert v core_var) Map.empty core
  boundVars = Set.union (Set.fromList [Core.evv, Core.marker, Core.add, Core.lt]) $ foldrOf (cosmos . coreBoundVars) (\(CoreV v _) -> Set.insert v) Set.empty core

rowEvType :: InternalRow -> InternalRow -> InternalRow -> CoreType
rowEvType left right goal =
  CoreProduct
    [ CoreFun leftProdTy (CoreFun rightProdTy goalProdTy)
    , let a = CoreTV (TV (-1)) KindType
          leftBranch = CoreFun leftSumTy (CoreVar a)
          rightBranch = CoreFun rightSumTy (CoreVar a)
          goalBranch = CoreFun goalSumTy (CoreVar a)
       in CoreForall a $ CoreFun leftBranch (CoreFun rightBranch goalBranch)
    , CoreProduct
        [ CoreFun goalProdTy leftProdTy
        , CoreFun leftSumTy goalSumTy
        ]
    , CoreProduct
        [ CoreFun goalProdTy rightProdTy
        , CoreFun rightSumTy goalSumTy
        ]
    ]
 where
  leftProdTy = fromType (ProdTy (rowToType left))
  rightProdTy = fromType (ProdTy (rowToType right))
  goalProdTy = fromType (ProdTy (rowToType goal))

  leftSumTy = fromType (SumTy (rowToType left)) 
  rightSumTy = fromType (SumTy (rowToType right))
  goalSumTy = fromType (SumTy (rowToType goal))


rowEvidence :: HasCallStack => Row -> Row -> Row -> Core
rowEvidence left right goal = 
  Product 
    [ Core.concat left right
    , branch left right
    , Product [prjL left goal, injL left goal]
    , Product [prjR right goal, injR right goal]
    ]

prjR :: Row -> Row -> Core
prjR outRow inRow = Lam inVar out
 where
  inSize = Map.size inRow - 1
  outSize = Map.size outRow - 1
  out =
    case [(inSize - outSize) .. inSize] of
      -- Special case for singleton
      [_] -> indx' inSize (Core.var inVar)
      idxs -> Product (fmap (\i -> indx' i (Core.var inVar)) idxs)
  inVar = CoreV (V (-6)) inTy
  indx' = indx inRow
  inTy = fromType (unwrapSingleton inRow)

prjL :: Row -> Row -> Core
prjL outRow inRow = Lam inVar out
 where
  out =
    case [0 .. (Map.size outRow - 1)] of
      [_] -> indx' 0 (Core.var inVar)
      idxs -> Product (fmap (\i -> indx' i (Core.var inVar)) idxs)
  indx' = indx inRow
  inVar = CoreV (V (-5)) inTy
  inTy = fromType (unwrapSingleton inRow)

concat :: Row -> Row -> Core
concat left right = Lam m $ Lam n prod
 where
  leftTy = fromType (unwrapSingleton left)
  rightTy = fromType (unwrapSingleton right)
  m = CoreV (V (-5)) leftTy
  n = CoreV (V (-6)) rightTy
  prod = if length splat == 1 then head splat else Product splat
  splat = Map.elems (calcOutputMap left right) --fmap (\i -> indx left i (Core.var m)) [0 .. (Map.size left - 1)] ++ fmap (\i -> indx right i (Core.var n)) [0 .. (Map.size right - 1)]
  calcOutputMap left right = 
    Map.merge 
      (Map.mapMissing (\_ (i, _) -> indx left i (Core.var m))) 
      (Map.mapMissing (\_ (i, _) -> indx right i (Core.var n)))
      -- Priortise right key when overlap occurs
      (Map.zipWithMatched (\_ _ (right_i, _) -> indx right right_i (Core.var n)))
      (enumerateMap left) (enumerateMap right)
  
enumerateMap :: Map.Map k a -> Map.Map k (Int, a)
enumerateMap = snd . Map.mapAccum (\indx val -> (indx + 1, (indx, val))) 0

injL :: HasCallStack => Row -> Row -> Core
injL inRow outRow = Lam m out
  where
    m = CoreV (V (-5)) left_ty
    left_ty@(CoreCoproduct tys) = fromType (SumTy (RowTy inRow))
    goal_ty = fromType (SumTy (RowTy outRow))
    out = 
      case [0..(Map.size inRow - 1)] of
        [_] -> inj goal_ty outRow 0 (Core.Var m)
        indxs -> Case (Core.Var m) (Seq.fromList (injCase (inj goal_ty outRow) (zip indxs tys) <$> indxs))--(Seq.fromList (branch_case <$> indxs))

injR :: HasCallStack => Row -> Row -> Core
injR inRow outRow = Lam n out
  where
    n = CoreV (V (-6)) right_ty
    inSize = max (Map.size inRow - 1) 0
    outSize = max (Map.size outRow - 1) 0
    right_ty@(CoreCoproduct tys) = fromType (SumTy (RowTy inRow))
    goal_ty = fromType (SumTy (RowTy outRow))
    out =
      case [(outSize - inSize)..outSize] of
        [_] -> inj goal_ty outRow outSize (Core.Var n)
        indxs -> Case (Core.Var n) (Seq.fromList $ injCase (inj goal_ty outRow) (zip indxs tys) <$> indxs)

branch :: HasCallStack => Row -> Row -> Core
branch left right = TyLam a $ Lam f $ Lam g coprod
  where
    f = CoreV (V $ -5) (CoreFun left_ty (CoreVar a))
    left_ty@(CoreCoproduct left_tys) = fromType (SumTy (RowTy left))
    g = CoreV (V $ -6) (CoreFun right_ty (CoreVar a))
    right_ty@(CoreCoproduct right_tys) = fromType (SumTy (RowTy right))
    goal_tys = left_tys ++ right_tys
    goal_ty = CoreCoproduct goal_tys
    a = CoreTV (TV $ -1) KindType
    left_size = Map.size left - 1
    right_size = Map.size right - 1
    coprod = let z = CoreV (V $ -7) (CoreCoproduct goal_tys)
              in Lam z $ Case (Core.var z) (Seq.fromList (left_case <$> [0..left_size]) >< Seq.fromList (right_case <$> [0..right_size]))
    left_case i = Lam (branchVar (zip [0..] left_tys) i) $ Core.App (Core.Var f) (inj goal_ty left i (Core.Var $ branchVar (zip [0..] left_tys) i))
    right_case i = Lam (branchVar (zip [0..] right_tys) i) $ Core.App (Core.Var g) (inj goal_ty right i (Core.Var $ branchVar (zip [0..] right_tys) i))

injCase :: HasCallStack => (Int -> Core -> Core) -> [(Int, CoreType)] -> Int -> Core
injCase inj tys i = Lam (branchVar tys i) (inj i (Core.Var $ branchVar tys i))

branchVar :: HasCallStack => [(Int, CoreType)] -> Int -> CoreVar
branchVar tys i = CoreV (V (-8)) branch_ty
  where
    branch_ty = fromMaybe (error $ "Index " ++ show i ++ " not found in " ++ show tys) (Prelude.lookup i tys)

inj ty row = 
  case Map.size row of
    1 -> \_ ty -> ty
    _ -> Coproduct ty

indx row =
  case Map.size row of
    1 -> \_ ty -> ty
    _ -> Project

unwrapSingleton row =
  case Map.size row of
    1 -> head $ Map.elems row
    _ -> ProdTy (RowTy row)

varSubst :: Map.Map Var Core.Core -> Core.Core -> Core.Core
varSubst env =
  transform
    ( \case
        core@(Core.Var (CoreV v _)) -> fromMaybe core $ env !? v
        core -> core
    )

tySubst :: Map.Map TVar CoreType -> Core -> Core
tySubst env =
  transform 
    (\case
      Core.Var (CoreV x ty) -> Core.Var (CoreV x (sub ty))
      TyApp forall ty -> TyApp forall (sub ty)
      core -> core)
  where
    sub ty = 
      case ty of 
        CoreVar (CoreTV tv _) -> fromMaybe ty (env !? tv)
        ty -> ty

var = Core.Var

lam args body = foldr Core.Lam body args

local var defn body = Core.App (Core.Lam var body) defn

zonk :: Subst -> Core.Core -> Core.Core
zonk subst =
  transform
    (\case
      Core.Lam (CoreV x ty) body -> Core.Lam (CoreV x (apply subst ty)) body
      Core.Var (CoreV x ty) -> Core.Var (CoreV x (apply subst ty))
      TyApp forall ty -> TyApp forall (apply subst ty)
      core -> core)

simplify :: Core -> Core
simplify = transform
  (\case
    TyApp (TyLam (CoreTV x _) body) ty -> simplify $ tySubst (Map.singleton x ty) body
    -- Simplify id x => x
    Core.App (Lam x (Core.Var y)) arg 
     | x == y -> simplify arg
    Core.App (Lam _ (Product [])) _ -> Product [] -- Unit cannot be simplified further
    Core.App (Lam (CoreV x _) body) arg 
     | isValue arg -> simplify $ varSubst (Map.singleton x (simplify arg)) (simplify body)
    Project idx (Product elems) ->
      if idx < length elems
        then simplify $ elems !! idx
        else error $ show elems ++ " does not contain index " ++ show idx
    core -> core)
  where
    isValue (Core.Var _) = True
    isValue (Lit _) = True
    isValue (Lam _ _) = True
    isValue (Product elems) = all isValue elems
    isValue (Coproduct _ _ elem) = isValue elem
    isValue _ = False
