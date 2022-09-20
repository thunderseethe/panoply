{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Core where

import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map

import Control.Lens
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import GHC.Stack
import Prettyprinter
import qualified Prettyprinter as Pretty
import qualified Prettyprinter.Render.Terminal as Terminal
import Subst
import Term
import Type
import Data.Text (Text, pack)
import Data.Bifunctor (Bifunctor(first))

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
prettyVar cv@(CoreV _ ty) = "(" <> shortVar cv <+> ":" <+> prettyTy ty <> ")"

shortVar :: CoreVar -> Doc SyntaxHighlight
shortVar (CoreV v _) = maybe (Pretty.parens $ pretty v) (annotate NamedVariable . pretty) (namedVar v)

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
  | CoreFun CoreType InternalRow CoreType
  | CoreProduct [CoreType]
  | CoreForall TypeVar CoreType
  deriving (Show, Read, Eq, Ord)

instance Pretty CoreType where
  pretty =
    \case
      CoreVar tyvar -> pretty tyvar
      CoreLit lit -> pretty lit
      CoreFun arg (Closed eff) ret
        | Map.null eff -> pretty arg <+> "->" <+> pretty ret
      CoreFun arg eff ret -> pretty arg <+> "-{" <> pretty eff <> "}->" <+> pretty ret
      CoreProduct tys -> "{" <+> hcat (punctuate ("," <> space) (pretty <$> tys)) <+> "}"
      CoreForall tyvar ty -> "∀" <+> pretty tyvar <+> "." <+> pretty ty

prettyTy :: CoreType -> Doc SyntaxHighlight
prettyTy = go
  where
    go = 
      \case
        CoreVar (CoreTV tyvar _) -> annotate TypeVariable (pretty tyvar)
        CoreLit lit -> annotate Literal (pretty lit)
        CoreFun arg (Closed eff) ret
          -- If function is total don't print effect
          | Map.null eff -> go arg <+> "->" <+> go ret
        CoreFun arg (Open eff) ret -> go arg <+> "-{" <> pretty eff <> "}->" <+> go ret
        CoreFun arg (Closed row) ret -> go arg <+> "-{" <> hsep (punctuate comma (pretty <$> Map.keys row)) <> "}->" <+> go ret
        CoreProduct tys -> "{" <> hcat (punctuate ("," <> space) (go <$> tys)) <> "}"
        CoreForall tyvar ty -> "∀" <+> pretty tyvar <+> "." <+> go ty

instance Plated CoreType where
  plate f core_ty =
    case core_ty of
      CoreFun arg eff ret -> CoreFun <$> f arg <*> pure eff <*> f ret
      CoreProduct tys -> CoreProduct <$> traverse f tys
      CoreForall tv typ -> CoreForall tv <$> f typ
      ty -> pure ty

instance SubstApp CoreType where
  apply (Subst map) =
    transform
      ( \case
          core_ty@(CoreVar (CoreTV tv _)) -> fromMaybe core_ty (Map.lookup tv map >>= fromTypeSafe)
          core_ty -> core_ty
      )

fromType :: HasCallStack => Type -> CoreType
fromType ty =
  case ty of
    VarTy tv -> CoreVar (coreTyTv tv)
    Type.IntTy -> CoreLit Core.IntTy
    RowTy _ -> error "Can't translate this as is"
    ProdTy (RowTy row) -> CoreProduct (fromType <$> Map.elems row)
    ProdTy (VarTy tv) -> CoreVar (coreRowTv tv)
    ProdTy _ -> error "Invalid product type"
    FunTy arg eff ret -> CoreFun (fromType arg) eff (fromType ret)

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
    ProdTy _ -> error "Invalid product type"
    FunTy arg eff ret -> Just $ CoreFun (fromType arg) eff (fromType ret)

toType :: CoreType -> Type
toType ty = 
  case ty of
    CoreVar (CoreTV tv KindType) -> VarTy tv
    CoreVar (CoreTV _ KindRow) -> error "idk what to do this yet"
    CoreLit Core.IntTy -> Type.IntTy
    CoreFun arg eff ret -> FunTy (toType arg) eff (toType ret)
    -- We make up names for our row type that will put them in the same order when we go back to  core
    CoreProduct elems -> ProdTy (RowTy (Map.fromList $ zip (pack . show <$> [0..]) (toType <$> elems)))
    CoreForall _ _ -> error "idk what to do  here"


data Core
  = Var CoreVar
  | Lit Literal
  | Lam CoreVar Core
  | App Core Core
  | TyLam TypeVar Core
  | TyApp Core CoreType
  | Product [Core]
  | Project Int Core
  | NewPrompt CoreVar Core
  | Prompt { prompt_marker :: Core, prompt_handler :: Core, prompt_body :: Core  }
  | Yield { yield_marker :: Core, yield_value :: Core }
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
      Prompt marker handler body -> Prompt <$> f marker <*> f handler <*> f body
      Yield marker value -> Yield <$> f marker <*> f value
      core -> pure core

-- Global core specific variables that users won't see
evv = V (-1)
marker = V (-2)

namedVar :: Var -> Maybe Text
namedVar (V (-1)) = Just "evv"
namedVar (V (-2)) = Just "marker"
namedVar (V (-3)) = Just "op_arg"
namedVar (V (-4)) = Just "k"
namedVar _ = Nothing


data SyntaxHighlight
  = Keyword
  | Literal
  | TypeVariable
  | NamedVariable

isSingleDoc :: Core -> Bool
-- Var technically isn't a single doc but we treat it as such because parens are handled by var printing logic
isSingleDoc (Core.Var _) = True
isSingleDoc (Lit _) = True
isSingleDoc (Product elems) 
  | Prelude.null elems = True
--isSingleDoc NewPrompt = True
isSingleDoc (Project _ _) = True
isSingleDoc _ = False

prettyCore :: Core -> Doc SyntaxHighlight
prettyCore = go
  where
    go core = 
      case core of
        Core.Var var -> shortVar var -- shortVar var 
        Lit lit -> annotate Literal (pretty lit)
        Lam x core -> annotate Keyword "fun" <+> align (group (prettyVar x <> line <> go core))
        Core.App (Lam x lets) defn ->
          let (defns_tail, body) = collectLets lets
              defns = (x, defn) : defns_tail
           in align (annotate Keyword "let" <+> (align (vcat $ punctuate line (prettyDefn <$> defns)) <> line <> group (annotate Keyword " in" <+> go body)))
        Core.App fn arg -> 
          let (head, spine) = collectSpine [arg] fn
           in parens head <> align (group (line <> vsep (parens <$> spine)))
           --"App" <> align (group (line <> parens fn <> line <> parens arg))
        TyLam x core -> annotate Keyword "ty_fun" <+> align (group (pretty x <> line <> go core))
        TyApp forall ty -> "TyApp" <+> go forall <+> pretty ty
        Product elems
          | Prelude.null elems -> annotate Literal "Unit"
          | otherwise -> "Product" <+> align (list (go <$> elems))
        Project indx core -> go core <> "[" <> pretty indx <> "]"
        NewPrompt x body -> align (annotate Keyword "let" <+> group (prettyVar x <+> "=" <+> annotate Literal "NewPrompt") <> line <> group (annotate Keyword " in" <+> go body))
        Prompt marker handler body -> "Prompt" <+> record [("prompt_marker", go marker), ("prompt_handler", go handler), ("prompt_body", go body)]
        Yield marker value -> "Yield" <+> record [("yield_marker", go marker), ("yield_value", go value)]

    record :: [(Text, Doc ann)] -> Doc ann
    record fields = align . group . encloseSep "{ " "}" ", " $ fmap prettyField fields

    prettyField (field_name, field) = group (hang 4 (pretty field_name <+> "=" <> line <> field))

    prettyDefn (x, defn) = group (align (prettyVar x <+> "=" <> line <> go defn))

    parens core = 
      if isSingleDoc core
         then go core
         else Pretty.parens (go core)

    collectLets (Core.App (Lam x body) defn) = first ((x, defn) :) (collectLets body)
    collectLets core = ([], core)

    collectSpine spine (Core.App fn arg) = collectSpine (arg:spine) fn
    collectSpine spine head = (head, spine)
  

prettyRender :: Core -> Text
prettyRender = Terminal.renderStrict . reAnnotateS toAnsi . layoutSmart layoutOpts . prettyCore
  where
    layoutOpts = LayoutOptions (AvailablePerLine 100 1.0)

    toAnsi :: SyntaxHighlight -> Terminal.AnsiStyle
    toAnsi =
      \case
        Keyword -> Terminal.color Terminal.Red
        Literal -> Terminal.color Terminal.Cyan
        NamedVariable -> Terminal.color Terminal.Green <> Terminal.underlined
        TypeVariable -> Terminal.color Terminal.Magenta <> Terminal.italicized

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
coreUnboundVars core = allVars `Set.difference` boundVars
 where
  allVars = foldrOf (cosmos . coreVars) Set.insert Set.empty core
  boundVars = foldrOf (cosmos . coreBoundVars) Set.insert Set.empty core

rowEvType :: InternalRow -> InternalRow -> InternalRow -> CoreType
rowEvType left right goal =
  CoreProduct
    [ CoreFun leftTy (Closed Map.empty) (CoreFun rightTy (Closed Map.empty) goalTy)
    , CoreProduct []
    , CoreProduct
        [ CoreFun goalTy (Closed Map.empty) leftTy
        , CoreProduct []
        ]
    , CoreProduct
        [ CoreFun goalTy (Closed Map.empty) rightTy
        , CoreProduct []
        ]
    ]
 where
  leftTy = fromType (ProdTy (rowToType left))
  rightTy = fromType (ProdTy (rowToType right))
  goalTy = fromType (ProdTy (rowToType goal))

rowEvidence :: Row -> Row -> Row -> Core
rowEvidence left right goal = Product [Core.concat left right, placeholder, Product [prjL left goal, placeholder], Product [prjR right goal, placeholder]]
 where
  -- Use unit for the sum type stuff so we still produce evidence with the right structure
  placeholder = Product []

prjR outRow inRow = Lam inVar out
 where
  inSize = Map.size inRow - 1
  outSize = Map.size outRow - 1
  out =
    case [(inSize - outSize) .. inSize] of
      -- Special case for singleton
      [_] -> indx' 0 (Core.var inVar)
      idxs -> Product (fmap (\i -> indx' i (Core.var inVar)) idxs)
  inVar = CoreV (V 0) inTy
  indx' = indx inRow
  inTy = fromType (unwrapSingleton inRow)

prjL outRow inRow = Lam inVar out
 where
  out =
    case [0 .. (Map.size outRow - 1)] of
      [_] -> indx' 0 (Core.var inVar)
      idxs -> Product (fmap (\i -> indx' i (Core.var inVar)) idxs)
  indx' = indx inRow
  inVar = CoreV (V 0) inTy
  inTy = fromType (ProdTy (RowTy inRow))

concat :: Row -> Row -> Core
concat left right = Lam m $ Lam n prod
 where
  m = CoreV (V 0) leftTy
  n = CoreV (V 1) rightTy
  prod = if length splat == 1 then head splat else Product splat
  splat = fmap (\i -> indx left i (Core.var m)) [0 .. (Map.size left - 1)] ++ fmap (\i -> indx right i (Core.var n)) [0 .. (Map.size right - 1)]
  leftTy = fromType (unwrapSingleton left)
  rightTy = fromType (unwrapSingleton right)

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

var = Core.Var

lam args body = foldr Core.Lam body args

simplify :: Core -> Core
simplify = transform
  (\case
    --Core.App (Lam (CoreV x _) body) arg -> simplify $ varSubst (Map.singleton x arg) body
    Project idx (Product elems) ->
      if idx < length elems
        then simplify $ elems !! idx
        else error $ show elems ++ " does not contain index " ++ show idx
    core -> core)
