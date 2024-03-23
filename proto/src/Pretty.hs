{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Pretty where

import Prettyprinter
import qualified Prettyprinter.Render.Terminal as Terminal
import Data.Text hiding (group)

data SyntaxHighlight
  = Keyword
  | Literal
  | TypeVariable
  | NamedVariable
  | DebugInfo

prettyRender :: Doc SyntaxHighlight -> Text
prettyRender = Terminal.renderStrict . reAnnotateS toAnsi . layoutSmart layoutOpts
  where
    layoutOpts = LayoutOptions (AvailablePerLine 100 1.0)

    toAnsi :: SyntaxHighlight -> Terminal.AnsiStyle
    toAnsi =
      \case
        Keyword -> Terminal.color Terminal.Red
        Literal -> Terminal.color Terminal.Cyan
        NamedVariable -> Terminal.color Terminal.Green <> Terminal.underlined
        TypeVariable -> Terminal.color Terminal.Magenta <> Terminal.italicized
        DebugInfo -> Terminal.color Terminal.Blue <> Terminal.bold
        
record :: [(Text, Doc ann)] -> Doc ann
record fields = align . group . encloseSep "{ " "}" ", " $ fmap prettyField fields

prettyField (field_name, field) = group (hang 4 (pretty field_name <+> "=" <> line <> field))

