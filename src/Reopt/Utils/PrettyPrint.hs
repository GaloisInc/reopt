module Reopt.Utils.PrettyPrint
  ( bracketsep
  , parenIf
  ) where

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

bracketsep :: [Doc] -> Doc
bracketsep [] = text "{}"
bracketsep (h:l) = vcat $
  [text "{" <+> h]
  ++ fmap (text "," <+>) l
  ++ [text "}"]

parenIf :: Bool -> Doc -> Doc
parenIf True d = parens d
parenIf False d = d
