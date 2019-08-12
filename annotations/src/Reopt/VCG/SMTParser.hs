{-|

This module defines a parser for precondition expressions.  The format
is a subset of SMTLIB expressions with the QF_BV theory.
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Reopt.VCG.SMTParser
  ( Expr(..)
  , VarParser
  , evalExpr
  , ExprType(..)
  , fromText
  , exprToText
  , IsExprVar(..)
  , SExpr(..)
  , ppSExpr
  , encodeExpr
    -- * Rendering
  , SExprEncoding
  , encodeList
  , IsString(..)
  ) where

import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Bits
import           Data.Char
import           Data.List
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as Text
import qualified Data.Text.Read as Text
import           GHC.Natural

simpleSymbolStartChars :: String
simpleSymbolStartChars = "A-Za-z-~!@$%^&*_+=<>.?/-"

simpleSymbolChar :: String
simpleSymbolChar = "0-9" ++ simpleSymbolStartChars

-- | A well-formed (i.e. non-dotted) SExpression
--
-- N.B. We may want to add string literals, hex, and binary.
data SExpr
   = List ![SExpr]
   | Number !Natural
   | Atom !Text

ppSExpr :: SExpr -> ShowS
ppSExpr (Atom nm) = showString (Text.unpack nm)
ppSExpr (Number n) = shows n
ppSExpr (List []) = showString "()"
ppSExpr (List (h:r)) = showParen True (ppSExpr h . showL r)
  where showL :: [SExpr] -> ShowS
        showL [] = id
        showL (h:l) = showChar ' ' . ppSExpr h . showL l

-- | An atto-parsec parser.
sexprParser :: Parser SExpr
sexprParser = do
  skipSpace
  mconcat
    [ do char '('
         l <- many' sexprParser
         skipSpace *> char ')'
         pure (List l)
    , do t <- takeWhile1 (inClass simpleSymbolChar)
         case () of
           _ | Right (n,"") <- Text.decimal t -> do
                 pure $! Number n
             | not (inClass "0-9" (Text.head t)) -> do
                 pure (Atom t)
             | otherwise -> do
                 fail $ "Could not interpret " ++ Text.unpack t
    ]

readSExpr :: Text -> Either String SExpr
readSExpr = parseOnly sexprParser

------------------------------------------------------------------------
-- SExpression rendering

-- | Format for efficiently rendering an s-expression
newtype SExprEncoding = SExprEncoding { encBuilder :: Text.Builder }

instance IsString SExprEncoding where
  fromString = SExprEncoding . fromString

encodeList :: [SExprEncoding] -> SExprEncoding
encodeList [] = "()"
encodeList l = SExprEncoding ("(" <> foldr (\e r -> encBuilder e <> " " <> r) ")" l)

encodingToLazy :: SExprEncoding -> LText.Text
encodingToLazy = Text.toLazyText . encBuilder

------------------------------------------------------------------------
-- Expression

data ExprType
   = BVType !Natural
   | BoolType
 deriving (Eq)

-- | An expression in the SMT bitvector theory.
--
-- The type @A@ allows one to create holes for
-- variables or other known constants.
data Expr (v :: *) where
  Eq    :: !(Expr v) -> !(Expr v) -> Expr v
  BVSub :: !(Expr v) -> !(Expr v) -> Expr v
  BVDecimal :: !Natural -> !Natural -> Expr v
  -- ^ @BVDecimal v w@ denotes the @w@-bit value @v@ which should
  -- satisfy the property that @0 <= v < s^w@.
  Var :: !v -> Expr v
 deriving (Show)

-- | Denotes a type that can be read from expression atoms.
class IsExprVar v where
  encodeVar :: v -> SExprEncoding

type Evaluator a = Either String a

-- | A parser from S-expressions to foreign variables.
type VarParser v = SExpr -> Either String (v, ExprType)

-- | Parse an expression and generate an expression along with its
-- type.
evalExpr :: VarParser a -> SExpr -> Either String (Expr a, ExprType)
evalExpr parseVar (List [Atom "=", x,y]) = do
  (xe, xtp) <- evalExpr parseVar x
  (ye, ytp) <- evalExpr parseVar y
  when (xtp /= ytp) $ do
    Left $ ppSExpr x (" and " ++ ppSExpr y " should have the same type.")
  pure (Eq xe ye, BoolType)
evalExpr parseVar (List [Atom "bvsub", x, y]) = do
  (xe, xtp) <- evalExpr parseVar x
  (ye, ytp) <- evalExpr parseVar y
  case (xtp, ytp) of
    (BVType xw, BVType yw) | xw == yw -> do
       pure (BVSub xe ye, xtp)
    _ -> Left $ ppSExpr x (" and " ++ ppSExpr y " should be bitvectors with the same width.")
evalExpr parseVar (List [Atom "_", Atom t, Number w])
  | "bv" `Text.isPrefixOf` t
  , Right (n,"") <- Text.decimal (Text.drop 2 t) =
      pure (BVDecimal (n .&. (2^w - 1)) w, BVType w)
evalExpr parseVar t =
  fmap (\(v,tp) -> (Var v, tp)) $ parseVar t

fromText :: VarParser a -> Text -> Either String (Expr a)
fromText varParser t = do
  se <- readSExpr t
  (e,tp) <- evalExpr varParser se
  case tp of
    BoolType -> pure e
    _ -> Left $ "Expected " ++ ppSExpr se " to be a predicate."

-- | Render the expression into a builder
encodeExpr :: IsExprVar a => Expr a -> SExprEncoding
encodeExpr (Eq x y) = encodeList ["=", encodeExpr x, encodeExpr y]
encodeExpr (BVSub x y) = encodeList ["bvsub", encodeExpr x, encodeExpr y]
encodeExpr (BVDecimal v w) = encodeList ["_", fromString ("bv" <> show v), fromString (show w)]
encodeExpr (Var v) = encodeVar v

-- | Render the expression into a builder
exprToText :: IsExprVar a => Expr a -> Text
exprToText = LText.toStrict . encodingToLazy . encodeExpr
