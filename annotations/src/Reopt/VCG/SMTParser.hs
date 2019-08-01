{-|

This module defines a parser for precondition expressions.  The format
is a subset of SMTLIB expressions with the QF_BV theory.
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Reopt.VCG.SMTParser
  ( Expr(..)
  , evalExpr
  , ExprType(..)
  , readPred
  , IsExprVar(..)
  , SExpr(..)
  , ppSExpr
  ) where

import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Bits
import           Data.Char
import           Data.List
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
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
  Var :: !v -> Expr v
 deriving (Show)

-- | Denotes a type that can be read from expression atoms.
class IsExprVar v where
  fromExpr :: SExpr -> Either String (Expr v, ExprType)


type Evaluator a = Either String a

-- | Parse an expression and generate an expression along with its
-- type.
evalExpr :: IsExprVar a => SExpr -> Either String (Expr a, ExprType)
evalExpr (List [Atom "=", x,y]) = do
  (xe, xtp) <- evalExpr x
  (ye, ytp) <- evalExpr y
  when (xtp /= ytp) $ do
    Left $ ppSExpr x (" and " ++ ppSExpr y " should have the same type.")
  pure (Eq xe ye, BoolType)
evalExpr (List [Atom "bvsub", x, y]) = do
  (xe, xtp) <- evalExpr x
  (ye, ytp) <- evalExpr y
  case (xtp, ytp) of
    (BVType xw, BVType yw) | xw == yw -> do
       pure (BVSub xe ye, xtp)
    _ -> Left $ ppSExpr x (" and " ++ ppSExpr y " should be bitvectors with the same width.")
evalExpr (List [Atom "_", Atom t, Number w])
  | "bv" `Text.isPrefixOf` t
  , Right (n,"") <- Text.decimal (Text.drop 2 t) =
      pure (BVDecimal (n .&. (2^w - 1)) w, BVType w)
evalExpr t =
  fromExpr t

readPred :: IsExprVar a => Text -> Either String (Expr a)
readPred t = do
  se <- readSExpr t
  (e,tp) <- evalExpr se
  case tp of
    BoolType -> pure e
    _ -> Left $ "Expected " ++ ppSExpr se " to be a predicate."
