{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -w #-}
module Reopt.BasicBlock.FunctionCFG
       ( CFG
       , Block(..)
       , Term(..), findBlocks, findBlock,
                                     termChildren, FunBounds(..), inFunBounds)
where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Word
import           Numeric
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Data.Macaw.Memory
import qualified Data.Macaw.Memory.Permissions as Perm
import           Data.Macaw.X86.Flexdis
import qualified Data.Macaw.X86.X86Reg as N

import           Reopt.BasicBlock.Extract
import           Reopt.Concrete.Semantics as CS

type CFG = Map Word64 Block

data FunBounds = FunBounds { funBase  :: Word64
                           , funSize  :: Word64
                           }

instance Pretty FunBounds where
  pretty fb = braces (text (showHex (funBase fb) "")
                      <+> text ".."
                      <+> text (showHex (funBase fb + funSize fb ) ""))

inFunBounds :: FunBounds -> Word64 -> Bool
inFunBounds fb addr = funBase fb <= addr
                      && fromIntegral addr < upper
  where
    upper :: Integer -- in case of wrap around, super unlikely though
    upper = fromIntegral (funBase fb) + fromIntegral (funSize fb)

data Block = Block { blockStmts :: [Stmt]
                   , blockTerm :: Term
                   }-- deriving (Eq, Ord)

data Term = Cond Word64 Word64 -- true, false
          | Call Word64 Word64 -- call, ret
          | Direct Word64
          | Fallthrough Word64
          | Indirect
          | Ret deriving (Eq, Ord, Show)

ppHex :: Word64 -> Doc
ppHex v = text (showHex v "")

instance Pretty Term where
  pretty t = case t of
    Cond t f -> text "Cond" <+> ppHex t <+> ppHex f
    Call f r -> text "Call" <+> ppHex f <+> ppHex r
    Direct p -> text "Direct" <+> ppHex p
    Fallthrough p -> text "Fallthrough" <+> ppHex p
    Indirect     -> text "Indirect"
    Ret         -> text "Ret"

instance Pretty Block where
  pretty (Block stmts term) = vcat [ppStmts stmts,  pretty term]

termChildren :: Term -> [Word64]
termChildren (Cond w1 w2) = [w1, w2]
termChildren (Call _ w) = [w]
termChildren (Direct w) = [w]
termChildren (Fallthrough w) = [w]
termChildren Indirect = []
termChildren Ret = []

-- FIXME: clag
findBlock :: Memory 64 -> Word64 -> Either String CFG
findBlock mem entry = do
  let Just addr = absoluteAddrSegment mem (fromIntegral entry)
  M.singleton entry <$>
   case runMemoryByteReader Perm.read mem addr $ extractBlock entry S.empty of
    Left err -> Left $ "runMemoryByteReader " ++ showHex entry " " ++ show err
    Right (Left err, _) | err >= 0 -> Left $ "Could not disassemble instruction at 0x" ++ showHex err ""
    Right (Right ([Absolute next], _, stmts), _) ->
      Right (Block stmts $ Direct next)
    Right (Right ([NFallthrough next], _, stmts), _) ->
      Right (Block stmts $ Fallthrough next)
    Right (Right ([Absolute branch, Absolute fallthrough], _, stmts), _) ->
      Right (Block stmts $ Cond branch fallthrough)
    Right (Right ([NIndirect], _, stmts), _) ->
      Right (Block stmts $ Indirect)

-- FIXME: fancy data structures could do this better - keep instruction
-- beginnings around, use a range map...
findBlocks :: Memory 64 -> FunBounds -> Set Word64 -> Word64 -> Either String CFG
findBlocks mem funBounds breaks entry = calcFixpoint M.empty
  where
    calcFixpoint cfg = do
      cfg' <- findBlocks' mem funBounds (breaks `S.union` M.keysSet cfg) (S.singleton entry) cfg
      if (M.keysSet cfg' /= M.keysSet cfg)
        then calcFixpoint cfg'
        else return cfg'

findBlocks' :: Memory 64
            -> FunBounds
            -> Set Word64
            -> Set Word64
            -> CFG
            -> Either String CFG
findBlocks' mem funBounds breaks queue cfg
  | Just (entry, queue') <- S.minView queue =
    let finishBlock :: [Stmt] -> Term -> [Word64] -> Either String CFG
        finishBlock stmts term nexts =
          let cfg'   = M.insert entry (Block stmts term) cfg
              addQ q next
                | inFunBounds funBounds next
                  && not (M.member next cfg) = S.insert next q
                | otherwise = q
              queue'' = foldl addQ queue' nexts
          in findBlocks' mem funBounds (breaks `S.union` queue'' `S.union` M.keysSet cfg') queue'' cfg' -- FIXME: performance
        Just entryAddr = absoluteAddrSegment mem (fromIntegral entry)
    in
    case runMemoryByteReader Perm.execute mem entryAddr $ extractBlock entry breaks of
      Left err -> Left $ "runMemoryByteReader " ++ showHex entry " " ++ show err
      Right (Left err, _) | err >= 0 -> Left $ "Could not disassemble instruction at 0x" ++ showHex err ""
      Right (Right ([Absolute callee], nextAddr, stmts), _)
        | not (inFunBounds funBounds callee) -> finishBlock stmts (Call callee nextAddr) [nextAddr]
      -- Does this case happen?
      Right (Right ([Absolute next], nextAddr, stmts), _)
        -> finishBlock stmts (Direct next) [next, nextAddr]
      Right (Right ([NFallthrough next], nextAddr, stmts), _)
        -> finishBlock stmts (Fallthrough next) [next, nextAddr]
      -- FIXME: hacky
      Right (Right ([Absolute branch, Absolute fallthrough], nextAddr, stmts), _)
        -> finishBlock stmts (Cond branch fallthrough) [branch, fallthrough, nextAddr]
      Right (Right ([NIndirect], nextAddr, stmts), _)
        -> finishBlock stmts Indirect [nextAddr]

  | otherwise = Right cfg
   where
