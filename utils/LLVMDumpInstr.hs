{-# LANGUAGE DataKinds #-}
-- Based on flexdis86.git/utils/DumpInstr.hs.
module Main where

import           Control.Lens
import           Control.Monad (forM_, when)
import           Control.Monad.State.Strict (execState, evalStateT)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import           Data.Word
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Nonce
import           Flexdis86
import           Numeric (readHex)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import qualified Text.LLVM as L
import           Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))

import           Data.Macaw.CFG
import           Data.Macaw.Discovery
import           Data.Macaw.Memory
import qualified Data.Macaw.Memory.Permissions as Perm

import           Data.Macaw.X86
import           Data.Macaw.X86.Semantics (execInstruction)

import           Reopt.CFG.LLVM
import           Reopt.CFG.Recovery

usageExit :: IO ()
usageExit = do putStrLn "LLVMDumpInstr aa bb cc dd ee ff ..."
               exitFailure

main :: IO ()
main = do
  args <- getArgs
  when (args == []) usageExit

  let nums = map readHex args

  when (any (\v -> length v /= 1 || any ((/=) 0 . length . snd) v) nums) usageExit

  let retBytes = [0xc3] -- terminate block
      bs = BS.pack $ (map (fst . head) nums) ++ retBytes
      seg = memSegment 0 (Just 0) Perm.execute [ByteRegion bs]
      mem =
        case insertMemSegment seg (emptyMemory Addr64) of
          Left e -> error $ "Segment " ++ showInsertError e
          Right mem' -> mem'
      base = SegmentedAddr seg 0
      -- cfg = cfgFromAddress mem base
      res = withGlobalSTNonceGenerator $ \gen -> do
        (bs, end, maybeError) <- disassembleBlock gen (rootLoc base) (segmentSize seg)
        case maybeError of
          Just err -> return $ Left err
          Nothing ->
            error "UNIMPLEMENTED"
            -- let cfg = eliminateDeadRegisters $ insertBlocksForCode base end bs emptyCFG
            --     bs' = Map.elems (cfg^.cfgBlocks)
            --     mkF = snd . L.runLLVM $ mapM_ functionToLLVM (finalFunctions cfg)
            -- in mapM_ (print . pretty) bs' >> print (L.ppModule $ mkF)
  case res of
    Left err -> putStrLn (show err)
    Right _ -> error "UNIMPLEMENTED"
