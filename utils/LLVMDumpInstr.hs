-- Based on flexdis86.git/utils/DumpInstr.hs.
module Main where

import           Control.Lens
import           Control.Monad (forM_, when)
import           Control.Monad.State.Strict (execState, evalStateT)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import           Numeric (readHex)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import qualified Text.LLVM as L
import           Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))

import           Flexdis86
-- import           Reopt.CFG.CFGDiscovery
import           Reopt.CFG.Implementation
import           Reopt.CFG.Representation
import           Reopt.CFG.Recovery
import           Reopt.CFG.LLVM
import           Reopt.Object.Memory
import           Reopt.Semantics.FlexdisMatcher (execInstruction)
import           Reopt.Semantics.DeadRegisterElimination

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
      base = (0 :: Word64)
      seg = MemSegment base pf_x bs
      mem = execState (insertMemSegment seg) emptyMemory
      -- cfg = cfgFromAddress mem base
      b   = evalStateT (disassembleBlock mem (const True) (rootLoc base))
                       emptyGlobalGenState
  --  mapM_ (print . pretty) (finalFunctions cfg)
      mkF = snd . L.runLLVM
            . L.defineFresh L.emptyFunAttrs L.voidT ()
            . mapM_ blockToLLVM
  case b of
   Left err       -> print err
   Right (bs, end)  ->
     let cfg = eliminateDeadRegisters $ insertBlocksForCode base end bs emptyCFG
         bs' = Map.elems (cfg^.cfgBlocks)
     in mapM_ (print . pretty) bs' >> print (L.ppModule $ mkF bs')
  return ()
