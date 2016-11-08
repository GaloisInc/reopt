-- Based on flexdis86.git/utils/DumpInstr.hs.
module Main where

import           Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import           Data.Maybe (catMaybes)
import           Numeric (readHex)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)

import           Flexdis86

import           Data.Macaw.CFG (Value(..))
import           Data.Parameterized.NatRepr

import           Reopt.Concrete.MachineState
import           Reopt.Concrete.Semantics as C
import           Reopt.Semantics.FlexdisMatcher (execInstruction)

usageExit :: IO ()
usageExit = do putStrLn "DumpInstr aa bb cc dd ee ff ..."
               exitFailure

main :: IO ()
main = do args <- getArgs
          when (args == []) usageExit

          let nums = map readHex args

          when (any (\v -> length v /= 1 || any ((/=) 0 . length . snd) v) nums) usageExit

          let bs = BS.pack $ map (fst . head) nums

          let diss = disassembleBuffer bs
          forM_ diss $ \dis -> do
            case disInstruction dis of
              Nothing ->return ()
              Just i -> do
                putStrLn $ "Semantics for " ++ show i ++ ":"
                case execInstruction (ValueExpr (Literal (bitVector knownNat (fromIntegral (disLen dis))))) i of
                  Nothing -> error "Reopt.Semantics.FlexdisMatcher.execInstruction returned 'Nothing'!"
                  Just result -> print . C.ppStmts . C.execSemantics $ result
