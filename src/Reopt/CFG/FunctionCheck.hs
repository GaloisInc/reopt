{-|
Provides utility for checking that all blocks in function info have
non-error terminal statements.
-}
{-# LANGUAGE FlexibleContexts #-}
module Reopt.CFG.FunctionCheck
   ( CheckFunctionResult(..)
   , checkFunction
   ) where

import           Control.Lens
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

import           Data.Macaw.CFG
import           Data.Macaw.Discovery.State
import           Data.Macaw.Memory (MemWidth)

data CheckFunctionResult
   = FunctionOK
   | FunctionIncomplete
   | FunctionHasPLT

-- | This analyzes the block terminator to statement list to determine
checkTermStmt :: ParsedTermStmt  arch ids
              -> Either CheckFunctionResult [ArchSegmentOff arch]
checkTermStmt ts =
  case ts of
    ParsedCall _ Nothing    -> pure []
    ParsedCall _ (Just a)   -> pure [a]
    -- PLT stubs are tail calls.
    PLTStub{} -> Left FunctionHasPLT
    ParsedJump _ a          -> pure [a]
    ParsedBranch _ _ t f    -> pure [t,f]
    ParsedLookupTable _ _ a -> pure $ V.toList a
    ParsedReturn{}          -> pure []
    ParsedTranslateError{}  -> Left FunctionIncomplete
    ClassifyFailure{}       -> Left FunctionIncomplete
    ParsedArchTermStmt _ _ a -> pure (maybeToList a)

checkFunction' :: MemWidth (ArchAddrWidth arch)
               => DiscoveryFunInfo arch ids
               -> Set (ArchSegmentOff arch)
               -> [ArchSegmentOff arch]
               -> CheckFunctionResult
checkFunction' _inf _visite [] = FunctionOK
checkFunction' info visited (lbl:rest)
  | Set.member lbl visited = checkFunction' info visited rest
  | otherwise =
    case Map.lookup lbl (info^.parsedBlocks) of
      Nothing -> error $ "Missing block: " ++ show lbl
      Just b -> do
        case checkTermStmt (pblockTermStmt b) of
          Left r -> r
          Right next -> checkFunction' info (Set.insert lbl visited) (next ++ rest)

-- | This returns true if all block terminators reachable from the
-- function entry point have non-error term stmts.
--
-- An error term statement is one of form translation error or
-- classify error.
checkFunction :: MemWidth (ArchAddrWidth arch)
              => DiscoveryFunInfo arch ids
              -> CheckFunctionResult
checkFunction info = checkFunction' info Set.empty [discoveredFunAddr info]
