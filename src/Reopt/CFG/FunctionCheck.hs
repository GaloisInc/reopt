{-|
Copyright        : (c) Galois, Inc 2017
Maintainer       : Joe Hendrix <jhendrix@galois.com>

Provides utility for checking that all blocks in function info have non-error terminal
statements.
-}
{-# LANGUAGE FlexibleContexts #-}
module Reopt.CFG.FunctionCheck
   ( checkFunction
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

-- | This returns true if all block terminators reachable from the
-- function entry point have non-error term stmts.
--
-- An error term statement is one of form translation error or
-- classify error.
checkFunction :: MemWidth (ArchAddrWidth arch)
              => DiscoveryFunInfo arch ids
              -> Bool
checkFunction info = checkFunction' info Set.empty [discoveredFunAddr info]

checkFunction' :: MemWidth (ArchAddrWidth arch)
               => DiscoveryFunInfo arch ids
               -> Set (ArchSegmentOff arch)
               -> [ArchSegmentOff arch]
               -> Bool
checkFunction' _inf _visite [] = True
checkFunction' info visited (lbl:rest)
  | Set.member lbl visited = checkFunction' info visited rest
  | otherwise =
    case Map.lookup lbl (info^.parsedBlocks) of
      Nothing -> error $ "Missing block: " ++ show lbl
      Just reg -> do
        let b = blockStatementList reg
        case checkRegion reg b of
          Nothing -> False
          Just next -> checkFunction' info (Set.insert lbl visited) (next ++ rest)

checkRegion :: ParsedBlock arch ids
            -> StatementList arch ids
            -> Maybe [ArchSegmentOff arch]
checkRegion reg stmts =
  case stmtsTerm stmts of
    ParsedCall _ Nothing    -> Just []
    ParsedCall _ (Just a)   -> Just [a]
    ParsedJump _ a          -> Just [a]
    ParsedLookupTable _ _ a -> Just $ V.toList a
    ParsedReturn{}          -> Just []
    ParsedIte _ x y      -> (++) <$> checkRegion reg x <*> checkRegion reg y
    ParsedTranslateError{}  -> Nothing
    ClassifyFailure _       -> Nothing
    ParsedArchTermStmt _ _ a -> Just (maybeToList a)
