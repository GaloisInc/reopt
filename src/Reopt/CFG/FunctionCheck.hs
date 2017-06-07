module Reopt.CFG.FunctionCheck
   ( checkFunction
   ) where


import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V

import Data.Macaw.CFG
import Data.Macaw.Discovery.State

blbl :: SegmentedAddr w -> BlockLabel w
blbl a = GeneratedBlock a 0

-- | This returns true if all block terminators reachable from the
-- function entry point have non-error term stmts.
--
-- An error term statement is one of form translation error or
-- classify error.
checkFunction :: DiscoveryFunInfo arch ids
              -> Bool
checkFunction info = checkFunction' info Set.empty [blbl (discoveredFunAddr info)]

checkFunction' :: DiscoveryFunInfo arch ids
               -> Set (ArchLabel arch)
               -> [ArchLabel arch]
               -> Bool
checkFunction' _inf _visite [] = True
checkFunction' info visited (lbl:rest)
  | Set.member lbl visited = checkFunction' info visited rest
  | otherwise =
    case lookupParsedBlock info lbl of
      Nothing -> error $ "Missing block: " ++ show lbl
      Just block -> do
        let go = checkFunction' info $! Set.insert lbl visited
        case pblockTerm block of
          ParsedCall _ Nothing    -> go rest
          ParsedCall _ (Just a)   -> go (blbl a:rest)
          ParsedJump _ a          -> go (blbl a:rest)
          ParsedLookupTable _ _ a -> go (fmap blbl (V.toList a) ++ rest)
          ParsedReturn{}          -> go rest
          ParsedBranch _ x y      -> go (lbl { labelIndex = x }:lbl { labelIndex = y }:rest)
          ParsedSyscall _ a       -> go (blbl a:rest)
          ParsedTranslateError{}  -> False
          ClassifyFailure _       -> False
