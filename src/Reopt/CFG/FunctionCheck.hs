module Reopt.CFG.FunctionCheck
   ( checkFunction
   ) where


import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V

import Data.Macaw.CFG
import Data.Macaw.Discovery.Info

blbl :: SegmentedAddr w -> BlockLabel w
blbl a = GeneratedBlock a 0

-- | Return true if the function entry point
checkFunction :: DiscoveryFunInfo arch ids
              -> ArchSegmentedAddr arch
              -> Bool
checkFunction info addr = checkFunction' info Set.empty [blbl addr]

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
