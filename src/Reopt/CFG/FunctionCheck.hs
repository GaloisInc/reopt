module Reopt.CFG.FunctionCheck
   ( checkFunction
   ) where

import           Control.Lens
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

import Data.Macaw.CFG
import Data.Macaw.Discovery.State

-- | This returns true if all block terminators reachable from the
-- function entry point have non-error term stmts.
--
-- An error term statement is one of form translation error or
-- classify error.
checkFunction :: DiscoveryFunInfo arch ids
              -> Bool
checkFunction info = checkFunction' info Set.empty [discoveredFunAddr info]

checkFunction' :: DiscoveryFunInfo arch ids
               -> Set (ArchSegmentedAddr arch)
               -> [ArchSegmentedAddr arch]
               -> Bool
checkFunction' _inf _visite [] = True
checkFunction' info visited (lbl:rest)
  | Set.member lbl visited = checkFunction' info visited rest
  | otherwise =
    case Map.lookup lbl (info^.parsedBlocks) of
      Nothing -> error $ "Missing block: " ++ show lbl
      Just reg ->
        case Map.lookup 0 (regionBlockMap reg) of
          Nothing -> error $ "Could not find first block in region."
          Just b ->
            case checkRegion reg b of
              Nothing -> False
              Just next -> checkFunction' info (Set.insert lbl visited) (next ++ rest)

checkRegion :: ParsedBlockRegion arch ids
            -> ParsedBlock arch ids
            -> Maybe [ArchSegmentedAddr arch]
checkRegion reg block =
  case pblockTerm block of
    ParsedCall _ Nothing    -> Just []
    ParsedCall _ (Just a)   -> Just [a]
    ParsedJump _ a          -> Just [a]
    ParsedLookupTable _ _ a -> Just $ V.toList a
    ParsedReturn{}          -> Just []
    ParsedBranch _ x y      -> do
      case (Map.lookup x (regionBlockMap reg), Map.lookup y (regionBlockMap reg)) of
        (Just tblock, Just fblock) ->
          (++) <$> checkRegion reg tblock <*> checkRegion reg fblock
        _ -> error "Could not find block."
    ParsedIte _ x y      -> (++) <$> checkRegion reg x <*> checkRegion reg y
    ParsedSyscall _ a       -> Just [a]
    ParsedTranslateError{}  -> Nothing
    ClassifyFailure _       -> Nothing
