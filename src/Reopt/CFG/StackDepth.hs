{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
module Reopt.CFG.StackDepth
  ( maximumStackDepth
  , StackDepthValue(..)
  , StackDepthOffset(..)
  , stackDepthOffsetValue
  ) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Foldable as Fold (traverse_)
import           Data.Int
import           Data.List (partition)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid (Any(..))
import           Data.Parameterized.Classes
import           Data.Set (Set)
import qualified Data.Set as Set
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Data.Macaw.Discovery.State
import           Data.Macaw.CFG
import           Data.Macaw.Types

import           Data.Macaw.X86.ArchTypes
import           Data.Macaw.X86.X86Reg

import           Reopt.CFG.BlockLabel

data StackDepthOffset arch ids
   = Pos (BVValue arch ids (ArchAddrWidth arch))
   | Neg (BVValue arch ids (ArchAddrWidth arch))

deriving instance OrdF  (ArchReg arch) => Eq (StackDepthOffset arch ids)
deriving instance OrdF  (ArchReg arch) => Ord (StackDepthOffset arch ids)
deriving instance ShowF (ArchReg arch) => Show (StackDepthOffset arch ids)

stackDepthOffsetValue :: StackDepthOffset arch ids -> BVValue arch ids (ArchAddrWidth arch)
stackDepthOffsetValue (Pos v) = v
stackDepthOffsetValue (Neg v) = v

negateStackDepthOffset :: StackDepthOffset arch ids -> StackDepthOffset arch ids
negateStackDepthOffset (Pos x) = Neg x
negateStackDepthOffset (Neg x) = Pos x

isNegativeDepth :: StackDepthOffset arch ids -> Bool
isNegativeDepth (Neg _) = True
isNegativeDepth _ = False

-- One stack expression, basically staticPart + \Sigma dynamicPart
data StackDepthValue arch ids = SDV { staticPart :: !Int64
                                    , dynamicPart :: !(Set (StackDepthOffset arch ids))
                                    }

deriving instance OrdF (ArchReg arch) => Eq (StackDepthValue arch ids)
deriving instance OrdF (ArchReg arch) => Ord (StackDepthValue arch ids)
deriving instance ShowF (ArchReg arch) => Show (StackDepthValue arch ids)

instance ShowF (ArchReg arch) => Pretty (StackDepthValue arch ids) where
  pretty sdv = integer (fromIntegral $ staticPart sdv)
               <+> go (Set.toList $ dynamicPart sdv)
    where
      go []           = mempty
      go (Pos x : xs) = text "+" <+> pretty x <+> go xs
      go (Neg x : xs) = text "-" <+> pretty x <+> go xs

-- isConstantDepthValue :: StackDepthValue -> Maybe Int64
-- isConstantDepthValue sv
--   | Set.null (dynamicPart sv) = Just (staticPart sv)
--   | otherwise                 = Nothing

constantDepthValue :: Int64 -> StackDepthValue arch ids
constantDepthValue c = SDV c Set.empty

addStackDepthValue :: OrdF (ArchReg arch)
                   => StackDepthValue arch ids
                   -> StackDepthValue arch ids
                   -> StackDepthValue arch ids
addStackDepthValue sdv1 sdv2  = SDV (staticPart sdv1 + staticPart sdv2)
                                    (dynamicPart sdv1 `Set.union` dynamicPart sdv2)

negateStackDepthValue :: OrdF (ArchReg arch)
                      => StackDepthValue arch ids
                      -> StackDepthValue arch ids
negateStackDepthValue sdv = SDV { staticPart  = - (staticPart sdv)
                                , dynamicPart = Set.map negateStackDepthOffset (dynamicPart sdv)
                                }

-- | v1 `subsumes` v2 if a stack of depth v1 is always larger than a
-- stack of depth v2.  Note that we are interested in negative values
-- primarily.
subsumes :: OrdF (ArchReg arch) => StackDepthValue arch ids -> StackDepthValue arch ids -> Bool
subsumes v1 v2
  | dynamicPart v2 `Set.isSubsetOf` dynamicPart v1 = staticPart v1 <= staticPart v2
  -- FIXME: subsets etc.
  | otherwise = False

-- could do this online, this helps with debugging though.
minimizeStackDepthValues :: OrdF (ArchReg arch)
                         => Set (StackDepthValue arch ids)
                         -> Set (StackDepthValue arch ids)
minimizeStackDepthValues = Set.fromList . Set.fold go [] . Set.map discardPositive
  where
    discardPositive v = v { dynamicPart = Set.filter isNegativeDepth (dynamicPart v) }
    -- FIXME: can we use ordering to simplify this?
    go v xs = let (_subs, xs') = partition (subsumes v) xs
                  dominated   = any (`subsumes` v) xs'
              in if not dominated then v : xs' else xs'

-- -----------------------------------------------------------------------------

-- For now this is just the set of stack addresses referenced by the
-- program --- note that as it is partially symbolic, we can't always
-- statically determine the stack depth (might depend on arguments, for example).
type BlockStackDepths arch ids = Set (StackDepthValue arch ids)

-- We use BlockLabel but only really need CodeAddr (sub-blocks shouldn't appear)
data StackDepthState arch ids
   = SDS { _blockInitStackPointers :: !(Map (ArchSegmentedAddr arch) (StackDepthValue arch ids))
         , _blockStackRefs :: !(BlockStackDepths arch ids)
         , _blockFrontier  :: ![ArchSegmentedAddr arch]
           -- ^ Set of blocks to explore next.
         }

-- | Maps blocks already seen to the expected depth at the start of the block.
blockInitStackPointers :: Simple Lens (StackDepthState arch ids)
                                      (Map (ArchSegmentedAddr arch) (StackDepthValue arch ids))
blockInitStackPointers = lens _blockInitStackPointers (\s v -> s { _blockInitStackPointers = v })

blockStackRefs :: Simple Lens (StackDepthState arch ids) (BlockStackDepths arch ids)
blockStackRefs = lens _blockStackRefs (\s v -> s { _blockStackRefs = v })

-- | Set of blocks to visit next.
blockFrontier :: Simple Lens (StackDepthState arch ids) [ArchSegmentedAddr arch]
blockFrontier = lens _blockFrontier (\s v -> s { _blockFrontier = v })

-- ----------------------------------------------------------------------------------------

-- FIXME: move

-- Unwraps all Apps etc, might visit an app twice (Add x x, for example)
-- foldValue :: forall m tp. Monoid m
--              => (forall n.  NatRepr n -> Integer -> m)
--              -> (forall cl. N.RegisterName cl -> m)
--              -> Value tp -> m
-- foldValue litf initf val = go val
--   where
--     go :: forall tp. Value tp -> m
--     go v = case v of
--              BVValue sz i -> litf sz i
--              Initial r    -> initf r
--              AssignedValue (Assignment _ rhs) -> goAssignRHS rhs

--     goAssignRHS :: forall tp. AssignRhs tp -> m
--     goAssignRHS v =
--       case v of
--         EvalApp a -> foldApp go a
--         SetUndefined w -> mempty
--         Read loc
--          | MemLoc addr _ <- loc -> go addr
--          | otherwise            -> mempty -- FIXME: what about ControlLoc etc.
--         MemCmp _sz cnt src dest rev -> mconcat [ go cnt, go src, go dest, go rev ]

-- ----------------------------------------------------------------------------------------

type StackDepthM arch ids a = ExceptT String (State (StackDepthState arch ids)) a

addBlock :: (OrdF (ArchReg arch), ShowF (ArchReg arch))
         => SegmentedAddr (ArchAddrWidth arch)
         -> StackDepthValue arch ids
         -> StackDepthM arch ids ()
addBlock addr start = do
  let lbl = mkRootBlockLabel addr
  x <- use $ blockInitStackPointers . at addr
  case x of
    Nothing -> do
      blockInitStackPointers %= Map.insert addr start
      blockFrontier %= (addr:)
    Just old_start
      | start == old_start ->
        return ()
      | otherwise       ->
        throwError $ "Block stack depth mismatch at " ++ show (pretty lbl) ++ ": "
             ++ show (pretty start) ++ " and " ++ show (pretty old_start)

addDepth :: OrdF (ArchReg arch) => Set (StackDepthValue arch ids) -> StackDepthM arch ids ()
addDepth v = blockStackRefs %= Set.union v

------------------------------------------------------------------------
-- Stack pointer detection

-- | Return true if value references stack pointer
valueHasSP :: forall ids utp . Value X86_64 ids utp -> Bool
valueHasSP v0 =
   case v0 of
     BoolValue{} -> False
     BVValue _sz _i -> False
     RelocatableValue{} -> False
     Initial r      -> testEquality r sp_reg /= Nothing
     AssignedValue (Assignment _ rhs) -> goAssignRHS rhs
  where
    goAssignRHS :: forall tp. AssignRhs X86_64 ids tp -> Bool
    goAssignRHS v =
      case v of
        EvalApp a -> getAny $ foldApp (Any . valueHasSP)  a
        EvalArchFn (MemCmp _sz cnt src dest rev) _ ->
          or [ valueHasSP cnt, valueHasSP src, valueHasSP dest, valueHasSP rev ]
        _ -> False

parseStackPointer' :: StackDepthValue X86_64 ids
                   -> BVValue X86_64 ids (ArchAddrWidth X86_64)
                   -> StackDepthValue X86_64 ids
parseStackPointer' sp0 addr
  -- assert sp occurs in at most once in either x and y
  | Just (BVAdd _ x y) <- valueAsApp addr =
      addStackDepthValue (parseStackPointer' sp0 x)
                         (parseStackPointer' sp0 y)

  | Just (BVSub _ x y) <- valueAsApp addr =
      addStackDepthValue (parseStackPointer' sp0 x)
                         (negateStackDepthValue (parseStackPointer' sp0 y))
  | BVValue _ i <- addr = constantDepthValue (fromIntegral i)
  | Initial n <- addr
  , Just Refl <- testEquality n sp_reg = sp0
  | otherwise = SDV { staticPart = 0
                    , dynamicPart = Set.singleton (Pos addr)
                    }


-- FIXME: performance
parseStackPointer :: StackDepthValue X86_64 ids
                  -> BVValue X86_64 ids 64
                  -> Set (StackDepthValue X86_64 ids)
parseStackPointer sp0 addr0
  | valueHasSP addr0 = Set.singleton (parseStackPointer' sp0 addr0)
  | otherwise        = Set.empty

-- -----------------------------------------------------------------------------

goStmt :: StackDepthValue X86_64 ids -> Stmt X86_64 ids -> StackDepthM X86_64 ids ()
goStmt init_sp (AssignStmt (Assignment _ (ReadMem addr _))) =
  addDepth $ parseStackPointer init_sp addr
goStmt init_sp (WriteMem addr _ v) = do
  addDepth $ parseStackPointer init_sp addr
  case testEquality (typeRepr v) (knownType :: TypeRepr (BVType 64)) of
    Just Refl -> addDepth $ parseStackPointer init_sp v
    _ -> return ()
goStmt _ _ = return ()

recoverBlock :: DiscoveryFunInfo X86_64 ids
             -> SegmentedAddr 64
             -> StackDepthM X86_64 ids ()
recoverBlock interp_state root_addr = do
    Just init_sp <- use $ blockInitStackPointers . at root_addr
    Just reg <- return $ Map.lookup root_addr (interp_state^.parsedBlocks)
    go init_sp (blockStatementList reg)
  where
    addStateVars init_sp s = do
      forM_ gpRegList $ \r -> do
        addDepth $ parseStackPointer init_sp (s ^. boundValue r)
    go init_sp b = do
          -- overapproximates by viewing all registers as uses of the
          -- sp between blocks

      traverse_ (goStmt init_sp) (stmtsNonterm b)
      case stmtsTerm b of
        ParsedTranslateError _ ->
          throwError "Cannot identify stack depth in code where translation error occurs"
        ClassifyFailure _ ->
          throwError $ "Classification failed in StackDepth: " ++ show root_addr
        ParsedIte _c tblock fblock -> do
          go init_sp tblock
          go init_sp fblock

        ParsedCall proc_state m_ret_addr -> do
          addStateVars init_sp proc_state

          let sp'  = parseStackPointer' init_sp (proc_state ^. boundValue sp_reg)
          case m_ret_addr of
            Nothing -> return ()
            Just ret_addr ->
              addBlock ret_addr (addStackDepthValue sp' $ constantDepthValue 8)

        ParsedJump proc_state tgt_addr -> do
          addStateVars init_sp proc_state

          let sp' = parseStackPointer' init_sp (proc_state ^. boundValue sp_reg)

          addBlock tgt_addr sp'

        ParsedReturn _proc_state -> do
          pure ()

        ParsedSyscall proc_state next_addr -> do
          addStateVars init_sp proc_state

          let sp'  = parseStackPointer' init_sp (proc_state ^. boundValue sp_reg)
          addBlock next_addr sp'

        ParsedLookupTable proc_state _idx vec -> do
          addStateVars init_sp proc_state

          let sp'  = parseStackPointer' init_sp (proc_state ^. boundValue sp_reg)

          traverse_ (\a -> addBlock a sp') vec

-- | Explore states until we have reached end of frontier.
recoverIter :: DiscoveryFunInfo X86_64 ids
            -> StackDepthM X86_64 ids ()
recoverIter ist = do
  s <- use blockFrontier
  case s of
    [] -> return ()
    next : s' -> do
      blockFrontier .= s'
      recoverBlock ist next
      recoverIter ist

-- | Returns the maximum stack argument used by the function, that is,
-- the highest index above sp0 that is read or written.
maximumStackDepth :: DiscoveryFunInfo X86_64 ids
                  -> SegmentedAddr 64
                  -> Either String (BlockStackDepths X86_64 ids)
maximumStackDepth ist addr = finish $ runState (runExceptT (recoverIter ist)) s0
  where
    s0   = SDS { _blockInitStackPointers = Map.singleton addr sdv0
               , _blockStackRefs         = Set.empty
               , _blockFrontier          = [addr]
               }
    sdv0 = SDV { staticPart = 0, dynamicPart = Set.empty }
    finish (Right (), s) = Right $ minimizeStackDepthValues $ s ^. blockStackRefs
    finish (Left e, _) = Left e
