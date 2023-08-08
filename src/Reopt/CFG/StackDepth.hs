{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Reopt.CFG.StackDepth (
  maximumStackDepth,
  StackDepthValue (..),
  StackDepthOffset (..),
  stackDepthOffsetValue,
) where

import Control.Lens (
  At (at),
  Lens,
  Simple,
  lens,
  use,
  (%=),
  (.=),
  (^.),
  type (:~:) (Refl),
 )
import Control.Monad.Except (
  ExceptT,
  MonadError (throwError),
  forM_,
  runExceptT,
 )
import Control.Monad.State.Strict (State, runState)
import Data.Foldable as Fold (traverse_)
import Data.Int (Int64)
import Data.List (partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, isJust)
import Data.Monoid (Any (..))
import Data.Parameterized.Classes (
  KnownRepr (knownRepr),
  OrdF,
  TestEquality (testEquality),
 )
import Data.Parameterized.Map qualified as MapF
import Data.Set (Set)
import Data.Set qualified as Set
import Prettyprinter (Pretty (pretty), (<+>))

import Data.Macaw.CFG
import Data.Macaw.Discovery.State
import Data.Macaw.Types (BVType, HasRepr (typeRepr), TypeRepr)
import Data.Macaw.X86.ArchTypes (X86_64)
import Data.Macaw.X86.X86Reg (gpRegList)

data StackDepthOffset arch ids
  = Pos (BVValue arch ids (ArchAddrWidth arch))
  | Neg (BVValue arch ids (ArchAddrWidth arch))

deriving instance OrdF (ArchReg arch) => Eq (StackDepthOffset arch ids)
deriving instance OrdF (ArchReg arch) => Ord (StackDepthOffset arch ids)
deriving instance RegisterInfo (ArchReg arch) => Show (StackDepthOffset arch ids)

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
data StackDepthValue arch ids = SDV
  { staticPart :: !Int64
  , dynamicPart :: !(Set (StackDepthOffset arch ids))
  }

deriving instance OrdF (ArchReg arch) => Eq (StackDepthValue arch ids)
deriving instance OrdF (ArchReg arch) => Ord (StackDepthValue arch ids)
deriving instance RegisterInfo (ArchReg arch) => Show (StackDepthValue arch ids)

instance RegisterInfo (ArchReg arch) => Pretty (StackDepthValue arch ids) where
  pretty sdv = pretty (staticPart sdv) <+> foldr go mempty (dynamicPart sdv)
   where
    go (Pos x) r = "+ " <> pretty x <+> r
    go (Neg x) r = "- " <> pretty x <+> r

constantDepthValue :: Int64 -> StackDepthValue arch ids
constantDepthValue c = SDV c Set.empty

addStackDepthValue ::
  OrdF (ArchReg arch) =>
  StackDepthValue arch ids ->
  StackDepthValue arch ids ->
  StackDepthValue arch ids
addStackDepthValue sdv1 sdv2 =
  SDV
    (staticPart sdv1 + staticPart sdv2)
    (dynamicPart sdv1 `Set.union` dynamicPart sdv2)

negateStackDepthValue ::
  OrdF (ArchReg arch) =>
  StackDepthValue arch ids ->
  StackDepthValue arch ids
negateStackDepthValue sdv =
  SDV
    { staticPart = -(staticPart sdv)
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
minimizeStackDepthValues ::
  OrdF (ArchReg arch) =>
  Set (StackDepthValue arch ids) ->
  Set (StackDepthValue arch ids)
minimizeStackDepthValues = Set.fromList . Set.fold go [] . Set.map discardPositive
 where
  discardPositive v = v{dynamicPart = Set.filter isNegativeDepth (dynamicPart v)}
  -- FIXME: can we use ordering to simplify this?
  go v xs =
    let
      (_subs, xs') = partition (subsumes v) xs
      dominated = any (`subsumes` v) xs'
     in
      if not dominated then v : xs' else xs'

-- -----------------------------------------------------------------------------

-- For now this is just the set of stack addresses referenced by the
-- program --- note that as it is partially symbolic, we can't always
-- statically determine the stack depth (might depend on arguments, for example).
type BlockStackDepths arch ids = Set (StackDepthValue arch ids)

-- We use BlockLabel but only really need CodeAddr (sub-blocks shouldn't appear)
data StackDepthState arch ids = SDS
  { _blockInitStackPointers :: !(Map (ArchSegmentOff arch) (StackDepthValue arch ids))
  , _blockStackRefs :: !(BlockStackDepths arch ids)
  , _blockFrontier :: ![ArchSegmentOff arch]
  -- ^ Set of blocks to explore next.
  }

-- | Maps blocks already seen to the expected depth at the start of the block.
blockInitStackPointers ::
  Simple
    Lens
    (StackDepthState arch ids)
    (Map (ArchSegmentOff arch) (StackDepthValue arch ids))
blockInitStackPointers = lens _blockInitStackPointers (\s v -> s{_blockInitStackPointers = v})

blockStackRefs :: Simple Lens (StackDepthState arch ids) (BlockStackDepths arch ids)
blockStackRefs = lens _blockStackRefs (\s v -> s{_blockStackRefs = v})

-- | Set of blocks to visit next.
blockFrontier :: Simple Lens (StackDepthState arch ids) [ArchSegmentOff arch]
blockFrontier = lens _blockFrontier (\s v -> s{_blockFrontier = v})

type StackDepthM arch ids a = ExceptT String (State (StackDepthState arch ids)) a

-- | Record that the stack at the block at the current address should have
-- the given StackDepthValue.
recordStackOffset ::
  RegisterInfo (ArchReg arch) =>
  -- | Starting address at block
  ArchSegmentOff arch ->
  StackDepthValue arch ids ->
  StackDepthM arch ids ()
recordStackOffset addr start = do
  x <- use $ blockInitStackPointers . at addr
  case x of
    Nothing -> do
      blockInitStackPointers %= Map.insert addr start
      blockFrontier %= (addr :)
    Just old_start
      | start == old_start ->
          return ()
      | otherwise ->
          throwError $
            "Block stack depth mismatch at "
              ++ show (pretty addr)
              ++ ": "
              ++ show (pretty start)
              ++ " and "
              ++ show (pretty old_start)

addDepth :: OrdF (ArchReg arch) => Set (StackDepthValue arch ids) -> StackDepthM arch ids ()
addDepth v = blockStackRefs %= Set.union v

------------------------------------------------------------------------
-- Stack pointer detection

-- | Return true if value references stack pointer
valueHasSP :: forall ids utp. Value X86_64 ids utp -> Bool
valueHasSP v0 =
  case v0 of
    BoolValue{} -> False
    BVValue _sz _i -> False
    RelocatableValue{} -> False
    SymbolValue{} -> False
    Initial r -> isJust (testEquality r sp_reg)
    AssignedValue (Assignment _ rhs) -> goAssignRHS rhs
 where
  goAssignRHS :: forall tp. AssignRhs X86_64 (Value X86_64 ids) tp -> Bool
  goAssignRHS v =
    case v of
      EvalApp a -> getAny $ foldMapFC (Any . valueHasSP) a
      EvalArchFn f _ -> getAny $ foldMapFC (Any . valueHasSP) f
      _ -> False

parseStackPointer' ::
  StackDepthValue X86_64 ids ->
  BVValue X86_64 ids (ArchAddrWidth X86_64) ->
  StackDepthValue X86_64 ids
parseStackPointer' sp0 addr
  -- assert sp occurs in at most once in either x and y
  | Just (BVAdd _ x y) <- valueAsApp addr =
      addStackDepthValue
        (parseStackPointer' sp0 x)
        (parseStackPointer' sp0 y)
  | Just (BVSub _ x y) <- valueAsApp addr =
      addStackDepthValue
        (parseStackPointer' sp0 x)
        (negateStackDepthValue (parseStackPointer' sp0 y))
  | BVValue _ i <- addr = constantDepthValue (fromIntegral i)
  | Initial n <- addr
  , Just Refl <- testEquality n sp_reg =
      sp0
  | otherwise =
      SDV
        { staticPart = 0
        , dynamicPart = Set.singleton (Pos addr)
        }

-- FIXME: performance
parseStackPointer ::
  StackDepthValue X86_64 ids ->
  BVValue X86_64 ids 64 ->
  Set (StackDepthValue X86_64 ids)
parseStackPointer sp0 addr0
  | valueHasSP addr0 = Set.singleton (parseStackPointer' sp0 addr0)
  | otherwise = Set.empty

-- -----------------------------------------------------------------------------

goStmt :: StackDepthValue X86_64 ids -> Stmt X86_64 ids -> StackDepthM X86_64 ids ()
goStmt init_sp (AssignStmt (Assignment _ (ReadMem addr _))) =
  addDepth $ parseStackPointer init_sp addr
goStmt init_sp (WriteMem addr _ v) = do
  addDepth $ parseStackPointer init_sp addr
  case testEquality (typeRepr v) (knownRepr :: TypeRepr (BVType 64)) of
    Just Refl -> addDepth $ parseStackPointer init_sp v
    _ -> return ()
goStmt _ _ = return ()

addStateVars ::
  StackDepthValue X86_64 ids ->
  RegState (ArchReg X86_64) (Value X86_64 ids) ->
  StackDepthM X86_64 ids ()
addStateVars init_sp s =
  forM_ gpRegList $ \r -> do
    addDepth $ parseStackPointer init_sp (s ^. boundValue r)

-- | This function finds references to the stack pointer in the statements to
-- infer the maximal stack offset that may be referenced by the points.
--
-- In this case, the stack pointer is assumed to grow down (X86 specific),
-- so maximal actually means the largest negative offset.
analyzeStmtReferences ::
  -- | The address that the statements started at.
  MemSegmentOff 64 ->
  StackDepthValue X86_64 ids ->
  ParsedBlock X86_64 ids ->
  StackDepthM X86_64 ids ()
analyzeStmtReferences root_addr init_sp b = do
  -- overapproximates by viewing all registers as uses of the
  -- sp between blocks
  traverse_ (goStmt init_sp) (pblockStmts b)
  case pblockTermStmt b of
    ParsedCall regs m_ret_addr -> do
      addStateVars init_sp regs

      let sp' = parseStackPointer' init_sp (regs ^. boundValue sp_reg)
      case m_ret_addr of
        Nothing -> return ()
        Just ret_addr ->
          recordStackOffset ret_addr (addStackDepthValue sp' $ constantDepthValue 8)

    -- PLT stubs are just tail calls.
    PLTStub regs _ _ -> do
      forM_ gpRegList $ \r -> do
        case MapF.lookup r regs of
          Just v -> addDepth $ parseStackPointer init_sp v
          Nothing -> pure ()
    ParsedTranslateError{} ->
      throwError "Cannot identify stack depth in code where translation error occurs"
    ClassifyFailure{} ->
      throwError $ "Classification failed in StackDepth: " ++ show root_addr
    ParsedJump regs tgtAddr -> do
      addStateVars init_sp regs
      let sp' = parseStackPointer' init_sp (regs ^. boundValue sp_reg)
      recordStackOffset tgtAddr sp'
    ParsedBranch regs _c trueAddr falseAddr -> do
      addStateVars init_sp regs
      let sp' = parseStackPointer' init_sp (regs ^. boundValue sp_reg)
      recordStackOffset trueAddr sp'
      recordStackOffset falseAddr sp'
    ParsedReturn _regs -> do
      pure ()
    ParsedLookupTable _layout regs _idx vec -> do
      addStateVars init_sp regs
      let sp' = parseStackPointer' init_sp (regs ^. boundValue sp_reg)
      traverse_ (`recordStackOffset` sp') vec
    ParsedArchTermStmt _ regs mnext_addr -> do
      addStateVars init_sp regs
      let sp' = parseStackPointer' init_sp (regs ^. boundValue sp_reg)
      traverse_ (`recordStackOffset` sp') mnext_addr

-- | Explore states until we have reached end of frontier.
recoverIter ::
  DiscoveryFunInfo X86_64 ids ->
  StackDepthM X86_64 ids ()
recoverIter finfo = do
  s <- use blockFrontier
  case s of
    [] -> return ()
    root_addr : s' -> do
      blockFrontier .= s'
      spMap <- use blockInitStackPointers
      let init_sp = fromJust $ Map.lookup root_addr spMap
      let reg = fromJust $ Map.lookup root_addr (finfo ^. parsedBlocks)
      analyzeStmtReferences root_addr init_sp reg
      recoverIter finfo

-- | Returns the maximum stack argument used by the function, that is,
-- the highest index above sp0 that is read or written.
maximumStackDepth ::
  DiscoveryFunInfo X86_64 ids ->
  Either String (BlockStackDepths X86_64 ids)
maximumStackDepth ist = finish $ runState (runExceptT (recoverIter ist)) s0
 where
  addr = discoveredFunAddr ist
  sdv0 = SDV{staticPart = 0, dynamicPart = Set.empty}
  s0 =
    SDS
      { _blockInitStackPointers = Map.singleton addr sdv0
      , _blockStackRefs = Set.empty
      , _blockFrontier = [addr]
      }
  finish (Right (), s) = Right $ minimizeStackDepthValues $ s ^. blockStackRefs
  finish (Left e, _) = Left e
