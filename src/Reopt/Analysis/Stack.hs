{--
This provides a capability to infer stack offsets in binaries so
that Reopt can generate annotations.
--}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Reopt.Analysis.Stack
  ( StackArchInvariants(..)
  , InferStackError
  , FunctionStackPrecond
  , BlockStackPrecond(..)
  , AddrReg
  , StackOffset
  , inferStackPrecond
  ) where

import           Control.Lens
import           Control.Monad
import           Data.Foldable
import           Data.Macaw.CFG
import           Data.Macaw.Discovery.State
import           Data.Macaw.Types
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.TraversableF
import           Data.Parameterized.TraversableFC
import           Data.Set (Set)
import           Data.Type.Equality
import           GHC.TypeNats

-- | This describes an offset from the initial stack value.
--
-- The offset is relative to the value stored in the stack register at
-- the function entry point.
type StackOffset = Integer

-- | A register capable of holding a pointer
type AddrReg r = r (BVType (RegAddrWidth r))


-- | Describes references to the stack at the start of block
-- execution.
data BlockStackPrecond arch = BlockStackPrecond
  { bspRegisterOffsets :: !(Map (AddrReg (ArchReg arch)) StackOffset)
    -- ^ Maps each register that points to the stack at the start of
    -- block execution to the offset in the stack of that register.
  }

instance Eq (ArchReg arch (BVType (ArchAddrWidth arch))) => Eq (BlockStackPrecond arch) where
  x == y = bspRegisterOffsets x == bspRegisterOffsets y

-- | Block stack references for function entry point.
fnEntryStackRefs :: RegisterInfo (ArchReg arch) => BlockStackPrecond arch
fnEntryStackRefs =
  BlockStackPrecond { bspRegisterOffsets = Map.singleton sp_reg 0 }

type InferStackError = String

-- | Maps assignment ids with the given width to their stack offset.
type BlockAssignMap w ids = Map (AssignId ids (BVType w)) StackOffset

-- | Maps each basic block address in the function to the precondition
-- associated with it.
type FunctionStackPrecond arch = Map (ArchSegmentOff arch) (BlockStackPrecond arch)

-- | List of blocks along with precondition that have been added to
-- function stack map, but have not yet had their out-edges processed.
type PendingBlockQueue arch = [(ArchSegmentOff arch, BlockStackPrecond arch)]

data FunProcessState arch = FPS { fpsMap :: !(FunctionStackPrecond arch)
                                , fpsQueue :: !(PendingBlockQueue arch)
                                }

data StackArchInvariants arch =
  StackArchInvariants { saiCallStackDelta :: !Integer
                        -- ^ Amount call stack changes after call returns (e.g. 8 on x86_64)
                      , saiCallPreservedRegs :: !(Set (ArchReg arch (BVType (ArchAddrWidth arch))))
                        -- ^ Set of registers preserved by call.
                        --
                        -- This should not include the stack pointer.
                      , saiAddrWidth :: !(NatRepr (ArchAddrWidth arch))
                      }

valueStackOffset :: Ord (AddrReg (ArchReg arch))
                 => BlockStackPrecond arch
                 -> BlockAssignMap (ArchAddrWidth arch) ids
                 -> Value arch ids (BVType (ArchAddrWidth arch))
                 -> Maybe StackOffset
valueStackOffset blockPrecond assignMap v =
  case v of
    CValue{}        -> Nothing
    AssignedValue a -> Map.lookup (assignId a) assignMap
    Initial r -> Map.lookup r (bspRegisterOffsets blockPrecond)

nextRegs :: ( Ord (AddrReg (ArchReg arch))
            , 1 <= ArchAddrWidth arch
            , HasRepr (ArchReg arch) TypeRepr
            )
         => NatRepr (ArchAddrWidth arch)
         -> BlockStackPrecond arch
         -> BlockAssignMap (ArchAddrWidth arch) ids
         -> RegState (ArchReg arch) (Value arch ids)
         -> BlockStackPrecond arch
nextRegs addrWidth blockPrecond assignMap regs =
  BlockStackPrecond
  { bspRegisterOffsets =
      Map.fromAscList
      [ (r,off)
      | MapF.Pair r v <- MapF.toAscList (regStateMap regs)
      , Just Refl <- [testEquality (typeRepr v) (BVTypeRepr addrWidth)]
      , off <- maybeToList (valueStackOffset blockPrecond assignMap v)
      ]
  }

addPrecond :: ( Eq (AddrReg (ArchReg arch))
              , MemWidth (ArchAddrWidth arch)
              )
           => ArchSegmentOff arch
           -> BlockStackPrecond arch
           -> FunProcessState arch
           -> Either InferStackError (FunProcessState arch)
addPrecond addr precond fps =
  case Map.lookup addr (fpsMap fps) of
    Nothing -> do
      pure $! fps { fpsMap = Map.insert addr precond (fpsMap fps)
                  , fpsQueue = (addr,precond):fpsQueue fps
                  }
    Just oldPrecond -> do
      if precond /= oldPrecond then
        Left $ "Block at address " ++ show addr ++ " has different preconditions."
       else
        pure fps

type InferStackMonad = Either InferStackError

testPtrType :: HasRepr r TypeRepr
            => StackArchInvariants arch
            -> r tp
            -> Maybe (tp :~: BVType (ArchAddrWidth arch))
testPtrType sai r =
  case typeRepr r of
    BVTypeRepr w -> do
      Refl <- testEquality w (saiAddrWidth sai)
      pure Refl
    _ -> Nothing


-- | Check that the value is not a reference to the stack pointer.
anyValueStackOffset :: ( Ord (AddrReg (ArchReg arch))
                       , HasRepr (ArchReg arch) TypeRepr
                       )
                    => StackArchInvariants arch
                    -> BlockStackPrecond arch
                    -> BlockAssignMap (ArchAddrWidth arch) ids
                    -> Value arch ids tp
                    -> Maybe StackOffset
anyValueStackOffset sai blockPrecond assignMap val = do
  case val of
    CValue{} -> Nothing
    Initial r -> do
      Refl <- testPtrType sai r
      Map.lookup r (bspRegisterOffsets blockPrecond)
    AssignedValue a -> do
      Refl <- testPtrType sai (assignRhs a)
      Map.lookup (assignId a) assignMap

-- | Check that the value is not a reference to the stack pointer.
checkNotStackPtr :: ( Ord (AddrReg (ArchReg arch))
                    , HasRepr (ArchReg arch) TypeRepr
                    )
                 => StackArchInvariants arch
                 -> BlockStackPrecond arch
                 -> BlockAssignMap (ArchAddrWidth arch) ids
                 -> Value arch ids tp
                 -> InferStackMonad ()
checkNotStackPtr sai blockPrecond assignMap val = do
  when (isJust (anyValueStackOffset sai blockPrecond assignMap val)) $ do
    fail "Value is a stack pointer."

evalStmts :: ( Ord (AddrReg (ArchReg arch))
             , HasRepr (ArchReg arch) TypeRepr
             , FoldableFC (ArchFn arch)
             , FoldableF (ArchStmt arch)
             )
          => StackArchInvariants arch
          -> BlockStackPrecond arch
          -> BlockAssignMap (ArchAddrWidth arch) ids
          -> [Stmt arch ids]
          -> InferStackMonad (BlockAssignMap (ArchAddrWidth arch) ids)
evalStmts _ _ assignMap [] = pure assignMap
evalStmts sai blockPrecond assignMap (nextStmt:rest) = do
  case nextStmt of
    AssignStmt a -> do
      case assignRhs a of
        EvalApp app -> do
          -- Check assignment does not depend on stack.
          traverseFC_ (checkNotStackPtr sai blockPrecond assignMap ) app
          evalStmts sai blockPrecond assignMap rest
        SetUndefined _ -> do
          evalStmts sai blockPrecond assignMap rest
        ReadMem addr _ -> do
          case valueStackOffset blockPrecond assignMap addr of
            Just{} -> Left "Do not yet support stack reads."
            Nothing -> pure ()
          evalStmts sai blockPrecond assignMap rest
        CondReadMem _repr cond addr falseVal -> do
          checkNotStackPtr sai blockPrecond assignMap cond
          when (isJust (valueStackOffset blockPrecond assignMap addr)) $ do
            Left "Do not yet support conditional stack reads."
          checkNotStackPtr sai blockPrecond assignMap falseVal
          evalStmts sai blockPrecond assignMap rest
        EvalArchFn archFn _ -> do
          traverseFC_ (checkNotStackPtr sai blockPrecond assignMap) archFn
          evalStmts sai blockPrecond assignMap rest
    WriteMem addr _repr val -> do
      case valueStackOffset blockPrecond assignMap addr of
        Just{} -> Left "Do not yet support stack writes."
        Nothing -> do
          case anyValueStackOffset sai blockPrecond assignMap val of
            Just{} -> Left "Do not support writing stack addresses to heap."
            Nothing -> pure ()
      evalStmts sai blockPrecond assignMap rest
    CondWriteMem cond addr _repr val -> do
      checkNotStackPtr sai blockPrecond assignMap cond
      case valueStackOffset blockPrecond assignMap addr of
        Just{} -> Left "Do not yet support stack writes."
        Nothing -> do
          case anyValueStackOffset sai blockPrecond assignMap val of
            Just{} -> Left "Do not support writing stack addresses to heap."
            Nothing -> pure ()
      evalStmts sai blockPrecond assignMap rest
    InstructionStart _ _ -> do
      evalStmts sai blockPrecond assignMap rest
    Comment _ -> do
      evalStmts sai blockPrecond assignMap rest
    ExecArchStmt archStmt -> do
      traverseF_ (checkNotStackPtr sai blockPrecond assignMap) archStmt
      evalStmts sai blockPrecond assignMap rest
    ArchState _ _ -> do
      evalStmts sai blockPrecond assignMap rest

-- | Analyze a @ParsedTermStmt@ to add successor blocks to function state.
nextStackPrecond :: ( RegisterInfo (ArchReg arch)
                    , Ord (AddrReg (ArchReg arch))
                    )
                 => StackArchInvariants arch
                    -- ^ Architecture specific features for analyzing stack.
                 -> FunProcessState arch
                 -> BlockStackPrecond arch
                 -> BlockAssignMap (ArchAddrWidth arch) ids
                 -> ParsedTermStmt arch ids
                 -> InferStackMonad (FunProcessState arch)
nextStackPrecond sai fps blockPrecond assignMap tstmt = do
  case tstmt of
    -- Tail calls have no successors in this function.
    ParsedCall _ Nothing -> do
      pure fps
    ParsedCall regs (Just retAddr) -> do
      let spVal = regs^.boundValue sp_reg
      case valueStackOffset blockPrecond assignMap spVal of
        Nothing ->
          Left "Call with unrecognized stack pointer."
        Just spOff -> do
          let nextPrecond = BlockStackPrecond
                { bspRegisterOffsets = Map.fromList $
                  [(sp_reg, spOff + saiCallStackDelta sai)]
                   ++ [ (r,off)
                      | r <- toList (saiCallPreservedRegs sai)
                      , let v = regs^.boundValue r
                      , off <- maybeToList $ valueStackOffset blockPrecond assignMap v
                      ]
                }
          addPrecond retAddr nextPrecond fps
    -- PLTStubs never return so no next states to consider.
    PLTStub{} ->
      pure fps
    ParsedJump regs addr -> do
      let nextPrecond = nextRegs (saiAddrWidth sai) blockPrecond assignMap regs
      addPrecond addr nextPrecond fps
    ParsedBranch regs _cond tblockAddr fblockAddr -> do
      let nextPrecond = nextRegs (saiAddrWidth sai) blockPrecond assignMap regs
      addPrecond tblockAddr nextPrecond
        =<< addPrecond fblockAddr nextPrecond fps
    ParsedLookupTable regs _ addrs -> do
      let nextPrecond = nextRegs (saiAddrWidth sai) blockPrecond assignMap regs
      foldlM (\pc addr -> addPrecond addr nextPrecond pc) fps addrs
    ParsedReturn _regs -> do
      pure fps
    ParsedArchTermStmt _astmt _reg Nothing -> do
      pure fps
    ParsedArchTermStmt _astmt regs (Just addr) -> do
      let nextPrecond = nextRegs (saiAddrWidth sai) blockPrecond assignMap regs
      addPrecond addr nextPrecond fps
    ParsedTranslateError _ -> do
      Left "Encountered translation error"
    ClassifyFailure _ -> do
      Left "Encountered classification failure."




processStackPrecond :: ( RegisterInfo (ArchReg arch)
                       , Ord (AddrReg (ArchReg arch))
                       , FoldableFC (ArchFn arch)
                       , FoldableF (ArchStmt arch)
                       )
                    => StackArchInvariants arch
                    -> DiscoveryFunInfo arch ids
                    -> FunProcessState arch
                       -- ^ Blocks discovered so far.
                    -> InferStackMonad (FunctionStackPrecond arch)
processStackPrecond sai finfo fps = do
  case fpsQueue fps of
    [] -> do
      pure (fpsMap fps)
    (blockAddr,blockPrecond):rest -> do
      let b = fromMaybe (error $ "Could not find block in info") $
            finfo^.parsedBlocks^.at blockAddr
      let fpsRest = fps { fpsQueue = rest }
      -- process statements.
      assignMap <- evalStmts sai blockPrecond Map.empty (pblockStmts b)
      fps' <- nextStackPrecond sai fpsRest blockPrecond assignMap (pblockTermStmt b)
      processStackPrecond sai finfo fps'

-- | This analyzes the given function to determine what
inferStackPrecond :: ( RegisterInfo (ArchReg arch)
                       , Ord (AddrReg (ArchReg arch))
                       , FoldableFC (ArchFn arch)
                       , FoldableF (ArchStmt arch)
                       )
                  => StackArchInvariants arch
                  -> DiscoveryFunInfo arch ids
                  -> Either InferStackError (FunctionStackPrecond arch)
inferStackPrecond sai finfo =
  let entryAddr = discoveredFunAddr finfo
      entryPrecond = fnEntryStackRefs
      fps0 = FPS { fpsMap = Map.singleton entryAddr entryPrecond
                 , fpsQueue = [(entryAddr, entryPrecond)]
                 }
   in processStackPrecond sai finfo fps0
