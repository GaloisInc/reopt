{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Werror #-}
module Reopt.CFG.FunctionArgs
  ( functionDemands
  , inferFunctionTypeFromDemands
  , functionArgs
  , debugPrintMap
  ) where

import           Control.Lens
import           Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable as Fold (traverse_)
import           Data.List (intercalate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Parameterized.Classes
import           Data.Parameterized.Some
import           Data.Proxy
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Data.Macaw.Architecture.Syscall
import           Data.Macaw.CFG
import           Data.Macaw.Memory (Memory, MemWidth, IsAddr(..))
import           Data.Macaw.Types
import           Data.Macaw.Discovery.Info
import           Reopt.CFG.FnRep (FunctionType(..))
import           Reopt.Machine.X86State

-- -----------------------------------------------------------------------------

-- The algorithm computes the set of direct deps (i.e., from writes)
-- and then iterates, propagating back via the register deps.  It
-- doesn't compute assignment uses (although it could) mainly to keep
-- memory use down.  We recompute assignment use later in RegisterUse.
--
-- The basic question this analysis answers is: what arguments does a
-- function require, and what results does it produce?
--
-- There are 3 phases
-- 1. Block-local summarization
-- 2. Function-local summarization
-- 3. Global fixpoint calculation.
--
-- The first 2 phases calculate, for each function, the following information:
--
-- A. What registers are required by a function (ignoring function
--    calls)?
--
-- B. Given that result register {rax, rdx, xmm0} is demanded, what
--    extra register arguments are required, and what extra result
--    arguments are required?
--
-- C. Given that function f now requires argument r, what extra
--    arguments are required, and what extra result registers are
--    demanded?

type RegisterSet (r :: Type -> *) = Set (Some r)

-- | If we demand a register from a function (or block, for phase 1),
-- this results in both direct argument register demands and function
-- result demands.
data DemandSet (r :: Type -> *) =
    DemandSet { registerDemands       :: !(RegisterSet r)
              , functionResultDemands :: !(Map (SegmentedAddr (RegAddrWidth r)) (RegisterSet r))
              }

deriving instance (ShowF r) => Show (DemandSet r)
deriving instance (TestEquality r) => Eq (DemandSet r)
deriving instance (OrdF r) => Ord (DemandSet r)

instance OrdF r => Monoid (DemandSet r) where
  mempty = DemandSet { registerDemands = Set.empty
                     , functionResultDemands = mempty
                     }
  mappend ds1 ds2 =
    DemandSet { registerDemands = registerDemands ds1 `mappend` registerDemands ds2
              , functionResultDemands =
                  Map.unionWith Set.union (functionResultDemands ds1)
                                          (functionResultDemands ds2)
              }

demandSetDifference :: OrdF r => DemandSet r -> DemandSet r -> DemandSet r
demandSetDifference ds1 ds2 =
  DemandSet (registerDemands ds1 `Set.difference` registerDemands ds2)
            (Map.differenceWith setDiff (functionResultDemands ds1)
                                        (functionResultDemands ds2))
  where
    setDiff s1 s2 =
      let s' = s1 `Set.difference` s2
      in if Set.null s' then Nothing else Just s'

-- | The types of demands we can make
data DemandType r
  -- | We just want a local register (e.g., a subsequent block needs
  -- register rax)
  = DemandAlways
  -- | A function requires an additional argument
  | forall tp. DemandFunctionArg (SegmentedAddr (RegAddrWidth r)) (r tp)
  -- | The result of the current function.
  | forall tp. DemandFunctionResult (r tp)

instance ShowF r => Show (DemandType r) where
  showsPrec _ DemandAlways  = showString "DemandAlways"
  showsPrec p (DemandFunctionArg a r) = showParen (p >= 10) $
    showString "DemandFunctionArg " . shows a . showChar ' ' . showsF r
  showsPrec p (DemandFunctionResult r) = showParen (p >= 10) $
    showString "DemandFunctionResult " . showsF r

instance TestEquality r => Eq (DemandType r) where
  DemandAlways == DemandAlways = True
  (DemandFunctionArg faddr1 r1) == (DemandFunctionArg faddr2 r2) =
    faddr1 == faddr2 && isJust (testEquality r1 r2)
  (DemandFunctionResult r1) == (DemandFunctionResult r2) =
    isJust (testEquality r1 r2)
  _ == _ = False

instance OrdF r => Ord (DemandType r) where
  DemandAlways `compare` DemandAlways = EQ
  DemandAlways `compare` _  = LT
  _ `compare` DemandAlways  = GT

  (DemandFunctionArg faddr1 r1) `compare` (DemandFunctionArg faddr2 r2)
    | faddr1 == faddr2 = toOrdering (compareF r1 r2)
    | otherwise = faddr1 `compare` faddr2

  (DemandFunctionArg {}) `compare` _ = LT
  _ `compare` (DemandFunctionArg {}) = GT

  (DemandFunctionResult r1) `compare` (DemandFunctionResult r2) =
    toOrdering (compareF r1 r2)

type DemandMap r = Map (DemandType r) (DemandSet r)

demandMapUnion :: OrdF r => DemandMap r -> DemandMap r -> DemandMap r
demandMapUnion = Map.unionWith mappend

type AssignmentCache r ids = Map (Some (AssignId ids)) (RegisterSet r)

type ResultDemandsMap r = Map (Some r) (DemandSet r)

-- Generated by phase 1, used by phase 2.
data FunctionArgsState arch ids = FAS
  -- | Holds state about the set of registers that a block uses
  -- (required by this block).
  { _blockTransfer :: !(Map (ArchLabel arch) (ResultDemandsMap (ArchReg arch)))

  -- | If a demand d is demanded of block lbl then the block demands S, s.t.
  -- blockDemandMap ^. at lbl ^. at d = Just S
  , _blockDemandMap    :: !(Map (ArchLabel arch) (DemandMap (ArchReg arch)))

  -- | The list of predecessors for a given block
  , _blockPreds     :: !(Map (ArchLabel arch) [ArchLabel arch])
  -- | A cache of the assignments and their deps.  The key is not included
  -- in the set of deps (but probably should be).
  , _assignmentCache :: !(AssignmentCache (ArchReg arch) ids)

  -- | The set of blocks that we have already visited.
  , _visitedBlocks  :: !(Set (ArchSegmentedAddr arch))

  -- | The set of blocks we need to consider (should be disjoint from visitedBlocks)
  , _blockFrontier  :: ![ParsedBlockRegion arch ids]
  , funSyscallPersonality :: !(SyscallPersonality arch)
  }

blockTransfer :: Simple Lens (FunctionArgsState arch ids)
                             (Map (ArchLabel arch) (ResultDemandsMap (ArchReg arch)))
blockTransfer = lens _blockTransfer (\s v -> s { _blockTransfer = v })

blockDemandMap :: Simple Lens (FunctionArgsState arch ids)
                    (Map (ArchLabel arch) (DemandMap (ArchReg arch)))
blockDemandMap = lens _blockDemandMap (\s v -> s { _blockDemandMap = v })

blockPreds :: Simple Lens (FunctionArgsState arch ids) (Map (ArchLabel arch) [ArchLabel arch])
blockPreds = lens _blockPreds (\s v -> s { _blockPreds = v })

assignmentCache :: Simple Lens (FunctionArgsState arch ids) (AssignmentCache (ArchReg arch) ids)
assignmentCache = lens _assignmentCache (\s v -> s { _assignmentCache = v })

-- |The set of blocks that we have already visited or added to frontier
visitedBlocks :: Simple Lens (FunctionArgsState arch ids) (Set (ArchSegmentedAddr arch))
visitedBlocks = lens _visitedBlocks (\s v -> s { _visitedBlocks = v })

blockFrontier :: Simple Lens (FunctionArgsState arch ids) [ParsedBlockRegion arch ids]
blockFrontier = lens _blockFrontier (\s v -> s { _blockFrontier = v })

initFunctionArgsState :: SyscallPersonality arch -> FunctionArgsState arch ids
initFunctionArgsState sysp =
  FAS { _blockTransfer     = Map.empty
      , _blockDemandMap    = Map.empty
      , _blockPreds        = Map.empty
      , _assignmentCache   = Map.empty
      , _visitedBlocks     = Set.empty
      , _blockFrontier     = []
      , funSyscallPersonality = sysp
      }

-- ----------------------------------------------------------------------------------------

type FunctionArgsM arch ids a = State (FunctionArgsState arch ids) a

-- ----------------------------------------------------------------------------------------
-- Phase one functions

type ArchConstraints arch =
  ( OrdF (ArchReg arch)
  , ShowF (ArchReg arch)
  , PrettyArch arch
  )

-- | This registers a block in the first phase (block discovery).
addEdge :: ArchConstraints arch
        => DiscoveryInfo arch ids
        -> ArchLabel arch
        -> ArchSegmentedAddr arch
        -> FunctionArgsM arch ids ()
addEdge interp_state src_block dest_addr = do  -- record the edge
  let dest = mkRootBlockLabel dest_addr
  blockPreds %= Map.insertWith (++) dest [src_block]
  visited <- use visitedBlocks
  when (Set.notMember dest_addr visited) $ do
    visitedBlocks %= Set.insert dest_addr
    case Map.lookup dest_addr (interp_state^.parsedBlocks) of
      Just dest_reg -> blockFrontier %= (dest_reg:)
      Nothing -> error $ show $
        text "Could not find target block" <+> text (show dest_addr) <$$>
        indent 2 (text "Source:" <$$> pretty src_block)

valueUses :: (OrdF (ArchReg arch), CanFoldValues arch)
          => Value arch ids tp
          -> FunctionArgsM arch ids (RegisterSet (ArchReg arch))
valueUses v =
  zoom assignmentCache $
            foldValueCached (\_ _    -> mempty)
                            (\_      -> mempty)
                            (\r      -> Set.singleton (Some r))
                            (\_ regs -> regs)
                            v

-- Figure out the deps of the given registers and update the state for the current label
recordPropagation :: ( Ord a
                     , OrdF (ArchReg arch)
                     , CanFoldValues arch
                     )
                  => Simple Lens (FunctionArgsState arch ids)
                                 (Map (ArchLabel arch) (Map a (DemandSet (ArchReg arch))))
                  -> ArchLabel arch
                  -> RegState (ArchReg arch) (Value arch ids)
                  -> (forall tp . ArchReg arch tp -> a)
                  -> [Some (ArchReg arch)]
                  -> FunctionArgsM arch ids () -- Map (Some N.RegisterName) RegDeps
recordPropagation l lbl s mk rs = do
  let doReg (Some r) = do
        rs' <- valueUses (s ^. boundValue r)
        return (mk r, DemandSet rs' mempty)
  vs <- mapM doReg rs
  l %= Map.insertWith (Map.unionWith mappend) lbl (Map.fromListWith mappend vs)

-- | A block requires a value, and so we need to remember which
-- registers are required.
demandValue :: (OrdF (ArchReg arch), CanFoldValues arch)
            => ArchLabel arch
            -> Value arch ids tp
            -> FunctionArgsM arch ids ()
demandValue lbl v = do
  regs <- valueUses v
  blockDemandMap %= Map.insertWith demandMapUnion lbl
                        (Map.singleton DemandAlways (DemandSet regs mempty))

-- -----------------------------------------------------------------------------
-- Entry point

type ArgDemandsMap r
   = Map (SegmentedAddr (RegAddrWidth r))
         (Map (Some r) (Map (SegmentedAddr (RegAddrWidth r)) (DemandSet r)))

type FunctionArgState r =
  ( ArgDemandsMap r
  , Map (SegmentedAddr (RegAddrWidth r)) (ResultDemandsMap r)
  , Map (SegmentedAddr (RegAddrWidth r)) (DemandSet r)
  )

-- PERF: we can calculate the return types as we go (instead of doing
-- so at the end).
calculateGlobalFixpoint :: OrdF r
                        => ArgDemandsMap r
                        -> Map (SegmentedAddr (RegAddrWidth r)) (ResultDemandsMap r)
                        -> Map (SegmentedAddr (RegAddrWidth r)) (DemandSet r)
                        -> Map (SegmentedAddr (RegAddrWidth r)) (DemandSet r)
calculateGlobalFixpoint argDemandsMap resultDemandsMap argsMap
  = go argsMap argsMap
  where
    go acc new
      | Just ((fun, newDemands), rest) <- Map.maxViewWithKey new =
          let (nexts, acc') = backPropagate acc fun newDemands
          in go acc' (Map.unionWith mappend rest nexts)
      | otherwise = acc

    backPropagate acc fun (DemandSet regs rets) =
      -- We need to push rets through the corresponding functions, and
      -- notify all functions which call fun regs.
      let goRet addr retRegs =
            mconcat [ resultDemandsMap ^. ix addr ^. ix r | r <- Set.toList retRegs ]
          retDemands = Map.mapWithKey goRet rets

          regsDemands =
            Map.unionsWith mappend [ argDemandsMap ^. ix fun ^. ix r | r <- Set.toList regs ]

          newDemands = Map.unionWith mappend regsDemands retDemands

          -- All this in newDemands but not in acc
          novelDemands = Map.differenceWith diff newDemands acc
      in (novelDemands, Map.unionWith mappend acc novelDemands )

    diff ds1 ds2 =
        let ds' = ds1 `demandSetDifference` ds2 in
        if ds' == mempty then Nothing else Just ds'

transferDemands :: OrdF r
                => Map (Some r) (DemandSet r)
                -> DemandSet r
                -> DemandSet r
transferDemands xfer (DemandSet regs funs) =
  -- Using ix here means we ignore any registers we don't know about,
  -- e.g. caller-saved registers after a function call.
  -- FIXME: is this the correct behavior?
  mconcat (DemandSet mempty funs : [ xfer ^. ix r | r <- Set.toList regs ])

class CanDemandValues arch where

  -- | Return the arguments expected by a function
  functionArgRegs :: p arch -> [Some (ArchReg arch)]

  -- | Return the registers returned by a function
  functionRetRegs :: p arch -> [Some (ArchReg arch)]

  -- | List of callee saved registers in function calls.
  calleeSavedRegs :: p arch -> Set (Some (ArchReg arch))

  -- | Return values that must be evaluated to execute this statement
  demandedArchStmtValues :: ArchStmt arch ids -> [Some (Value arch ids)]

-- A function call is the only block type that results in the
-- generation of function call demands, so we split that aspect out
-- (callee saved are handled in summarizeBlock).
summarizeCall :: forall arch ids
              .  ( CanDemandValues arch
                 , CanFoldValues arch
                 , RegisterInfo (ArchReg arch)
                 , IsAddr (ArchAddrWidth arch)
                 )
              => Memory (ArchAddrWidth arch)
              -> ArchLabel arch
              -> RegState (ArchReg arch) (Value arch ids)
              -> Bool
              -> FunctionArgsM arch ids ()
summarizeCall mem lbl proc_state isTailCall = do
  case asLiteralAddr mem (proc_state^.boundValue ip_reg) of
    Just faddr -> do
      -- If a subsequent block demands r, then we note that we want r from
      -- function faddr
      -- FIXME: refactor out Some s
      let retRegs = functionRetRegs (Proxy :: Proxy arch)
      -- singleton for now, but propagating back will introduce more deps.
      let demandSet sr         = DemandSet mempty (Map.singleton faddr (Set.singleton sr))
      if isTailCall then
        -- tail call, propagate demands for our return regs to the called function
        let propMap = map (\(Some r) -> (DemandFunctionResult r, demandSet (Some r))) retRegs
         in  blockDemandMap %= Map.insertWith (Map.unionWith mappend) lbl (Map.fromList propMap)
       else do
        let propResult :: Some (ArchReg arch) -> FunctionArgsM arch ids ()
            propResult sr = do
              let srDemandSet = Map.singleton sr (demandSet sr)
              blockTransfer %= Map.insertWith (Map.unionWith mappend) lbl srDemandSet
        traverse_ propResult retRegs

      -- If a function wants argument register r, then we note that this
      -- block needs the corresponding state values.  Note that we could
      -- do this for _all_ registers, but this should make the summaries somewhat smaller.
      let argRegs = functionArgRegs (Proxy :: Proxy arch)
      recordPropagation blockDemandMap lbl proc_state (DemandFunctionArg faddr) argRegs
    Nothing -> do
      -- In the dynamic case, we just assume all arguments (FIXME: results?)
      let argRegs = [Some ip_reg] ++ functionArgRegs (Proxy :: Proxy arch)
      recordPropagation blockDemandMap lbl proc_state (\_ -> DemandAlways) argRegs


demandStmtValues :: (OrdF (ArchReg arch), CanDemandValues arch, CanFoldValues arch)
       => ArchLabel arch
       -> Stmt arch ids
       -> FunctionArgsM arch ids ()
demandStmtValues lbl (WriteMem addr v) = do
  demandValue lbl addr
  demandValue lbl v
demandStmtValues lbl (ExecArchStmt stmt) =
  mapM_ (\(Some v) -> demandValue lbl v) (demandedArchStmtValues stmt)
demandStmtValues _ _ = return ()

type SummarizeConstraints arch ids
  = ( CanDemandValues arch
    , CanFoldValues arch
    , PrettyArch arch
    , ArchConstraint arch ids
    , MemWidth (ArchAddrWidth arch)
    , Show (ArchReg arch (BVType (ArchAddrWidth arch)))
    )

-- | This function figures out what the block requires
-- (i.e., addresses that are stored to, and the value stored), along
-- with a map of how demands by successor blocks map back to
-- assignments and registers.
summarizeBlock :: forall arch ids
               .  SummarizeConstraints arch ids
               => DiscoveryInfo arch ids
               -> ParsedBlockRegion arch ids
               -> Word64 -- ^ Index of region
               -> FunctionArgsM arch ids ()
summarizeBlock interp_state reg idx = do
  let addr = regionAddr reg
  b <-
    case Map.lookup idx (regionBlockMap reg) of
      Just b -> pure b
      Nothing ->
        error $ "summarizeBlock asked to find block with bad index " ++ show idx
          ++ " in " ++ show addr ++ "\n"
          ++ show reg


  let lbl = GeneratedBlock addr idx
  -- By default we have no arguments, return nothing
  blockDemandMap %= Map.insertWith demandMapUnion lbl mempty

        -- FIXME: rsp here?
  let callRegs = [Some sp_reg]
                 ++ Set.toList (calleeSavedRegs (Proxy :: Proxy arch))
  let recordCallPropagation proc_state =
        recordPropagation blockTransfer lbl proc_state Some callRegs
  traverse_ (demandStmtValues lbl) (pblockStmts b)
  case pblockTerm b of
    ParsedTranslateError _ ->
      error "Cannot identify arguments in code where translation error occurs"
    ClassifyFailure _ ->
      error $ "Classification failed: " ++ show addr
    ParsedBranch c x y -> do
      demandValue lbl c
      summarizeBlock interp_state reg x
      summarizeBlock interp_state reg y

    ParsedCall proc_state m_ret_addr -> do
      summarizeCall (memory interp_state) lbl proc_state (not $ isJust m_ret_addr)
      case m_ret_addr of
        Nothing       -> return ()
        Just ret_addr -> do
          addEdge interp_state lbl ret_addr
          recordCallPropagation proc_state

    ParsedJump proc_state tgt_addr -> do
      -- record all propagations
      recordPropagation blockTransfer lbl proc_state Some archRegs
      addEdge interp_state lbl tgt_addr

    ParsedReturn proc_state -> do
      recordPropagation blockDemandMap lbl proc_state DemandFunctionResult $
          (functionRetRegs (Proxy :: Proxy arch))

    ParsedSyscall proc_state next_addr -> do
      sysp <- gets funSyscallPersonality
      -- FIXME: we ignore the return type for now, probably not a problem.
      do let syscallRegs :: [ArchReg arch (BVType (ArchAddrWidth arch))]
             syscallRegs = syscallArgumentRegs
         let argRegs
               | Just call_no <- tryGetStaticSyscallNo interp_state (labelAddr lbl) proc_state
               , Just (_,_,argtypes) <- Map.lookup (fromIntegral call_no) (spTypeInfo sysp) =
                   take (length argtypes) syscallRegs
               | otherwise =
                   syscallRegs

         recordPropagation blockDemandMap lbl proc_state (\_ -> DemandAlways) (Some <$> argRegs)

      recordCallPropagation proc_state
      addEdge interp_state lbl next_addr

    ParsedLookupTable proc_state lookup_idx vec -> do
      demandValue lbl lookup_idx
      -- record all propagations
      recordPropagation blockTransfer lbl proc_state Some archRegs
      traverse_ (addEdge interp_state lbl) vec

-- | Explore states until we have reached end of frontier.
summarizeIter :: SummarizeConstraints arch ids
              => DiscoveryInfo arch ids
              -> FunctionArgsM arch ids ()
summarizeIter ist = do
  fnFrontier <- use blockFrontier
  case fnFrontier of
    [] ->
      return ()
    reg : frontier' -> do
      blockFrontier .= frontier'
      summarizeBlock ist reg 0
      summarizeIter ist

calculateLocalFixpoint :: forall arch ids
                       .  OrdF (ArchReg arch)
                       => Map (ArchLabel arch) (DemandMap (ArchReg arch))
                       -> FunctionArgsM arch ids ()
calculateLocalFixpoint new
  | Just ((currLbl, newDemands), rest) <- Map.maxViewWithKey new =
      -- propagate backwards any new demands to the predecessors
      do preds <- use (blockPreds . ix (rootBlockLabel currLbl))
         nexts <- filter (not . Map.null . snd) <$> mapM (doOne newDemands) preds
         calculateLocalFixpoint (Map.unionWith demandMapUnion rest
                                 (Map.fromListWith demandMapUnion nexts))
  | otherwise = return ()
  where
    doOne :: DemandMap (ArchReg arch)
          -> ArchLabel arch
          -> FunctionArgsM arch ids (ArchLabel arch, DemandMap (ArchReg arch))
    doOne newDemands predLbl = do
      xfer   <- use (blockTransfer . ix predLbl)

      let demands' = transferDemands xfer <$> newDemands
          lbl' = rootBlockLabel predLbl

      -- update uses, returning value before this iteration
      seenDemands <- use (blockDemandMap . ix lbl')
      blockDemandMap . at lbl' .= Just (Map.unionWith mappend demands' seenDemands)
      -- seenDemands <- blockDemandMap . ix lbl' <<%= demandMapUnion demands'

      return (lbl', Map.differenceWith diff demands' seenDemands)

    diff ds1 ds2 =
        let ds' = ds1 `demandSetDifference` ds2 in
        if ds' == mempty then Nothing else Just ds'


decomposeMap :: OrdF r
             => DemandSet r
             -> SegmentedAddr (RegAddrWidth r)
             -> FunctionArgState r
             -> DemandType r
             -> DemandSet r
             -> FunctionArgState r
decomposeMap _ addr acc (DemandFunctionArg f r) v =
  -- FIXME: A bit of an awkward datatype ...
  let m = Map.singleton (Some r) (Map.singleton addr v)
   in acc & _1 %~ Map.insertWith (Map.unionWith (Map.unionWith mappend)) f m
decomposeMap _ addr acc (DemandFunctionResult r) v =
  acc & _2 %~ Map.insertWith (Map.unionWith mappend) addr (Map.singleton (Some r) v)
-- Strip out callee saved registers as well.
decomposeMap ds addr acc DemandAlways v =
  acc & _3 %~ Map.insertWith mappend addr (v `demandSetDifference` ds)

-- This function computes the following 3 pieces of information:
-- 1. Initial function arguments (ignoring function calls)
-- 2. Function arguments to function arguments
-- 3. Function results to function arguments.
doOneFunction :: forall arch ids
              .  SummarizeConstraints arch ids
              => SyscallPersonality arch
              -> DiscoveryInfo arch ids
              -> FunctionArgState (ArchReg arch)
              -> SegmentedAddr (ArchAddrWidth arch)
              -> FunctionArgState (ArchReg arch)
doOneFunction sysp ist acc addr =
  flip evalState (initFunctionArgsState sysp) $ do
    let lbl0 = mkRootBlockLabel addr
    -- Run the first phase (block summarization)
    visitedBlocks .= Set.singleton addr
    case Map.lookup addr (ist^.parsedBlocks) of
      Just b -> blockFrontier .= [b]
      Nothing -> error $ "Could not find initial block for " ++ show addr

    summarizeIter ist
    -- propagate back uses
    new <- use blockDemandMap

    -- debugM DFunctionArgs (">>>>>>>>>>>>>>>>>>>>>>>>" ++ (showHex addr "" ))
    -- debugM' DFunctionArgs (ppMap (text . show) (ppMap (text . show) (text . show)) new)
    -- debugM DFunctionArgs ("------------------------" ++ (showHex addr "" ))
    -- xfer <- use blockTransfer
    -- debugM' DFunctionArgs (ppMap (text . show) (ppMap (text . show) (text . show)) xfer)

    calculateLocalFixpoint new
    -- summary for entry block has what we want.
    -- m <- use (blockDemandMap . ix lbl0)
    -- debugM DFunctionArgs ("*************************"  ++ (showHex addr "" ))
    -- debugM' DFunctionArgs (ppMap (text . show) (text . show) m)
    -- debugM DFunctionArgs ("<<<<<<<<<<<<<<<<<<<<<<<<<" ++ (showHex addr "" ))

    funDemands <- use (blockDemandMap . ix lbl0)

    let proxy :: Proxy arch
        proxy = Proxy

    -- A function may demand a callee saved register as it will store
    -- it onto the stack in order to use it later.  This will get
    -- recorded as a use, which is erroneous, so we strip out any
    -- reference to them here.
    let calleeDemandSet = DemandSet { registerDemands =
                                        Set.insert (Some sp_reg) (calleeSavedRegs proxy)
                                    , functionResultDemands = mempty
                                    }

    return (Map.foldlWithKey' (decomposeMap calleeDemandSet addr) acc funDemands)


-- | Returns the set of argument registers and result registers for each function.
functionDemands :: forall arch ids
                .  SummarizeConstraints arch ids
                => SyscallPersonality arch
                -> DiscoveryInfo arch ids
                -> [ArchSegmentedAddr arch]
                -> Map (SegmentedAddr (ArchAddrWidth arch)) (DemandSet (ArchReg arch))
functionDemands sysp ist entries =
    calculateGlobalFixpoint argDemandsMap resultDemandsMap argsMap
  where
    (argDemandsMap, resultDemandsMap, argsMap)
      = foldl (doOneFunction sysp ist) mempty entries




instance CanDemandValues X86_64 where

  functionArgRegs _
    = [Some rax_reg]
    ++ (Some <$> x86ArgumentRegs)
    ++ (Some <$> x86FloatArgumentRegs)

  functionRetRegs _ = ((Some <$> x86ResultRegs) ++ (Some <$> x86FloatResultRegs))

  calleeSavedRegs _ = x86CalleeSavedRegs

  demandedArchStmtValues stmt =
    case stmt of
      MemCopy _sz cnt src dest rev -> [ Some cnt, Some src, Some dest, Some rev ]
      MemSet cnt v ptr df -> [ Some cnt, Some v, Some ptr, Some df ]
      _ -> []

inferFunctionTypeFromDemands :: Map (SegmentedAddr 64) (DemandSet X86Reg)
                             -> Map (SegmentedAddr 64) FunctionType
inferFunctionTypeFromDemands dm =
  let go ds m = Map.unionWith Set.union (functionResultDemands ds) m
      retDemands = foldr go Map.empty dm

      -- drop the suffix which isn't a member of the arg set.  This
      -- allows e.g. arg0, arg2 to go to arg0, arg1, arg2.
      maximumArgPrefix :: [X86Reg tp] -> RegisterSet X86Reg -> Int
      maximumArgPrefix regs rs =
        length $ dropWhile (not . (`Set.member` rs) . Some) $ reverse regs

      -- Turns a set of arguments into a prefix of x86ArgumentRegisters and friends
      orderPadArgs :: (RegisterSet X86Reg, RegisterSet X86Reg) -> FunctionType
      orderPadArgs (args, rets) =
        FunctionType (maximumArgPrefix x86ArgumentRegs args)
                     (maximumArgPrefix x86FloatArgumentRegs args)
                     (maximumArgPrefix x86ResultRegs rets)
                     (maximumArgPrefix x86FloatResultRegs rets)
  in fmap orderPadArgs
     $ Map.mergeWithKey (\_ ds rets -> Just (registerDemands ds, rets))
                        (fmap (\ds ->  (registerDemands ds, mempty)))
                        (fmap ((,) mempty))
                        dm
                        retDemands

-- | Returns the set of argument registers and result registers for each function.
functionArgs :: SyscallPersonality X86_64
             -> DiscoveryInfo X86_64 ids
             -> [SegmentedAddr 64]
             -> Map (SegmentedAddr 64) FunctionType
functionArgs sysp info = inferFunctionTypeFromDemands . functionDemands sysp info

debugPrintMap :: DiscoveryInfo X86_64 ids -> Map (SegmentedAddr 64) FunctionType -> String
debugPrintMap ist m = "Arguments: \n\t" ++ intercalate "\n\t" (Map.elems comb)
  where -- FIXME: ignores those functions we don't have names for.
        comb = Map.intersectionWith doOne (symbolNames ist) m
        doOne n ft = BSC.unpack n ++ ": " ++ show (pretty ft)
