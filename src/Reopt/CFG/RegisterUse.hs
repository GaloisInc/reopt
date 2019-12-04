{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Reopt.CFG.RegisterUse
  ( FunPredMap
  , funBlockPreds
  , RegisterUseContext(..)
  , FunctionTypeRegs(..)
  , registerUse
  , ppBlockDependencySetMap
  , AssignStackOffsetMap
  , valueStackOffset
  , DependencySet(..)
  , StmtIndex
    -- * Monadic implementation
  , RegisterUseM
  , demandValue
  , addRegisterUses
  , clearDependencySet
  ) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Foldable
import           Data.Kind
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Parameterized
import qualified Data.Parameterized.Map as MapF
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Stack
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Data.Macaw.AbsDomain.JumpBounds
import           Data.Macaw.CFG.DemandSet
  ( DemandContext
  , demandConstraints
  , archFnHasSideEffects
  )
import           Data.Macaw.CFG
import           Data.Macaw.Discovery.State
import           Data.Macaw.Types hiding (Type)
import qualified Data.Macaw.Types as M

-------------------------------------------------------------------------------
-- funBlockPreds

-- | A map from each address `l` to the addresses of blocks that may
-- jump to `l`.
type FunPredMap w = Map (MemSegmentOff w) [MemSegmentOff w]

-- | Return the FunPredMap for the discovered block function.
funBlockPreds :: DiscoveryFunInfo arch ids -> FunPredMap (ArchAddrWidth arch)
funBlockPreds info = Map.fromListWith (++)
  [ (next, [addr])
  | b <- Map.elems (info^.parsedBlocks)
    -- Get address of region
  , let addr = pblockAddr b
    -- get the block successors
  , next <- parsedTermSucc (pblockTermStmt b)
  ]

-------------------------------------------------------------------------------
-- StackOffset information

-- | Map from assignment ids to stack offseets
type AssignStackOffsetMap w ids = Map (Some (AssignId ids)) (MemInt w)

-- | Return offset of value if it is a stack offset, and @Nothing@ if
-- value is not expected to point to stack.
valueStackOffset :: ( MemWidth (ArchAddrWidth arch)
                    , OrdF (ArchReg arch)
                    )
                 => InitJumpBounds arch
                 -> AssignStackOffsetMap (ArchAddrWidth arch) ids
                 -> Value arch ids (BVType (ArchAddrWidth arch))
                 -> Maybe (MemInt (ArchAddrWidth arch))
valueStackOffset bnds amap v =
  case v of
    CValue{} ->
      Nothing
    Initial r ->
      case boundsLocationInfo bnds (RegLoc r) of
        (_rep, IsStackOffset o) -> Just o
        _ -> Nothing
    AssignedValue (Assignment aid _) ->
      Map.lookup (Some aid) amap

-------------------------------------------------------------------------------
-- StmtIndex

-- | Index of a stmt in a block.
type StmtIndex = Int

-------------------------------------------------------------------------------
-- DependencySet

-- | This records what assignments and initial value locations are
-- needed to compute a value or execute code in a block with side
-- effects.
data DependencySet (r :: M.Type -> Type) ids =
  DepSet { dsLocSet :: !(Set (Some (BoundLoc r)))
           -- ^ Set of locations that block reads the initial
           -- value of.
         , dsAssignSet :: !(Set (Some (AssignId ids)))
           -- ^ Set of assignments that must be executed.
         , dsWriteStmtIndexSet :: !(Set StmtIndex)
           -- ^ Block start address and index of write statement that
           -- writes a value to the stack that is read later.
         }

ppSet :: (a -> Doc) -> Set a -> Doc
ppSet f s = encloseSep lbrace rbrace comma (f <$> Set.toList s)

ppSomeAssignId :: Some (AssignId ids) -> Doc
ppSomeAssignId (Some aid) = text (show aid)

ppSomeBoundLoc :: MapF.ShowF r => Some (BoundLoc r) -> Doc
ppSomeBoundLoc (Some loc) = pretty loc

instance MapF.ShowF r => Pretty (DependencySet r ids) where
  pretty ds =
    vcat [ text "Assignments:" <+> ppSet ppSomeAssignId (dsAssignSet ds)
         , text "Locations:  " <+> ppSet ppSomeBoundLoc (dsLocSet ds)
         , text "Write Stmts:" <+> ppSet pretty (dsWriteStmtIndexSet ds)
         ]

-- | Empty dependency set.
emptyDeps :: DependencySet r ids
emptyDeps =
  DepSet { dsLocSet = Set.empty
         , dsAssignSet = Set.empty
         , dsWriteStmtIndexSet = Set.empty
         }

-- | Dependency set for a single assignment
assignSet :: AssignId ids tp -> DependencySet r ids
assignSet aid =
  DepSet { dsLocSet = Set.empty
         , dsAssignSet = Set.singleton (Some aid)
         , dsWriteStmtIndexSet = Set.empty
         }

-- | Create a dependency set for a single location.
locDepSet :: BoundLoc r tp -> DependencySet r ids
locDepSet l =
  DepSet { dsLocSet = Set.singleton (Some l)
         , dsAssignSet = Set.empty
         , dsWriteStmtIndexSet = Set.empty
         }

-- | @addWriteDep stmtIdx
addWriteDep :: StmtIndex -> DependencySet r ids -> DependencySet r ids
addWriteDep idx s = seq idx $
  s { dsWriteStmtIndexSet = Set.insert idx (dsWriteStmtIndexSet s) }

instance MapF.OrdF r => Semigroup (DependencySet r ids) where
  x <> y = DepSet { dsAssignSet = Set.union (dsAssignSet x) (dsAssignSet y)
                  , dsLocSet = Set.union (dsLocSet x) (dsLocSet y)
                  , dsWriteStmtIndexSet =
                      Set.union (dsWriteStmtIndexSet x) (dsWriteStmtIndexSet y)
                  }

instance MapF.OrdF r => Monoid (DependencySet r ids) where
  mempty = emptyDeps

------------------------------------------------------------------------
--

type AssignmentCache r ids = Map (Some (AssignId ids)) (DependencySet r ids)

-- | This maps each location that could be accessed after a block
-- terminates to the set of values needed to compute the value of the
-- location.
type BlockProvideDepMap r ids = LocMap r (Const (DependencySet r ids))

emptyBlockProvideDepMap :: BlockProvideDepMap r ids
emptyBlockProvideDepMap = locMapEmpty

-- | Get dependency set for location.
--
-- Note. This code is currently buggy in that it will back propagate
-- stack reads that are partially overwritten.
getLocDependencySet :: (MapF.OrdF r, MemWidth (RegAddrWidth r))
                    => BlockProvideDepMap r ids
                    -> BoundLoc r tp
                    -> DependencySet r ids
getLocDependencySet srcDepMap l =
  case locLookup l srcDepMap of
    Nothing -> locDepSet l
    Just (Const s) -> s

addLocDependency :: (MapF.OrdF r, MemWidth (RegAddrWidth r))
                 => BoundLoc r tp
                 -> DependencySet r ids
                 -> BlockProvideDepMap r ids
                 -> BlockProvideDepMap r ids
addLocDependency l ds m =
  locOverwriteWith (\(Const n) (Const o) -> Const (mappend n o)) l (Const ds) m

------------------------------------------------------------------------
-- FunctionTypeRegs

-- | This indicates which registers a function is expected to
-- read/write.
data FunctionTypeRegs (r :: M.Type -> Type) =
  FunctionTypeRegs { fnArgRegs :: [Some r]
                   , fnReturnRegs :: [Some r]
                   }
  deriving (Ord, Eq, Show)

------------------------------------------------------------------------
-- RegisterUseM

type ArchTermStmtSummarizationFn arch ids
  = ArchTermStmt arch ids
  -> RegState (ArchReg arch) (Value arch ids)
  -> RegisterUseM arch ids ()

-- | Information about architecture, rest of the program, and current
-- function needed to determine register usage.
data RegisterUseContext arch ids
  = RegisterUseContext
  { -- | Map function entry points to function type above.
    functionArgs    :: !(Map (MemSegmentOff (ArchAddrWidth arch)) (FunctionTypeRegs (ArchReg arch)))
    -- | Registers to use for indirect calls or targets not in list.
  , defaultCallRegs :: !(FunctionTypeRegs (ArchReg arch))
    -- | Return registers demanded by this function
  , returnRegisters :: ![Some (ArchReg arch)]
    -- | List of registers that calls must preserve.
  , calleeSavedRegisters :: ![Some (ArchReg arch)]
    -- | List of registers that callers may freely change.
  , callScratchRegisters :: ![Some (ArchReg arch)]
    -- | Callback function for summarizing register usage of terminal
    -- statements.
  , summarizeTermFn :: !(ArchTermStmtSummarizationFn arch ids)
    -- | Demand context for registers
  , demandContext :: !(DemandContext arch)
  }

-- | This contains information about a specific block needed to infer
-- which locations and assignments are needed to execute the block
-- along with information about the demands to compute the value of
-- particular locations after the block executes.
data BlockUsageSummary (arch :: Type) ids = RUS
  { -- | Information about stack layout/jump bounds at start of block.
    blockPrecond :: !(InitJumpBounds arch)
    -- | Maps locations to the dependencies needed to compute values in that location.
  , _blockInitDeps  :: !(BlockProvideDepMap (ArchReg arch) ids)
    -- | Dependencies needed to execute statements with side effects.
  , _blockExecDemands :: !(DependencySet (ArchReg arch) ids)
    -- | Maps assignments to their dependencies.
  , _assignmentCache :: !(Map (Some (AssignId ids)) (DependencySet (ArchReg arch) ids))
    -- | Map assignment ids that point to the current stack frame to
    -- the stack offset.
  , _assignStackOffset :: !(AssignStackOffsetMap (ArchAddrWidth arch) ids)
  }

type RegisterUseM arch ids a =
  ReaderT (RegisterUseContext arch ids) (StateT (BlockUsageSummary arch ids) (Except String)) a

initBlockUsageSummary :: InitJumpBounds arch -> BlockUsageSummary arch ids
initBlockUsageSummary bnds =
  RUS { blockPrecond       = bnds
      , _blockInitDeps     = emptyBlockProvideDepMap
      , _blockExecDemands  = emptyDeps
      , _assignmentCache   = Map.empty
      , _assignStackOffset = Map.empty
      }

blockInitDeps :: Lens' (BlockUsageSummary arch ids) (BlockProvideDepMap (ArchReg arch) ids)
blockInitDeps = lens _blockInitDeps (\s v -> s { _blockInitDeps = v })

blockExecDemands :: Lens' (BlockUsageSummary arch ids) (DependencySet (ArchReg arch) ids)
blockExecDemands = lens _blockExecDemands (\s v -> s { _blockExecDemands = v })

assignmentCache :: Lens' (BlockUsageSummary arch ids) (AssignmentCache (ArchReg arch) ids)
assignmentCache = lens _assignmentCache (\s v -> s { _assignmentCache = v })

-- ----------------------------------------------------------------------------------------
-- Phase one functions
-- ----------------------------------------------------------------------------------------

-- | Return the register and assignment dependencies needed to
valueUses :: (HasCallStack, OrdF (ArchReg arch))
          => Map (Some (AssignId ids)) (DependencySet (ArchReg arch) ids)
          -> Value arch ids tp
          -> DependencySet (ArchReg arch) ids
valueUses _ (CValue _) = emptyDeps
valueUses _ (Initial r) = locDepSet (RegLoc r)
valueUses m (AssignedValue (Assignment a _)) =
  case Map.lookup (Some a) m of
    Nothing -> error $ "Assignment " ++ show a ++ " is not defined."
    Just r -> r

-- | Record the given dependencies are needed to execute this block.
addDeps :: (HasCallStack, OrdF (ArchReg arch))
        => DependencySet (ArchReg arch) ids
        -> RegisterUseM arch ids ()
addDeps deps = blockExecDemands %= mappend deps

-- | Record the values needed to compute the given value.
demandValue :: (HasCallStack, OrdF (ArchReg arch))
            => Value arch ids tp
            -> RegisterUseM arch ids ()
demandValue v = do
  depCache <- gets _assignmentCache
  addDeps (valueUses depCache v)

-- | Return the values bound to the given registers
registerValues :: (OrdF (ArchReg arch), Functor t)
               => RegState (ArchReg arch) (Value arch ids)
               -> t (Some (ArchReg arch))
               -> t (Some (Value arch ids))
registerValues regs = fmap (\(Some r) -> Some (regs^.boundValue r))

-- | Figure out the deps of the given registers and update the state
-- for the current label
addRegisterUses :: forall arch ids
                .  (MemWidth (ArchAddrWidth arch), OrdF (ArchReg arch))
                => RegState (ArchReg arch) (Value arch ids)
                   -- ^ Register values when block terminates.
                -> [Some (ArchReg arch)]
                   -- ^ List of registers that future blocks are allowed
                   -- to depend on.
                -> RegisterUseM arch ids ()
addRegisterUses regs rs = do
  depCache <- gets _assignmentCache
  let insReg :: BlockProvideDepMap (ArchReg arch) ids
             -> Some (ArchReg arch)
             -> BlockProvideDepMap (ArchReg arch) ids
      insReg m (Some r) = addLocDependency (RegLoc r) (valueUses depCache (regs^.boundValue r)) m
  blockInitDeps %= \m -> foldl' insReg m rs

-- | Mark the given register has no dependencies
clearDependencySet :: (MemWidth (ArchAddrWidth arch), OrdF (ArchReg arch))
                   => Some (ArchReg arch)
                   -> RegisterUseM arch ids ()
clearDependencySet (Some r) = blockInitDeps %= locOverwriteWith (\n _ -> n) (RegLoc r) (Const mempty)

-- | Set dependencies for an assignment whose right-hand-side must be
-- evaluated for side effects.
requiredAssignDeps :: OrdF (ArchReg arch)
                   => AssignId ids tp
                   -> DependencySet (ArchReg arch) ids
                   -> RegisterUseM arch ids ()
requiredAssignDeps aid deps = do
  addDeps $ deps
  assignmentCache %= Map.insert (Some aid) mempty

-- | Return values that must be evaluated to execute side effects.
demandStmtValues :: ( HasCallStack
                    , OrdF (ArchReg arch)
                    , MemWidth (ArchAddrWidth arch)
                    )
                 => StmtIndex -- ^ Index of statement we are processing.
                 -> Stmt arch ids
                    -- ^ Statement we want to demand.
                 -> RegisterUseM arch ids ()
demandStmtValues stmtIdx stmt = do
 ctx <- asks demandContext
 demandConstraints ctx $ do
  case stmt of
    AssignStmt (Assignment aid arhs) -> do
      case arhs of
        EvalApp app -> do
          bnds <- gets blockPrecond
          amap <- gets _assignStackOffset
          let stackFn v = toInteger <$> valueStackOffset bnds amap v
          case appAsStackOffset stackFn app of
            Just (StackOffsetView o) -> do
              assignmentCache %= Map.insert (Some aid) mempty
              modify $ \s ->
                s { _assignStackOffset =
                      Map.insert (Some aid)
                                 (fromInteger o)
                                 (_assignStackOffset s)
                  }
            Nothing -> do
              depCache <- gets _assignmentCache
              let deps = foldlFC' (\s v -> mappend s (valueUses depCache v))
                                  (assignSet aid)
                                  app
              assignmentCache %= Map.insert (Some aid) deps
        SetUndefined{} -> do
          assignmentCache %= Map.insert (Some aid) (assignSet aid)
        ReadMem addr repr -> do
          bnds <- gets blockPrecond
          amap <- gets _assignStackOffset
          case valueStackOffset bnds amap addr of
            Just o -> do
              wmap <- use blockInitDeps
              let deps = getLocDependencySet wmap (StackOffLoc o repr)
              let deps' = assignSet aid <> deps
              assignmentCache %= Map.insert (Some aid) deps'
            Nothing -> do
              depCache <- gets _assignmentCache
              let deps = assignSet aid <> valueUses depCache addr
              requiredAssignDeps aid deps
        CondReadMem _repr c addr val -> do
          depCache <- gets _assignmentCache
          let deps = mconcat
                [ assignSet aid
                , valueUses depCache c
                , valueUses depCache addr
                , valueUses depCache val
                ]
          requiredAssignDeps aid deps
          addDeps $ assignSet aid
        EvalArchFn fn _ -> do
          depCache <- gets _assignmentCache
          let deps = foldlFC' (\s v -> mappend s (valueUses depCache v))
                              (assignSet aid)
                              fn
          if archFnHasSideEffects ctx fn then do
            requiredAssignDeps aid deps
           else
            assignmentCache %= Map.insert (Some aid) deps
    WriteMem addr repr v -> do
      bnds <- gets blockPrecond
      amap <- gets _assignStackOffset
      case valueStackOffset bnds amap addr of
        Just o -> do
          depCache <- gets _assignmentCache
          -- Record value dependency in stack offset.
          let valDeps = addWriteDep stmtIdx (valueUses depCache v)
          blockInitDeps %= addLocDependency (StackOffLoc o repr) valDeps
        Nothing -> do
          demandValue addr
          demandValue v
    CondWriteMem c addr _ v -> do
      demandValue c
      demandValue addr
      demandValue v
    InstructionStart _ _ ->
      pure ()
    -- Comment statements have no specific value.
    Comment _ ->
      pure ()
    ExecArchStmt astmt -> do
      traverseF_ demandValue astmt
    ArchState _addr _assn -> do
      pure ()

-- | This function figures out what the block requires (i.e.,
-- addresses that are stored to, and the value stored), along with a
-- map of how demands by successor blocks map back to assignments and
-- registers.
summarizeBlock :: forall arch ids
               .  RegisterInfo (ArchReg arch)
               => RegisterUseContext arch ids
               -> Memory (ArchAddrWidth arch)
               -> ParsedBlock arch ids
               -> Except String (BlockUsageSummary arch ids)
summarizeBlock ctx mem blk = do
 let s0 = initBlockUsageSummary (blockJumpBounds blk)
 flip execStateT s0 $ flip runReaderT ctx $ do
  let addr = pblockAddr blk
  -- Add demanded values for terminal
  zipWithM_ demandStmtValues [0..] (pblockStmts blk)
  case pblockTermStmt blk of
    ParsedJump regs _ ->
      addRegisterUses regs archRegs
    ParsedBranch regs cond _ _  -> do
      demandValue cond
      addRegisterUses regs archRegs
    ParsedLookupTable regs idx _ -> do
      demandValue idx
      addRegisterUses regs archRegs
    ParsedCall regs  _ -> do
      funTypeMap <- asks functionArgs
      defCallRegs <- asks defaultCallRegs
      -- Get function type associated with function
      let ftr | Just fSegOff <- valueAsSegmentOff mem (regs^.boundValue ip_reg)
              , Just ftp <- Map.lookup fSegOff funTypeMap =
                  ftp
              | otherwise =
                  defCallRegs
      traverse_ (\(Some v) -> demandValue v) $
        registerValues regs (Some ip_reg : fnArgRegs ftr)
      do savedRegs <- asks calleeSavedRegisters
         addRegisterUses regs (Some sp_reg : savedRegs)
      -- Ensure that result registers are defined, but do not have any deps.
      do clearedRegs <- asks callScratchRegisters
         traverse_ clearDependencySet clearedRegs
      traverse_ clearDependencySet (fnReturnRegs ftr)
    PLTStub regs _ _ -> do
      traverseF_ demandValue regs
      depCache <- gets _assignmentCache
      let insDeps :: BlockProvideDepMap (ArchReg arch) ids
                  -> ArchReg arch tp
                  -> Value arch ids tp
                  -> BlockProvideDepMap (ArchReg arch) ids
          insDeps m r v = addLocDependency (RegLoc r) (valueUses depCache v) m
      blockInitDeps %= \m -> MapF.foldlWithKey' insDeps m regs
    ParsedReturn regs -> do
      retRegs     <- asks $ returnRegisters
      let demandRet (Some r) = demandValue (regs^.boundValue r)
      traverse_ demandRet retRegs
    ParsedArchTermStmt tstmt regs _ -> do
      summaryFn <- asks summarizeTermFn
      summaryFn tstmt regs
    ParsedTranslateError _ ->
      error "Cannot identify register use in code where translation error occurs"
    ClassifyFailure _ _ ->
      error $ "Classification failed: " ++ show addr

-- | Maps the starting address of a block with the given register type to the value.
type BlockAddrMap r v = Map (MemSegmentOff (RegAddrWidth r)) v

-- | The set of demanded assignments and a map from block addresses to registers demanded.
type FixState r ids = BlockAddrMap r (DependencySet r ids)

-- | Maps each block to the complete list of blocks that may transition to that block
-- along with the @BlockProvideDepMap@ for that block.
--
-- This data structure is used to reduce lookups in back-propagation
-- of demands.
type PredProvideMap r ids =
  BlockAddrMap r [(MemSegmentOff (RegAddrWidth r), BlockProvideDepMap r ids)]


type NewDemandMap r = BlockAddrMap r (Set (Some (BoundLoc r)))
-- ^ Maps block addresses to the set of register demands we
-- have not yet back propagated.

-- | This takes a list of registers that a block demands that have not
-- been back-propogated, and infers new demands for predecessor
-- registers.
backPropagateOne :: forall r ids
                 .  (MapF.OrdF r, MemWidth (RegAddrWidth r))
                 => BlockAddrMap r (DependencySet r ids)
                 -- ^ State that we are computing fixpint for.
                 -> NewDemandMap r
                 -- ^ Maps block addresses to the set of register demands we
                 -- have not yet back propagated.
                 -> Set (Some (BoundLoc r))
                 -- ^ Set of new locations the target block depends on
                 -- that we have not yet backpropagate demands to the
                 -- previous block for.

                 -> [( MemSegmentOff (RegAddrWidth r)
                     , BlockProvideDepMap r ids
                     )]
                 -- ^ Predecessors for the target block and their provide map
                 -> (FixState r ids, NewDemandMap r)
backPropagateOne s rest _ [] = (s, rest)
backPropagateOne s rest newLocs ((srcAddr,srcDepMap):predRest) = do
  -- Get dependencies for all new locations that are demanded.
  let allDeps :: DependencySet r ids
      allDeps = mconcat [ getLocDependencySet srcDepMap l | Some l <- Set.toList newLocs ]
  -- Add demands for srcAddr and get existing demands.
  let (mseenRegs, s') =
        Map.insertLookupWithKey (\_ x y -> x <> y) srcAddr allDeps s
  -- Get the difference in demands so that we can propagate further.
  let d = case mseenRegs of
            Nothing -> dsLocSet allDeps
            Just oldDems -> dsLocSet allDeps `Set.difference` dsLocSet oldDems
  -- Update list of additional propagations.
  let rest' | Set.null d = rest
            | otherwise = Map.insertWith Set.union srcAddr d rest
  seq s' $ seq rest' $ backPropagateOne s' rest' newLocs predRest

-- | This transitively back propagates blocks across
backPropagate :: forall r ids
              .  (MapF.OrdF r, MemWidth (RegAddrWidth r))
              => PredProvideMap r ids
              -- ^ Pred provide map computed during summarization.
              -> FixState r ids
              -> NewDemandMap r
              -- ^ New demands on block addresses.
              -> FixState r ids
backPropagate predMap s new =
  case Map.maxViewWithKey new of
    Nothing -> s
    Just ((currAddr, newRegs), rest) ->
      let predAddrs = Map.findWithDefault [] currAddr predMap
          (s', rest') = backPropagateOne s rest newRegs predAddrs
       in backPropagate predMap s' rest'

------------------------------------------------------------------------
-- registerUse

-- | Returns the maximum stack argument used by the function, that is,
-- the highest index above sp0 that is read or written.
registerUse :: forall arch ids
            .  RegisterInfo (ArchReg arch)
            => Memory (ArchAddrWidth arch)
            -> RegisterUseContext arch ids
            -> DiscoveryFunInfo arch ids
            -> FunPredMap (ArchAddrWidth arch)
               -- ^ Predecessors for each block in function
            -> Except String (Map (MemSegmentOff (ArchAddrWidth arch)) (DependencySet (ArchReg arch) ids))
registerUse mem ctx finfo predMap = do
  -- Run the first phase (block summarization)
  m <- traverse (summarizeBlock ctx mem) (finfo^.parsedBlocks)
  -- Get all assignments that must be executed for all blocks
  -- to execute in program.
  -- Build map from summarization
  let bru :: BlockAddrMap (ArchReg arch) (DependencySet (ArchReg arch) ids)
      bru = view blockExecDemands <$> m
  let providePair prev = (prev, _blockInitDeps rus)
        where Just rus = Map.lookup prev m

  let predProvideMap = fmap (fmap providePair) predMap
  pure $! backPropagate predProvideMap bru (dsLocSet <$> bru)

ppBlockDependencySetMap :: (MemWidth w, MapF.ShowF r)
                        => Map (MemSegmentOff w) (DependencySet r ids)
                        -> Doc
ppBlockDependencySetMap m =
  vcat [ text "Address:" <+> text (show addr) <$$> indent 2 (pretty deps)
       | (addr, deps) <-  Map.toList m
       ]
