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
  , registerUse
  , ppBlockDependencySetMap
  , AssignStackOffsetMap
  , valueStackOffset
  , DependencySet(..)
  , StmtIndex

    -- * Type information
  , X86FunTypeInfo(..)
  , maximumFunTypeInfo
  , X86ArgInfo(..)
  , argReg
  , X86RetInfo(..)
  , retReg
  ) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Foldable
import           Data.Kind
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
import           Data.Parameterized.TraversableF
import           Data.Parameterized.TraversableFC
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import qualified Flexdis86 as F
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

import           Data.Macaw.X86.ArchTypes (X86_64, X86TermStmt(..))
import           Data.Macaw.X86.SyscallInfo
  (SyscallPersonality
  , spTypeInfo
  , spResultRegisters
  )
import           Data.Macaw.X86.X86Reg
  ( X86Reg(..), pattern DF, x86StateRegs, x86CalleeSavedRegs
  , x86GPPArgumentRegs
  )
import           Data.Macaw.X86 (x86DemandContext)

import Debug.Trace

-- | This identifies how a argument is passed into a function, or
-- a return value is passed out.
data X86ArgInfo where
  ArgBV64 :: !F.Reg64 -> X86ArgInfo
  -- ^ This identifies a 64-bit value passed as a register.
  --
  -- The register should be compatible with the ABI.
  ArgMM512D :: !Word8 -> X86ArgInfo
  -- ^ This identifies one of the zmm registers used as arguments (zmm0-7).

-- | The register types this return value is associated with.
argReg :: X86ArgInfo -> Some X86Reg
argReg (ArgBV64 r) = Some (X86_GP r)
argReg (ArgMM512D i) = Some (X86_ZMMReg i)

-- | This identifies how a return value is passed from a callee to
-- the callee.
data X86RetInfo where
  RetBV64 :: !F.Reg64 -> X86RetInfo
  -- ^ This identifies a 64-bit value returned as a register (RAX/RDX)
  --
  -- The register should be compatible with the ABI.
  RetMM512D :: !Word8 -> X86RetInfo
  -- ^ This identifies one of the two zmm registers used as argument (zmm0/1).

-- | The register types this return value is associated with.
retReg :: X86RetInfo -> Some X86Reg
retReg (RetBV64 r) = Some (X86_GP r)
retReg (RetMM512D i) = Some (X86_ZMMReg i)

-- | This describes the registers and return value of an x86_64 ABI
-- compliant function.
--
-- This representation does not support arguments that spilled on the
-- stack, but this would be a good feature to add.
--
-- It uses a list for arguments so that we can use C headers and
-- ensure the arguments appear in a particular order (e.g. from the
-- binary perspective a function that takes two integers followed by a
-- float is indistinguishable from a function that takes a float
-- followed by two integers.
data X86FunTypeInfo
   = X86FunTypeInfo { ftiArgRegs :: [X86ArgInfo]
                    , ftiRetRegs :: [X86RetInfo]
                    }

-- |  Maximum function type.
maximumFunTypeInfo :: X86FunTypeInfo
maximumFunTypeInfo =
  X86FunTypeInfo { ftiArgRegs = fmap ArgBV64 x86GPPArgumentRegs
                             ++ fmap ArgMM512D [0..7]
                 , ftiRetRegs = fmap RetBV64   [ F.RAX, F.RDX ]
                             ++ fmap RetMM512D [0,1]
                 }

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
valueStackOffset :: InitJumpBounds X86_64
                 -> AssignStackOffsetMap 64 ids
                 -> Value X86_64 ids tp
                 -> Maybe (MemInt 64)
valueStackOffset bnds amap v =
  case v of
    CValue{} ->
      Nothing
    Initial r ->
      case boundsLocationInfo bnds (RegLoc r) of
        (_rep, IsStackOffset o, _cnt) -> Just o
        _ -> Nothing
    AssignedValue (Assignment aid _) ->
      Map.lookup (Some aid) amap

-------------------------------------------------------------------------------
-- StmtIndex

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

-- | This maps each block to the @BlockProvideDepMap@ for the block.
--
-- These are used as function summaries to simplify the computation of
-- overall function register uses.
--type FunctionBlockProvideDepMap r ids
--   = Map (MemSegmentOff (RegAddrWidth r)) (BlockProvideDepMap r ids)

-- This pair identifies write within a function by the starting
-- address of the block it is in, and the index of the instruction
-- with the write.
--type WriteRef w = (MemSegmentOff w, Natural)

--type ReadRef ids = AssignId ids

-- | Information about architecture, rest of the program, and current
-- function needed to determine register usage.
data RegisterUseContext = RUC
  { functionArgs    :: !(Map (MemSegmentOff 64) X86FunTypeInfo)
  , currentFunctionType :: !X86FunTypeInfo
    -- | System call personality
  , thisSyscallPersonality :: !SyscallPersonality
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

type RegisterUseM ids a =
  ReaderT RegisterUseContext (StateT (BlockUsageSummary X86_64 ids) (Except String)) a

-- ----------------------------------------------------------------------------------------
-- Phase one functions
-- ----------------------------------------------------------------------------------------

-- | Return the register and assignment dependencies needed to
valueUses :: (HasCallStack, MapF.OrdF (ArchReg arch))
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
addDeps :: HasCallStack => DependencySet X86Reg ids -> RegisterUseM ids ()
addDeps deps = trace ("Adding demands:\n" ++ show (pretty deps) ++ "\n" ++ prettyCallStack callStack) $ do
  blockExecDemands %= mappend deps

-- | Record the values needed to compute the given value.
demandValue :: HasCallStack
            => Value X86_64 ids tp
            -> RegisterUseM ids ()
demandValue v = do
  depCache <- gets _assignmentCache
  addDeps (valueUses depCache v)

-- | Return the values bound to the given registers
registerValues :: Functor t
               => RegState X86Reg (Value X86_64 ids)
               -> t (Some X86Reg)
               -> t (Some (Value X86_64 ids))
registerValues regs = fmap (\(Some r) -> Some (regs^.boundValue r))

x86TermStmtValues :: SyscallPersonality
                  -> X86TermStmt ids
                  -> RegState (ArchReg X86_64) (Value X86_64 ids)
                  -> [Some (Value X86_64 ids)]
x86TermStmtValues _ Hlt _ = []
x86TermStmtValues _ UD2 _ = []
x86TermStmtValues sysp X86Syscall regs =
    registerValues regs (Some <$> (sysReg : argRegs))
   where sysReg ::  ArchReg X86_64 (BVType 64)
         sysReg = syscall_num_reg
         -- Get list of registers used as arguments to system calls
         syscallRegs :: [ArchReg X86_64 (BVType 64)]
         syscallRegs = syscallArgumentRegs
         -- Get arguments registers if this is a static system call number
         argRegs
             | BVValue _ call_no <- regs^.boundValue syscall_num_reg
             , Just (_,_,argtypes) <- Map.lookup (fromInteger call_no) (spTypeInfo sysp) =
               take (length argtypes) syscallRegs
             | otherwise = syscallRegs

-- | Figure out the deps of the given registers and update the state
-- for the current label
addRegisterUses :: forall ids
                .  RegState X86Reg (Value X86_64 ids)
                   -- ^ Register values when block terminates.
                -> [Some X86Reg]
                   -- ^ List of registers that future blocks are allowed
                   -- to depend on.
                -> RegisterUseM ids ()
addRegisterUses regs rs = do
  depCache <- gets _assignmentCache
  let insReg :: BlockProvideDepMap X86Reg ids
             -> Some X86Reg
             -> BlockProvideDepMap X86Reg ids
      insReg m (Some r) = addLocDependency (RegLoc r) (valueUses depCache (regs^.boundValue r)) m
  blockInitDeps %= \m -> foldl' insReg m rs

-- | Mark the given register has no dependencies
clearDependencySet :: Some X86Reg -> RegisterUseM ids ()
clearDependencySet (Some r) = blockInitDeps %= locOverwriteWith (\n _ -> n) (RegLoc r) (Const mempty)

-- | Set dependencies for an assignment whose right-hand-side must be
-- evaluated for side effects.
requiredAssignDeps :: AssignId ids tp
                   -> DependencySet X86Reg ids
                   -> RegisterUseM ids ()
requiredAssignDeps aid deps = do
  addDeps $ deps
  assignmentCache %= Map.insert (Some aid) mempty

-- | Return values that must be evaluated to execute side effects.
demandStmtValues :: HasCallStack
                 => DemandContext X86_64
                 -> StmtIndex -- ^ Index of statement.
                 -> Stmt X86_64 ids
                    -- ^ Statement we want to demand.
                 -> RegisterUseM ids ()
demandStmtValues ctx stmtIdx stmt = demandConstraints ctx $
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
summarizeBlock :: forall ids
               .  RegisterUseContext
               -> Memory 64
               -> ParsedBlock X86_64 ids
               -> Except String (BlockUsageSummary X86_64 ids)
summarizeBlock ctx mem blk = do
 let s0 = initBlockUsageSummary (blockJumpBounds blk)
 flip execStateT s0 $ flip runReaderT ctx $ do
  let addr = pblockAddr blk
  -- Add demanded values for terminal
  zipWithM_ (demandStmtValues x86DemandContext) [0..] (pblockStmts blk)
  case pblockTermStmt blk of
    ParsedJump regs _ ->
      addRegisterUses regs x86StateRegs
    ParsedBranch regs cond _ _  -> do
      demandValue cond
      addRegisterUses regs x86StateRegs
    ParsedLookupTable regs idx _ -> do
      demandValue idx
      addRegisterUses regs x86StateRegs
    ParsedCall regs  _ -> do
      funTypeMap <- asks functionArgs
      -- Get function type associated with function
      let fti | Just fSegOff <- valueAsSegmentOff mem (regs^.boundValue ip_reg)
              , Just ftp <- Map.lookup fSegOff funTypeMap =
                  ftp
              | otherwise =
                  maximumFunTypeInfo
      traverse_ (\(Some v) -> demandValue v) $
        registerValues regs (Some ip_reg : fmap argReg (ftiArgRegs fti))
      addRegisterUses regs (Some sp_reg : Set.toList x86CalleeSavedRegs)
      -- Ensure that result registers are defined, but do not have any deps.
      traverse_ clearDependencySet $ [Some DF] ++ fmap retReg (ftiRetRegs fti)
    PLTStub regs _ _ -> do
      traverseF_ demandValue regs
      depCache <- gets _assignmentCache
      let insDeps :: BlockProvideDepMap X86Reg ids
                  -> X86Reg tp
                  -> Value X86_64 ids tp
                  -> BlockProvideDepMap X86Reg ids
          insDeps m r v = addLocDependency (RegLoc r) (valueUses depCache v) m
      blockInitDeps %= \m -> MapF.foldlWithKey' insDeps m regs
    ParsedReturn regs -> do
      curFTI     <- asks currentFunctionType
      let demandRet ri =
            case retReg ri of
              Some r -> demandValue (regs^.boundValue r)
      traverse_ demandRet (ftiRetRegs curFTI)
    ParsedArchTermStmt tstmt regs _ -> do
      sysp  <- asks thisSyscallPersonality
      traverse_ (\(Some r) -> demandValue r) $
        x86TermStmtValues sysp tstmt regs
      summarizeX86ArchTermStmt sysp tstmt regs
    ParsedTranslateError _ ->
      error "Cannot identify register use in code where translation error occurs"
    ClassifyFailure _ _ ->
      error $ "Classification failed: " ++ show addr

summarizeX86ArchTermStmt :: SyscallPersonality
                         -> X86TermStmt ids
                         -> RegState (ArchReg X86_64) (Value X86_64 ids)
                         -> RegisterUseM ids ()
summarizeX86ArchTermStmt sysp X86Syscall regs = do
  -- FIXME: clagged from call above
  addRegisterUses regs (Some sp_reg : (Set.toList x86CalleeSavedRegs))
  traverse_ clearDependencySet (spResultRegisters sysp)
summarizeX86ArchTermStmt _ Hlt _ = pure ()
summarizeX86ArchTermStmt _ UD2 _ = pure ()


{-
-- | Map each block starting address to a set of registers that the
-- block needs to be computed to successfully execute the block.
type X86BlockRegMap = Map (MemSegmentOff 64) (Set (Some X86Reg))
-}

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
backPropagateOne :: forall ids
                 .  BlockAddrMap X86Reg (DependencySet X86Reg ids)
                 -- ^ State that we are computing fixpint for.
                 -> NewDemandMap X86Reg
                 -- ^ Maps block addresses to the set of register demands we
                 -- have not yet back propagated.
                 -> Set (Some (BoundLoc X86Reg))
                 -- ^ Set of new locations the target block depends on
                 -- that we have not yet backpropagate demands to the
                 -- previous block for.

                 -> [(MemSegmentOff 64,  BlockProvideDepMap X86Reg ids)]
                 -- ^ Predecessors for the target block and their provide map
                 -> (FixState X86Reg ids, NewDemandMap X86Reg)
backPropagateOne s rest _ [] = (s, rest)
backPropagateOne s rest newLocs ((srcAddr,srcDepMap):predRest) = do
  -- Get dependencies for all new locations that are demanded.
  let allDeps :: DependencySet X86Reg ids
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
backPropagate :: PredProvideMap X86Reg ids
              -- ^ Pred provide map computed during summarization.
              -> FixState X86Reg ids
              -> NewDemandMap X86Reg
              -- ^ New demands on block addresses.
              -> FixState X86Reg ids
backPropagate predMap s new =
  case Map.maxViewWithKey new of
    Nothing -> s
    Just ((currAddr, newRegs), rest) ->
      let predAddrs = Map.findWithDefault [] currAddr predMap
          (s', rest') = backPropagateOne s rest newRegs predAddrs
       in backPropagate predMap s' rest'

-- | Returns the maximum stack argument used by the function, that is,
-- the highest index above sp0 that is read or written.
registerUse :: forall ids
            .  Memory 64
            -> SyscallPersonality
            -> Map (MemSegmentOff 64) X86FunTypeInfo
               -- ^ Map from function entry points we have analyzed to their
               -- inferred function type.
            -> DiscoveryFunInfo X86_64 ids
            -> X86FunTypeInfo
               -- ^ Expected type of this function
            -> FunPredMap 64
               -- ^ Predecessors for each block in function
            -> Except String (Map (MemSegmentOff 64) (DependencySet X86Reg ids))
registerUse mem sysp fArgs finfo ftp predMap = do
  let ctx = RUC { functionArgs = fArgs
                , currentFunctionType = ftp
                , thisSyscallPersonality = sysp
                }
  -- Run the first phase (block summarization)
  m <- traverse (summarizeBlock ctx mem) (finfo^.parsedBlocks)
  -- Get all assignments that must be executed for all blocks
  -- to execute in program.
  -- Build map from summarization
  let bru :: BlockAddrMap X86Reg (DependencySet X86Reg ids)
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
