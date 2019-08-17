{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Reopt.CFG.RegisterUse
  ( FunPredMap
  , funBlockPreds
  , DemandedUseMap
  , ppDemandedUseMap
  , registerUse
    -- * Type information
  , X86FunTypeInfo(..)
  , maximumFunTypeInfo
  , X86ArgInfo(..)
  , argReg
  , argRegTypeRepr
  , X86RetInfo(..)
  , retReg
  , retRegTypeRepr
  ) where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Foldable as Fold (traverse_)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
import           Data.Parameterized.TraversableF
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import qualified Flexdis86 as F
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Data.Macaw.CFG.DemandSet
  ( DemandContext
  , demandConstraints
  , hasSideEffects
  )
import           Data.Macaw.CFG
import           Data.Macaw.Discovery.State
import           Data.Macaw.Fold
import           Data.Macaw.Types

import           Data.Macaw.X86.ArchTypes (X86_64, X86TermStmt(..))
import           Data.Macaw.X86.SyscallInfo (SyscallPersonality, spTypeInfo, spResultRegisters)
import           Data.Macaw.X86.X86Reg
  ( X86Reg(..), pattern DF, x86StateRegs, x86CalleeSavedRegs
  , x86GPPArgumentRegs
  )
import           Data.Macaw.X86 (x86DemandContext)

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

-- | The register type this argument is associated with.
argRegTypeRepr :: X86ArgInfo -> Some TypeRepr
argRegTypeRepr ArgBV64{} = Some (BVTypeRepr n64)
argRegTypeRepr ArgMM512D{} = Some (BVTypeRepr n512)


-- | This identifies how a argument is passed into a function, or
-- a return value is passed out.
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

-- | The register types this return value is associated with.
retRegTypeRepr :: X86RetInfo -> Some TypeRepr
retRegTypeRepr r = mapSome typeRepr (retReg r)

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

-- | A map from each address `l` to the addresses of blocks that may jump to `l`.
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

-- What does a given register depend upon?  Records both assignments
-- and registers (transitively through Apps etc.)
type RegDeps (r :: Type -> *) ids = (Set (Some (AssignId ids)), Set (Some r))

type AssignmentCache r ids = Map (Some (AssignId ids)) (RegDeps r ids)

-- The algorithm computes the set of direct deps (i.e., from writes)
-- and then iterates, propagating back via the register deps.
data RegisterUseState ids = RUS {
    -- | Holds the set of registers that we need.  This is the final
    -- output of the algorithm, along with the _blockRegUses below.
    _assignmentUses     :: !(Set (Some (AssignId ids)))
    -- | Holds state about the set of registers that a block uses
    -- (required by this block or a successor).
  , _blockRegUses :: !(Map (MemSegmentOff 64) (Set (Some X86Reg)))
    -- | Maps a each label that sets processor registers to the defined registers to their deps.  Not defined for all
    -- variables, hence use of Map instead of RegState X86Reg
  , _blockInitDeps  :: !(Map (MemSegmentOff 64) (Map (Some X86Reg) (RegDeps X86Reg ids)))
    -- | A cache of the registers and their deps.  The key is not included
    -- in the set of deps (but probably should be).
  , _assignmentCache :: !(AssignmentCache X86Reg ids)
    -- | The set of addresses we need to consider next.
  , _blockFrontier  :: !(Set (MemSegmentOff 64))
    -- | Function arguments derived from FunctionTypeMap
  , functionArgs    :: !(Map (MemSegmentOff 64) X86FunTypeInfo)
  , currentFunctionType :: !X86FunTypeInfo
  , thisSyscallPersonality :: !SyscallPersonality
    -- ^ System call personality
  }

initRegisterUseState :: SyscallPersonality
                     -> Map (MemSegmentOff 64) X86FunTypeInfo
                     -> MemSegmentOff 64
                     -> X86FunTypeInfo -- ^ Type of current function
                     -> RegisterUseState ids
initRegisterUseState sysp fArgs fn ftp =
  RUS { _assignmentUses     = Set.empty
      , _blockRegUses       = Map.empty
      , _blockInitDeps      = Map.empty
      , _assignmentCache    = Map.empty
      , _blockFrontier      = Set.singleton fn
      , functionArgs        = fArgs
      , currentFunctionType = ftp
      , thisSyscallPersonality = sysp
      }

assignmentUses :: Simple Lens (RegisterUseState ids) (Set (Some (AssignId ids)))
assignmentUses = lens _assignmentUses (\s v -> s { _assignmentUses = v })

blockRegUses :: Simple Lens (RegisterUseState ids)
                            (Map (MemSegmentOff 64) (Set (Some X86Reg)))
blockRegUses = lens _blockRegUses (\s v -> s { _blockRegUses = v })

blockInitDeps :: Simple Lens (RegisterUseState ids)
                   (Map (MemSegmentOff 64) (Map (Some X86Reg) (RegDeps X86Reg ids)))
blockInitDeps = lens _blockInitDeps (\s v -> s { _blockInitDeps = v })

blockFrontier :: Simple Lens (RegisterUseState ids) (Set (MemSegmentOff 64))
blockFrontier = lens _blockFrontier (\s v -> s { _blockFrontier = v })

assignmentCache :: Simple Lens (RegisterUseState ids) (AssignmentCache X86Reg ids)
assignmentCache = lens _assignmentCache (\s v -> s { _assignmentCache = v })

type RegisterUseM ids a = State (RegisterUseState ids) a

-- ----------------------------------------------------------------------------------------
-- Phase one functions
-- ----------------------------------------------------------------------------------------

valueUses :: Value X86_64 ids tp -> RegisterUseM ids (RegDeps X86Reg ids)
valueUses = zoom assignmentCache . foldValueCached fns
  where fns = ValueFold { foldBoolValue  = \_   -> (mempty, mempty)
                        , foldBVValue    = \_ _ -> (mempty, mempty)
                        , foldAddr       = \_ -> (mempty,mempty)
                        , foldIdentifier = \_ -> (mempty, mempty)
                        , foldInput      = \r -> (mempty, Set.singleton (Some r))
                        , foldAssign     = \asgn (assigns, regs) ->
                              (Set.insert (Some asgn) assigns, regs)
                        }

demandValue :: MemSegmentOff 64 -> Value X86_64 ids tp -> RegisterUseM ids ()
demandValue addr v = do
  (assigns, regs) <- valueUses v
  assignmentUses %= Set.union assigns
  -- When we demand a value at a label, any register deps need to
  -- get propagated to the parent block (i.e., we only record reg
  -- uses for root labels)
  blockRegUses   %= Map.insertWith Set.union addr regs

-- | Return the values bound to the given registers
registerValues :: Functor t
               => RegState X86Reg (Value X86_64 ids)
               -> t (Some X86Reg)
               -> t (Some (Value X86_64 ids))
registerValues regState = fmap (\(Some r) -> Some (regState^.boundValue r))


x86TermStmtValues :: SyscallPersonality
                  -> X86TermStmt ids
                  -> RegState (ArchReg X86_64) (Value X86_64 ids)
                  -> [Some (Value X86_64 ids)]
x86TermStmtValues _ Hlt _ = []
x86TermStmtValues _ UD2 _ = []
x86TermStmtValues sysp X86Syscall proc_state =
    registerValues proc_state (Some <$> (sysReg : argRegs))
   where sysReg ::  ArchReg X86_64 (BVType 64)
         sysReg = syscall_num_reg
         -- Get list of registers used as arguments to system calls
         syscallRegs :: [ArchReg X86_64 (BVType 64)]
         syscallRegs = syscallArgumentRegs
         -- Get arguments registers if this is a static system call number
         argRegs
             | BVValue _ call_no <- proc_state^.boundValue syscall_num_reg
             , Just (_,_,argtypes) <- Map.lookup (fromInteger call_no) (spTypeInfo sysp) =
               take (length argtypes) syscallRegs
             | otherwise = syscallRegs

-- | Figure out the deps of the given registers and update the state for the current label
addRegisterUses :: MemSegmentOff 64
                -> RegState X86Reg (Value X86_64 ids)
                -> [Some X86Reg]
                -> RegisterUseM ids () -- Map (Some N.RegisterName) RegDeps
addRegisterUses addr s rs = do
  vs <- mapM (\(Some r) -> valueUses (s ^. boundValue r)) rs
  blockInitDeps %= Map.insertWith (Map.unionWith mappend) addr (Map.fromList $ zip rs vs)

clearRegDeps :: MemSegmentOff 64 -> Some X86Reg -> RegisterUseM ids ()
clearRegDeps addr r = blockInitDeps . ix addr %= Map.insert r (Set.empty, Set.empty)

-- | Return values that must be evaluated to execute side effects.
stmtDemandedValues :: DemandContext arch
                   -> Stmt arch ids
                   -> [Some (Value arch ids)]
stmtDemandedValues ctx stmt = demandConstraints ctx $
  case stmt of
    AssignStmt a
      | hasSideEffects ctx (assignRhs a) -> do
          foldMapFC (\v -> [Some v]) (assignRhs a)
      | otherwise ->
          []
    WriteMem addr _ v -> [Some addr, Some v]
    CondWriteMem cond addr _ v -> [Some cond, Some addr, Some v]
    InstructionStart _ _ -> []
    -- Comment statements have no specific value.
    Comment _ -> []
    ExecArchStmt astmt -> foldMapF (\v -> [Some v]) astmt
    ArchState _addr assn -> foldMapF (\v -> [Some v]) assn

-- | This function figures out what the block requires (i.e.,
-- addresses that are stored to, and the value stored), along with a
-- map of how demands by successor blocks map back to assignments and
-- registers.
summarizeBlock :: forall ids
               .  Memory 64
               -> ParsedBlock X86_64 ids
               -> RegisterUseM ids ()
summarizeBlock mem blk = do
  let addr = pblockAddr blk
  blockInitDeps %= Map.insert addr Map.empty
  -- Add demanded values for terminal
  sysp    <- gets thisSyscallPersonality
  funTypeMap <- gets functionArgs
  curFTI  <- gets $ currentFunctionType
  let ctx = x86DemandContext
  traverse_ (\(Some r) -> demandValue addr r)
            (concatMap (stmtDemandedValues ctx) (pblockStmts blk))
  case pblockTermStmt blk of
    ParsedJump regs _ ->
      addRegisterUses addr regs x86StateRegs
    ParsedBranch regs cond _ _  -> do
      demandValue addr cond
      addRegisterUses addr regs x86StateRegs
    ParsedLookupTable regs idx _ -> do
      demandValue addr idx
      addRegisterUses addr regs x86StateRegs
    ParsedCall regs  _ -> do
      -- Get function type associated with function
      let fti | Just fSegOff <- valueAsSegmentOff mem (regs^.boundValue ip_reg)
              , Just ftp <- Map.lookup fSegOff funTypeMap =
                  ftp
              | otherwise =
                  maximumFunTypeInfo
      traverse_ (\(Some r) -> demandValue addr r) $
        registerValues regs (Some ip_reg : fmap argReg (ftiArgRegs fti))
      addRegisterUses addr regs (Some sp_reg : Set.toList x86CalleeSavedRegs)
      -- Ensure that result registers are defined, but do not have any deps.
      traverse_ (clearRegDeps addr) $ [Some DF] ++ fmap retReg (ftiRetRegs fti)
    PLTStub regs _ _ -> do
      traverseF_ (demandValue addr) regs
      entries <- traverse (\(MapF.Pair r v) -> (Some r,) <$> valueUses v) (MapF.toList regs)
      blockInitDeps %= Map.insertWith (Map.unionWith mappend) addr (Map.fromList entries)
    ParsedReturn regs -> do
      traverse_ (\(Some r) -> demandValue addr r) $
        registerValues regs (fmap argReg (ftiArgRegs curFTI))
    ParsedArchTermStmt tstmt regs _ -> do
      traverse_ (\(Some r) -> demandValue addr r) $
        x86TermStmtValues sysp tstmt regs
      summarizeX86ArchTermStmt sysp addr tstmt regs
    ParsedTranslateError _ ->
      error "Cannot identify register use in code where translation error occurs"
    ClassifyFailure _ ->
      error $ "Classification failed: " ++ show addr

summarizeX86ArchTermStmt :: SyscallPersonality
                         -> MemSegmentOff 64
                         -> X86TermStmt ids
                         -> RegState (ArchReg X86_64) (Value X86_64 ids)
                         -> RegisterUseM ids ()
summarizeX86ArchTermStmt sysp addr X86Syscall regs = do
  -- FIXME: clagged from call above
  addRegisterUses addr regs (Some sp_reg : (Set.toList x86CalleeSavedRegs))
  traverse_ (clearRegDeps addr) (spResultRegisters sysp)
summarizeX86ArchTermStmt _ _ Hlt _ = pure ()
summarizeX86ArchTermStmt _ _ UD2 _ = pure ()


-- | Explore states until we have reached end of frontier.
summarizeIter :: Memory 64
              -> DiscoveryFunInfo X86_64 ids
              -> Set (MemSegmentOff 64)
              -> RegisterUseM ids ()
summarizeIter mem ist seen = do
  f <- use blockFrontier
  case Set.maxView f of
    Nothing -> pure ()
    Just (addr,rest) -> do
      blockFrontier .= rest
      if addr `Set.member` seen then
        summarizeIter mem ist seen
       else do
        let Just blk = Map.lookup addr (ist^.parsedBlocks)
        blockFrontier %= \s -> foldr Set.insert s (parsedTermSucc (pblockTermStmt blk))
        summarizeBlock mem blk
        summarizeIter mem ist (Set.insert addr seen)

-- | Map from addresses to the registers they depend on
type DemandedUseMap = Map (MemSegmentOff 64) (Set (Some X86Reg))

type FixState ids = (Set (Some (AssignId ids)), DemandedUseMap)

doOne :: Map (MemSegmentOff 64) (Map (Some X86Reg) (RegDeps X86Reg ids))
      -> FunPredMap 64
         -- ^ Map addresses to the predessor blocks.
      -> FixState ids
         -- ^ State that we are computing fixpint for.
      -> DemandedUseMap
         -- ^ Remaining registers to compute demanded use map
      -> Set (Some X86Reg)
         -- ^ Set of new registers the target block depends on.
      -> [MemSegmentOff 64]
         -- ^ Predecessors for the target block.
      -> FixState ids
doOne bid predMap s rest _ [] = calculateFixpoint bid predMap s rest
doOne bid predMap (aUse, bru) rest newRegs (predAddr:predRest) = do
  let Just depMap = Map.lookup predAddr bid

  let (assigns, regs) = mconcat [ depMap ^. ix r | r <- Set.toList newRegs ]
  -- Get dependencies of predecessor currently.
  let seenRegs = fromMaybe Set.empty (Map.lookup predAddr bru)

  let aUse' = Set.union assigns aUse
  let bru' = Map.insertWith Set.union predAddr regs bru
  let d = regs `Set.difference` seenRegs

  let rest' | Set.null d = rest
            | otherwise = Map.insertWith Set.union predAddr d rest
  seq aUse' $ seq bru' $ seq rest' $ doOne bid predMap (aUse', bru') rest' newRegs predRest

-- We use ix here as it returns mempty if there is no key, which is
-- the right behavior here.
calculateFixpoint :: Map (MemSegmentOff 64) (Map (Some X86Reg) (RegDeps X86Reg ids))
                     -- Maps blocks labels to the dependencies for each register they provide.
                  -> FunPredMap 64
                     -- ^ Map addresses to the predessor blocks.
                  -> FixState ids
                     -- ^ Map addresses to the register they need.
                  -> DemandedUseMap
                  -> FixState ids
calculateFixpoint bid predMap s new =
  case Map.maxViewWithKey new of
    Nothing -> s
    Just ((currAddr, newRegs), rest) -> do
      doOne bid predMap s rest newRegs $ fromMaybe [] $ Map.lookup currAddr predMap

-- | Pretty print a demanded use map
ppDemandedUseMap :: DemandedUseMap -> Doc
ppDemandedUseMap m = vcat (ppEntry <$> Map.toList m)
  where ppEntry :: (MemSegmentOff 64, Set (Some X86Reg)) -> Doc
        ppEntry (addr, regs) = text (show addr) <> char ':' <+> hsep (ppReg <$> Set.toList regs)
        ppReg :: Some X86Reg -> Doc
        ppReg (Some r) = text (show r)


-- | Returns the maximum stack argument used by the function, that is,
-- the highest index above sp0 that is read or written.
registerUse :: Memory 64
            -> SyscallPersonality
            -> Map (MemSegmentOff 64) X86FunTypeInfo
               -- ^ Map from function entry points we have analyzed to their
               -- inferred function type.
            -> DiscoveryFunInfo X86_64 ids
            -> X86FunTypeInfo
               -- ^ Expected type of this function
            -> FunPredMap 64
               -- ^ Predecessors for each block in function
            -> ( Set (Some (AssignId ids))
               , DemandedUseMap
               )
registerUse mem sysp fArgs disFunInfo ftp predMap = do
  let addr = discoveredFunAddr disFunInfo
  flip evalState (initRegisterUseState sysp fArgs addr ftp) $ do
    -- Run the first phase (block summarization)
    summarizeIter mem disFunInfo Set.empty
    -- propagate back uses
    bid <- use blockInitDeps
    assignUse <- use assignmentUses
    bru <- use blockRegUses
    pure $! calculateFixpoint bid predMap (assignUse, bru) bru
