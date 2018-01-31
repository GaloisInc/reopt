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
  , AddrToX86FunctionTypeMap
  , DemandedUseMap
  , ppDemandedUseMap
  , registerUse
  ) where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Foldable as Fold (traverse_)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Parameterized.Some
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import           Data.Word
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Data.Macaw.Analysis.FunctionArgs (stmtDemandedValues)
import           Data.Macaw.CFG
import           Data.Macaw.CFG.BlockLabel
import           Data.Macaw.Discovery.State
import           Data.Macaw.Fold
import           Data.Macaw.Types

import           Data.Macaw.X86.ArchTypes (X86_64, X86TermStmt(..))
import           Data.Macaw.X86.SyscallInfo (SyscallPersonality, spTypeInfo, spResultRegisters)
import           Data.Macaw.X86.X86Reg
  ( X86Reg, pattern DF, x86StateRegs, x86ResultRegs, x86FloatResultRegs, x86CalleeSavedRegs
  )
import           Data.Macaw.X86 (x86DemandContext)

import           Reopt.CFG.FnRep ( FunctionType(..)
                                 , ftMaximumFunctionType
                                 , ftArgRegs
                                 , ftIntRetRegs
                                 , ftFloatRetRegs
                                 )

-------------------------------------------------------------------------------
-- funBlockPreds

-- | A map from each address `l` to the labels of blocks that may jump to `l`.
type FunPredMap w = Map (MemSegmentOff w) [BlockLabel w]

-- | Get all successor blocks for the given list of statements.
stmtListSucc :: StatementList arch ids -> [(ArchSegmentOff arch, Word64)]
stmtListSucc stmts = do
  let idx = stmtsIdent stmts
  case stmtsTerm stmts of
    ParsedCall _ (Just ret_addr) -> [(ret_addr, idx)]
    ParsedCall _ Nothing -> []
    ParsedJump _ tgt -> [(tgt, idx)]
    ParsedLookupTable _ _ v -> (,idx) <$> V.toList v
    ParsedReturn{} -> []
    ParsedIte _ t f -> stmtListSucc t ++ stmtListSucc f
    ParsedTranslateError{} -> []
    ParsedArchTermStmt _ _ ret -> maybeToList ((, idx) <$> ret)
    ClassifyFailure{} -> []

-- | Return the FunPredMap for the discovered block function.
funBlockPreds :: DiscoveryFunInfo arch ids -> FunPredMap (ArchAddrWidth arch)
funBlockPreds info = Map.fromListWith (++)
  [ (next, [GeneratedBlock addr idx])
  | b <- Map.elems (info^.parsedBlocks)
    -- Get address of region
  , let addr = pblockAddr b
    -- get the block successors
  , (next,idx) <- stmtListSucc (blockStatementList b)
  ]

-------------------------------------------------------------------------------

-- What does a given register depend upon?  Records both assignments
-- and registers (transitively through Apps etc.)
type RegDeps (r :: Type -> *) ids = (Set (Some (AssignId ids)), Set (Some r))

type AssignmentCache r ids = Map (Some (AssignId ids)) (RegDeps r ids)

-- | Map from address to type of function at that address
--
-- We use the given key type so that we do not need access to memory object
-- in computing types.
type AddrToX86FunctionTypeMap = Map (MemSegmentOff 64) FunctionType

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
  , _blockInitDeps  :: !(Map (BlockLabel 64) (Map (Some X86Reg) (RegDeps X86Reg ids)))
    -- | A cache of the registers and their deps.  The key is not included
    -- in the set of deps (but probably should be).
  , _assignmentCache :: !(AssignmentCache X86Reg ids)
    -- | The set of addresses we need to consider next.
  , _blockFrontier  :: !(Set (MemSegmentOff 64))
    -- | Function arguments derived from AddrToX86FunctionTypeMap
  , functionArgs    :: !AddrToX86FunctionTypeMap
  , currentFunctionType :: !FunctionType
  , thisSyscallPersonality :: !SyscallPersonality
    -- ^ System call personality
  }

initRegisterUseState :: SyscallPersonality
                     -> AddrToX86FunctionTypeMap
                     -> MemSegmentOff 64
                     -> FunctionType -- ^ Type of current function
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
                   (Map (BlockLabel 64) (Map (Some X86Reg) (RegDeps X86Reg ids)))
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
valueUses = zoom assignmentCache .
            foldValueCached (\_ _      -> (mempty, mempty))
                            (\_        -> (mempty, mempty))
                            (\r        -> (mempty, Set.singleton (Some r)))
                            (\asgn (assigns, regs) ->
                              (Set.insert (Some asgn) assigns, regs))

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

-- | Get values that must be evaluated to execute terminal statement.
termStmtValues :: Memory 64
               -> SyscallPersonality
               -> AddrToX86FunctionTypeMap
                  -- ^ Map from addresses to function type
               -> FunctionType
                  -- ^ Type of this function
               -> ParsedTermStmt X86_64 ids
                  -- ^ Statement to get value of
               -> [Some (Value X86_64 ids)]
termStmtValues mem sysp typeMap curFunType tstmt =
  case tstmt of
    ParsedCall proc_state _m_ret_addr ->
      -- Get function type associated with function
      let ft | Just fSegOff <- valueAsSegmentOff mem (proc_state^.boundValue ip_reg)
             , Just ftp <- Map.lookup fSegOff typeMap = ftp
             | otherwise = ftMaximumFunctionType
       in registerValues proc_state (Some ip_reg : ftArgRegs ft)
    ParsedJump _proc_state _tgt_addr -> []
    ParsedLookupTable _proc_state idx _vec -> [Some idx]
    ParsedReturn proc_state ->
      let regs = (Some <$> take (fnNIntRets curFunType) x86ResultRegs)
              ++ (Some <$> take (fnNFloatRets curFunType) x86FloatResultRegs)
       in registerValues proc_state regs
    ParsedIte c _ _ -> [Some c]
    ParsedArchTermStmt ts proc_state _ ->
      x86TermStmtValues sysp ts proc_state
    ParsedTranslateError _ -> []
    ClassifyFailure _ -> []

-- | This function figures out what the block requires (i.e.,
-- addresses that are stored to, and the value stored), along with a
-- map of how demands by successor blocks map back to assignments and
-- registers.
summarizeBlock :: forall ids
               .  Memory 64
               -> DiscoveryFunInfo X86_64 ids
               -> MemSegmentOff 64
               -> StatementList X86_64 ids
               -> RegisterUseM ids ()
summarizeBlock mem interp_state addr stmts = do
  let lbl = GeneratedBlock addr (stmtsIdent stmts)
  blockInitDeps %= Map.insert lbl Map.empty
  let -- Figure out the deps of the given registers and update the state for the current label
      addRegisterUses :: RegState X86Reg (Value X86_64 ids)
                      -> [Some X86Reg]
                      -> RegisterUseM ids () -- Map (Some N.RegisterName) RegDeps
      addRegisterUses s rs = do
        vs <- mapM (\(Some r) -> (Some r,) <$> valueUses (s ^. boundValue r)) rs
        blockInitDeps %= Map.insertWith (Map.unionWith mappend) lbl (Map.fromList vs)

  -- Add demanded values for terminal
  sysp <- gets thisSyscallPersonality
  typeMap <- gets $ functionArgs
  cur_ft <- gets currentFunctionType
  let termValues = termStmtValues mem sysp typeMap cur_ft (stmtsTerm stmts)
  let ctx = x86DemandContext
  traverse_ (\(Some r) -> demandValue addr r)
            (concatMap (stmtDemandedValues ctx) (stmtsNonterm stmts) ++ termValues)

  case stmtsTerm stmts of
          ParsedCall proc_state _ -> do
            -- Get function type associated with function
            let ft | Just fSegOff <- valueAsSegmentOff mem (proc_state^.boundValue ip_reg)
                   , Just ftp <- Map.lookup fSegOff typeMap = ftp
                   | otherwise = ftMaximumFunctionType
            addRegisterUses proc_state (Some sp_reg : Set.toList x86CalleeSavedRegs)
            -- Ensure that result registers are defined, but do not have any deps.
            traverse_ (\r -> blockInitDeps . ix lbl %= Map.insert r (Set.empty, Set.empty)) $
              (Some <$> ftIntRetRegs ft) ++ (Some <$> ftFloatRetRegs ft) ++ [Some DF]

          ParsedJump proc_state _ ->
            addRegisterUses proc_state x86StateRegs
          ParsedLookupTable proc_state _ _ ->
            addRegisterUses proc_state x86StateRegs
          ParsedReturn _ ->
            pure ()
          ParsedIte _ tblock fblock -> do
            summarizeBlock mem interp_state addr tblock
            summarizeBlock mem interp_state addr fblock
          ParsedArchTermStmt X86Syscall proc_state _ -> do
            -- FIXME: clagged from call above
            addRegisterUses proc_state (Some sp_reg : (Set.toList x86CalleeSavedRegs))
            let insReg :: Some X86Reg -> RegisterUseM ids ()
                insReg sr = blockInitDeps . ix lbl %= Map.insert sr (Set.empty, Set.empty)
            traverse_ insReg (spResultRegisters sysp)
          ParsedTranslateError _ ->
            error "Cannot identify register use in code where translation error occurs"
          ClassifyFailure _ ->
            error $ "Classification failed: " ++ show addr

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
        let stmts = blockStatementList blk
        blockFrontier %= \s -> foldr Set.insert s (fst <$> stmtListSucc stmts)
        summarizeBlock mem ist addr stmts
        summarizeIter mem ist (Set.insert addr seen)

-- | Map from addresses to the registers they depend on
type DemandedUseMap = Map (MemSegmentOff 64) (Set (Some X86Reg))

type FixState ids = (Set (Some (AssignId ids)), DemandedUseMap)

-- |
doOne :: Map (BlockLabel 64) (Map (Some X86Reg) (RegDeps X86Reg ids))
      -> FunPredMap 64
         -- ^ Map addresses to the predessor blocks.
      -> FixState ids
         -- ^ State that we are computing fixpint for.
      -> DemandedUseMap
         -- ^ Remaining registers to compute demanded use map
      -> Set (Some X86Reg)
         -- ^ Set of new registers the target block depends on.
      -> [BlockLabel 64]
         -- ^ Predecessors for the target block.
      -> FixState ids
doOne bid predMap s rest _ [] = calculateFixpoint bid predMap s rest
doOne bid predMap (aUse, bru) rest newRegs (predLabel:predRest) = do
  let Just depMap = Map.lookup predLabel bid

  let (assigns, regs) = mconcat [ depMap ^. ix r | r <- Set.toList newRegs ]
  -- Get dependencies of predecessor currently.
  let predAddr = labelAddr predLabel
  let seenRegs = fromMaybe Set.empty (Map.lookup predAddr bru)

  let aUse' = Set.union assigns aUse
  let bru' = Map.insertWith Set.union predAddr regs bru
  let d = regs `Set.difference` seenRegs

  let rest' | Set.null d = rest
            | otherwise = Map.insertWith Set.union predAddr d rest
  seq aUse' $ seq bru' $ seq rest' $ doOne bid predMap (aUse', bru') rest' newRegs predRest

-- We use ix here as it returns mempty if there is no key, which is
-- the right behavior here.
calculateFixpoint :: Map (BlockLabel 64) (Map (Some X86Reg) (RegDeps X86Reg ids))
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
            -> AddrToX86FunctionTypeMap
            -> DiscoveryFunInfo X86_64 ids
            -> FunctionType
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
