{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Reopt.CFG.RegisterUse
  ( FunPredMap
  , funBlockPreds
  , AddrToFunctionTypeMap
  , DemandedUseMap
  , ppDemandedUseMap
  , registerUse
  ) where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Foldable as Fold (traverse_)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Parameterized.Some
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Data.Macaw.Architecture.Syscall
import           Data.Macaw.CFG
import           Data.Macaw.DebugLogging
import           Data.Macaw.Discovery.Info
import           Data.Macaw.Memory (Memory)
import           Data.Macaw.Types

import           Reopt.CFG.FnRep ( FunctionType(..)
                                 , ftMaximumFunctionType
                                 , ftMinimumFunctionType
                                 , ftArgRegs
                                 , ftIntRetRegs
                                 , ftFloatRetRegs
                                 )
import           Reopt.CFG.FunctionArgs (stmtDemandedValues)
import           Reopt.Machine.X86State

-------------------------------------------------------------------------------
-- funBlockPreds

-- | Return intra-procedural labels this will jump to.
termSucc :: SegmentedAddr 64 -> ParsedTermStmt X86_64 ids -> [BlockLabel 64]
termSucc addr stmt =
  case stmt of
    ParsedCall _ (Just ret_addr) -> [mkRootBlockLabel ret_addr]
    ParsedCall _ Nothing -> []
    ParsedJump _ tgt -> [mkRootBlockLabel tgt]
    ParsedLookupTable _ _ v -> V.toList (mkRootBlockLabel <$> v)
    ParsedReturn{} -> []
    ParsedBranch _ t_idx f_idx -> [GeneratedBlock addr t_idx, GeneratedBlock addr f_idx]
    ParsedSyscall _ ret -> [mkRootBlockLabel ret]
    ParsedTranslateError{} -> []
    ClassifyFailure{} -> []

type FunPredMap w = Map (BlockLabel w) [BlockLabel w]

-- | This function figures out what the block requires
-- (i.e., addresses that are stored to, and the value stored), along
-- with a map of how demands by successor blocks map back to
-- assignments and registers.
funBlockPreds :: DiscoveryFunInfo X86_64 ids -> FunPredMap 64
funBlockPreds info = Map.fromListWith (++)
  [ (next, [prev])
  | region <- Map.elems (info^.parsedBlocks)
    -- Get address of region
  , let addr = regionAddr region
    -- Get blocks in region along with index.
  , (idx, block) <- Map.toList (regionBlockMap region)
    -- Create index for input block
  , let prev = GeneratedBlock addr idx
    -- Create input for next block
  , next <- termSucc addr (pblockTerm block)
  ]

-------------------------------------------------------------------------------

-- What does a given register depend upon?  Records both assignments
-- and registers (transitively through Apps etc.)
type RegDeps ids = (Set (Some (AssignId ids)), Set (Some X86Reg))
type AssignmentCache ids = Map (Some (AssignId ids)) (RegDeps ids)

-- | Map from address to type of function at that address
type AddrToFunctionTypeMap = Map (SegmentedAddr 64) FunctionType

-- The algorithm computes the set of direct deps (i.e., from writes)
-- and then iterates, propagating back via the register deps.
data RegisterUseState ids = RUS {
    -- | Holds the set of registers that we need.  This is the final
    -- output of the algorithm, along with the _blockRegUses below.
  _assignmentUses     :: !(Set (Some (AssignId ids)))
    -- | Holds state about the set of registers that a block uses
    -- (required by this block or a successor).
  , _blockRegUses :: !(Map (SegmentedAddr 64) (Set (Some X86Reg)))
    -- | Holds the set of registers a block should define (used by a successor).
  , _blockRegProvides :: !(Map (BlockLabel 64) (Set (Some X86Reg)))
    -- | Maps defined registers to their deps.  Not defined for all
    -- variables, hence use of Map instead of RegState X86Reg
  , _blockInitDeps  :: !(Map (BlockLabel 64) (Map (Some X86Reg) (RegDeps ids)))
    -- | A cache of the registers and their deps.  The key is not included
    -- in the set of deps (but probably should be).
  , _assignmentCache :: !(AssignmentCache ids)
    -- | The set of blocks we need to consider.
  , _blockFrontier  :: !(Set (BlockLabel 64))
    -- | Function arguments derived from AddrToFunctionTypeMap
  , functionArgs    :: !AddrToFunctionTypeMap
  , currentFunctionType :: !FunctionType
  , thisSyscallPersonality :: !(SyscallPersonality X86_64)
    -- ^ System call personality
  }

initRegisterUseState :: SyscallPersonality X86_64 -> AddrToFunctionTypeMap -> SegmentedAddr 64 -> RegisterUseState ids
initRegisterUseState sysp fArgs fn =
  RUS { _assignmentUses     = Set.empty
      , _blockRegUses       = Map.empty
      , _blockRegProvides   = Map.empty
      , _blockInitDeps      = Map.empty
      , _assignmentCache    = Map.empty
      , _blockFrontier      = Set.empty
      , functionArgs        = fArgs
      , currentFunctionType = fromMaybe ftMinimumFunctionType (fArgs ^. at fn)
      , thisSyscallPersonality = sysp
      }

assignmentUses :: Simple Lens (RegisterUseState ids) (Set (Some (AssignId ids)))
assignmentUses = lens _assignmentUses (\s v -> s { _assignmentUses = v })

blockRegUses :: Simple Lens (RegisterUseState ids)
                            (Map (SegmentedAddr 64) (Set (Some X86Reg)))
blockRegUses = lens _blockRegUses (\s v -> s { _blockRegUses = v })

blockRegProvides :: Simple Lens (RegisterUseState ids)
                      (Map (BlockLabel 64) (Set (Some X86Reg)))
blockRegProvides = lens _blockRegProvides (\s v -> s { _blockRegProvides = v })

blockInitDeps :: Simple Lens (RegisterUseState ids)
                   (Map (BlockLabel 64) (Map (Some X86Reg) (RegDeps ids)))
blockInitDeps = lens _blockInitDeps (\s v -> s { _blockInitDeps = v })

blockFrontier :: Simple Lens (RegisterUseState ids) (Set (BlockLabel 64))
blockFrontier = lens _blockFrontier (\s v -> s { _blockFrontier = v })

assignmentCache :: Simple Lens (RegisterUseState ids) (AssignmentCache ids)
assignmentCache = lens _assignmentCache (\s v -> s { _assignmentCache = v })

type RegisterUseM ids a = State (RegisterUseState ids) a

-- ----------------------------------------------------------------------------------------
-- Phase one functions
-- ----------------------------------------------------------------------------------------

valueUses :: Value X86_64 ids tp -> RegisterUseM ids (RegDeps ids)
valueUses = zoom assignmentCache .
            foldValueCached (\_ _      -> (mempty, mempty))
                            (const (mempty, mempty))
                            (\r        -> (mempty, Set.singleton (Some r)))
                            (\asgn (assigns, regs) ->
                              (Set.insert (Some asgn) assigns, regs))

demandValue :: BlockLabel 64 -> Value X86_64 ids tp -> RegisterUseM ids ()
demandValue lbl v = do
  (assigns, regs) <- valueUses v
  assignmentUses %= Set.union assigns
  -- When we demand a value at a label, any register deps need to
  -- get propagated to the parent block (i.e., we only record reg
  -- uses for root labels)
  blockRegUses   %= Map.insertWith Set.union (labelAddr lbl) regs

nextBlock :: RegisterUseM ids (Maybe (BlockLabel 64))
nextBlock = blockFrontier %%= \s -> let x = Set.maxView s in (fmap fst x, maybe s snd x)

-- | Return the values bound to the given registers
registerValues :: Functor t
               => RegState X86Reg (Value X86_64 ids)
               -> t (Some X86Reg)
               -> t (Some (Value X86_64 ids))
registerValues regState = fmap (\(Some r) -> Some (regState^.boundValue r))

-- | Get values that must be evaluated to execute terminal statement.
termStmtValues :: SyscallPersonality X86_64
               -> Memory 64
               -> AddrToFunctionTypeMap
                  -- ^ Map from addresses to function type
               -> FunctionType
                  -- ^ Type of this function
               -> ParsedTermStmt X86_64 ids
                  -- ^ Statement to get value of
               -> [Some (Value X86_64 ids)]
termStmtValues sysp mem typeMap curFunType tstmt =
  case tstmt of
    ParsedCall proc_state _m_ret_addr ->
      -- Get function type associated with function
      let ft | Just faddr <- asLiteralAddr mem (proc_state^.boundValue ip_reg)
             , Just ftp <- Map.lookup faddr typeMap = ftp
             | otherwise = ftMaximumFunctionType
       in registerValues proc_state (Some ip_reg : ftArgRegs ft)
    ParsedJump _proc_state _tgt_addr -> []
    ParsedLookupTable _proc_state idx _vec -> [Some idx]
    ParsedReturn proc_state ->
      let regs = (Some <$> take (fnNIntRets curFunType) x86ResultRegs)
              ++ (Some <$> take (fnNFloatRets curFunType) x86FloatResultRegs)
       in registerValues proc_state regs
    ParsedBranch c _ _ -> [Some c]
    ParsedSyscall proc_state _ -> registerValues proc_state (Some <$> (sysReg : argRegs))
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
               | otherwise =
                   syscallRegs
    ParsedTranslateError _ -> []
    ClassifyFailure _ -> []

-- | This function figures out what the block requires (i.e.,
-- addresses that are stored to, and the value stored), along with a
-- map of how demands by successor blocks map back to assignments and
-- registers.
summarizeBlock :: forall ids
               .  Memory 64
               -> DiscoveryFunInfo X86_64 ids
               -> BlockLabel 64
               -> RegisterUseM ids ()
summarizeBlock mem interp_state root_label = go root_label
  where
    go :: BlockLabel 64 -> RegisterUseM ids ()
    go lbl = do
      debugM DRegisterUse ("Summarizing " ++ show lbl)
      Just b <- return $ lookupParsedBlock interp_state lbl

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
      let termValues = termStmtValues sysp mem typeMap cur_ft (pblockTerm b)
      traverse_ (\(Some r) -> demandValue lbl r)
                (concatMap stmtDemandedValues (pblockStmts b) ++ termValues)


      -- Update frontier with successor states for terminal
      blockFrontier %= \s -> foldr Set.insert s (termSucc (labelAddr lbl) (pblockTerm b))

      case pblockTerm b of
        ParsedCall proc_state _ -> do
          -- Get function type associated with function
          let ft | Just faddr <- asLiteralAddr mem (proc_state^.boundValue ip_reg)
                 , Just ftp <- Map.lookup faddr typeMap = ftp
                 | otherwise = ftMaximumFunctionType
          addRegisterUses proc_state (Some sp_reg : Set.toList x86CalleeSavedRegs)
          -- Ensure that result registers are defined, but do not have any deps.
          traverse_ (\r -> blockInitDeps . ix lbl %= Map.insert r (Set.empty, Set.empty)) $
                    (Some <$> ftIntRetRegs ft)
                    ++ (Some <$> ftFloatRetRegs ft)
                    ++ [Some df_reg]

        ParsedJump proc_state _ ->
          addRegisterUses proc_state x86StateRegs
        ParsedLookupTable proc_state _ _ ->
          addRegisterUses proc_state x86StateRegs
        ParsedReturn _ ->
          pure ()
        ParsedBranch _ x y -> do
          go (lbl { labelIndex = x })
          go (lbl { labelIndex = y })
        ParsedSyscall proc_state _ -> do
          -- FIXME: clagged from call above
          addRegisterUses proc_state (Some sp_reg : (Set.toList x86CalleeSavedRegs))
          let insReg sr = blockInitDeps . ix lbl %= Map.insert sr (Set.empty, Set.empty)
          traverse_ insReg (spResultRegisters sysp)
        ParsedTranslateError _ ->
          error "Cannot identify register use in code where translation error occurs"
        ClassifyFailure _ ->
          error $ "Classification failed: " ++ show (labelAddr root_label)

-- | Explore states until we have reached end of frontier.
summarizeIter :: Memory 64
              -> DiscoveryFunInfo X86_64 ids
              -> Set (BlockLabel 64)
              -> Maybe (BlockLabel 64)
              -> RegisterUseM ids ()
summarizeIter _   _   _     Nothing = return ()
summarizeIter mem ist seen (Just lbl)
  | lbl `Set.member` seen = nextBlock >>= summarizeIter mem ist seen
  | otherwise = do
     summarizeBlock mem ist lbl
     lbl' <- nextBlock
     summarizeIter mem ist (Set.insert lbl seen) lbl'

-- We use ix here as it returns mempty if there is no key, which is
-- the right behavior here.
calculateFixpoint :: FunPredMap 64
                  -> Map (SegmentedAddr 64) (Set (Some X86Reg))
                  -> RegisterUseM ids ()
calculateFixpoint predMap new
  | Just ((currAddr, newRegs), rest) <- Map.maxViewWithKey new = do
      -- Get predicates for current block
      let preds = fromMaybe [] (Map.lookup (mkRootBlockLabel currAddr) predMap)
      -- propagate backwards any new registers in the predecessors
      nexts <- filter (not . Set.null . snd) <$> mapM (doOne newRegs) preds
      calculateFixpoint predMap (Map.unionWith Set.union rest (Map.fromList nexts))
  | otherwise = return ()
  where
    doOne :: Set (Some X86Reg)
             -> BlockLabel 64
             -> RegisterUseM ids (SegmentedAddr 64, Set (Some X86Reg))
    doOne newRegs predLbl = do
      depMap   <- use (blockInitDeps . ix predLbl)

      debugM DRegisterUse (show predLbl ++ " -> " ++ show newRegs)

      -- record that predLbl provides newRegs
      blockRegProvides %= Map.insertWith Set.union predLbl newRegs

      let (assigns, regs) = mconcat [ depMap ^. ix r | r <- Set.toList newRegs ]
      assignmentUses %= Set.union assigns
      -- update uses, returning value before this iteration
      seenRegs <- uses blockRegUses (fromMaybe Set.empty . Map.lookup (labelAddr predLbl))
      blockRegUses  %= Map.insertWith Set.union (labelAddr predLbl) regs

      return (labelAddr predLbl, regs `Set.difference` seenRegs)

-- | Map from addresses to the registers they depend on
type DemandedUseMap = Map (SegmentedAddr 64) (Set (Some X86Reg))

-- | Pretty pritn a demanded use map
ppDemandedUseMap :: DemandedUseMap -> Doc
ppDemandedUseMap m = vcat (ppEntry <$> Map.toList m)
  where ppEntry :: (SegmentedAddr 64, Set (Some X86Reg)) -> Doc
        ppEntry (addr, regs) = text (show addr) <> char ':' <+> hsep (ppReg <$> Set.toList regs)
        ppReg :: Some X86Reg -> Doc
        ppReg (Some r) = text (show r)


-- | Returns the maximum stack argument used by the function, that is,
-- the highest index above sp0 that is read or written.
registerUse :: SyscallPersonality X86_64
            -> AddrToFunctionTypeMap
            -> Memory 64
            -> DiscoveryFunInfo X86_64 ids
            -> FunPredMap 64
               -- ^ Predecessors for each block in function
            -> ( Set (Some (AssignId ids))
               , DemandedUseMap
               , Map (BlockLabel 64) (Set (Some X86Reg))
               )
registerUse sysp fArgs mem ist predMap =
  flip evalState (initRegisterUseState sysp fArgs addr) $ do
    -- Run the first phase (block summarization)
    summarizeIter mem ist Set.empty (Just lbl0)
    -- propagate back uses
    new <- use blockRegUses
    -- debugM DRegisterUse ("0x40018d ==> " ++ show (Map.lookup (GeneratedBlock 0x40018d 0) new))
    -- let new' = Map.singleton (GeneratedBlock 0x40018d 0) (Set.fromList (Some <$> [N.rax, N.rdx]))
    calculateFixpoint predMap new
    (,,) <$> use assignmentUses
         <*> use blockRegUses
         <*> use blockRegProvides
  where
    addr = discoveredFunAddr ist
    lbl0 = GeneratedBlock addr 0
