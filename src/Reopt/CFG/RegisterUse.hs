{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Reopt.CFG.RegisterUse
  ( FunctionArgs
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
import qualified Data.Vector as V ()

import           Data.Macaw.Architecture.Syscall
import           Data.Macaw.CFG
import           Data.Macaw.DebugLogging
import           Data.Macaw.Discovery.Info
import           Data.Macaw.Types

import           Reopt.CFG.FnRep ( FunctionType(..)
                                 , ftMaximumFunctionType
                                 , ftMinimumFunctionType
                                 , ftIntArgRegs
                                 , ftFloatArgRegs
                                 , ftIntRetRegs
                                 , ftFloatRetRegs
                                 )
import           Reopt.Machine.X86State

-- -----------------------------------------------------------------------------

-- What does a given register depend upon?  Records both assignments
-- and registers (transitively through Apps etc.)
type RegDeps ids = (Set (Some (AssignId ids)), Set (Some X86Reg))
type AssignmentCache ids = Map (Some (AssignId ids)) (RegDeps ids)

type FunctionArgs = Map (SegmentedAddr 64) FunctionType

-- The algorithm computes the set of direct deps (i.e., from writes)
-- and then iterates, propagating back via the register deps.
data RegisterUseState ids = RUS {
    -- | Holds the set of registers that we need.  This is the final
    -- output of the algorithm, along with the _blockRegUses below.
  _assignmentUses     :: !(Set (Some (AssignId ids)))
    -- | Holds state about the set of registers that a block uses
    -- (required by this block or a successor).
  , _blockRegUses :: !(Map (BlockLabel 64) (Set (Some X86Reg)))
    -- | Holds the set of registers a block should define (used by a successor).
  , _blockRegProvides :: !(Map (BlockLabel 64) (Set (Some X86Reg)))
    -- | Maps defined registers to their deps.  Not defined for all
    -- variables, hence use of Map instead of RegState X86Reg
  , _blockInitDeps  :: !(Map (BlockLabel 64) (Map (Some X86Reg) (RegDeps ids)))
    -- | The list of predecessors for a given block
  , _blockPreds     :: !(Map (BlockLabel 64) [BlockLabel 64])
    -- | A cache of the registers and their deps.  The key is not included
    -- in the set of deps (but probably should be).
  , _assignmentCache :: !(AssignmentCache ids)
    -- | The set of blocks we need to consider.
  , _blockFrontier  :: !(Set (BlockLabel 64))
    -- | Function arguments derived from FunctionArgs
  , functionArgs    :: !FunctionArgs
  , currentFunctionType :: !FunctionType
  , thisSyscallPersonality :: !(SyscallPersonality X86_64)
    -- ^ System call personality
  }

initRegisterUseState :: SyscallPersonality X86_64 -> FunctionArgs -> SegmentedAddr 64 -> RegisterUseState ids
initRegisterUseState sysp fArgs fn =
  RUS { _assignmentUses     = Set.empty
      , _blockRegUses       = Map.empty
      , _blockRegProvides   = Map.empty
      , _blockInitDeps      = Map.empty
      , _blockPreds         = Map.empty
      , _assignmentCache    = Map.empty
      , _blockFrontier      = Set.empty
      , functionArgs        = fArgs
      , currentFunctionType = fromMaybe ftMinimumFunctionType (fArgs ^. at fn)
      , thisSyscallPersonality = sysp
      }

assignmentUses :: Simple Lens (RegisterUseState ids) (Set (Some (AssignId ids)))
assignmentUses = lens _assignmentUses (\s v -> s { _assignmentUses = v })

blockRegUses :: Simple Lens (RegisterUseState ids)
                  (Map (BlockLabel 64) (Set (Some X86Reg)))
blockRegUses = lens _blockRegUses (\s v -> s { _blockRegUses = v })

blockRegProvides :: Simple Lens (RegisterUseState ids)
                      (Map (BlockLabel 64) (Set (Some X86Reg)))
blockRegProvides = lens _blockRegProvides (\s v -> s { _blockRegProvides = v })

blockInitDeps :: Simple Lens (RegisterUseState ids)
                   (Map (BlockLabel 64) (Map (Some X86Reg) (RegDeps ids)))
blockInitDeps = lens _blockInitDeps (\s v -> s { _blockInitDeps = v })

blockFrontier :: Simple Lens (RegisterUseState ids) (Set (BlockLabel 64))
blockFrontier = lens _blockFrontier (\s v -> s { _blockFrontier = v })

blockPreds :: Simple Lens (RegisterUseState ids)
              (Map (BlockLabel 64) [BlockLabel 64])
blockPreds = lens _blockPreds (\s v -> s { _blockPreds = v })

assignmentCache :: Simple Lens (RegisterUseState ids) (AssignmentCache ids)
assignmentCache = lens _assignmentCache (\s v -> s { _assignmentCache = v })

-- ----------------------------------------------------------------------------------------

-- FIXME: move

-- Unwraps all Apps etc, might visit an app twice (Add x x, for example)
-- foldValue :: forall m tp. Monoid m
--              => (forall n.  NatRepr n -> Integer -> m)
--              -> (forall cl. N.RegisterName cl -> m)
--              -> (forall tp. Assignment tp -> m -> m)
--              -> Value tp -> m
-- foldValue litf initf assignf val = go val
--   where
--     go :: forall tp'. Value tp' -> m
--     go v = case v of
--              BVValue sz i -> litf sz i
--              Initial r    -> initf r
--              AssignedValue asgn@(Assignment _ rhs) ->
--                assignf asgn (goAssignRHS rhs)

--     goAssignRHS :: forall tp'. AssignRhs tp' -> m
--     goAssignRHS v =
--       case v of
--         EvalApp a -> foldApp go a
--         SetUndefined _w -> mempty
--         Read loc
--          | MemLoc addr _ <- loc -> go addr
--          | otherwise            -> mempty -- FIXME: what about ControlLoc etc.
--         MemCmp _sz cnt src dest rev -> mconcat [ go cnt, go src, go dest, go rev ]


-- ----------------------------------------------------------------------------------------

type RegisterUseM ids a = State (RegisterUseState ids) a

-- ----------------------------------------------------------------------------------------
-- Phase one functions
-- ----------------------------------------------------------------------------------------

-- | This registers a block in the first phase (block discovery).
addEdge :: BlockLabel 64 -> BlockLabel 64 -> RegisterUseM ids ()
addEdge source dest = do
  -- record the edge
  blockPreds    %= Map.insertWith mappend dest [source]
  blockFrontier %= Set.insert dest

valueUses :: Value X86_64 ids tp -> RegisterUseM ids (RegDeps ids)
valueUses = zoom assignmentCache .
            foldValueCached (\_ _      -> (mempty, mempty))
                            (const (mempty, mempty))
                            (\r        -> (mempty, Set.singleton (Some r)))
                            (\asgn (assigns, regs) ->
                              (Set.insert (Some asgn) assigns, regs))

demandValue :: BlockLabel 64 -> Value X86_64 ids tp -> RegisterUseM ids ()
demandValue lbl v =
  do (assigns, regs) <- valueUses v
     assignmentUses %= Set.union assigns
     blockRegUses   %= Map.insertWith Set.union lbl' regs
  where
    -- When we demand a value at a label, any register deps need to
    -- get propagated to the parent block (i.e., we only record reg
    -- uses for root labels)
    -- FIXME: just use CodeAddr instead of BlockLabel?
    lbl' = rootBlockLabel lbl

nextBlock :: RegisterUseM ids (Maybe (BlockLabel 64))
nextBlock = blockFrontier %%= \s -> let x = Set.maxView s in (fmap fst x, maybe s snd x)

-- | Returns the maximum stack argument used by the function, that is,
-- the highest index above sp0 that is read or written.
registerUse :: SyscallPersonality X86_64
            -> FunctionArgs
            -> DiscoveryInfo X86_64 ids
            -> SegmentedAddr 64
            -> ( Set (Some (AssignId ids))
               , Map (BlockLabel 64) (Set (Some X86Reg))
               , Map (BlockLabel 64) (Set (Some X86Reg))
               , Map (BlockLabel 64) [BlockLabel 64]
               )
registerUse sysp fArgs ist addr =
  flip evalState (initRegisterUseState sysp fArgs addr) $ do
    -- Run the first phase (block summarization)
    summarizeIter ist Set.empty (Just lbl0)
    -- propagate back uses
    new <- use blockRegUses
    -- debugM DRegisterUse ("0x40018d ==> " ++ show (Map.lookup (GeneratedBlock 0x40018d 0) new))
    -- let new' = Map.singleton (GeneratedBlock 0x40018d 0) (Set.fromList (Some <$> [N.rax, N.rdx]))
    calculateFixpoint new
    (,,,) <$> use assignmentUses
          <*> use blockRegUses
          <*> use blockRegProvides
          <*> use blockPreds
  where
    lbl0 = GeneratedBlock addr 0

-- We use ix here as it returns mempty if there is no key, which is
-- the right behavior here.
calculateFixpoint :: Map (BlockLabel 64) (Set (Some X86Reg)) ->
                     RegisterUseM ids ()
calculateFixpoint new
  | Just ((currLbl, newRegs), rest) <- Map.maxViewWithKey new =
      -- propagate backwards any new registers in the predecessors
      do preds <- use (blockPreds . ix currLbl)
         nexts <- filter (not . Set.null . snd) <$> mapM (doOne newRegs) preds
         calculateFixpoint (Map.unionWith Set.union rest (Map.fromList nexts))
  | otherwise = return ()
  where
    doOne :: Set (Some X86Reg)
             -> BlockLabel 64
             -> RegisterUseM ids (BlockLabel 64, Set (Some X86Reg))
    doOne newRegs predLbl = do
      depMap   <- use (blockInitDeps . ix predLbl)

      debugM DRegisterUse (show predLbl ++ " -> " ++ show newRegs)

      -- record that predLbl provides newRegs
      blockRegProvides %= Map.insertWith Set.union predLbl newRegs

      let (assigns, regs) = mconcat [ depMap ^. ix r | r <- Set.toList newRegs ]
          lbl' = rootBlockLabel predLbl

      assignmentUses %= Set.union assigns
      -- update uses, returning value before this iteration
      seenRegs <- uses blockRegUses (maybe Set.empty id . Map.lookup lbl')
      blockRegUses  %= Map.insertWith Set.union lbl' regs

      return (lbl', regs `Set.difference` seenRegs)

-- | Explore states until we have reached end of frontier.
summarizeIter :: DiscoveryInfo X86_64 ids
               -> Set (BlockLabel 64)
               -> Maybe (BlockLabel 64)
               -> RegisterUseM ids ()
summarizeIter _   _     Nothing = return ()
summarizeIter ist seen (Just lbl)
  | lbl `Set.member` seen = nextBlock >>= summarizeIter ist seen
  | otherwise = do
      summarizeBlock ist lbl
      lbl' <- nextBlock
      summarizeIter ist (Set.insert lbl seen) lbl'

-- | This returns the arguments associated with a particular function.
lookupFunctionArgs :: SegmentedAddr 64
                   -> RegisterUseM ids FunctionType
lookupFunctionArgs faddr = do
  fArgs <- gets (Map.lookup faddr . functionArgs)
  case fArgs of
    Nothing -> do debugM DUrgent ("Warning: no args for function " ++ show faddr)
                  return ftMaximumFunctionType
    Just v  -> return v

-- | This function figures out what the block requires
-- (i.e., addresses that are stored to, and the value stored), along
-- with a map of how demands by successor blocks map back to
-- assignments and registers.
summarizeBlock :: forall ids
               .  DiscoveryInfo X86_64 ids
               -> BlockLabel 64
               -> RegisterUseM ids ()
summarizeBlock interp_state root_label = go root_label
  where
    go :: BlockLabel 64 -> RegisterUseM ids ()
    go lbl = do
      debugM DRegisterUse ("Summarizing " ++ show lbl)
      Just b <- return $ lookupParsedBlock interp_state lbl

      let goStmt (WriteMem addr v) = do
            demandValue lbl addr
            demandValue lbl v

          goStmt (ExecArchStmt (MemCopy _sz cnt src dest rev)) = do
            demandValue lbl cnt
            demandValue lbl src
            demandValue lbl dest
            demandValue lbl rev

          goStmt (ExecArchStmt (MemSet cnt v ptr df)) = do
            demandValue lbl cnt
            demandValue lbl v
            demandValue lbl ptr
            demandValue lbl df

          goStmt _ = return ()

          -- Demand the values from the state at the given registers
          demandRegisters :: forall t. Foldable t
                          => RegState X86Reg (Value X86_64 ids)
                          -> t (Some X86Reg)
                          -> RegisterUseM ids ()
          demandRegisters s rs = traverse_ (\(Some r) -> demandValue lbl (s^.boundValue r)) rs

          -- Figure out the deps of the given registers and update the state for the current label
          addRegisterUses :: RegState X86Reg (Value X86_64 ids)
                          -> [Some X86Reg]
                          -> RegisterUseM ids () -- Map (Some N.RegisterName) RegDeps
          addRegisterUses s rs = do
             vs <- mapM (\(Some r) -> (Some r,) <$> valueUses (s ^. boundValue r)) rs
             blockInitDeps %= Map.insertWith (Map.unionWith mappend) lbl (Map.fromList vs)

      traverse_ goStmt (pblockStmts b)
      case pblockTerm b of
        ParsedTranslateError _ ->
          error "Cannot identify register use in code where translation error occurs"
        ClassifyFailure msg ->
          error $ "Classification failed: " ++ msg
        ParsedBranch c x y -> do
          demandValue lbl c
          go (lbl { labelIndex = x })
          go (lbl { labelIndex = y })

        ParsedCall proc_state m_ret_addr -> do

          ft <-
            case asLiteralAddr (memory interp_state) (proc_state^.boundValue ip_reg) of
              Nothing -> pure ftMaximumFunctionType
              Just faddr -> lookupFunctionArgs faddr

          demandRegisters proc_state [Some ip_reg]

          demandRegisters proc_state $ Some <$> ftIntArgRegs ft
          demandRegisters proc_state $ Some <$> ftFloatArgRegs ft

          case m_ret_addr of
            Nothing       -> return ()
            Just ret_addr -> addEdge lbl (mkRootBlockLabel ret_addr)

          addRegisterUses proc_state (Some sp_reg : Set.toList x86CalleeSavedRegs)
          -- Ensure that result registers are defined, but do not have any deps.
          traverse_ (\r -> blockInitDeps . ix lbl %= Map.insert r (Set.empty, Set.empty)) $
                    (Some <$> ftIntRetRegs ft)
                    ++ (Some <$> ftFloatRetRegs ft)
                    ++ [Some df_reg]

        ParsedJump proc_state tgt_addr -> do
            addRegisterUses proc_state x86StateRegs
            addEdge lbl (mkRootBlockLabel tgt_addr)

        ParsedReturn proc_state -> do
            ft <- gets currentFunctionType
            demandRegisters proc_state ((Some <$> take (fnNIntRets ft) x86ResultRegs)
                                        ++ (Some <$> take (fnNFloatRets ft) x86FloatResultRegs))

        ParsedSyscall proc_state next_addr -> do

          sysp <- gets thisSyscallPersonality
          let rregs = spResultRegisters sysp

          do let syscallRegs :: [ArchReg X86_64 (BVType 64)]
                 syscallRegs = syscallArgumentRegs
             let argRegs
                   | Just call_no <- tryGetStaticSyscallNo interp_state (labelAddr lbl) proc_state
                   , Just (_,_,argtypes) <- Map.lookup (fromIntegral call_no) (spTypeInfo sysp) =
                     take (length argtypes) syscallRegs
                   | otherwise =
                     syscallRegs
             demandRegisters proc_state (Some <$> argRegs)


          -- FIXME: clagged from call above
          addEdge lbl (mkRootBlockLabel next_addr)
          addRegisterUses proc_state (Some sp_reg : (Set.toList x86CalleeSavedRegs))

          let insReg sr =
                blockInitDeps . ix lbl %= Map.insert sr (Set.empty, Set.empty)

          traverse_ insReg rregs

        ParsedLookupTable proc_state _idx vec -> do
          demandRegisters proc_state [Some ip_reg]
          addRegisterUses proc_state x86StateRegs
          traverse_ (addEdge lbl . mkRootBlockLabel) vec
