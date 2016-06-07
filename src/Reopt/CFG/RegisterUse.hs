{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Reopt.CFG.RegisterUse
  ( registerUse
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
import           Data.Word

import           Reopt.CFG.DiscoveryInfo
import           Reopt.CFG.Representation
import           Reopt.CFG.FnRep (FunctionType(..)
                                  , ftMaximumFunctionType
                                  , ftMinimumFunctionType
                                  , ftIntArgRegs , ftFloatArgRegs
                                  , ftIntRetRegs , ftFloatRetRegs)
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.X86State
import           Reopt.Utils.Debug

import           Debug.Trace

-- -----------------------------------------------------------------------------

-- What does a given register depend upon?  Records both assignments
-- and registers (transitively through Apps etc.)
type RegDeps = (Set AssignId, Set (Some N.RegisterName))
type AssignmentCache = Map (Some (Assignment X86_64)) RegDeps

type FunctionArgs = Map CodeAddr FunctionType

-- The algorithm computes the set of direct deps (i.e., from writes)
-- and then iterates, propagating back via the register deps.
data RegisterUseState = RUS {
    -- | Holds the set of registers that we need.  This is the final
    -- output of the algorithm, along with the _blockRegUses below.
  _assignmentUses     :: !(Set AssignId)
    -- | Holds state about the set of registers that a block uses
    -- (required by this block or a successor).
  , _blockRegUses :: !(Map (BlockLabel Word64) (Set (Some N.RegisterName)))
    -- | Holds the set of registers a block should define (used by a successor).
  , _blockRegProvides :: !(Map (BlockLabel Word64) (Set (Some N.RegisterName)))

  -- | Maps defined registers to their deps.  Not defined for all
  -- variables, hence use of Map instead of X86State
  , _blockInitDeps  :: !(Map (BlockLabel Word64) (Map (Some N.RegisterName) RegDeps))
  -- | The list of predecessors for a given block
  , _blockPreds     :: !(Map (BlockLabel Word64) [BlockLabel Word64])
  -- | A cache of the registers and their deps.  The key is not included
  -- in the set of deps (but probably should be).
  , _assignmentCache :: !AssignmentCache
  -- | The set of blocks we need to consider.
  , _blockFrontier  :: !(Set (BlockLabel Word64))
  -- | Function arguments derived from FunctionArgs
  , functionArgs    :: !FunctionArgs
  , currentFunctionType :: !FunctionType
  }

initRegisterUseState :: FunctionArgs -> CodeAddr -> RegisterUseState
initRegisterUseState fArgs fn =
  RUS { _assignmentUses  = Set.empty
      , _blockRegUses    = Map.empty
      , _blockRegProvides = Map.empty
      , _blockInitDeps   = Map.empty
      , _blockPreds      = Map.empty
      , _assignmentCache = Map.empty
      , _blockFrontier   = Set.empty
      , functionArgs     = fArgs
      , currentFunctionType = cft }
  where
    cft = fromMaybe ftMinimumFunctionType (fArgs ^. at fn)

assignmentUses :: Simple Lens RegisterUseState (Set AssignId)
assignmentUses = lens _assignmentUses (\s v -> s { _assignmentUses = v })

blockRegUses :: Simple Lens RegisterUseState (Map (BlockLabel Word64) (Set (Some N.RegisterName)))
blockRegUses = lens _blockRegUses (\s v -> s { _blockRegUses = v })

blockRegProvides :: Simple Lens RegisterUseState (Map (BlockLabel Word64) (Set (Some N.RegisterName)))
blockRegProvides = lens _blockRegProvides (\s v -> s { _blockRegProvides = v })

blockInitDeps :: Simple Lens RegisterUseState (Map (BlockLabel Word64) (Map (Some N.RegisterName) RegDeps))
blockInitDeps = lens _blockInitDeps (\s v -> s { _blockInitDeps = v })

blockFrontier :: Simple Lens RegisterUseState (Set (BlockLabel Word64))
blockFrontier = lens _blockFrontier (\s v -> s { _blockFrontier = v })

blockPreds :: Simple Lens RegisterUseState (Map (BlockLabel Word64) [BlockLabel Word64])
blockPreds = lens _blockPreds (\s v -> s { _blockPreds = v })

assignmentCache :: Simple Lens RegisterUseState AssignmentCache
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

type RegisterUseM a = State RegisterUseState a

-- ----------------------------------------------------------------------------------------
-- Phase one functions
-- ----------------------------------------------------------------------------------------

-- | This registers a block in the first phase (block discovery).
addEdge :: BlockLabel Word64 -> BlockLabel Word64 -> RegisterUseM ()
addEdge source dest =
  do -- record the edge
     debugM DRegisterUse ("Adding edge " ++ show source ++ " -> " ++ show dest)
     blockPreds    %= Map.insertWith mappend dest [source]
     blockFrontier %= Set.insert dest

valueUses :: Value X86_64 tp -> RegisterUseM RegDeps
valueUses = zoom assignmentCache .
            foldValueCached (\_ _      -> (mempty, mempty))
                            (\r        -> (mempty, Set.singleton (Some r)))
                            (\asgn (assigns, regs) -> (Set.insert (assignId asgn) assigns, regs))

demandValue :: BlockLabel Word64 -> Value X86_64 tp -> RegisterUseM ()
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

nextBlock :: RegisterUseM (Maybe (BlockLabel Word64))
nextBlock = blockFrontier %%= \s -> let x = Set.maxView s in (fmap fst x, maybe s snd x)

-- | Returns the maximum stack argument used by the function, that is,
-- the highest index above sp0 that is read or written.
registerUse :: FunctionArgs
            -> DiscoveryInfo
            -> CodeAddr
            -> ( Set AssignId
               , Map (BlockLabel Word64) (Set (Some N.RegisterName))
               , Map (BlockLabel Word64) (Set (Some N.RegisterName))
               , Map (BlockLabel Word64) [BlockLabel Word64]
               )
registerUse fArgs ist addr =
  flip evalState (initRegisterUseState fArgs addr) $ do
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
calculateFixpoint :: Map (BlockLabel Word64) (Set (Some N.RegisterName)) -> RegisterUseM ()
calculateFixpoint new
  | Just ((currLbl, newRegs), rest) <- Map.maxViewWithKey new =
      -- propagate backwards any new registers in the predecessors
      do preds <- use (blockPreds . ix currLbl)
         nexts <- filter (not . Set.null . snd) <$> mapM (doOne newRegs) preds
         calculateFixpoint (Map.unionWith Set.union rest (Map.fromList nexts))
  | otherwise = return ()
  where
    doOne :: Set (Some N.RegisterName)
             -> BlockLabel Word64
             -> RegisterUseM (BlockLabel Word64, Set (Some N.RegisterName))
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
summarizeIter :: DiscoveryInfo
               -> Set (BlockLabel Word64)
               -> Maybe (BlockLabel Word64)
               -> RegisterUseM ()
summarizeIter _   _     Nothing = return ()
summarizeIter ist seen (Just lbl)
  | lbl `Set.member` seen = nextBlock >>= summarizeIter ist seen
  | otherwise = do summarizeBlock ist lbl
                   lbl' <- nextBlock
                   summarizeIter ist (Set.insert lbl seen) lbl'

-- | This returns the arguments associated with a particular function.
lookupFunctionArgs :: Either Word64 (BVValue X86_64 64)
                   -> RegisterUseM FunctionType
lookupFunctionArgs fn =
  case fn of
    Right _dynaddr -> return ftMaximumFunctionType
    Left faddr -> do
      fArgs <- gets (Map.lookup faddr . functionArgs)
      case fArgs of
        Nothing -> do debugM DUrgent ("Warning: no args for function " ++ show faddr)
                      return ftMaximumFunctionType
        Just v  -> return v

-- | This function figures out what the block requires
-- (i.e., addresses that are stored to, and the value stored), along
-- with a map of how demands by successor blocks map back to
-- assignments and registers.
summarizeBlock :: DiscoveryInfo
                  -> BlockLabel Word64
                  -> RegisterUseM ()
summarizeBlock interp_state root_label = go root_label
  where
    go :: BlockLabel Word64 -> RegisterUseM ()
    go lbl = do
      debugM DRegisterUse ("Summarizing " ++ show lbl)
      Just (b, m_pterm) <- return $ getClassifyBlock lbl interp_state

      let goStmt (WriteMem addr v) = do
            demandValue lbl addr
            demandValue lbl v

          goStmt (MemCopy _sz cnt src dest rev) = do
            demandValue lbl cnt
            demandValue lbl src
            demandValue lbl dest
            demandValue lbl rev

          goStmt (MemSet cnt v ptr df) = do
            demandValue lbl cnt
            demandValue lbl v
            demandValue lbl ptr
            demandValue lbl df

          goStmt _ = return ()

          -- Demand the values from the state at the given registers
          demandRegisters :: forall t. Foldable t
                          => X86State (Value X86_64)
                          -> t (Some N.RegisterName)
                          -> RegisterUseM ()
          demandRegisters s rs = traverse_ (\(Some r) -> demandValue lbl (s ^. register r)) rs

          -- Figure out the deps of the given registers and update the state for the current label
          addRegisterUses :: X86State (Value X86_64)
                             -> [Some N.RegisterName]
                             -> RegisterUseM () -- Map (Some N.RegisterName) RegDeps
          addRegisterUses s rs = do
             vs <- mapM (\(Some r) -> (Some r,) <$> valueUses (s ^. register r)) rs
             blockInitDeps %= Map.insertWith (Map.unionWith mappend) lbl (Map.fromList vs)

      when (labelAddr lbl == 0x400128) $ do
        trace ("Computing registers uses for block " ++ show m_pterm) $ do
          return ()

      case m_pterm of
        Just (ParsedBranch c x y) -> do
          traverse_ goStmt (blockStmts b)
          demandValue lbl c
          go x
          go y

        Just (ParsedCall proc_state stmts' fn m_ret_addr) -> do
          traverse_ goStmt stmts'

          ft <- lookupFunctionArgs fn
          when (labelAddr lbl == 0x400128) $ do
            trace ("Computing registers uses for call " ++ show ft) $ do
              return ()

          demandRegisters proc_state [Some N.rip]
          demandRegisters proc_state (Some <$> ftIntArgRegs ft)
          demandRegisters proc_state (Some <$> ftFloatArgRegs ft)

          case m_ret_addr of
            Nothing       -> return ()
            Just ret_addr -> addEdge lbl (mkRootBlockLabel ret_addr)

          addRegisterUses proc_state (Some N.rsp : (Set.toList x86CalleeSavedRegisters))
          -- Ensure that result registers are defined, but do not have any deps.
          traverse_ (\r -> blockInitDeps . ix lbl %= Map.insert r (Set.empty, Set.empty))
                    ((Some <$> ftIntRetRegs ft) ++ (Some <$> ftFloatRetRegs ft) ++ ([Some N.df]))

        Just (ParsedJump proc_state tgt_addr) -> do
            traverse_ goStmt (blockStmts b)
            addRegisterUses proc_state x86StateRegisters
            addEdge lbl (mkRootBlockLabel tgt_addr)

        Just (ParsedReturn proc_state stmts') -> do
            traverse_ goStmt stmts'
            ft <- gets currentFunctionType
            demandRegisters proc_state ((Some <$> take (fnNIntRets ft) x86ResultRegisters)
                                        ++ (Some <$> take (fnNFloatRets ft) x86FloatResultRegisters))

        Just (ParsedSyscall proc_state next_addr _call_no _pname _name argRegs rregs) -> do
          -- FIXME: clagged from call above
          traverse_ goStmt (blockStmts b)
          demandRegisters proc_state (Some <$> argRegs)
          addEdge lbl (mkRootBlockLabel next_addr)
          addRegisterUses proc_state (Some N.rsp : (Set.toList x86CalleeSavedRegisters))

          traverse_ (\r -> blockInitDeps . ix lbl %= Map.insert r (Set.empty, Set.empty)) rregs

        Just (ParsedLookupTable proc_state _idx vec) -> do
          traverse_ goStmt (blockStmts b)

          demandRegisters proc_state [Some N.rip]
          addRegisterUses proc_state x86StateRegisters
          traverse_ (addEdge lbl . mkRootBlockLabel) vec

        Nothing -> return () -- ???
