{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reopt.CFG.RegisterUse
  ( registerUse
  ) where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Foldable as Fold (traverse_)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Parameterized.Some
import           Data.Set (Set)
import qualified Data.Set as Set

import           Reopt.CFG.InterpState
import           Reopt.CFG.Representation
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types


-- -----------------------------------------------------------------------------

-- What does a given register depend upon?  Records both assignments
-- and registers (transitively through Apps etc.)
type RegDeps = (Set (Some Assignment), Set (Some N.RegisterName))
type AssignmentCache = Map (Some Assignment) RegDeps

-- The algorithm computes the set of direct deps (i.e., from writes)
-- and then iterates, propagating back via the register deps.
data RegisterUseState = RUS {
  -- | Holds the set of registers that we need.  This is the final
  -- output of the algorithm, along with the _blockRegUses below.
  _assignmentUses     :: !(Set (Some Assignment))  
  -- | Holds state about the set of registers that a block uses
  -- (required by this block or a successor).
  , _blockRegUses :: !(Map BlockLabel (Set (Some N.RegisterName)))
  -- | Holds the set of registers a blocm should define (used by a successor).
  , _blockRegProvides :: !(Map BlockLabel (Set (Some N.RegisterName)))

  -- | Maps defined registers to their deps.  Not defined for all
  -- variables, hence use of Map instead of X86State
  , _blockInitDeps  :: !(Map BlockLabel (Map (Some N.RegisterName) RegDeps))
  -- | The list of predecessors for a given block
  , _blockPreds     :: !(Map BlockLabel [BlockLabel])
  -- | A cache of the registers and their deps.  The key is not included
  -- in the set of deps (but probably should be).
  , _assignmentCache :: !AssignmentCache
  -- | The set of blocks we need to consider.    
  , _blockFrontier  :: !(Set BlockLabel)  }

initRegisterUseState :: RegisterUseState
initRegisterUseState =
  RUS { _assignmentUses  = Set.empty
      , _blockRegUses    = Map.empty
      , _blockRegProvides = Map.empty
      , _blockInitDeps   = Map.empty
      , _blockPreds      = Map.empty
      , _assignmentCache = Map.empty
      , _blockFrontier   = Set.empty }

assignmentUses :: Simple Lens RegisterUseState (Set (Some Assignment))
assignmentUses = lens _assignmentUses (\s v -> s { _assignmentUses = v })

blockRegUses :: Simple Lens RegisterUseState (Map BlockLabel (Set (Some N.RegisterName)))
blockRegUses = lens _blockRegUses (\s v -> s { _blockRegUses = v })

blockRegProvides :: Simple Lens RegisterUseState (Map BlockLabel (Set (Some N.RegisterName)))
blockRegProvides = lens _blockRegUses (\s v -> s { _blockRegUses = v })

blockInitDeps :: Simple Lens RegisterUseState (Map BlockLabel (Map (Some N.RegisterName) RegDeps))
blockInitDeps = lens _blockInitDeps (\s v -> s { _blockInitDeps = v })

blockFrontier :: Simple Lens RegisterUseState (Set BlockLabel)
blockFrontier = lens _blockFrontier (\s v -> s { _blockFrontier = v })

blockPreds :: Simple Lens RegisterUseState (Map BlockLabel [BlockLabel])
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

-- helper type to make a monad a monoid in the obvious way
newtype StateMonadMonoid s m = SMM { getStateMonadMonoid :: State s m }
                               deriving (Functor, Applicative, Monad, MonadState s)

instance Monoid m => Monoid (StateMonadMonoid s m) where
  mempty = return mempty
  mappend m m' = do mv <- m
                    mv' <- m'
                    return (mappend mv mv')

foldValueCached :: forall m tp. (Monoid m)
                   => (forall n.  NatRepr n -> Integer -> m)
                   -> (forall cl. N.RegisterName cl -> m)
                   -> (forall tp'. Assignment tp' -> m -> m)
                   -> Value tp -> State (Map (Some Assignment) m) m
foldValueCached litf initf assignf val = getStateMonadMonoid (go val)
  where
    go :: forall tp'. Value tp' -> StateMonadMonoid (Map (Some Assignment) m) m 
    go v =
      case v of
        BVValue sz i -> return $ litf sz i
        Initial r    -> return $ initf r 
        AssignedValue asgn@(Assignment _ rhs) ->
          do m_v <- use (at (Some asgn))
             case m_v of
               Just v' -> return $ assignf asgn v'
               Nothing -> 
                  do rhs_v <- goAssignRHS rhs
                     at (Some asgn) .= Just rhs_v
                     return (assignf asgn rhs_v)

    goAssignRHS :: forall tp'. AssignRhs tp' -> StateMonadMonoid (Map (Some Assignment) m) m 
    goAssignRHS v =
      case v of
        EvalApp a -> foldApp go a
        SetUndefined _w -> mempty
        Read loc
         | MemLoc addr _ <- loc -> go addr
         | otherwise            -> mempty -- FIXME: what about ControlLoc etc.
        MemCmp _sz cnt src dest rev -> mconcat [ go cnt, go src, go dest, go rev ]

-- ----------------------------------------------------------------------------------------

type RegisterUseM a = State RegisterUseState a

-- ----------------------------------------------------------------------------------------
-- Phase one functions
-- ----------------------------------------------------------------------------------------

-- | This registers a block in the first phase (block discovery).
addEdge :: BlockLabel -> BlockLabel -> RegisterUseM ()
addEdge source dest = 
  do -- record the edge
     blockPreds    %= Map.insertWith mappend dest [source]
     blockFrontier %= Set.insert dest

valueUses :: Value tp -> RegisterUseM RegDeps
valueUses = zoom assignmentCache .
            foldValueCached (\_ _      -> (mempty, mempty))
                            (\r        -> (mempty, Set.singleton (Some r)))
                            (\asgn (assigns, regs) -> (Set.insert (Some asgn) assigns, regs))

demandValue :: BlockLabel -> Value tp -> RegisterUseM ()
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

nextBlock :: RegisterUseM (Maybe BlockLabel)
nextBlock = blockFrontier %%= \s -> let x = Set.maxView s in (fmap fst x, maybe s snd x)

-- | Returns the maximum stack argument used by the function, that is,
-- the highest index above sp0 that is read or written.
registerUse :: InterpState -> CodeAddr -> (Set (Some Assignment)
                                          , Map BlockLabel (Set (Some N.RegisterName))
                                          , Map BlockLabel (Set (Some N.RegisterName))                                            
                                          , Map BlockLabel [BlockLabel])
registerUse ist addr = 
  flip evalState initRegisterUseState $ do
    -- Run the first phase (block summarization)
    summarizeIter ist Set.empty (Just lbl0)
    -- propagate back uses
    new <- use blockRegUses
    calculateFixpoint new
    (,,,) <$> use assignmentUses
          <*> use blockRegUses
          <*> use blockRegProvides
          <*> use blockPreds
  where
    lbl0 = GeneratedBlock addr 0

-- We use ix here as it returns mempty if there is no key, which is
-- the right behavior here.
calculateFixpoint :: Map BlockLabel (Set (Some N.RegisterName)) -> RegisterUseM ()
calculateFixpoint new
  | Just ((currLbl, newRegs), rest) <- Map.maxViewWithKey new =
      -- propagate backwards any new registers in the predecessors
      do preds <- use (blockPreds . ix currLbl) 
         nexts <- filter (not . Set.null . snd) <$> mapM (doOne newRegs) preds
         calculateFixpoint (Map.unionWith Set.union rest (Map.fromList nexts))
  | otherwise = return ()
  where
    doOne :: Set (Some N.RegisterName) -> BlockLabel
             -> RegisterUseM (BlockLabel, Set (Some N.RegisterName))
    doOne newRegs predLbl = do
      depMap   <- use (blockInitDeps . ix predLbl)

      -- record that predLbl provides newRegs
      blockRegProvides %= Map.insertWith Set.union predLbl newRegs

      let (assigns, regs) = mconcat [ depMap ^. ix r | r <- Set.toList newRegs ]
          lbl' = rootBlockLabel predLbl

      assignmentUses %= Set.union assigns
      -- update uses, returning value before this iteration
      seenRegs <- blockRegUses  . ix lbl' <<%= Set.union regs

      return (lbl', regs `Set.difference` seenRegs)

-- | Explore states until we have reached end of frontier.
summarizeIter :: InterpState
               -> Set BlockLabel
               -> Maybe BlockLabel
               -> RegisterUseM ()
summarizeIter _   _     Nothing = return ()
summarizeIter ist seen (Just lbl)
  | lbl `Set.member` seen = nextBlock >>= summarizeIter ist seen
  | otherwise = do summarizeBlock ist lbl
                   lbl' <- nextBlock
                   summarizeIter ist (Set.insert lbl seen) lbl'


-- | This function figures out what the block requires 
-- (i.e., addresses that are stored to, and the value stored), along
-- with a map of how demands by successor blocks map back to
-- assignments and registers.
summarizeBlock :: InterpState 
                  -> BlockLabel
                  -> RegisterUseM ()
summarizeBlock interp_state root_label = go root_label
  where
    go :: BlockLabel -> RegisterUseM ()
    go lbl = do
      Just (b, m_pterm) <- return $ getClassifyBlock lbl interp_state

      let goStmt (Write (MemLoc addr _tp) v)
            = do demandValue lbl addr
                 demandValue lbl v

          goStmt _ = return ()

          -- Demand the values from the state at the given registers
          demandRegisters :: forall t. Foldable t =>
                             X86State Value
                             -> t (Some N.RegisterName)
                             -> RegisterUseM ()
          demandRegisters s rs = traverse_ (\(Some r) -> demandValue lbl (s ^. register r)) rs

          -- Figure out the deps of the given registers and update the state for the current label
          addRegisterUses :: X86State Value
                             -> [Some N.RegisterName]
                             -> RegisterUseM () -- Map (Some N.RegisterName) RegDeps
          addRegisterUses s rs = do 
             vs <- mapM (\(Some r) -> (,) (Some r) <$> valueUses (s ^. register r)) rs
             blockInitDeps %= Map.insertWith Map.union lbl (Map.fromList vs)

      case m_pterm of
        Just (ParsedBranch c x y) -> do
          traverse_ goStmt (blockStmts b)
          demandValue lbl c
          go x
          go y

        Just (ParsedCall proc_state stmts' _fn m_ret_addr) -> do 
          traverse_ goStmt stmts'

          demandRegisters proc_state [Some N.rip]
          demandRegisters proc_state (Some <$> x86ArgumentRegisters)
          demandRegisters proc_state (Some <$> x86FloatArgumentRegisters) -- FIXME: required?

          case m_ret_addr of
            Nothing       -> return ()
            Just ret_addr -> addEdge lbl (mkRootBlockLabel ret_addr)

          -- FIXME: rsp here?
          addRegisterUses proc_state (Some N.rsp : (Set.toList x86CalleeSavedRegisters))
          -- Ensure that result registers are defined, but do not have any deps.
          traverse_ (\r -> blockInitDeps . ix lbl %= Map.insert r (Set.empty, Set.empty)) x86ResultRegisters

        Just (ParsedJump proc_state tgt_addr) -> do 
            traverse_ goStmt (blockStmts b)
            addRegisterUses proc_state x86StateRegisters
            addEdge lbl (mkRootBlockLabel tgt_addr)

        Just (ParsedReturn proc_state stmts') -> do
            traverse_ goStmt stmts'
            demandRegisters proc_state x86ResultRegisters

        Just (ParsedLookupTable _proc_state _idx _vec) -> error "LookupTable"

        Nothing -> return () -- ???


