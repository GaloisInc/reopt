------------------------------------------------------------------------
-- |
-- Module           : Reopt.Semantics.CFGDiscovery
-- Description      : Control Flow Graph discovery support
-- Copyright        : (c) Galois, Inc 2015
-- Maintainer       : Simon Winwood <sjw@galois.com>
-- Stability        : provisional
--
-- This contains an implementation of a CFG discovery algorithm based
-- upon an interleaved abstract interpretation (currently unsound)
------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}

module Reopt.CFG.CFGDiscovery
       ( FinalCFG(..)
       , cfgFromAddress
       , assignmentAbsValues
       ) where

import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad.Error
import           Control.Monad.Identity
import           Control.Monad.State.Strict
import qualified Data.ByteString as BS
import qualified Data.Foldable as Fold
import           Data.Int
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import           Data.Word
import           Debug.Trace
import           Numeric
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Reopt.Analysis.AbsState
import qualified Reopt.Analysis.Domains.StridedInterval as SI
import           Reopt.Object.Memory
import           Reopt.CFG.Implementation
import           Reopt.CFG.Representation
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types

------------------------------------------------------------------------
-- Utilities

-- | Run a computation over a part of the current state.
subMonad :: (MonadState s m)
         => Simple Lens s t
         -> State t r
         -> m r
subMonad l m = l %%= runState m

liftEither :: StateT s (Either e) a -> State s (Either e a)
liftEither m = state go
  where
    go s = case runStateT m s of
             Left e       -> (Left e,  s)
             Right (r, t) -> (Right r, t)

doMaybe :: Monad m => m (Maybe a) -> b -> (a -> m b) -> m b
doMaybe m n j = do
  ma <- m
  case ma of
    Nothing -> return n
    Just a -> j a

-- | Get code pointers out of a abstrcct value.
concretizeAbsCodePointers :: Memory Word64 -> AbsValue (BVType 64) -> [CodeAddr]
concretizeAbsCodePointers mem (FinSet s) =
  filter (isCodePointer mem) $ fromInteger <$> Set.toList s
concretizeAbsCodePointers mem (CodePointers s) =
  filter (isCodePointer mem) $ Set.toList s
concretizeAbsCodePointers mem (StridedInterval s) =
  filter (isCodePointer mem) $ fromInteger <$> SI.toList s
concretizeAbsCodePointers mem _ = []

------------------------------------------------------------------------
-- AbsState

-- | Maps each code address to a set of abstract states
type AbsState = Map CodeAddr AbsBlockState

defBlockState :: Memory Word64 -> CodeAddr -> AbsBlockState
defBlockState mem addr =
  top & setAbsIP mem addr
      & absX86State . register N.rsp .~ concreteStackOffset 0
      & absX86State . x87TopReg .~ abstractSingleton mem knownNat 7

emptyAbsState :: Memory Word64 -> CodeAddr -> AbsState
emptyAbsState mem start = Map.singleton start (defBlockState mem start)

lookupAbsBlock :: CodeAddr -> AbsState -> AbsBlockState
lookupAbsBlock addr s = fromMaybe (error msg) (Map.lookup addr s)
  where msg = "Could not find block " ++ show addr

------------------------------------------------------------------------
-- FrontierReason

-- | Data describing why an address was added to the frontier.
data FrontierReason
     -- | Exploring because a pointer to this address was found stored in
     -- memory.
   = InInitialData
     -- | Exploring because the given block writes it to memory.
   | InWrite       !BlockLabel
     -- | Exploring because the given block stores address as a
     -- return address.
   | ReturnAddress !BlockLabel
     -- | Exploring because the given block jumps here.
   | NextIP !BlockLabel
     -- | Added as the initial start state.
   | StartAddr
     -- | Added because a previous block split this block.
   | BlockSplit
  deriving (Show)

------------------------------------------------------------------------
-- BlockRegion

data BlockRegion = BlockRegion { brEnd :: !CodeAddr
                                 -- | Map from labelIndex to associated block.
                               , brBlocks :: !(Map Word64 Block)
                               }

-- | Does a simple lookup in the cfg at a given DecompiledBlock address.
lookupBlock :: Map CodeAddr BlockRegion -> BlockLabel -> Maybe Block
lookupBlock m lbl = do
  br <-  Map.lookup (labelAddr lbl) m
  Map.lookup (labelIndex lbl) (brBlocks br)

------------------------------------------------------------------------
-- Interpreter state



-- | The state of the interpreter
data InterpState
   = InterpState { -- | The initial memory when disassembly started.
                   memory   :: !(Memory Word64)
                 , _genState :: !GlobalGenState
                   -- | Addresses that we have attempted to disassemble as the
                   -- start of a block.
                 , _blockStartAddrs :: !(Set CodeAddr)
                   -- | Intervals maps code addresses to blocks at address.
                 , _blocks   :: !(Map CodeAddr BlockRegion)
                   -- | Set of code adddresses that could not be interpreted.
                 , _failedAddrs  :: !(Set CodeAddr)
                   -- | Maps addresses that are marked as the start of a function
                 , _functionEntries :: !(Set CodeAddr)
                   -- | Maps each code address to the list of predecessors that
                   -- affected its abstract state.
                 , _reverseEdges :: !(Map CodeAddr (Set CodeAddr))
                   -- | Abstract state common to all code that can be jumped to in memory.
                 , memBlockState :: !AbsBlockState
                   -- | Map each jump table start to the address just after the end.
                 , _jumpTableAddrs :: !(Map CodeAddr CodeAddr)
                   -- | Set of addresses to explore next.
                   -- This is a map so that we can associate a reason why a code address
                   -- was added to the frontier.
                 , _frontier :: !(Map CodeAddr FrontierReason)
                   -- | Map from code addresses to the abstract state at the start of
                   -- the block.
                 , _absState :: !AbsState
                 }

-- | Empty interpreter state.
emptyInterpState :: Memory Word64 -> CodeAddr -> InterpState
emptyInterpState mem start = InterpState
      { memory        = mem
      , _genState     = emptyGlobalGenState
      , _blockStartAddrs    = Set.empty
      , _blocks       = Map.empty
      , _failedAddrs  = Set.empty
      , _functionEntries   = Set.singleton start
      , _reverseEdges = Map.empty
      , memBlockState = defBlockState mem 0
      , _jumpTableAddrs = Map.empty
      , _frontier     = Map.singleton start StartAddr
      , _absState     = emptyAbsState mem start
      }

genState :: Simple Lens InterpState GlobalGenState
genState = lens _genState (\s v -> s { _genState = v })

blockStartAddrs :: Simple Lens InterpState (Set CodeAddr)
blockStartAddrs = lens _blockStartAddrs (\s v -> s { _blockStartAddrs = v })

blocks :: Simple Lens InterpState (Map CodeAddr BlockRegion)
blocks = lens _blocks (\s v -> s { _blocks = v })

failedAddrs :: Simple Lens InterpState (Set CodeAddr)
failedAddrs = lens _failedAddrs (\s v -> s { _failedAddrs = v })

-- | Addresses that start each function.
functionEntries :: Simple Lens InterpState (Set CodeAddr)
functionEntries = lens _functionEntries (\s v -> s { _functionEntries = v })

reverseEdges :: Simple Lens InterpState (Map CodeAddr (Set CodeAddr))
reverseEdges = lens _reverseEdges (\s v -> s { _reverseEdges = v })

jumpTableAddrs :: Simple Lens InterpState (Map CodeAddr Word64)
jumpTableAddrs = lens _jumpTableAddrs (\s v -> s { _jumpTableAddrs = v })

frontier :: Simple Lens InterpState (Map CodeAddr FrontierReason)
frontier = lens _frontier (\s v -> s { _frontier = v })

absState :: Simple Lens InterpState AbsState
absState = lens _absState (\s v -> s { _absState = v })

-- | Record the given address as the start of a function.
recordFunctionEntry :: BlockLabel
                    -> FrontierReason
                    -> AbsBlockState
                    -> CodeAddr
                    -> State InterpState ()
recordFunctionEntry src rsn ab addr =
  trace ("Found function entry " ++ showHex addr ".") $ do
  mergeBlock src rsn ab addr
  functionEntries %= Set.insert addr

-- | Returns the guess on the entry point of the given function.
getFunctionEntryPoint :: CodeAddr -> InterpState -> CodeAddr
getFunctionEntryPoint addr s = do
  case Set.lookupLE addr (s^.functionEntries) of
    Just a -> a
    Nothing -> error $ "Could not find address of " ++ showHex addr "."

inSameFunction :: CodeAddr -> CodeAddr -> InterpState -> Bool
inSameFunction x y s =
  getFunctionEntryPoint x s == getFunctionEntryPoint y s

------------------------------------------------------------------------
-- Block discovery

-- | Does a simple lookup in the cfg at a given DecompiledBlock address.
lookupBlock' :: MonadState InterpState m => BlockLabel -> m (Maybe Block)
lookupBlock' lbl = uses blocks (`lookupBlock` lbl)

getAbsBlockState :: CodeAddr -> State InterpState AbsBlockState
getAbsBlockState a = do
  s <- get
  return $ lookupAbsBlock a (s^.absState)

-- | This is the worker for getBlock, in the case that we have not already
-- read the block.
reallyGetBlock :: FrontierReason
                  -- ^ Reason we are exploring block.
               -> CodeAddr
               -> State InterpState (Maybe Block)
reallyGetBlock rsn addr = do
  -- Record this as the start of a block.
  blockStartAddrs %= Set.insert addr
  -- Check to see if we should delete an overlapping block
  do -- Lookup block just before this address
     m_lt <- uses blocks (Map.lookupLT addr)
     case m_lt of
       -- If that block overlaps with the address
       Just (l,br) | brEnd br > addr -> do
         -- Delete the old block
         blocks  %= Map.delete l
         -- Add its address to the frontier.
         frontier %= Map.insert l BlockSplit
       _ -> return ()
  -- Get top
  ab <- getAbsBlockState addr
  t <- getAbsX87Top ab
  -- Create explore loc
  let loc = ExploreLoc { loc_ip = addr
                       , loc_x87_top = t
                       }
  -- Attempt to disassemble block.
  r <- do
    -- Get memory so that we can decode from it.
    addrs <- use blockStartAddrs
        -- Returns true if we are not at the start of a block.
        -- This is used to stop the diassembler when we reach code
        -- that is part of a new block.
    let not_at_block addr0 = Set.notMember addr0 addrs
    mem <- gets memory
    subMonad genState $ do
      liftEither $ disassembleBlock mem not_at_block loc
  case r of
    Left _e -> trace ("Block failed: 0x" ++ showHex addr (", Reason " ++ show rsn)) $ do
      failedAddrs %= Set.insert addr
      return Nothing
    Right (bs@(first_b:_), next_ip) -> assert (next_ip > addr) $ do
      let block_map = Map.fromList [ (labelIndex (blockLabel b), b) | b <- bs ]
      -- Add block region to blocks.
      let br = BlockRegion { brEnd = next_ip
                           , brBlocks = block_map
                           }
      blocks %= Map.insert addr br
      -- Return first block.
      return $ Just first_b

-- | Returns a block at the given location, if at all possible.  This
-- will disassemble the binary if the block hasn't been seen before.
-- In particular, this ensures that a block and all it's children are
-- present in the cfg (assuming successful disassembly)
getBlock :: FrontierReason -> CodeAddr -> State InterpState (Maybe Block)
getBlock rsn addr = do
  let lbl = GeneratedBlock { labelAddr = addr
                           , labelIndex = 0
                           }
  m_b <- lookupBlock' lbl
  case m_b of
    Just b ->
      return (Just b)
    Nothing -> do
      failed <- uses failedAddrs (Set.member addr)
      if failed then
        return Nothing
      else
        reallyGetBlock rsn addr

------------------------------------------------------------------------
-- Transfer stmts

transferStmt :: Monad m
             => Stmt
             -> StateT AbsRegs m ()
transferStmt stmt =
  case stmt of
    AssignStmt a -> do
      modify $ addAssignment a
    Write (MemLoc addr _) v -> do
      modify $ addMemWrite addr v
    _ -> return ()

fnRegCodePointers :: X86State AbsValue -> [CodeAddr]
fnRegCodePointers s = Set.toList (foldX86StateValue codePointerSet s)

stackCodePointers :: AbsBlockStack -> [CodeAddr]
stackCodePointers stk =
  [ ptr
  | (offset, StackEntry _ v) <- Map.toList stk
  , offset /= 0
  , ptr <- Set.toList (codePointerSet v)
  ]



-- | Mark a escaped code pointer as a function entry.
recordEscapedCodePointer :: Word64 -> FrontierReason -> InterpState -> InterpState
recordEscapedCodePointer addr rsn s
  | addr == 0 = s
  | Set.member addr (s^.functionEntries) = s
  | otherwise =
     let mem = memory s
         s' = s & functionEntries %~ Set.insert addr
                & absState        %~ Map.insert addr (defBlockState mem addr)
                & frontier        %~ Map.insert addr rsn
         new_addrs = case Map.lookup addr (s^.absState) of
                       Nothing -> []
                       Just abst -> do
                         let reg_ptrs = fnRegCodePointers (abst^.absX86State)
                             stk_ptr = stackCodePointers (abst^.startAbsStack)
                          in reg_ptrs ++ stk_ptr
      in recordEscapedCodePointers new_addrs rsn s'


recordEscapedCodePointers :: [Word64] -> FrontierReason -> InterpState -> InterpState
recordEscapedCodePointers addrs rsn s0 =
  foldl' (\s v -> recordEscapedCodePointer v rsn s) s0 addrs

recordWriteStmt :: BlockLabel -> AbsRegs -> Stmt -> State InterpState ()
recordWriteStmt lbl regs (Write (MemLoc _addr _) v)
  | Just Refl <- testEquality (valueType v) (knownType :: TypeRepr (BVType 64))
  , av <- transferValue regs v
  , Just sz <- size av
    -- FIXME: GIANT HACK (avoids explosion in concretize)
  , sz < 100 = do
    mem <- gets memory
    let vs2 = concretizeAbsCodePointers mem av
    modify $ recordEscapedCodePointers vs2 (InWrite lbl)
recordWriteStmt _ _ _ = return ()

transferStmts :: Monad m => AbsRegs -> [Stmt] -> m AbsRegs
transferStmts r stmts = execStateT (mapM_ transferStmt stmts) r

finalBlockState :: Memory Word64 -> CodeAddr -> FinalCFG -> AbsBlockState
finalBlockState mem a g
  | Set.member a (finalCodePointersInMem g) = defBlockState mem a
  | otherwise = lookupAbsBlock a (finalAbsState g)

-- | Generate map that maps each assignment in the CFG to the abstract value
-- associated with it.
assignmentAbsValues :: Memory Word64 -> FinalCFG -> MapF Assignment AbsValue
assignmentAbsValues mem fg = foldl' go MapF.empty (Map.elems (g^.cfgBlocks))
  where g = finalCFG fg

        go :: MapF Assignment AbsValue
           -> Block
           -> MapF Assignment AbsValue
        go m0 b =
          case blockLabel b of
            GeneratedBlock a 0 -> insBlock b (initAbsRegs mem (finalBlockState mem a fg)) m0
            _ -> m0

        insBlock :: Block
                 -> AbsRegs
                 -> MapF Assignment AbsValue
                 -> MapF Assignment AbsValue
        insBlock b r0 m0 =
            case blockTerm b of
              Branch _ lb rb -> do
                let Just l = findBlock g lb
                let Just r = findBlock g rb
                insBlock l final $
                  insBlock r final $
                  m
              FetchAndExecute _ -> m
              Syscall _ -> m

          where final = runIdentity $ transferStmts r0 (blockStmts b)
                m = MapF.union (final^.absAssignments) m0

------------------------------------------------------------------------
-- Transfer functions

-- | Joins in the new abstract state and returns the locations for
-- which the new state is changed.
mergeBlock :: BlockLabel
              -- ^ Source label that we are jumping from.
           -> FrontierReason
              -- ^ "Reason" we are merging this block.
           -> AbsBlockState
              -- ^ Block state after executing instructions.
           -> CodeAddr
              -- ^ Address we are trying to reach.
           -> State InterpState ()
mergeBlock src rsn ab addr = do
  s <- get
  -- Associate a new abstract state with the code region.
  let upd new = do
        -- Add reverse edge
        reverseEdges %= Map.insertWith Set.union addr (Set.singleton (labelAddr src))
        absState %= Map.insert addr new
        frontier %= Map.insert addr rsn
  case Map.lookup addr (s^.absState) of
    -- We have seen this block before, so need to join and see if
    -- the results is changed.
    Just ab_old ->
      case joinD ab_old ab of
        Nothing  -> return ()
        Just new -> upd new
    -- We haven't seen this block before
    Nothing  -> upd ab

-- | This updates the state of a function when returning from a function.
mergeFreeBSDSyscall :: BlockLabel
                       -- ^ Label for callee block that is making this call.
                    -> AbsBlockState
                       -- ^ Block state just before this call.
                    -> CodeAddr
                       -- ^ Address that system call should return to.
                    -> State InterpState ()
mergeFreeBSDSyscall src_lbl ab0 addr = do
  let regFn :: N.RegisterName cl -> AbsValue (N.RegisterType cl)
      regFn r
          -- Set IPReg
        | N.IPReg <- r =
          CodePointers (Set.singleton addr)
          -- Two pointers that are modified by syscalls
        | Just Refl <- testEquality r N.rcx =
          TopV
        | Just Refl <- testEquality r N.r11 =
          TopV
          -- Carry flag and rax is used by BSD for return values.
        | Just Refl <- testEquality r N.cf =
          TopV
        | Just Refl <- testEquality r N.rax =
          TopV
          -- Keep other registers the same.
        | otherwise =
          ab0^.absX86State^.register r
  let ab = mkAbsBlockState regFn (ab0^.startAbsStack)

  -- Merge the new abstract
  mergeBlock src_lbl (NextIP src_lbl) ab addr

-- | This updates the state of a function when returning from a function.
mergeCalleeReturn :: BlockLabel
                     -- ^ Label for callee block that is return.
                  -> Memory Word64
                     -- ^ State of memory
                  -> AbsBlockState
                     -- ^ Block state just before return.
                  -> CodeAddr
                     -- ^ Address we are returning to.
                  -> State InterpState ()
mergeCalleeReturn lbl mem ab0 addr = do
  s <- get
  let regsToCopy :: Set (Some N.RegisterName)
      regsToCopy = Set.fromList
        [ Some N.rsp -- Stack pointer
        , Some N.rax -- Integer return pointer
        , Some (N.XMMReg 7) -- Floating point return value
        ]
      -- Get register function.
  let regFn :: N.RegisterName cl -> AbsValue (N.RegisterType cl)
      regFn r
          -- Set IPReg
        | N.IPReg <- r =
          CodePointers (Set.singleton addr)
          -- Stack pointer
        | Set.member (Some r) regsToCopy =
            ab0^.absX86State^.register r
          -- Floating point height
        | N.X87TopReg <- r =
          ab0^.absX86State^.register N.X87TopReg
          -- Return no value for GPRegs
        | N.GPReg _ <- r =
          emptyAbsValue
          -- We know nothing about other registers.
        | otherwise =
          TopV
  let ab = mkAbsBlockState regFn (ab0^.startAbsStack)
  mergeBlock lbl (NextIP lbl) ab addr

-- | This updates the abstract information based on the assumption that
-- a called method will return to the return address, and will follow
-- x86_64 ABI.
mergeCallerReturn :: BlockLabel
                     -- ^ Label of block maing call.
                  -> AbsBlockState
                     -- ^ Block state just before call.
                  -> CodeAddr
                     -- ^ Address we will return to.
                  -> State InterpState ()
mergeCallerReturn lbl ab0 addr = do
  s <- get
  let regFn :: N.RegisterName cl -> AbsValue (N.RegisterType cl)
      regFn r
          -- We set IPReg
        | Just Refl <- testEquality r N.IPReg =
          CodePointers (Set.singleton addr)
          -- We don't want to add any values to rax as that is the
          -- return value.
        | Just Refl <- testEquality r N.rax =
          emptyAbsValue
          -- TODO: Transmit no value to first floating point register.
        | N.XMMReg 0 <- r =
          ab0^.absX86State^.register r
          -- TODO: Fix this (can we prove detect whether a floating point value was read)?
        | N.X87TopReg <- r =
          ab0^.absX86State^.register r
          -- Copy callee saved registers
        | Set.member (Some r) x86CalleeSavedRegisters =
          ab0^.absX86State^.register r
          -- We know nothing about other registers.
        | otherwise =
          TopV
      -- Get values below return address.
  let stk = Map.filterWithKey (\k _ -> k >= 8) (ab0^.startAbsStack)
  let ab = shiftSpecificOffset regFn stk 8

  mergeBlock lbl (ReturnAddress lbl) ab addr

_showAbsDiff :: AbsBlockState -> AbsBlockState -> Doc
_showAbsDiff x y = vcat (pp <$> absBlockDiff x y)
  where pp (Some n) = pretty (show n) <+> pretty (x^.absX86State^.register n)
                                      <+> pretty (x^.absX86State^.register n)

-- Check floating point top.
getAbsX87Top :: Monad m => AbsBlockState -> m Int
getAbsX87Top abst =
  case asConcreteSingleton (abst^.absX86State^. x87TopReg) of
    Just v -> return (fromInteger v)
    _ -> fail "x87top is not concrete"

-- | @isrWriteTo stmt add tpr@ returns true if @stmt@ writes to @addr@
-- with a write having the given type.
isWriteTo :: Stmt -> Value (BVType 64) -> TypeRepr tp -> Maybe (Value tp)
isWriteTo (Write (MemLoc a _) val) expected tp
  | Just _ <- testEquality a expected
  , Just Refl <- testEquality (valueType val) tp =
    Just val
isWriteTo _ _ _ = Nothing

-- | @isCodePointerWriteTo mem stmt addr@ returns true if @stmt@ writes
-- a single address to a marked executable in @mem@ to @addr@.
isCodePointerWriteTo :: Memory Word64 -> Stmt -> Value (BVType 64) -> Maybe Word64
isCodePointerWriteTo mem s sp
  | Just (BVValue _ val) <- isWriteTo s sp (knownType :: TypeRepr (BVType 64))
  , isCodePointer mem (fromInteger val)
  = Just (fromInteger val)
isCodePointerWriteTo _ _ _ = Nothing

-- -----------------------------------------------------------------------------
-- Refining an abstract state based upon a condition

-- FIXME: if val \notin av then we should return bottom
refineValue :: Value tp
               -> AbsValue tp
               -> AbsRegs
               -> AbsRegs
refineValue (BVValue _n _val) _av regs = regs
refineValue (Initial r) av regs =
  regs & (absInitialRegs . register r) %~ flip meet av
refineValue (AssignedValue ass@(Assignment _ rhs)) av regs
  -- av is not a subset.
  | Nothing <- joinAbsValue av av_old = regs
  -- av adds new information, we need to refine any parents
  | EvalApp app <- rhs = refineApp app av' regs'
  -- no parents, but update ass
  | otherwise          = regs'
  where
    av_old = regs ^. absAssignments ^. assignLens ass
    av'    = meet av_old av
    regs'  = regs & (absAssignments . assignLens ass) .~ av'

refineApp :: App Value tp
             -> AbsValue tp
             -> AbsRegs
             -> AbsRegs
refineApp app av regs =
  case app of
   -- We specialise these to booleans for the moment
   -- BVComplement sz v
   --   | Just Refl <- testEquality sz n1
   --   , Just b    <- asConcreteSingleton av ->
   --     refineValue v (abstractSingleton n1 (1 - b)) regs
   -- BVAnd sz l r
   --   | Just Refl <- testEquality sz n1
   --   , Just b    <- asConcreteSingleton av ->
   --     let l_regs = refineValue l av regs
   --         r_regs = refineValue r av regs
   --     in if b == 1 then  -- both are true, so we do a meet
   --          glb l_regs r_regs
   --        else -- one is false, so we do a join
   --          lub l_regs r_regs

   -- If we know something about the result of a trunc, we can
   -- propagate back a subvalue.
   Trunc x sz -> refineTrunc x sz av regs

   -- basically less-than: does x - y overflow? only if x < y.
   UsbbOverflows sz l r (BVValue _ 0)
     | Just b    <- asConcreteSingleton av -> refineLt (BVTypeRepr sz) l r b regs

   -- FIXME: HACK
   -- This detects r - x < 0 || r - x == 0, i.e. r <= x
   BVOr _ (getAssignApp -> Just (UsbbOverflows _ r xv@(BVValue sz x) (BVValue _ 0)))
          (getAssignApp -> Just (BVEq (getAssignApp -> Just (BVAdd _ r' y)) (BVValue _ 0)))
     | Just Refl <- testEquality r r'
     , Just Refl <- testEquality y (mkLit sz (negate x))
     , Just b    <- asConcreteSingleton av ->
       -- trace ("Saw the OR abomination: " ++ show (pretty r <+> pretty x)) $
       refineLeq (BVTypeRepr sz) r xv b regs

   -- FIXME: HACK
   -- This detects not (r - x < 0) && not (r - x == 0), i.e. x < r
   BVAnd _ (getAssignApp -> Just (BVComplement _
                                  (getAssignApp -> Just (UsbbOverflows _ r xv@(BVValue sz x) (BVValue _ 0)))))
           (getAssignApp -> Just (BVComplement _
                                  (getAssignApp -> Just (BVEq (getAssignApp -> Just (BVAdd _ r' y)) (BVValue _ 0)))))
     | Just Refl <- testEquality r r'
     , Just Refl <- testEquality y (mkLit sz (negate x))
     , Just b    <- asConcreteSingleton av ->
       -- trace ("Saw the AND abomination: " ++ show (pretty r <+> pretty x)) $
       refineLt (BVTypeRepr sz) xv r b regs

  -- Mux can let us infer the condition?
   _ -> regs
  where
    getAssignApp (AssignedValue (Assignment _ (EvalApp a))) = Just a
    getAssignApp _ = Nothing

refineTrunc :: ((n + 1) <= n') =>
               Value (BVType n') -> NatRepr n -> AbsValue (BVType n)
               -> AbsRegs -> AbsRegs
refineTrunc v sz av regs = refineValue v (subValue sz av) regs

refineLeq :: TypeRepr tp -> Value tp -> Value tp -> Integer -> AbsRegs -> AbsRegs
refineLeq tp x y b regs
  -- y < x
  | b == 0     = refineValue x x_lt (refineValue y y_lt regs)
  -- x <= y
  | otherwise  = refineValue x x_leq (refineValue y y_leq regs)
  where
    x_av = transferValue regs x
    y_av = transferValue regs y
    (x_leq, y_leq)   = abstractLeq tp x_av y_av
    (y_lt, x_lt)     = abstractLt  tp y_av x_av

refineLt :: TypeRepr tp -> Value tp -> Value tp -> Integer -> AbsRegs -> AbsRegs
refineLt tp x y b regs
  -- y <= x
  | b == 0     = refineValue x x_leq (refineValue y y_leq regs)
  -- x < y case
  | otherwise  = refineValue x x_lt (refineValue y y_lt regs)
  where
    x_av = transferValue regs x
    y_av = transferValue regs y
    (x_lt, y_lt)   = abstractLt tp x_av y_av
    (y_leq, x_leq) = abstractLeq tp y_av x_av

-- -- FIXME: bottom
-- refineLVal :: Simple Lens AbsRegs (AbsValue tp)
--               -> Value tp
--               -> AbsRegs
--               -> AbsRegs
-- refineLVal l (BVValue n val) regs =
--   -- FIXME: if val \notin absinit l then we should return bottom
--   regs & l .~ abstractSingleton n val
-- refineLVal l (Initial r) regs =
--   regs & l .~ new_v & (absInitialRegs . register r) .~ new_v
--   where
--     abs_l  = regs ^. l
--     abs_r  = regs ^. absInitialRegs ^. register r
--     new_v  = meet abs_l abs_r
-- refineLVal l (AssignedValue ass@(Assignment _ rhs)) regs =
--   regs & l .~ new_v & (absInitialRegs .  assignLens ass) .~ new_v
--   where
--     abs_l  = regs ^. l
--     abs_r  = regs ^. absInitialRegs ^. assignLens ass
--     new_v  = meet abs_l abs_r


-- | Attempt to identify the write to a stack return address, returning
-- instructions prior to that write and return  values.
identifyCall :: Memory Word64
             -> [Stmt]
             -> X86State Value
             -> Maybe (Seq Stmt, Word64)
identifyCall mem stmts0 s = go (Seq.fromList stmts0)
  where next_sp = s^.register N.rsp
        go stmts =
          case Seq.viewr stmts of
            Seq.EmptyR -> Nothing
            prev Seq.:> stmt
              | Just ret <- isCodePointerWriteTo mem stmt next_sp ->
                Just (prev, ret)
              | Write{} <- stmt -> Nothing
              | otherwise -> go prev

-- | List of registers that a callee must save.
x86CalleeSavedRegisters :: Set (Some N.RegisterName)
x86CalleeSavedRegisters = Set.fromList $
  [ Some N.rsp
  , Some N.rbp
  , Some N.rbx
  , Some N.r12
  , Some N.r13
  , Some N.r14
  , Some N.r15
  ]

intervalForSegment :: MemSegment Word64 -> SI.StridedInterval (BVType 64)
intervalForSegment roseg =
  let base = fromIntegral $ memBase roseg
      sz   = fromIntegral $ BS.length (memBytes roseg)
   in SI.mkStridedInterval (BVTypeRepr n64) False base (base + sz - 1) 1

-- | Return true if abstract value only reads from read only memory.
intervalInReadonlyMem :: Memory Word64 -> SI.StridedInterval (BVType 64) -> Bool
intervalInReadonlyMem mem si_new =
  any (\seg  -> si_new `SI.isSubsetOf` intervalForSegment seg)
      (readonlySegments mem)


rangeInReadonlySegment :: Word64 -- ^ Start of range
                       -> Word64 -- ^ One past last index in range.
                       -> Memory Word64 -> Bool
rangeInReadonlySegment base end mem =
  case findSegment base mem of
    Nothing -> False
    Just seg -> isReadonly seg
             && end <= memBase seg + segmentSize seg

-- looks for jump tables
getJumpTable :: Memory Word64 -- ^ State of memory
             -> AbsRegs       -- ^ Current processor registers.
             -> Value (BVType 64) -- ^ Memory address that IP is read from.
             -> Maybe (SI.StridedInterval (BVType 64))
getJumpTable mem regs read_addr
    -- Turn the read address into base + offset.
  | Just (BVAdd _ offset (BVValue _ base)) <- valueAsApp read_addr
    -- Turn the offset into a multiple by an index.
  , Just (BVMul _ (BVValue _ 8) index) <- valueAsApp offset
    -- Get range for the index.
  , StridedInterval index_interval  <- transferValue regs index
    -- Check that relevant interval is completely contained within a read-only
    -- read only range in the memory.
  , SI.StridedInterval _ index_base index_range index_stride <- index_interval
  , index_end <- index_base + index_range * index_stride
  , read_end <- base + 8 * index_end
  , rangeInReadonlySegment (fromInteger base) (fromInteger read_end) mem

  , base_interval <- SI.singleton (BVTypeRepr n64) base
  , offset_interval  <- SI.bvmul n64 (SI.singleton (BVTypeRepr n64) 8) index_interval
  , read_interval <- SI.bvadd n64 base_interval offset_interval =
    -- Get the addresses associated.
    trace ("Fixed table " ++ showHex base (" [" ++ shows index "]")) $
      Just $! read_interval
      {-
getJumpTable mem regs read_addr
    -- Turn the read address into base + offset.
  | Just (BVAdd _ offset (BVValue _ base)) <- valueAsApp read_addr
    -- Turn the offset into a multiple by an index.
  , Just (BVMul _ (BVValue _ stride) index) <- valueAsApp offset
    -- Get range for the index.
  , StridedInterval index_interval  <- transferValue regs index
    -- Check that relevant interval is completely contained within a read-only
    -- read only range in the memory.
  , base_interval <- SI.singleton (BVTypeRepr n64) base
  , offset_interval <- SI.bvmul n64 (SI.singleton (BVTypeRepr n64) stride) index_interval
  , read_interval <- SI.bvadd n64 base_interval offset_interval
  , intervalInReadonlyMem mem read_interval
    -- Check stride is large enough.
  , stride >= 8 = trace ("Indexed table " ++ show read_addr) $
    -- Get the addresses associated.
    Just $! read_interval-}
getJumpTable _mem _regs _ = Nothing

  -- -- basically, (8 * x) + addr
  -- | AssignedValue (Assignment _ (Read (MemLoc ptr _))) <- conc^.curIP
  -- , AssignedValue (Assignment _ (EvalApp (BVAdd _ lhs (BVValue _ base)))) <- ptr
  -- , AssignedValue (Assignment _ (EvalApp (BVMul _ (BVValue _ 8) xv))) <- lhs
  -- , AssignedValue x <- xv
  -- , Just vs <- concretize (regs^.absAssignments^.assignLens x) =
  --   let addrs = Set.map (\off -> fromIntegral $ base + off * 8) vs
  --       bptrs = Set.map (\addr -> fromIntegral $ fromJust
  --                                 $ Map.lookup addr wordMap) addrs
  --   in
  --   if all (isRODataPointer mem) $ Set.toList addrs
  --      then trace ("getJumpTable: " ++ show (pretty x)
  --                  ++ " " ++ show (Set.map (flip showHex "") bptrs)) $
  --           Just bptrs
  --      else Nothing
  -- | otherwise = Nothing

-- | This is designed to detect returns from the X86 representation.
-- It pattern matches on a X86State to detect if it read its instruction
-- pointer from an address that is 8 below the stack pointer.
recoverIsReturnStmt :: X86State Value -> Bool
recoverIsReturnStmt s = do
  let next_ip = s^.register N.rip
      next_sp = s^.register N.rsp
  case next_ip of
    AssignedValue (Assignment _ (Read (MemLoc ip_stored_here _))) ->
      let (ip_base, ip_off) = asBaseOffset ip_stored_here
          (sp_base, sp_off) = asBaseOffset next_sp
       in ip_base == sp_base
          && ip_off + 8 == sp_off
    _ -> False

transferBlock :: Block   -- ^ Block to start from.
              -> AbsRegs -- ^ Registers at this block.
              -> State InterpState ()
transferBlock b regs = do
  let lbl = blockLabel b
  mem <- gets memory
  regs' <- transferStmts regs (blockStmts b)
  -- FIXME: we should propagate c back to the initial block, not just b
  case blockTerm b of
    Branch c lb rb -> do
      mapM_ (recordWriteStmt lbl regs') (blockStmts b)
      Just l <- lookupBlock' lb
      let  l_regs = refineValue c (abstractSingleton mem n1 1) regs'
      Just r <- lookupBlock' rb
      let r_regs = refineValue c (abstractSingleton mem n1 0) regs'
      -- We re-transfer the stmts to propagate any changes from
      -- the above refineValue.  This could be more efficient by
      -- tracking what (if anything) changed.  We also might
      -- need to keep going back and forth until we reach a
      -- fixpoint
      transferBlock l =<< transferStmts l_regs (blockStmts b)
      transferBlock r =<< transferStmts r_regs (blockStmts b)

    Syscall s' -> do
      mapM_ (recordWriteStmt lbl regs') (blockStmts b)
      let abst = finalAbsBlockState regs' s'
      let ips = concretizeAbsCodePointers mem (abst^.absX86State^.curIP)
      -- Merge system call result with possible next IPs.
      Fold.forM_ ips $ \addr -> do
        mergeFreeBSDSyscall lbl abst addr

    FetchAndExecute s' -> do
      -- See if next statement appears to end with a call.
      -- We define calls as statements that end with a write that
      -- stores the pc to an address.
      case () of
          -- The last statement was a call.
        _ | Just (prev_stmts, ret) <- identifyCall mem (blockStmts b) s' -> do
            Fold.mapM_ (recordWriteStmt lbl regs') prev_stmts
            let abst = finalAbsBlockState regs' s'
            seq abst $ do
            mergeCallerReturn lbl abst ret
            -- Look for new ips.
            let ips = concretizeAbsCodePointers mem (abst^.absX86State^.curIP)
            Fold.forM_ ips $ \addr -> do
              recordFunctionEntry lbl (NextIP lbl) (abst & setAbsIP mem addr) addr
          -- This block ends with a return.
          | recoverIsReturnStmt s' -> do
            mapM_ (recordWriteStmt lbl regs') (blockStmts b)
            let abst = finalAbsBlockState regs' s'
            let ips = concretizeAbsCodePointers mem (abst^.absX86State^.curIP)
            -- Look for new ips.
            Fold.forM_ ips $ \addr -> do
              mergeCalleeReturn lbl mem abst addr

          -- Jump to concrete offset.
          | BVValue _ tgt_addr_as_integer <- s'^.register N.rip -> do

            let tgt_addr = fromInteger tgt_addr_as_integer
            let abst = finalAbsBlockState regs' s'
            seq abst $ do
            -- Try to check for a tail call.
            this_fn <- gets $ getFunctionEntryPoint (labelAddr lbl)
            tgt_fn  <- gets $ getFunctionEntryPoint tgt_addr
            -- When the jump appears to go to another function, assume the
            -- jump if a tail call.
            if (this_fn /= tgt_fn) then do
              -- Check that the current stack height is correct so that a
              -- tail call when go to the right place.
              -- TODO: Add check to ensure stack height is correct.
              recordFunctionEntry lbl (NextIP lbl) (abst & setAbsIP mem tgt_addr) tgt_addr
            else
              -- Merge block state.
              mergeBlock lbl (NextIP lbl) (abst & setAbsIP mem tgt_addr) tgt_addr

          -- Block ends with what looks like a jump table.
          | AssignedValue (Assignment _ (Read (MemLoc ptr _))) <- s'^.curIP
          , Just read_interval <- getJumpTable mem regs' ptr -> do

            mapM_ (recordWriteStmt lbl regs') (blockStmts b)
            -- Record memory as in a jump table.
            case read_interval of
              SI.StridedInterval _ base n stride -> do
                jumpTableAddrs %= Map.insert (fromInteger base) (fromInteger (base + n * stride))
              _ -> return ()
            -- Read addresses in interval.
            case traverse (memLookupWord64 mem pf_r . fromInteger) (SI.toList read_interval) of
              Left e -> error "getJumpTable returned inconsistent value."
              Right ips -> do
                -- Look for new ips.
                Fold.forM_ ips $ \tgt_addr -> do
                  let abst = finalAbsBlockState regs' s'
                  seq abst $ do
                  mergeBlock lbl (NextIP lbl) (abst & setAbsIP mem tgt_addr) tgt_addr

          -- We have a jump that we do not understand.
          -- This could be a tail call.
          | otherwise -> do
            Just br <- Map.lookup (labelAddr lbl) <$> use blocks
            mapM_ (recordWriteStmt lbl regs') (blockStmts b)
            let abst = finalAbsBlockState regs' s'
            seq abst $ do
            let ips = concretizeAbsCodePointers mem (abst^.absX86State^.curIP)
            -- Look for new ips.
            Fold.forM_ ips $ \tgt_addr -> do
              -- Treat the jump to the tgt_addr as a tail call.
              -- TODO: Add check to ensure stack height is correct.
              recordFunctionEntry lbl (NextIP lbl) (abst & setAbsIP mem tgt_addr) tgt_addr

transfer :: FrontierReason -> CodeAddr -> State InterpState ()
transfer rsn addr = trace ("transfer " ++ showHex addr ".") $ do
  mem <- gets memory
  doMaybe (getBlock rsn addr) () $ \root -> do
    ab <- getAbsBlockState addr
    fn_addr <- gets $ getFunctionEntryPoint addr
    trace ("In function at " ++ showHex fn_addr ".") $ do
    transferBlock root (initAbsRegs mem ab)


------------------------------------------------------------------------
-- Main loop

data FinalCFG = FinalCFG { finalCFG :: !CFG
                         , finalAbsState :: !AbsState
                         , finalCodePointersInMem :: !(Set CodeAddr)
                         , finalFailedAddrs :: !(Set CodeAddr)
                         , finalFunctions :: ![Function]
                         }

mkCFG :: Map CodeAddr BlockRegion -> CFG
mkCFG m = Map.foldlWithKey' go emptyCFG m
  where go g addr br = insertBlocksForCode addr (brEnd br) l g
          where l = Map.elems (brBlocks br)

mkFinalCFG :: InterpState -> FinalCFG
mkFinalCFG s =
  case traverse (recoverFunction s) (Set.toList (s^.functionEntries)) of
    Left msg -> error msg
    Right fns ->
      FinalCFG { finalCFG = mkCFG (s^.blocks)
               , finalAbsState = s^.absState
               , finalCodePointersInMem = s^.functionEntries
               , finalFailedAddrs = s^.failedAddrs
               , finalFunctions = fns
               }

explore_frontier :: InterpState -> InterpState
explore_frontier st =
  case Map.minViewWithKey (st^.frontier) of
    Nothing -> st
    Just ((addr,rsn), next_roots) ->
      let st_pre = st & frontier .~ next_roots
          st_post = flip execState st_pre $ transfer rsn addr
       in explore_frontier st_post

addrInJumpTable :: InterpState -> Word64 -> Bool
addrInJumpTable s a =
  case Map.lookupLE a (s^.jumpTableAddrs) of
    Nothing -> False
    Just (_,end) -> a < end

cfgFromAddress :: Memory Word64
                  -- ^ Memory to use when decoding instructions.
               -> CodeAddr
                  -- ^ Location to start disassembler form.
               -> FinalCFG
cfgFromAddress mem start = g

  where
--    fn = recoverFunction s3 0x422b10
--    0x422030

--    functionAddrs = Set.toList (s3^.functionEntries)
--    stack_heights = recoverFunction s3 <$> functionAddrs


--    ppAddr a = showHex a "\n"
--    code_pointers = filter (isCodePointer mem) (memAsWord64le mem)
    s0 = emptyInterpState mem start

    s1 = explore_frontier s0

    -- Add in code pointers from memory.
    go s (a,v)
      | not (isCodePointer mem v) = s
        -- Already a code pointer
      | Set.member v (s^.functionEntries) = s
        -- Ignore jump table entries.
      | addrInJumpTable s a = s
      | Set.member v (s^.blockStartAddrs) =
        recordEscapedCodePointer v InInitialData s
      | otherwise =
        trace ("Found new code pointer " ++ showHex v " at " ++ showHex a ".") $
        recordEscapedCodePointer v InInitialData s

    -- Explore data values
    s2  = foldl' go s1 (memAsWord64le_withAddr mem)
    s3 = explore_frontier s2
    g = mkFinalCFG s3

------------------------------------------------------------------------
-- Function

data Function = Function { fnAddr :: CodeAddr
                         , fnBlocks :: [FnBlock]
                         }

instance Pretty Function where
  pretty fn =
    pretty (showHex (fnAddr fn) "") <$$>
    vcat (pretty <$> fnBlocks fn)

data FnBlock
   = FnBlock { fbLabel :: !BlockLabel
             , fbStmts :: ![FnStmt]
             , fbTerm :: !(FnTermStmt)
             }


instance Pretty FnBlock where
  pretty b =
    pretty (fbLabel b) <$$>
    indent 2 (vcat (pretty <$> fbStmts b) <$$> pretty (fbTerm b))


data FnStmt
  = forall tp . FnWriteMem !(FnValue (BVType 64)) !(FnValue tp)
    -- | A comment
  | FnComment !Text
    -- | An assignment statement
  | forall tp . FnAssignStmt !(FnAssignment tp)


instance Pretty FnStmt where
  pretty s =
    case s of
      FnWriteMem addr val -> text "*" <> parens (pretty addr) <+> text "=" <+> pretty val
      FnComment msg -> text "#" <+> text (Text.unpack msg)

data FnTermStmt
   = FnTermStmtUndefined
   | FnBranch !(FnValue BoolType) !BlockLabel !BlockLabel

  {-
  FnReturn :: !(FnValue w (BVType 64)) -> FnTermStmt w
  FnBranch :: !(FnValue w BoolType) -> !BlockLabel -> !BlockLabel -> FnTermStmt w
  FnJump :: !BlockLabel -> FnTermStmt w
-}

instance Pretty FnTermStmt where
  pretty s =
    case s of
      FnTermStmtUndefined -> text "undefined term"
      FnBranch c x y -> text "branch" <+> pretty c <+> pretty x <+> pretty y


data FnAssignment tp
   = FnAssignment { fnAssignId :: !AssignId
                  , fnAssignRhs :: !(FnAssignRhs tp)
                  }

data FnAssignRhs (tp :: Type) where
  -- An expression with an undefined value.
  FnSetUndefined :: !(NatRepr n) -- Width of undefined value.
                 -> FnAssignRhs (BVType n)
  FnReadMem :: !(FnValue (BVType 64))
            -> !(TypeRepr tp)
            -> FnAssignRhs tp
  FnEvalApp :: !(App FnValue tp)
            -> FnAssignRhs tp
  FnAlloca :: !(FnValue (BVType 64))
           -> FnAssignRhs (BVType 64)

{-

  -- An expression that is computed from evaluating subexpressions.
  FnEvalApp :: !(App Value tp)
            -> FnAssignRhs w tp


  -- Read given location.
  FnRead :: !(StmtLoc (FnValue w (BVType 64)) tp) -> FnAssignRhs w tp

  -- Allocate a given number of bytes on the stack.
  FnAlloca :: !w -> FnAssignRhs w (BVType 64)
-}

-- | A function value.
data FnValue (tp :: Type) where
  FnValueUnsupported :: FnValue tp
  FnConstantValue :: NatRepr n -> Integer -> FnValue (BVType n)
  -- Value from an assignment statement.
  FnAssignedValue :: !(FnAssignment tp) -> FnValue tp
  -- The entry pointer to a function.
  FnEntryValue :: Word64 -> FnValue (BVType 64)
  -- A pointer to an internal block.
  FnBlockValue :: Word64 -> FnValue (BVType 64)
  -- The address at the implicit alloca that occurs at the start of a block.
  FnBlockAllocAddr :: Word64 -> FnValue (BVType 64)



{-
  -- Bitvector value.
  FnBVValue :: !(NatRepr n) -> Integer -> FnValue w (BVType n)


  FnInitial :: !(N.RegisterName cl) -> FnValue w (N.RegisterType cl)
-}

instance Pretty (FnValue tp) where
  pretty FnValueUnsupported = text "unsupported"

------------------------------------------------------------------------
-- FnStack

-- | This stores information about the current stack when working on function
-- identification.
data FnStack = UndefinedFnStack

initFnStack :: FnStack
initFnStack = UndefinedFnStack

-- | Given information about the stack and a offset into the stack, return
-- a function value denoting the given location.
stackOffsetAddr :: FnStack -> Value (BVType 64) -> FnValue (BVType 64)
stackOffsetAddr _ _ = trace "stackOffsetAddr unsupported" $
 FnValueUnsupported

{-
-- |
markCalleeSavedRegister :: FnStack -> Int64 -> FnStack
markCalleeSavedRegister _ _ = UndefinedFnStack
-}

------------------------------------------------------------------------
-- StackHeight

-- | Describe the amount of space needed to allocate for the stack.
-- The first parameter is a constant, the remaining values are the
-- set of stack locations accessed.  The height of the stack must be
-- at least as large as each value in the set.
data StackHeight = StackHeight Int64 (Set (Value (BVType 64)))

-- | Create a stack height from a single value.
valueAsStackHeight :: Value (BVType 64) -> StackHeight
valueAsStackHeight x
  | Just xc <- asInt64Constant x = StackHeight (min xc 0) Set.empty
  | otherwise = StackHeight 0 (Set.singleton x)

-- | Conjoin two stack heights to compute the maximum height.
mergeStackHeight :: StackHeight -> StackHeight -> StackHeight
mergeStackHeight (StackHeight xc xs) (StackHeight yc ys) =
  StackHeight (min xc yc) (Set.union xs ys)

------------------------------------------------------------------------
-- Function recovery

data FnRegValue cl where
  -- | This is a callee saved register.
  CalleeSaved :: N.RegisterName cl -> FnRegValue cl
  FnIntArg :: Int -> FnRegValue 'N.GP


data RecoverState = RS { _rsInterp :: !InterpState
                       , _rsBlocks :: !(Map BlockLabel FnBlock)
                       , _rsFrontier :: !(Set BlockLabel)
                       , _rsNextAssignId :: !AssignId
                       , _rsRegMap    :: !(Map CodeAddr (MapF N.RegisterName FnRegValue))
                       , _rsStackMap  :: !(Map CodeAddr FnStack)

                       , _rsCurAddr   :: !CodeAddr
                       , _rsCurStmts  :: !(Seq FnStmt)
                       , _rsAssignMap :: !(MapF Assignment FnAssignment)
                       }


rsInterp :: Simple Lens RecoverState InterpState
rsInterp = lens _rsInterp (\s v -> s { _rsInterp = v })

rsBlocks :: Simple Lens RecoverState (Map BlockLabel FnBlock)
rsBlocks = lens _rsBlocks (\s v -> s { _rsBlocks = v })

rsFrontier :: Simple Lens RecoverState (Set BlockLabel)
rsFrontier = lens _rsFrontier (\s v -> s { _rsFrontier = v })

rsNextAssignId :: Simple Lens RecoverState AssignId
rsNextAssignId = lens _rsNextAssignId (\s v -> s { _rsNextAssignId = v })

rsRegMap :: Simple Lens RecoverState (Map CodeAddr (MapF N.RegisterName FnRegValue))
rsRegMap = lens _rsRegMap (\s v -> s { _rsRegMap = v })

rsStackMap :: Simple Lens RecoverState (Map CodeAddr FnStack)
rsStackMap = lens _rsStackMap (\s v -> s { _rsStackMap = v })

rsCurAddr :: Simple Lens RecoverState CodeAddr
rsCurAddr = lens _rsCurAddr (\s v -> s { _rsCurAddr = v })

-- | List of statements accumulated so far.
rsCurStmts :: Simple Lens RecoverState (Seq FnStmt)
rsCurStmts = lens _rsCurStmts (\s v -> s { _rsCurStmts = v })

-- | Map from assignments in original block to assignment in
rsAssignMap :: Simple Lens RecoverState (MapF Assignment FnAssignment)
rsAssignMap = lens _rsAssignMap (\s v -> s { _rsAssignMap = v })

{-
-- | Map from assignments in original block to assignment in
rsStackHeight :: Simple Lens RecoverState StackHeight
rsStackHeight = lens _rsStackHeight (\s v -> s { _rsStackHeight = v })
-}

type Recover a = StateT RecoverState (ErrorT String Identity) a

runRecover :: RecoverState -> Recover a -> Either String a
runRecover s m = runIdentity $ runErrorT $ evalStateT m s

recoverFunction :: InterpState -> CodeAddr -> Either String Function
recoverFunction s a = do
  let initRegs = MapF.empty
               & MapF.insert N.rdi (FnIntArg 0)
               & MapF.insert N.rsi (FnIntArg 1)
               & MapF.insert N.rdx (FnIntArg 2)
               & MapF.insert N.rcx (FnIntArg 3)
               & MapF.insert N.r8  (FnIntArg 4)
               & MapF.insert N.r9  (FnIntArg 5)
               & MapF.insert N.rbx (CalleeSaved N.rbx)
               & MapF.insert N.r12 (CalleeSaved N.r12)
               & MapF.insert N.r13 (CalleeSaved N.r13)
               & MapF.insert N.r14 (CalleeSaved N.r14)
               & MapF.insert N.r15 (CalleeSaved N.r15)
               & MapF.insert N.rbp (CalleeSaved N.rbp)
  let rs = RS { _rsInterp = s
              , _rsBlocks = Map.empty
              , _rsFrontier = Set.singleton (GeneratedBlock a 0)
              , _rsNextAssignId = 0
              , _rsRegMap = Map.singleton a initRegs
              , _rsStackMap = Map.singleton a initFnStack
              , _rsCurAddr = 0
              , _rsCurStmts = Seq.empty
              , _rsAssignMap = MapF.empty
              }
  runRecover rs $ do
    recoverIter
    block_map <- use rsBlocks
    return $! Function { fnAddr = a
                       , fnBlocks = Map.elems block_map
                       }

getCurStack :: Recover FnStack
getCurStack = do
  addr <- use rsCurAddr
  m_stk <- uses rsStackMap (Map.lookup addr)
  case m_stk of
    Nothing -> error "Current stack undefined."
    Just stk -> return stk


addFnStmt :: FnStmt -> Recover ()
addFnStmt stmt = rsCurStmts %= (Seq.|> stmt)

recoverIter :: Recover ()
recoverIter = do
  f <- use rsFrontier
  case Set.maxView f of
    Nothing -> return ()
    Just (lbl,f') -> do
      trace ("Exploring " ++ show lbl) $ do
      rsCurAddr .= labelAddr lbl
      rsCurStmts  .= Seq.empty
      rsAssignMap .= MapF.empty
      b <- recoverBlock lbl
      rsFrontier %= Set.delete lbl
      rsBlocks   %= Map.insert lbl b

recoverBlock :: BlockLabel
             -> Recover FnBlock
recoverBlock lbl = do
  Just b <- uses (rsInterp . blocks)  (`lookupBlock` lbl)


  let ht0 = StackHeight 0 Set.empty
      ht = flip execState ht0 $ do
             Fold.traverse_ recoverStmtStackHeight (blockStmts b)
             recoverTermStmtStackHeight (blockTerm b)
  case ht of
    StackHeight c s
      | c == 0 && Set.null s -> return ()
      | Set.null s -> do
        let sz = FnConstantValue n64 (toInteger (negate c))
        fnAssign <- mkFnAssign (FnAlloca sz)
        addFnStmt $ FnAssignStmt fnAssign
      | otherwise ->
        trace "Unsupported stack height" $ return ()

  Fold.traverse_ recoverStmt (blockStmts b)
  term <- recoverTermStmt (blockTerm b)
  stmts <- use rsCurStmts
  return $! FnBlock { fbLabel = lbl
                    , fbStmts = Fold.toList stmts
                    , fbTerm = term
                    }


mkFnAssign :: FnAssignRhs tp -> Recover (FnAssignment tp)
mkFnAssign rhs = do
  next_id <- use rsNextAssignId
  rsNextAssignId .= next_id + 1
  return $! FnAssignment next_id rhs

asStackAddrOffset :: Value (BVType 64) -> Maybe (Value (BVType 64))
asStackAddrOffset addr = do
  BVAdd _ (Initial base) offset <- valueAsApp addr
  Refl <- testEquality base N.rsp
  return offset


asInt64Constant :: Value (BVType 64) -> Maybe Int64
asInt64Constant (BVValue _ o) = Just (fromInteger o)
asInt64Constant _ = Nothing

asFixedStackAddrOffset :: Value (BVType 64) -> Maybe Int64
asFixedStackAddrOffset addr = do
  asInt64Constant =<< asStackAddrOffset addr


-- | This code is reponsible for parsing the statement to determine
-- what code needs to be added to support an alloca at the beginning
-- of the block.
recoverStmtStackHeight :: Stmt -> State StackHeight ()
recoverStmtStackHeight s = do
  case s of
    Write (MemLoc addr _) val
      | Just offset <- asStackAddrOffset addr -> do
        modify $ mergeStackHeight (valueAsStackHeight offset)
    _ -> do
      return ()

recoverTermStmtStackHeight :: TermStmt -> State StackHeight ()
recoverTermStmtStackHeight (FetchAndExecute s) =
  case asStackAddrOffset (s ^. register N.rsp) of
    Nothing ->
      trace "Could not intepret stack height" $ do
        return ()
    Just offset -> do
      modify $ mergeStackHeight (valueAsStackHeight offset)
recoverTermStmtStackHeight _ = do
  return ()


-- | This should add code as needed to support the statement.
recoverStmt :: Stmt -> Recover ()
recoverStmt s =
  case s of
    AssignStmt assign -> do
      let lhs = assignId assign
      case assignRhs assign of
        EvalApp _ -> return ()
        SetUndefined w -> do
          fnAssign <- mkFnAssign (FnSetUndefined w)
          rsAssignMap %= MapF.insert assign fnAssign
          addFnStmt $ FnAssignStmt fnAssign
        Read (MemLoc addr tp) -> do
          fn_addr <- recoverValue addr
          fnAssign <- mkFnAssign (FnReadMem fn_addr tp)
          rsAssignMap %= MapF.insert assign fnAssign
          addFnStmt $ FnAssignStmt fnAssign
        _ -> trace ("recoverStmt undefined for " ++ show (pretty s)) $ do
          return ()
    Write (MemLoc addr _) val
      | Just int_addr_off <- asStackAddrOffset addr
      , Initial reg <- val -> do
        stk <- getCurStack
        m_reg_val <- lookupInitialReg reg
        case m_reg_val of
          Just (CalleeSaved _) -> do
            -- TODO: Update stack with this information.
            return ()
          _ -> do
            let r_addr = stackOffsetAddr stk int_addr_off
            r_val  <- recoverValue val
            addFnStmt $ FnWriteMem r_addr r_val
      | otherwise -> do
        r_addr <- recoverValue addr
        r_val  <- recoverValue val
        addFnStmt $ FnWriteMem r_addr r_val
    Comment msg -> do
      addFnStmt $ FnComment msg
    _ -> trace ("recoverStmt undefined for " ++ show (pretty s)) $ do
      return ()

addFrontier :: BlockLabel -> Recover ()
addFrontier lbl = do
  mr <- uses rsBlocks (Map.lookup lbl)
  case mr of
    Nothing -> do
      rsFrontier %= Set.insert lbl
    Just{} -> do
      return ()

recoverTermStmt :: TermStmt -> Recover FnTermStmt
recoverTermStmt s =
  case s of
    Branch c x y -> do
      cv <- recoverValue c
      addFrontier x
      addFrontier y
      return $! FnBranch cv x y
    _ -> trace ("recoverTermStmt undefined for " ++ show (pretty s)) $ do
      return $ FnTermStmtUndefined

lookupInitialReg :: N.RegisterName cl -> Recover (Maybe (FnRegValue cl))
lookupInitialReg reg = do
  addr <- use rsCurAddr
  maybe_map <- uses rsRegMap (Map.lookup addr)
  case maybe_map of
    Nothing -> do
      error $ "Did not define register map for " ++ showHex addr "."
    Just reg_map -> do
      return $! MapF.lookup reg reg_map

recoverValue :: Value tp -> Recover (FnValue tp)
recoverValue v = do
  interpState <- use rsInterp
  mem <- uses rsInterp memory
  case v of
    BVValue w i
      | Just Refl <- testEquality w n64
      , let addr = fromInteger i
      , Just seg <- findSegment addr mem -> do
        case () of
          _ | memFlags seg `hasPermissions` pf_x
            , Set.member addr (interpState^.functionEntries) -> do
              return $ FnEntryValue addr

            | memFlags seg `hasPermissions` pf_x
            , Map.member addr (interpState^.blocks) -> do
              cur_addr <- use rsCurAddr
              when (not (inSameFunction cur_addr addr interpState)) $ do
                trace ("Cross function jump " ++ showHex cur_addr " to " ++ showHex addr ".") $
                  return ()
              return $ FnBlockValue addr

            | otherwise -> do
              trace ("recoverValue given segment pointer: " ++ showHex i "") $ do
                return $ FnValueUnsupported
      | otherwise -> do
        return $ FnConstantValue w i

    AssignedValue assign -> do
      m_seen <- uses rsAssignMap (MapF.lookup assign)
      case m_seen of
        Just fnAssign ->
          return $! FnAssignedValue fnAssign
        Nothing -> do
          case assignRhs assign of
            EvalApp app -> do
              app' <- traverseApp recoverValue app
              fnAssign <- mkFnAssign (FnEvalApp app')
              rsAssignMap %= MapF.insert assign fnAssign
              return $! FnAssignedValue fnAssign
            _ -> do
              trace ("recoverValue does not yet support assignment " ++ show (pretty assign)) $
                return $ FnValueUnsupported
    Initial reg -> do
      m_reg_val <- lookupInitialReg reg
      case m_reg_val of
        Just (CalleeSaved _) -> do
          trace ("recoverValue unexpectedly encountered callee saved register: " ++ show reg) $ do
          return $ FnValueUnsupported
        _ ->
          trace ("recoverValue does not yet support initial value " ++ show reg) $
            return $ FnValueUnsupported