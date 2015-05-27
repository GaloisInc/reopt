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

module Reopt.Semantics.CFGDiscovery
       ( FinalCFG(..)
       , cfgFromAddress
       , assignmentAbsValues
       ) where

import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad.State.Strict
import qualified Data.ByteString as BS
import qualified Data.Foldable as Fold
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
import qualified Data.Vector as V
import           Data.Word
import           Debug.Trace
import           Numeric
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Reopt.AbsState
import qualified Reopt.Domains.StridedInterval as SI
import           Reopt.Memory
import           Reopt.Semantics.Implementation
import           Reopt.Semantics.Representation
import qualified Reopt.Semantics.StateNames as N
import           Reopt.Semantics.Types

------------------------------------------------------------------------
-- AbsState

-- | Maps each code address to a set of abstract states
type AbsState = Map CodeAddr AbsBlockState


setAbsIP :: Memory Word64 -> CodeAddr -> AbsBlockState -> AbsBlockState
setAbsIP mem a = absX86State . curIP .~ abstractSingleton mem knownNat (toInteger a)


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
-- FrontierState

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
  deriving (Show)

------------------------------------------------------------------------
-- Interpreter state

data BlockRegion = BlockRegion { brEnd :: !CodeAddr
                               , brBlocks :: !(V.Vector Block)
                               }


-- | The state of the interpreter
data InterpState
   = InterpState { -- | The initial memory when disassembly started.
                   memory   :: !(Memory Word64)
                 , _genState :: !GlobalGenState
                   -- | Addresses that we have seen.
                 , _seenAddrs :: !(Set CodeAddr)
                   -- | Intervals maps code addresses to blocks at address.
                 , _blocks   :: !(Map CodeAddr BlockRegion)
                   -- | Set of code adddresses that could not be interpreted.
                 , _failedAddrs  :: !(Set CodeAddr)
                   -- | Addresses that are marked as the start of a function
                 , _functionEntries :: !(Set CodeAddr)
                   -- | Set of code addresses stored in memory.
                 , _codePointersInMem :: !(Set CodeAddr)
                   -- | Abstract state common to all code that can be jumped to in memory.
                 , memBlockState :: !AbsBlockState
                   -- | Set of addresses to explore next.
                 , _frontier :: !(Map CodeAddr FrontierReason)
                   -- | Abstract state
                 , _absState :: !AbsState
                 }

-- | Empty interpreter state.
emptyInterpState :: Memory Word64 -> CodeAddr -> InterpState
emptyInterpState mem start = InterpState
      { memory        = mem
      , _genState     = emptyGlobalGenState
      , _seenAddrs    = Set.empty
      , _blocks       = Map.empty
      , _failedAddrs  = Set.empty
      , _functionEntries   = Set.empty
      , _codePointersInMem = Set.empty
      , memBlockState = defBlockState mem 0
      , _frontier     = Map.singleton start StartAddr
      , _absState     = emptyAbsState mem start
      }

genState :: Simple Lens InterpState GlobalGenState
genState = lens _genState (\s v -> s { _genState = v })

seenAddrs :: Simple Lens InterpState (Set CodeAddr)
seenAddrs = lens _seenAddrs (\s v -> s { _seenAddrs = v })

blocks :: Simple Lens InterpState (Map CodeAddr BlockRegion)
blocks = lens _blocks (\s v -> s { _blocks = v })

failedAddrs :: Simple Lens InterpState (Set CodeAddr)
failedAddrs = lens _failedAddrs (\s v -> s { _failedAddrs = v })

functionEntries :: Simple Lens InterpState (Set CodeAddr)
functionEntries = lens _functionEntries (\s v -> s { _functionEntries = v })

codePointersInMem :: Simple Lens InterpState (Set CodeAddr)
codePointersInMem = lens _codePointersInMem (\s v -> s { _codePointersInMem = v })

frontier :: Simple Lens InterpState (Map CodeAddr FrontierReason)
frontier = lens _frontier (\s v -> s { _frontier = v })

absState :: Simple Lens InterpState AbsState
absState = lens _absState (\s v -> s { _absState = v })

liftEither :: StateT s (Either e) a -> State s (Either e a)
liftEither m = state go
  where
    go s = case runStateT m s of
             Left e       -> (Left e,  s)
             Right (r, t) -> (Right r, t)

  -- FIXME: move
subMonad :: (MonadState s m)
         => Simple Lens s t
         -> State t r
         -> m r
subMonad l m = l %%= runState m

------------------------------------------------------------------------
-- Block discovery

-- | Does a simple lookup in the cfg at a given DecompiledBlock address.
lookupBlock :: MonadState InterpState m => CodeAddr -> m (Maybe Block)
lookupBlock addr = do
  m <- use blocks
  case Map.lookup addr m of
    Nothing -> return Nothing
    Just br -> assert (V.length (brBlocks br) > 0) $ do
      let b = brBlocks br V.! 0
      seq b $ return $! Just b

-- | Does a simple lookup in the cfg at a given DecompiledBlock address.
lookupBlock' :: MonadState InterpState m => BlockLabel -> m Block
lookupBlock' lbl = do
  m <- use blocks
  let i = fromIntegral (blockIndex lbl)
  case Map.lookup (blockParent lbl) m of
    Nothing -> error $ "Could not find block for " ++ show lbl
    Just br -> assert (V.length (brBlocks br) > i) $ do
      return $! brBlocks br V.! i

getAbsBlockState :: CodeAddr -> State InterpState AbsBlockState
getAbsBlockState a = do
  s <- get
  if Set.member a (s^.codePointersInMem) then
    return (memBlockState s & setAbsIP (memory s) a)
  else
    return $ lookupAbsBlock a (s^.absState)

-- | This is the worker for getBlock, in the case that the cfg doesn't
-- contain the address of interest.
reallyGetBlock :: FrontierReason
                  -- ^ Reason we are exploring block.
               -> CodeAddr
               -> State InterpState (Maybe Block)
reallyGetBlock rsn addr = do
  seenAddrs %= Set.insert addr
  -- Check to see if we should delete an overlapping block
  do m_lt <- uses blocks (Map.lookupLT addr)
     case m_lt of
       Just (l,br) | brEnd br > addr -> do
         blocks %= Map.delete l
         frontier %= Map.insert l rsn
       _ -> return ()
  -- Get top
  ab <- getAbsBlockState addr
  t <- getAbsX87Top ab
  -- Create explore loc
  let loc = ExploreLoc { loc_ip = addr
                       , loc_x87_top = t
                       }
  addrs <- use seenAddrs
  let -- Returns true if we are not at the start of a block
      not_at_block addr0 = Set.notMember addr0 addrs
  mem <- gets memory
  r <- subMonad genState $ do
    liftEither $ disassembleBlock mem not_at_block loc
  -- disassemble block.
  case r of
   Left _e -> trace ("Block failed: 0x" ++ showHex addr "" ++ ", Reason " ++ show rsn) $ do
     failedAddrs %= Set.insert addr
     return Nothing
   Right (bs, next_ip) -> assert (next_ip > addr) $ do
     let block_vec = V.fromList bs
     -- Add block region to blocks.
     let br = BlockRegion { brEnd = next_ip
                          , brBlocks = block_vec
                          }
     blocks %= Map.insert addr br
     -- Return first block.
     return $ Just (block_vec V.! 0)

-- | Returns a block at the given location, if at all possible.  This
-- will disassemble the binary if the block hasn't been seen before.
-- In particular, this ensures that a block and all it's children are
-- present in the cfg (assuming successful disassembly)
getBlock :: FrontierReason -> CodeAddr -> State InterpState (Maybe Block)
getBlock rsn addr = do
  m_b <- lookupBlock addr
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
      regs <- get
      put $ addMemWrite addr v regs
    _ -> return ()

recordEscapedCodePointer :: Word64 -> FrontierReason -> InterpState -> InterpState
recordEscapedCodePointer val rsn s
  | Set.member val (s^.codePointersInMem) = s
  | otherwise =
     s & codePointersInMem %~ Set.insert val
       & absState          %~ Map.delete val
       & frontier          %~ Map.insert val rsn

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
    let vs2 = Set.toList (getNextIps' mem av)
    modify $ recordEscapedCodePointers vs2 (InWrite lbl)
recordWriteStmt _ _ _ = return ()

transferStmts :: Monad m => AbsRegs -> [Stmt] -> m AbsRegs
transferStmts r stmts = execStateT (mapM_ transferStmt stmts) r

finalBlockState :: Memory Word64 -> CodeAddr -> FinalCFG -> AbsBlockState
finalBlockState mem a g
  | Set.member a (finalCodePointersInMem g) = defBlockState mem a
  | otherwise = lookupAbsBlock a (finalAbsState g)

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

          where final = runIdentity $ transferStmts r0 (blockStmts b)
                m = MapF.union (final^.absAssignments) m0

------------------------------------------------------------------------
-- Transfer functions

-- | Joins in the new abstract state and returns the locations for
-- which the new state is changed.
mergeBlock :: Memory Word64
           -> FrontierReason
              -- ^ Reason we are going to explore the next address.
           -> AbsBlockState
              -- ^ Block state after executing instructions.
           -> CodeAddr
              -- ^ Address we are trying to reach.
           -> State InterpState ()
mergeBlock mem rsn ab0 addr = do
  s <- get
  when (Set.member addr (s^.codePointersInMem) == False) $ do
    let upd new = do
          absState %= Map.insert addr new
          frontier %= Map.insert addr rsn
    let ab = ab0 & setAbsIP mem addr
    case Map.lookup addr (s^.absState) of
      -- We have seen this block before, so need to join and see if
      -- the results is changed.
      Just ab_old ->
        case joinD ab_old ab of
          Nothing  -> return ()
          Just new -> upd new
      -- We haven't seen this block before
      Nothing  -> upd ab

_showAbsDiff :: AbsBlockState -> AbsBlockState -> Doc
_showAbsDiff x y = vcat (pp <$> absBlockDiff x y)
  where pp (Some n) = pretty (show n) <+> pretty (x^.absX86State^.register n)
                                      <+> pretty (x^.absX86State^.register n)

doMaybe :: Monad m => m (Maybe a) -> b -> (a -> m b) -> m b
doMaybe m n j = do
  ma <- m
  case ma of
    Nothing -> return n
    Just a -> j a

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
  -- av adds no new information
  | av_old `leq` av = regs
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

{-
-- | List of registers stored in X86State
x86CallerStateRegisters :: [Some N.RegisterName]
x86StateRegisters
  = [Some N.IPReg]
  ++ (Some <$> N.gpRegs)
  ++ (Some <$> N.flagRegs)
  ++ (Some <$> N.x87ControlRegs)
  ++ (Some <$> N.x87StatusRegs)
  ++ [Some N.X87TopReg]
  ++ (Some <$> N.x87TagRegs)
  ++ (Some <$> N.x87FPURegs)
  ++ (Some <$> N.xmmRegs)
-}

-- looks for jump tables
getJumpTable :: Memory Word64
             -> AbsRegs
             -> Value (BVType 64)
             -> Maybe (Set CodeAddr)
getJumpTable mem regs ptr
  | AssignedValue ass <- ptr
  , absAddrs <- regs^.absAssignments^.assignLens ass
  , any (\rorange -> absAddrs `leq` rorange) roranges
  , Just addrs <- concretize absAddrs =
    let bptrs = Set.map (\addr -> fromIntegral $ fromJust
                                  $ Map.lookup (fromIntegral addr) wordMap) addrs
    in
    if all (isRODataPointer mem . fromIntegral) $ Set.toList addrs
       then trace ("getJumpTable: " ++ show (pretty ass)
                   ++ " " ++ show (Set.map (flip showHex "") bptrs)) $
            Just bptrs
       else Nothing
  where
    wordMap = Map.fromAscList $ memAsWord64le_withAddr mem
    -- FIXME: move to Memory?
    rosegs  = readonlySegments mem
    -- FIXME: could also use 8 here
    roranges = map (\roseg ->
                     let base = fromIntegral $ memBase roseg
                         sz   = fromIntegral $ BS.length (memBytes roseg)
                     in
                     stridedInterval (SI.mkStridedInterval (BVTypeRepr n64) False
                                      base (base + sz - 1) 1))
               rosegs

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

transferBlock :: Block   -- ^ Block to start from.
              -> AbsRegs -- ^ Registers at this block.
              -> State InterpState ()
transferBlock b regs = do
  mem <- gets memory
  regs' <- transferStmts regs (blockStmts b)
  -- FIXME: we should propagate c back to the initial block, not just b
  case blockTerm b of
    Branch c lb rb -> do
      mapM_ (recordWriteStmt (blockLabel b) regs') (blockStmts b)
      l <- lookupBlock' lb
      let  l_regs = refineValue c (abstractSingleton mem n1 1) regs'
      r <- lookupBlock' rb
      let r_regs = refineValue c (abstractSingleton mem n1 0) regs'
      -- We re-transfer the stmts to propagate any changes from
      -- the above refineValue.  This could be more efficient by
      -- tracking what (if anything) changed.  We also might
      -- need to keep going back and forth until we reach a
      -- fixpoint
      transferBlock l =<< transferStmts l_regs (blockStmts b)
      transferBlock r =<< transferStmts r_regs (blockStmts b)

    FetchAndExecute s' -> do
      let abst = finalAbsBlockState regs' s'
      let lbl = blockLabel b
      seq abst $ do
      -- See if next statement appears to end with a call.
      -- We define calls as statements that end with a write that
      -- stores the pc to an address.
      case identifyCall mem (blockStmts b) s' of
        -- The last statement was a call.
        Just (prev_stmts, ret) -> do
          Fold.mapM_ (recordWriteStmt (blockLabel b) regs') prev_stmts
          mergeBlock mem (ReturnAddress lbl) abst ret
          -- Look for new ips.
          let ips = getNextIps mem s' regs'
          functionEntries %= Set.union ips
          Fold.forM_ ips $ \addr -> do
            mergeBlock mem (NextIP lbl) abst addr
        Nothing -> do
          mapM_ (recordWriteStmt (blockLabel b) regs') (blockStmts b)
          -- Look for new ips.
          Fold.forM_ (getNextIps mem s' regs') $ \addr -> do
            mergeBlock mem (NextIP lbl) abst addr


getNextIps :: Memory Word64 -> X86State Value -> AbsRegs -> Set CodeAddr
getNextIps mem s' regs' = do
    case s'^.curIP of
      AssignedValue (Assignment _ (Read (MemLoc ptr _))) ->
        fromMaybe Set.empty (getJumpTable mem regs' ptr)
      _ -> getNextIps' mem (abst^.absX86State^.curIP)
  where abst = finalAbsBlockState regs' s'

getNextIps' :: Memory Word64 -> AbsValue (BVType 64) -> Set CodeAddr
getNextIps' mem v =
  case concretize v of
    Nothing -> Set.empty
    Just ips -> Set.filter (isCodePointer mem) $
                  Set.mapMonotonic fromInteger $
                    ips

transfer :: FrontierReason -> CodeAddr -> State InterpState ()
transfer rsn addr = do
  mem <- gets memory
  doMaybe (getBlock rsn addr) () $ \root -> do
    ab <- getAbsBlockState addr
    transferBlock root (initAbsRegs mem ab)

------------------------------------------------------------------------
-- Main loop

data FinalCFG = FinalCFG { finalCFG :: !CFG
                         , finalAbsState :: !AbsState
                         , finalCodePointersInMem :: !(Set CodeAddr)
                         , finalFailedAddrs :: !(Set CodeAddr)
                         }

mkCFG :: Map CodeAddr BlockRegion -> CFG
mkCFG m = Map.foldlWithKey' go emptyCFG m
  where go g addr br = insertBlocksForCode addr (brEnd br) l g
          where l = V.toList (brBlocks br)

mkFinalCFG :: InterpState -> FinalCFG
mkFinalCFG s = FinalCFG { finalCFG = mkCFG (s^.blocks)
                        , finalAbsState = s^.absState
                        , finalCodePointersInMem = s^.codePointersInMem
                        , finalFailedAddrs = s^.failedAddrs
                        }

explore_frontier :: InterpState -> InterpState
explore_frontier st =
  case Map.minViewWithKey (st^.frontier) of
    Nothing -> st
    Just ((addr,rsn), next_roots) ->
      let st_pre = st & frontier .~ next_roots
          st_post = flip execState st_pre $ transfer rsn addr
       in explore_frontier st_post

cfgFromAddress :: Memory Word64
                  -- ^ Memory to use when decoding instructions.
               -> CodeAddr
                  -- ^ Location to start disassembler form.
               -> FinalCFG
cfgFromAddress mem start = g -- trace ("Function:\n" ++ concatMap ppStackHeights stack_heights) g

  where
--    fn = recoverFunction s3 0x422b10
--    0x422030

--    functionAddrs = Set.toList (s3^.functionEntries)
--    stack_heights = recoverFunction s3 <$> functionAddrs


--    ppAddr a = showHex a "\n"
--    code_pointers = filter (isCodePointer mem) (memAsWord64le mem)
    s0 = emptyInterpState mem start

    s1 = explore_frontier s0

    go s (a,v)
      | not (isCodePointer mem v) = s
      | Set.member v (s^.codePointersInMem) = s
      | Set.member v (s^.seenAddrs) =
        recordEscapedCodePointer v InInitialData s
      | otherwise =
        trace ("Found new code pointer " ++ showHex v " at " ++ showHex a ".") $
        recordEscapedCodePointer v InInitialData s

    s2  = foldl' go s1 (memAsWord64le_withAddr mem)

    s3 = explore_frontier s2
    g = mkFinalCFG s3

{-
------------------------------------------------------------------------
-- Function recovery

data FnAssignRhs w tp where
  -- An expression that is computed from evaluating subexpressions.
  FnEvalApp :: !(App Value tp)
            -> FnAssignRhs w tp

  -- An expression with an undefined value.
  FnSetUndefined :: !(NatRepr n) -- Width of undefined value.
                 -> FnAssignRhs w (BVType n)

  -- Read given location.
  FnRead :: !(StmtLoc tp) -> FnAssignRhs w tp

  -- Allocate a given number of bytes on the stack.
  FnAlloca :: !w -> FnAssignRhs w (BVType 64)

data FnAssignment w tp
   = FnAssignment { fnAssignId :: !AssignId
                  , fnAssignRhs :: !(FnAssignRhs w tp)
                  }


-- | A value at runtime.
data FnValue w tp where

  -- Bitvector value.
  FnBVValue :: !(NatRepr n) -> Integer -> FnValue w (BVType n)

  -- Value from an assignment statement.
  FnAssignedValue :: !(FnAssignment w tp) -> FnValue w tp

  FnInitial :: !(N.RegisterName cl) -> FnValue w (N.RegisterType cl)

data FnStmt w where
  FnAssignStmt :: !(FnAssignment w tp) -> FnStmt
  FnComment :: !Text -> FnStmt
  FnWrite :: !(StmtLoc (FnValue w (BVType 64)) tp) -> !(FnValue w tp) -> FnStmt


data FnTermStmt w where

  FnReturn :: !(FnValue w (BVType 64)) -> FnTermStmt w
  FnBranch :: !(FnValue w BoolType) -> !BlockLabel -> !BlockLabel -> FnTermStmt w
  FnJump :: !BlockLabel -> FnTermStmt w

data FnBlock w
   = FnBlock { fbLabel :: !BlockLabel
             , fbStmts :: Seq (FnStmt w)
             , fbTerm :: !(FnTermStmt w)
             }

data Function = Function { fnAddr :: CodeAddr
                         , fnBlocks :: [FnBlockunctionBlock]
                         }

data DelayedAllocaValue s where
  ConcreteAValue :: !(FnValue (DelayedAllocaValue s) (BVType 64))
                 -> DelayedAllocaValue s
  DelayedAValue  :: !(STRef s Word64)-> DelayedAllocaValue s

type DelayedFnBlock s = FnBlock (DelayedAllocaValue s)

data RecoverStates s = RS { rsBlocks :: !(Map CodeAddr BlockRegion)
                          , _rsFnBlocks :: !(Seq (DelayedFnBlock s))
                          }


rsFnBlocks :: Simple Lens (RecoverState s) (Seq (DelayedFnBlock s))
rsFnBlocks = lens _rsFnBlocks (\s v -> s { _rsFnBlocks = v })

type Recover s = StateT (RecoverState s) (ST s)
-}

{-
recoverBlock :: CodeAddr -> Int -> Block
recoverBlock a i = do
  mbr <- gets $ Map.lookup a . rsBlocks
  case mbr of
    Nothing -> fail $ "Could not recover " ++ showHex a "."
    Just br
      | 0 <= i && i < V.length (brBlocks br) -> do
        return $! brBlocks br V.! i
      | otherwise ->
        fail $ "Could not identify block."

recoverFunctionBlock :: Block -> Recover FunctionBlock
recoverFunctionBlock = error "recoverFunctionBlock unimplemented."

recoverStmtLoc :: StmtLoc (Value (BVType 64)) tp -> Recover (StmtLoc (FnValue (BVType 64)) tp)
recoverStmtLoc = error "recoverStmtLoc unimplemented."

recoverValue :: Value tp -> Recover (FnValue tp)
recoverValue = error "recoverValue unimplemented."

recoverStmt :: Stmt -> Recover FunctionStmt
recoverStmt stmt =
  case stmt of
    AssignStmt a -> do
      error "recoverStmt AssignStmt unimplemented."
    Write loc v -> do
      loc' <- recoverStmtLoc loc
      v' <- recoverValue v
      return $! FnWrite loc' v'
    PlaceHolderStmt _ _ -> do
      error "recoverStmt PlaceHolderStmt unimplemented."
    Comment msg -> do
      return $! FnComment msg

recoverAddrs :: Set CodeAddr
             -> Map CodeAddr RecoverBlockState
             -> Recover ()
recoverAddrs seen next = )do
  case Map.minViewWithKey next of
    Nothing -> return $! ()
    Just ((a,rbs), next') -> do
      b <- recoverBlock a 0
      stmts' <- mapM recoverStmt (blockStmts b)

      case blockTerm b of
        FetchAndExecute s -> undefined s
        Branch c x y -> undefined c x y
-}

data StackInfo = StackInfo { siAllocs :: ![Value (BVType 64)]
                           , siReserved :: !Word64
                           , siBPIsFramePtr :: !Bool
                           }
  deriving (Eq, Show)


type Function = Map BlockLabel StackInfo

ppStackHeights :: Function -> String
ppStackHeights m = concatMap go (Map.toList m)
  where go (lbl,si) = show lbl ++ "\n" ++ show si ++ "\n\n"

data RecoverState = RS { rsStart :: CodeAddr
                       , rsEnd :: CodeAddr
                       , rsBlocks :: Map CodeAddr BlockRegion
                       }

recoverBlock :: RecoverState -> BlockLabel -> Block
recoverBlock s (GeneratedBlock a i) =
  case Map.lookup a (rsBlocks s) of
    Nothing -> error $ "recoverBlock given bad address: " ++ showHex a "."
    Just br | fromIntegral i < V.length (brBlocks br) -> brBlocks br V.! fromIntegral i
            | otherwise -> error $ "recoverBlock given bad index: " ++ show i ++ "."

recoverStmt :: Stmt -> State StackInfo ()
recoverStmt stmt0 =
  case stmt0 of
    _ -> return ()

mergeSI :: BlockLabel
        -> StackInfo
        -> (Function, Set BlockLabel)
        -> (Function, Set BlockLabel)
mergeSI lbl si (m,s) =
  case Map.lookup lbl m of
    Nothing -> (Map.insert lbl si m, Set.insert lbl s)
    Just si'| si == si' -> (m, s)
            | otherwise ->
              error $ "mergeSI given incompatiable values:\n"
                 ++ show si  ++ "\n"
                 ++ show si' ++ "\n"

-- | This is designed to detect returns from the X86 representation.
-- It pattern matches on a X86State to detect if it read its instruction
-- pointer from an address that is 8 below the stack pointer.
recoverIsReturnStmt :: X86State Value -> Bool
recoverIsReturnStmt s = do
  let next_ip = s^.register N.rip
      next_sp = s^.register N.rsp
  let expected_base = Initial N.rsp
  case () of
    _ | AssignedValue (Assignment _ (Read (MemLoc a _))) <- next_ip
      , (ip_base, ip_off) <- asBaseOffset a
      , (sp_base, sp_off) <- asBaseOffset next_sp
      , ip_base == expected_base
      , sp_base == expected_base
      , ip_off + 8 == sp_off -> True
    _ -> False

{-
recoverIsTailCall :: X86State Value -> Bool
recoverIsTailCall _ = error "recoverIsTailCall"
-}



inFunctionRange :: RecoverState -> CodeAddr -> Bool
inFunctionRange s a = rsStart s <= a && a < rsEnd s

recoverAddrs :: RecoverState  -> Function -> Set BlockLabel -> Function
recoverAddrs s m next =
  case Set.minView next of
    Nothing -> m
    Just (lbl, next1) -> do
      let Just si = Map.lookup lbl m
      let b   = recoverBlock s lbl
      let si' = execState (mapM_ recoverStmt (blockStmts b)) si
      case blockTerm b of
        FetchAndExecute xs
           -- Stop at return stmt
          | recoverIsReturnStmt xs -> do
            recoverAddrs s m next1
           -- TODO:check if s is a function call, tail call, or jump to another block
           -- within the procedure.
          | BVValue _ (fromInteger -> concrete_ip) <- next_ip
          , next_sp == Initial N.rsp
          , inFunctionRange s concrete_ip -> do
            let lbl' = GeneratedBlock concrete_ip 0
            let (m',next2) = (m,next1) & mergeSI lbl' si'
            recoverAddrs s m' next2
          | otherwise -> error $ "recoverAddrs FetchAndExecute unimplemented:" ++ show lbl
         where next_ip = xs^.register N.rip
               next_sp = xs^.register N.rsp

        Branch _c x y -> do
          let (m', next2) = (m,next1) & mergeSI x si' & mergeSI y si'
          recoverAddrs s m' next2

recoverFunction :: InterpState -> CodeAddr -> Function
recoverFunction s addr = recoverAddrs rs m (Set.singleton lbl)
  where rs = RS { rsStart = addr
                , rsEnd = case Map.lookupGT addr (s^.blocks) of
                            Just (e,_) -> e
                            Nothing -> fromInteger (-1)
                , rsBlocks = s^.blocks
                }
        lbl = GeneratedBlock addr 0
        m = Map.singleton lbl si
        si = StackInfo { siAllocs = []
                       , siReserved = 0
                       , siBPIsFramePtr = False
                       }