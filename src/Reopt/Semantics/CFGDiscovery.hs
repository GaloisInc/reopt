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

module Reopt.Semantics.CFGDiscovery
       ( FinalCFG(..)
       , cfgFromAddress
       , assignmentAbsValues
       ) where

import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Monad.State.Strict
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import Data.Parameterized.Some
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           Debug.Trace
import Numeric
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Reopt.AbsState
import           Reopt.Memory
import           Reopt.Semantics.Implementation
import           Reopt.Semantics.Representation
import qualified Reopt.Semantics.StateNames as N
import           Reopt.Semantics.Types

------------------------------------------------------------------------
-- AbsState

-- | Maps each code address to a set of abstract states
type AbsState = Map CodeAddr AbsBlockState


setAbsIP :: CodeAddr -> AbsBlockState -> AbsBlockState
setAbsIP a = absX86State . curIP .~ abstractSingleton knownNat (toInteger a)


defBlockState :: CodeAddr -> AbsBlockState
defBlockState addr =
  top & setAbsIP addr
      & absX86State . register N.rsp .~ concreteStackOffset 0
      & absX86State . x87TopReg .~ abstractSingleton knownNat 7

emptyAbsState :: CodeAddr -> AbsState
emptyAbsState start = Map.singleton start (defBlockState start)

lookupAbsBlock :: CodeAddr -> AbsState -> AbsBlockState
lookupAbsBlock addr s = fromMaybe (error msg) (Map.lookup addr s)
  where msg = "Could not find block " ++ show addr

------------------------------------------------------------------------
-- Interpreter state

-- | The state of the interpreter
data InterpState
   = InterpState { -- | The initial memory when disassembly started.
                   memory   :: !(Memory Word64)
                 , _cfg      :: !CFG
                 , _genState :: !GlobalGenState
                   -- | Set of code adddresses that could not be interpreted.
                 , _failedAddrs  :: !(Set CodeAddr)
                   -- | Set of code addresses stored in memory.
                 , _codePointersInMem :: !(Set CodeAddr)
                   -- | Abstract state common to all code that can be jumped to in memory.
                 , memBlockState :: !AbsBlockState
                   -- | Set of addresses to explore next.
                 , _frontier :: !(Set CodeAddr)
                   -- | Abstract state
                 , _absState :: !AbsState
                 }

emptyInterpState :: Memory Word64 -> CodeAddr -> InterpState
emptyInterpState mem start = InterpState
      { memory        = mem
      , _cfg = emptyCFG
      , _genState     = emptyGlobalGenState
      , _failedAddrs = Set.empty
      , _codePointersInMem = Set.empty
      , memBlockState = defBlockState 0
      , _frontier     = Set.singleton start
      , _absState     = emptyAbsState start
      }

cfg :: Simple Lens InterpState CFG
cfg = lens _cfg (\s v -> s { _cfg = v })

genState :: Simple Lens InterpState GlobalGenState
genState = lens _genState (\s v -> s { _genState = v })

failedAddrs :: Simple Lens InterpState (Set CodeAddr)
failedAddrs = lens _failedAddrs (\s v -> s { _failedAddrs = v })

codePointersInMem :: Simple Lens InterpState (Set CodeAddr)
codePointersInMem = lens _codePointersInMem (\s v -> s { _codePointersInMem = v })

frontier :: Simple Lens InterpState (Set CodeAddr)
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
lookupBlock addr = uses (cfg . cfgBlocks) (Map.lookup (DecompiledBlock addr))

getAbsBlockState :: CodeAddr -> State InterpState AbsBlockState
getAbsBlockState a = do
  s <- get
  if Set.member a (s^.codePointersInMem) then
    return (memBlockState s & setAbsIP a)
  else
    return $ lookupAbsBlock a (s^.absState)


-- | This is the worker for getBlock, in the case that the cfg doesn't
-- contain the address of interest.
reallyGetBlock :: CodeAddr -> State InterpState (Maybe Block)
reallyGetBlock addr = do
  mem <- gets memory
  -- Get top
  ab <- getAbsBlockState addr
  t <- getAbsX87Top ab
  -- Create explore loc
  let loc = ExploreLoc { loc_ip = addr
                       , loc_x87_top = t
                       }
  r <- subMonad genState $ do
    liftEither $ disassembleBlock mem loc
  case r of
   Left _e -> trace ("Block failed: 0x" ++ showHex addr "") $ do
     failedAddrs %= Set.insert addr
     return Nothing
   Right (bs, next_ip) -> do
     cfg %= insertBlocksForCode addr next_ip bs
     lookupBlock addr

-- | Returns a block at the given location, if at all possible.  This
-- will disassemble the binary if the block hasn't been seen before.
-- In particular, this ensures that a block and all it's children are
-- present in the cfg (assuming successful disassembly)
getBlock :: CodeAddr -> State InterpState (Maybe Block)
getBlock addr = do
  m_b <- lookupBlock addr
  case m_b of
    Just b -> return (Just b)
    Nothing -> do
      failed <- uses failedAddrs (Set.member addr)
      if failed then
        return Nothing
      else
        reallyGetBlock addr

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

recordEscapedCodePointer :: Word64 -> InterpState -> InterpState
recordEscapedCodePointer val s
  | Set.member val (s^.codePointersInMem) = s
  | otherwise = s & codePointersInMem %~ Set.insert val
                  & absState          %~ Map.delete val
                  & frontier          %~ Set.insert val

recordEscapedCodePointers :: [Word64] -> InterpState -> InterpState
recordEscapedCodePointers = flip (foldl' (flip recordEscapedCodePointer))

recordWriteStmt :: AbsRegs -> Stmt -> State InterpState ()
recordWriteStmt regs (Write (MemLoc _addr _) v)
  | Just Refl <- testEquality (valueType v) (knownType :: TypeRepr (BVType 64))
  , Just vs1 <- concretize (transferValue regs v) = do
    mem <- gets memory
    let vs2 = filter (isCodePointer mem) $ map fromInteger (Set.toList vs1)
    modify $ recordEscapedCodePointers vs2
recordWriteStmt _ _ = return ()

transferStmts :: Monad m => AbsRegs -> [Stmt] -> m AbsRegs
transferStmts r stmts = execStateT (mapM_ transferStmt stmts) r

finalBlockState :: CodeAddr -> FinalCFG -> AbsBlockState
finalBlockState a g
  | Set.member a (finalCodePointersInMem g) = defBlockState a
  | otherwise = lookupAbsBlock a (finalAbsState g)

assignmentAbsValues :: FinalCFG -> MapF Assignment AbsValue
assignmentAbsValues fg = foldl' go MapF.empty (Map.elems (g^.cfgBlocks))
  where g = finalCFG fg

        go :: MapF Assignment AbsValue
           -> Block
           -> MapF Assignment AbsValue
        go m0 b =
          case blockLabel b of
            GeneratedBlock{} -> m0
            DecompiledBlock a -> insBlock b (initAbsRegs (finalBlockState a fg)) m0

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
mergeBlock :: AbsBlockState
           -> CodeAddr
           -> State InterpState ()
mergeBlock ab0 addr = do
  s <- get
  when (Set.member addr (s^.codePointersInMem) == False) $ do
    let upd new = do
          absState %= Map.insert addr new
          frontier %= Set.insert addr

    let ab = ab0 & setAbsIP addr
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
refineValue v@(BVValue _n _val) av regs = regs
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
   --  -- basically less-than: does x - y overflow? only if x < y.
   UsbbOverflows sz l r (BVValue _ 0)
     | Just b    <- asConcreteSingleton av -> refineLt (BVTypeRepr sz) l r b regs
   -- Mux can let us infer the condition?
   _ -> regs

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


-- | Returns true if it looks like block ends with a call.
checkBlockCall :: Memory Word64
               -> AbsRegs
               -> [Stmt]
               -> X86State Value
               -> State InterpState [Word64]
checkBlockCall mem regs stmts s = go (reverse stmts)
  where next_sp = s^.register N.rsp
        go (stmt:r)
          | Just ret <- isCodePointerWriteTo mem stmt next_sp = do
            mapM_ (recordWriteStmt regs) r
            return [ret]
        go r@(Write{}:_) = do
          mapM_ (recordWriteStmt regs) r
          return []
        go (_:r) = go r
        go [] = return []

transfer :: CodeAddr -> State InterpState ()
transfer addr = do
  let goBlock :: Block   -- ^ Block to start from.
              -> AbsRegs -- ^ Registers at this block.
              -> State InterpState ()
      goBlock b regs = do
        regs' <- transferStmts regs (blockStmts b)
        -- FIXME: we should propagate c back to the initial block, not just b
        case blockTerm b of
          Branch c lb rb -> do
            mapM_ (recordWriteStmt regs') (blockStmts b)
            g <- use cfg
            let Just l = findBlock g lb
                l_regs = refineValue c (abstractSingleton n1 1) regs'
            let Just r = findBlock g rb
                r_regs = refineValue c (abstractSingleton n1 0) regs'
            -- We re-transfer the stmts to propagate any changes from
            -- the above refineValue.  This could be more efficient by
            -- tracking what (if anything) changed.  We also might
            -- need to keep going back and forth until we reach a
            -- fixpoint
            goBlock l =<< transferStmts l_regs (blockStmts b)
            goBlock r =<< transferStmts r_regs (blockStmts b)

          FetchAndExecute s' -> do
            mem <- gets memory
            rets <- checkBlockCall mem regs' (blockStmts b) s'
            let abst = finalAbsBlockState regs' s'
            -- Look for new ips.
            let ips :: [Word64]
                ips = case concretize (abst^.absX86State^.curIP) of
                        Nothing -> []
                        Just ips' -> fmap fromInteger (Set.toList ips')
            mapM_ (mergeBlock abst) (rets ++ ips)

  doMaybe (getBlock addr) () $ \root -> do
    ab <- getAbsBlockState addr
    goBlock root (initAbsRegs ab)

------------------------------------------------------------------------
-- Main loop

data FinalCFG = FinalCFG { finalCFG :: !CFG
                         , finalAbsState :: !AbsState
                         , finalCodePointersInMem :: !(Set CodeAddr)
                         , finalFailedAddrs :: !(Set CodeAddr)
                         }


cfgFromAddress :: Memory Word64
                  -- ^ Memory to use when decoding instructions.
               -> CodeAddr
                  -- ^ Location to start disassembler form.
               -> FinalCFG
cfgFromAddress mem start = r
  where
    code_pointers = filter (isCodePointer mem) (memAsWord64le mem)
    s0 = recordEscapedCodePointers code_pointers
       $ emptyInterpState mem start

    s' = go s0

    r = FinalCFG { finalCFG = s'^.cfg
                 , finalAbsState = s'^.absState
                 , finalCodePointersInMem = s'^.codePointersInMem
                 , finalFailedAddrs = s'^.failedAddrs
                 }

    go :: InterpState
       -> InterpState
    go st =
      case Set.minView (st^.frontier) of
        Nothing -> st
        Just (addr, next_roots) ->
          let st_pre = st & frontier .~ next_roots
              st_post = flip execState st_pre $ transfer addr
           in go st_post
