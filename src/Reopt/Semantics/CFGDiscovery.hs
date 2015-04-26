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

class (Applicative m, Monad m) => TransferMonad m where
  recordWrite :: AbsRegs -> Value (BVType 64) -> Value tp -> m ()

transferStmt :: TransferMonad m
             => Stmt
             -> StateT AbsRegs m ()
transferStmt stmt =
  case stmt of
    AssignStmt a -> do
      modify $ addAssignment a
    Write (MemLoc addr _) v -> do
      regs <- get
      lift $ recordWrite regs addr v
    _ -> return ()

transferStmts :: TransferMonad m => AbsRegs -> [Stmt] -> m AbsRegs
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
              Branch _ lb rb ->
                insBlock (findBlock g lb) r $
                insBlock (findBlock g rb) r $
                m
              FetchAndExecute _ -> m

          where r = runIdentity $ transferStmts r0 (blockStmts b)
                m = MapF.union (r^.absAssignments) m0



------------------------------------------------------------------------
-- Transfer functions

instance TransferMonad Identity where
  recordWrite _ _ _ = return ()

instance TransferMonad (State InterpState) where
  recordWrite regs addr v
    | Just Refl <- testEquality (valueType v) (knownType :: TypeRepr (BVType 64)) = do
      let block_addr = absInitial regs^.absX86State^.curIP
      case transferValue regs addr of
        StackOffset _ -> return ()
        AbsValue _ -> return ()
        TopV -> trace ("Write to top at " ++ show (pretty block_addr)) return ()
      case concretize (transferValue regs v) of
        Nothing  -> return ()
        Just vs1 -> do
          mem <- gets memory
          let vs2 = map fromInteger (Set.toList vs1)
          forM_ vs2 $ \val -> do
            alreadyInMem <- uses codePointersInMem $ Set.member val
            when (isCodePointer mem val && not alreadyInMem) $ do
              codePointersInMem %= Set.insert val
              absState %= Map.delete val
              frontier %= Set.insert val
    | otherwise = return ()


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

isCodePointer :: Memory Word64 -> CodeAddr -> Bool
isCodePointer mem val = addrHasPermissions (fromIntegral val) pf_x mem

doMaybe :: Monad m => m (Maybe a) -> b -> (a -> m b) -> m b
doMaybe m n j = do
  ma <- m
  case ma of
    Nothing -> return n
    Just a -> j a

findBlock :: CFG -> BlockLabel -> Block
findBlock g l = b
  where Just b = g^.cfgBlocks^.at l

-- Check floating point top.
getAbsX87Top :: Monad m => AbsBlockState -> m Int
getAbsX87Top abst =
  case asConcreteSingleton (abst^.absX86State^. x87TopReg) of
    Just v -> return (fromInteger v)
    _ -> fail "x87top is not concrete"

hasCall :: Block -> Bool
hasCall b = any isCallComment (blockStmts b)
  where isCallComment (Comment s) = "call" `isInfixOf` s
        isCallComment _ = False

transfer :: CodeAddr -> State InterpState ()
transfer addr = do
  let goBlock :: Block   -- ^ Block to start from.
              -> AbsRegs -- ^ Registers at this block.
              -> State InterpState ()
      goBlock b regs = do
        regs' <- transferStmts regs (blockStmts b)
        case blockTerm b of
          Branch _ lb rb -> do
            g <- use cfg
            goBlock (findBlock g lb) regs'
            goBlock (findBlock g rb) regs'

          FetchAndExecute s' -> do
            let abst = finalAbsBlockState regs' s'
            -- Look for new ips.
            case concretize (abst^.absX86State^.curIP) of
              Nothing  -> do
                case s'^.curIP of
                  AssignedValue (assignRhs -> Read (MemLoc _ _)) -> do
                    -- we hit top, so give up
                    return ()
                  _ | hasCall b -> do
                    -- This likely ends with a call.
                    return ()

                  next -> assert (addr >= 0) $ do
                    trace ("INVALID IP " ++ showHex addr "\n" ++ show (pretty next)) $ do
                    -- we hit top, so give up
                    return ()
              Just ips -> do
                mapM_ (mergeBlock abst) (fmap fromInteger (Set.toList ips))

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
    s0 = emptyInterpState mem start
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