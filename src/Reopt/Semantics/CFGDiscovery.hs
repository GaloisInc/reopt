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
       ( cfgFromAddress
       ) where

import           Control.Lens
import           Control.Monad.State.Strict
import Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import           Data.Parameterized.NatRepr
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           Debug.Trace
import Numeric
import           Text.PrettyPrint.ANSI.Leijen (pretty, Pretty(..))

import Reopt.AbsState
import           Reopt.Memory
import           Reopt.Semantics.Implementation
import           Reopt.Semantics.Representation
import           Reopt.Semantics.Types
import qualified Reopt.Semantics.WorkList as WL

------------------------------------------------------------------------
-- AbsState

-- | Maps each code address to a set of abstract states
type AbsState = Map CodeAddr AbsBlockState


defBlockState :: CodeAddr -> AbsBlockState
defBlockState addr =
  top & absX86State . curIP .~ abstractSingleton (toInteger addr)
      & absX86State . x87TopReg .~ abstractSingleton 7

emptyAbsState :: CodeAddr -> AbsState
emptyAbsState start = Map.singleton start (defBlockState start)

addAddr :: CodeAddr -> AbsState -> AbsState
addAddr a s = Map.insertWith (\_ old -> old) a (defBlockState a) s

addAddrs :: [CodeAddr] -> AbsState -> AbsState
addAddrs l a = foldl' (flip addAddr) a l

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


setAbsIP :: CodeAddr -> AbsBlockState -> AbsBlockState
setAbsIP a = absX86State . curIP .~ abstractSingleton (toInteger a)

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
   Left _e -> do
     failedAddrs %= Set.insert addr
     return Nothing
   Right (bs, next_ip) -> do
     cfg       %= insertBlocksForCode addr next_ip bs
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
-- Transfer functions

--type Path = [(Value BoolType, Bool)]

-- blockPaths :: CFG -> Block -> [(X86State, Path)]
-- blockPaths c root = traverseBlockAndChildren c root go merge
--   where
--     merge cond l _ r = map (_2 %~ (:) (cond, True)) l
--                        ++ map (_2 %~ (:) (cond, False)) r
--     go b = case blockTerm b of
--             FetchAndExecute s -> [(s, [])]
--             _                 -> []

transferStmt :: Memory Word64
             -> Stmt
             -> StateT AbsRegs (State InterpState) ()
transferStmt mem stmt =
  case stmt of
    AssignStmt a -> do
      modify $ addAssignment a
    Write (MemLoc _ (BVTypeRepr n)) v | widthVal n == 64 -> do
      vs <- gets (`transferValue` v)
      case concretize vs of
        Nothing  -> return ()
        Just vs1 -> do
          let vs2 = map fromInteger (Set.toList vs1)
          lift $ do
            forM_ vs2 $ \val -> do
              alreadyInMem <- uses codePointersInMem $ Set.member val
              when (isCodePointer mem val && not alreadyInMem) $ do
                codePointersInMem %= Set.insert val
                absState %= Map.delete val
                frontier %= Set.insert val

    _ -> return ()

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

    let ab = ab0 & absX86State . curIP .~ abstractSingleton (toInteger addr)
    case Map.lookup addr (s^.absState) of
      -- We have seen this block before, so need to join and see if
      -- the results is changed.
      Just ab_old ->
        case joinD ab_old ab of
          Nothing  -> return ()
          Just new -> upd new
      -- We haven't seen this block before
      Nothing  -> upd ab

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
        mem <- gets memory
        regs' <- flip execStateT regs $ do
          mapM_ (transferStmt mem) (blockStmts b)

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

                  next -> do
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

cfgFromAddress :: Memory Word64
                  -- ^ Memory to use when decoding instructions.
               -> CodeAddr
                  -- ^ Location to start disassembler form.
               -> CFG
cfgFromAddress mem start = s'^.cfg
  where
    s0 = emptyInterpState mem start
    s' = go s0

    go :: InterpState
       -> InterpState
    go st =
      case Set.minView (st^.frontier) of
        Nothing -> st
        Just (addr, next_roots) ->
          let st_pre = st & frontier .~ next_roots
              st_post = flip execState st_pre $ do
                transfer addr
           in go st_post