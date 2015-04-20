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
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Reopt.Semantics.CFGDiscovery
       ( cfgFromAddress
       ) where

import           Control.Lens
import           Control.Monad.State.Strict
import qualified Data.Foldable as Fold
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid (mappend, mempty)
import           Data.Parameterized.NatRepr
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           Debug.Trace
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Reopt.AbsState
import           Reopt.Memory
import           Reopt.Semantics.Implementation
import           Reopt.Semantics.Representation
import           Reopt.Semantics.Types
import qualified Reopt.Semantics.WorkList as WL

------------------------------------------------------------------------
-- Interpreter state

-- | Maps each code address to a set of abstract states
type AbsState = Map CodeAddr AbsBlockState

emptyAbsState :: AbsState
emptyAbsState = Map.empty

-- | The state of the interpreter
data InterpState
   = InterpState { _cfg      :: !CFG
                 , _failedAddrs  :: !(Set CodeAddr)
                 , _guessedAddrs :: !(Set CodeAddr)
                 , _genState :: !GlobalGenState
                   -- | The initial memory when disassembly started.
                 , memory   :: !(Memory Word64)
                 , _absState :: !AbsState
                 }

emptyInterpState :: Memory Word64 -> InterpState
emptyInterpState mem = InterpState
      { _cfg = emptyCFG
      , _failedAddrs = Set.empty
      , _guessedAddrs = Set.empty
      , _genState    = emptyGlobalGenState
      , memory      = mem
      , _absState    = emptyAbsState
      }

cfg :: Simple Lens InterpState CFG
cfg = lens _cfg (\s v -> s { _cfg = v })

genState :: Simple Lens InterpState GlobalGenState
genState = lens _genState (\s v -> s { _genState = v })

failedAddrs :: Simple Lens InterpState (Set CodeAddr)
failedAddrs = lens _failedAddrs (\s v -> s { _failedAddrs = v })

guessedAddrs :: Simple Lens InterpState (Set CodeAddr)
guessedAddrs = lens _guessedAddrs (\s v -> s { _guessedAddrs = v })

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

lookupAbsState :: MonadState InterpState m => CodeAddr -> m (Maybe AbsBlockState)
lookupAbsState addr = uses absState (Map.lookup addr)

-- | This is the worker for getBlock, in the case that the cfg doesn't
-- contain the address of interest.
reallyGetBlock :: MonadState InterpState m => ExploreLoc -> m (Maybe Block)
reallyGetBlock loc = do
  mem <- gets memory
  r <- subMonad genState $ do
    liftEither $ disassembleBlock mem loc
  case r of
   Left _e -> trace ("Failed for address " ++ show (pretty loc)) $ do
     failedAddrs %= Set.insert (loc_ip loc)
     return Nothing
   Right (bs, next_ip) -> do
     cfg       %= insertBlocksForCode (loc_ip loc) next_ip bs
     lookupBlock (loc_ip loc)

-- | Returns a block at the given location, if at all possible.  This
-- will disassemble the binary if the block hasn't been seen before.
-- In particular, this ensures that a block and all it's children are
-- present in the cfg (assuming successful disassembly)
getBlock :: MonadState InterpState m => ExploreLoc -> m (Maybe Block)
getBlock loc = do
  let addr = (loc_ip loc)
  m_b <- lookupBlock addr
  failed <- uses failedAddrs (Set.member addr)
  case m_b of
    Nothing | not failed -> reallyGetBlock loc
    _                    -> return m_b

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

transferStmt :: Stmt
             -> State AbsCache (Set CodeAddr)
transferStmt stmt =
  case stmt of
    AssignStmt a -> do
      modify (addAssignment a)
      return Set.empty
    Write (MemLoc _ (BVTypeRepr n)) v | widthVal n == 64 -> do
      vs <- gets (`transferValue` v)
      return $ case concretize vs of
                 Nothing  -> Set.empty
                 Just vs' -> Set.map fromInteger vs'
    _ -> return Set.empty

abstractState :: AbsBlockState
              -> X86State Value
              -> [(ExploreLoc, AbsBlockState)]
abstractState abst s =
  case concretize (abst^.absX86State^.curIP) of
    Nothing  -> trace "Hit top" [] -- we hit top, so give up
    Just ips ->
      [ (loc, abst & absX86State . curIP .~ abstractSingleton x)
      | x <- Set.toList ips
      , let t = case s ^. x87TopReg of
                  BVValue _ v -> v
                  _ -> error "x87top is not concrete"
      , let loc = ExploreLoc { loc_ip = fromInteger x
                             , loc_x87_top = fromInteger t
                             }
      ]

transferBlock :: CFG
              -> Block
              -> AbsBlockState
              -> (Set CodeAddr, [(ExploreLoc, AbsBlockState)])
transferBlock g root ab =
    traverseBlockAndChildren g root leaf merge (initAbsCache ab) & _1 %~ Set.unions
  where
    merge b l r m = let (guesses, m_b) = go b m
                    in l m_b `mappend` r m_b `mappend` (guesses, [])
    leaf b m = case blockTerm b of
                 FetchAndExecute s ->
                   let (guesses, c) = go b m
                       abst = finalAbsBlockState c s
                    in (guesses, abstractState abst s)
                 _ -> mempty -- can't happen

    go b = runState (mapM transferStmt (blockStmts b))


-- | Joins in the new abstract state and returns the locations for
-- which the new state is changed.
mergeBlocks :: [(ExploreLoc, AbsBlockState)] -> State AbsState [ExploreLoc]
mergeBlocks bs = state (\s -> Fold.foldl' mergeBlock ([], s) bs)
  where
    mergeBlock r@(locs, s) (loc, ab) =
      let upd new = ( loc : locs, Map.insert (loc_ip loc) new s )
      in
      case Map.lookup (loc_ip loc) s of
       -- We have seen this block before, so need to join and see if
       -- the results is changed.
       Just ab' -> case joinD ab' ab of
                    Nothing  -> r
                    Just new -> trace ("Merging state for " ++ show (pretty loc)
                                      ++ "\n" ++ show (pretty new))
                                $ upd new

       -- We haven't seen this block before
       Nothing  -> trace ("Adding state for " ++ show (pretty loc)
                          ++ "\n" ++ show (pretty ab)) $ upd ab

transfer :: MonadState InterpState m => ExploreLoc -> m [ExploreLoc]
transfer loc = do m_b  <- getBlock loc
                  m_ab <- lookupAbsState (loc_ip loc)
                  case (m_b, m_ab) of
                   (Just b, Just s) ->
                     do c <- use cfg
                        let (guesses, vs) = transferBlock c b s
                        guessedAddrs %= Set.union guesses
                        subMonad absState (mergeBlocks vs)
                   _                -> return []

------------------------------------------------------------------------
-- Main loop

cfgFromAddress :: Memory Word64
                  -- ^ Memory to use when decoding instructions.
               -> CodeAddr
                  -- ^ Location to start disassembler form.
               -> CFG
cfgFromAddress mem start = s'^.cfg
  where
    s' = go (emptyInterpState mem) (Set.singleton start)
    go st roots
      | Set.null roots = st
      | otherwise    =
          let wl   = WL.fromSet (Set.map rootLoc roots)
              st'  = st & absState %~ addTops roots
              st'' = execState (WL.iterate transfer wl) st'
          in go (st'' & guessedAddrs .~ Set.empty) (st'' ^. guessedAddrs)

    addTops roots m =
      Fold.foldl' (\m' k -> Map.insertWith (\_ old -> old) k top m') m
      (Set.filter isCodePointer roots)

    isCodePointer :: CodeAddr -> Bool
    isCodePointer val = addrHasPermissions (fromIntegral val) pf_x mem