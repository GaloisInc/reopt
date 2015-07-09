------------------------------------------------------------------------
-- |
-- Module           : Reopt.Semantics.DeadRegisterElimination
-- Description      : A CFG pass to remove registers which are not used
-- Copyright        : (c) Galois, Inc 2015
-- Maintainer       : Joe Hendrix <jhendrix@galois.com>
-- Stability        : provisional
--
-- Given a code block like
--    r0 := initial
--    r1 := f(r0)
--    r2 := g(initial)
--    fetch_and_execute { rax := r1 }
--
-- this code will remove the (unused) r2
------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Reopt.Semantics.DeadRegisterElimination (eliminateDeadRegisters) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Lens
import           Control.Monad.State (State, evalState, gets, modify)
import           Data.Foldable (foldrM)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

import           Data.Parameterized.Some
import           Reopt.CFG.Representation
import           Reopt.Machine.Types

eliminateDeadRegisters :: CFG -> CFG
eliminateDeadRegisters cfg = (cfgBlocks .~ newCFG) cfg
  where
    newCFG = M.unions [ liveRegisters cfg l | l <- M.keys (cfg ^. cfgBlocks)
                                            , isRootBlockLabel l ]

-- | Find the set of referenced registers, via a post-order traversal of the
-- CFG.
liveRegisters :: CFG -> BlockLabel -> Map BlockLabel Block
liveRegisters cfg root = evalState (traverseBlocks cfg root blockLiveRegisters merge) S.empty
  where
    merge l v r = M.union <$> (M.union <$> l <*> r) <*> v

blockLiveRegisters :: Block -> State (Set AssignId) (Map BlockLabel Block)
blockLiveRegisters b = do addIDs terminalIds
                          stmts' <- foldrM noteAndFilter [] (blockStmts b)
                          return $ M.singleton (blockLabel b) (b { blockStmts = stmts' })
  where
    terminalIds = case blockTerm b of
                   Branch v _ _      -> refsInValue v
                   FetchAndExecute s -> foldX86StateValue refsInValue s
                   Primitive _ s     -> foldX86StateValue refsInValue s
    addIDs ids = modify (S.union ids)
    noteAndFilter stmt@(AssignStmt (Assignment v rhs)) ss
      = do v_in <- gets (S.member v)
           if v_in then
             do addIDs (refsInAssignRhs rhs)
                return (stmt : ss)
             else return ss
    noteAndFilter stmt@(Write loc rhs) ss
      = do addIDs (refsInLoc loc)
           addIDs (refsInValue rhs)
           return (stmt : ss)
    noteAndFilter stmt@(MemCopy _ cnt src dest df) ss = do
      addIDs (refsInValue cnt)
      addIDs (refsInValue src)
      addIDs (refsInValue dest)
      addIDs (refsInValue df)      
      return (stmt : ss)
    noteAndFilter stmt@(MemSet cnt val dest) ss = do
      addIDs (refsInValue cnt)
      addIDs (refsInValue val)
      addIDs (refsInValue dest)
      return (stmt : ss)
    noteAndFilter stmt@(PlaceHolderStmt vals _) ss
      = do mapM_ (addIDs . viewSome refsInValue) vals
           return (stmt : ss)
    noteAndFilter stmt@Comment{} ss = return (stmt : ss)

refsInAssignRhs :: AssignRhs tp -> Set AssignId
refsInAssignRhs rhs = case rhs of
                       EvalApp v      -> refsInApp v
                       SetUndefined _ -> S.empty
                       Read loc       -> refsInLoc loc
                       MemCmp _ cnt src dest dir ->
                         S.unions [ refsInValue cnt
                                  , refsInValue src
                                  , refsInValue dest
                                  , refsInValue dir ]

refsInApp :: App Value tp -> Set AssignId
refsInApp app = foldApp (\v s -> refsInValue v `S.union` s) S.empty app

refsInLoc :: StmtLoc (Value (BVType 64)) tp -> Set AssignId
refsInLoc (MemLoc v _) = refsInValue v
refsInLoc _            = S.empty

refsInValue :: Value tp -> Set AssignId
refsInValue (AssignedValue (Assignment v _)) = S.singleton v
refsInValue _                                = S.empty
