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
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Werror #-}
module Reopt.Semantics.DeadRegisterElimination
  ( eliminateDeadRegisters
  ) where

import           Control.Lens
import           Control.Monad.State (State, evalState, gets, modify)
import           Data.Foldable (foldrM)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Parameterized.Some
import           Data.Parameterized.TraversableF
import           Data.Set (Set)
import qualified Data.Set as Set

import           Data.Macaw.CFG

blockLiveRegisters :: (StmtHasRefs (ArchStmt arch), FnHasRefs (ArchFn arch))
                   => Block arch ids
                   -> State (Set (Some (AssignId ids))) (Map (BlockLabel (ArchAddrWidth arch)) (Block arch ids))
blockLiveRegisters b = do
    addIDs terminalIds
    stmts' <- foldrM noteAndFilter [] (blockStmts b)
    return $ M.singleton (blockLabel b) (b { blockStmts = stmts' })
  where
    terminalIds = case blockTerm b of
                   Branch v _ _      -> refsInValue v
                   FetchAndExecute s -> foldMapF refsInValue s
                   Syscall s     -> foldMapF refsInValue s
                   TranslateError s _ -> foldMapF refsInValue s
    addIDs ids = modify (Set.union ids)
    noteAndFilter stmt@(AssignStmt (Assignment v rhs)) ss = do
      v_in <- gets (Set.member (Some v))
      if v_in then do
        addIDs (refsInAssignRhs rhs)
        return (stmt : ss)
       else
        return ss
    noteAndFilter stmt@(WriteMem loc rhs) ss = do
      addIDs (refsInValue loc)
      addIDs (refsInValue rhs)
      return (stmt : ss)
    noteAndFilter stmt@(PlaceHolderStmt vals _) ss = do
      mapM_ (addIDs . viewSome refsInValue) vals
      return (stmt : ss)
    noteAndFilter stmt@Comment{} ss = return (stmt : ss)
    noteAndFilter stmt@(ExecArchStmt stmts') ss = do
      addIDs (refsInStmt stmts')
      return (stmt : ss)

-- | Find the set of referenced registers, via a post-order traversal of the
-- CFG.
liveRegisters :: (StmtHasRefs (ArchStmt arch), FnHasRefs (ArchFn arch))
              => CFG arch ids
              -> ArchLabel arch
              -> Map (ArchLabel arch) (Block arch ids)
liveRegisters cfg root = evalState (traverseBlocks cfg root blockLiveRegisters merge) Set.empty
  where merge l v r = M.union <$> (M.union <$> l <*> r) <*> v

eliminateDeadRegisters :: (StmtHasRefs (ArchStmt arch), FnHasRefs (ArchFn arch))
                       => CFG arch ids
                       -> CFG arch ids
eliminateDeadRegisters cfg = cfg & cfgBlocks .~ newCFG
  where
    newCFG = M.unions [ liveRegisters cfg l | l <- M.keys (cfg ^. cfgBlocks)
                                            , isRootBlockLabel l ]
