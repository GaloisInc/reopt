------------------------------------------------------------------------
-- |
-- Module           : Reopt.Symbolic.Semantics
-- Description      : Instance for Reopt.Semantics.Monad.Semantics that
--                    produces crucible app datatypes.
-- Copyright        : (c) Galois, Inc 2015
-- Maintainer       : Michael Hueschen <mhueschen@galois.com>
-- Stability        : provisional
--
------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-} -- MaybeF
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Reopt.Symbolic.Semantics
       ( gimmeCFG
       , gen1
       , gen2
       , module Lang.Crucible.FunctionHandle
       , argTypes
       , retType
       , translateBlock
       ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>), pure, Applicative)
#endif
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.ST
import           Control.Monad.Writer
  (censor, execWriterT, listen, tell, MonadWriter, WriterT)
import           Data.Binary.IEEE754
import           Data.Bits
import           Data.BitVector (BV)
import qualified Data.BitVector as BV
import qualified Data.Foldable as Fold
import           Data.Functor
import           Data.IORef
import           Data.Maybe
import           Data.Monoid (mempty)
import           Data.Parameterized.Classes (OrderingF(..), OrdF, compareF, fromOrdering)
import           Data.Parameterized.Context
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Vector as V

import           Data.Word
import           GHC.TypeLits

import qualified Flexdis86 as Flexdis
import qualified Lang.Crucible.Core as C
import           Lang.Crucible.FunctionHandle
import qualified Lang.Crucible.Generator as G
import           Lang.Crucible.ProgramLoc
import           Lang.Crucible.Simulator.Evaluation
import           Lang.Crucible.Simulator.RegMap
import           Lang.Crucible.Solver.Interface
import           Lang.Crucible.Solver.SimpleBuilder
import           Lang.Crucible.Solver.SimpleBackend
import           Lang.Crucible.SSAConversion (toSSA)
import           Reopt.BasicBlock.FunctionCFG
import           Reopt.Object.Memory
import           Reopt.Semantics.FlexdisMatcher (execInstruction)
import           Reopt.Semantics.Monad
  ( Type(..)
  , TypeRepr(..)
  , BoolType
  , bvLit
  )
import qualified Reopt.Semantics.Monad as S
import qualified Reopt.CFG.Representation as R
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types ( FloatInfo(..), FloatInfoRepr, FloatType
                                     , TypeBits, floatInfoBits, n4, n32, n64
                                     , type_width
                                     )
import           Reopt.Reified.Semantics

------------------------------------------------------------------------
-- Hand-written Crucible CFG (for learning)
------------------------------------------------------------------------

type ArgTys = EmptyCtx ::> C.BVType 32
type RetTy = C.BVType 32

argTypes = C.ctxRepr :: C.CtxRepr ArgTys
retType  = C.BVRepr n32 :: C.TypeRepr RetTy

gen1 :: Assignment (G.Atom s) ArgTys -> G.Generator s t RetTy a
gen1 assn = do
  reg <- G.newReg $ G.AtomExpr $ assn^._1 -- assn ! base
  val <- G.readReg reg
  let foo = G.App (C.BVLit n32 11)
  G.assignReg reg (G.App $ C.BVMul n32 val foo)
  G.returnFromFunction =<< G.readReg reg

gen2 :: Assignment (G.Atom s) ArgTys -> G.Generator s t RetTy a
gen2 assn = do
  reg <- G.newReg $ G.AtomExpr $ assn^._1 -- assn ! base
  val <- G.readReg reg
  let foo1 = G.App (C.BVLit n32 6)
      foo2 = G.App (C.BVLit n32 5)
      bar = G.App (C.BVMul n32 val foo1)
      baz = G.App (C.BVMul n32 val foo2)
  G.assignReg reg (G.App $ C.BVAdd n32 bar baz)
  G.returnFromFunction =<< G.readReg reg


gimmeCFG :: HandleAllocator s
         -> (forall s. Assignment (G.Atom s) ArgTys -> G.Generator s [] RetTy (G.Expr s RetTy))
         -- -> ST s (G.CFG s EmptyCtx RetTy, [C.AnyCFG])
         -> ST s C.AnyCFG
gimmeCFG halloc gen = do
  fnH <- mkHandle' halloc "testFun" argTypes retType
  let fnDef :: G.FunctionDef [] ArgTys RetTy
      fnDef inputs = (s, f)
        where s = []
              f = gen inputs
  (g,[]) <- G.defineFunction halloc InternalPos fnH fnDef
  case toSSA g of
    C.SomeCFG g_ssa -> return (C.AnyCFG g_ssa)



------------------------------------------------------------------------
-- Stmt -> RegCFG translation
------------------------------------------------------------------------
type RegBank = C.WordMapType 4 (C.BVType 64)

type MachineCtx = EmptyCtx ::> RegBank
type MachineState = C.StructType MachineCtx

argState = C.ctxRepr :: C.CtxRepr MachineCtx
retState = C.StructRepr argState :: C.TypeRepr MachineState

translateBlock :: HandleAllocator s
               -> Block
               -> ST s C.AnyCFG
translateBlock halloc block = do
  fnH <- mkHandle' halloc "testBlock" argState retState
  (g,[]) <- G.defineFunction halloc InternalPos fnH $ mkDefn block
  case toSSA g of
    C.SomeCFG g_ssa -> return (C.AnyCFG g_ssa)

mkDefn :: Block -> G.FunctionDef [] MachineCtx MachineState
mkDefn (Block stmts term) assn = (s, f)
  where s = []
        f = do regBank <- G.newReg $ G.AtomExpr $ assn^._1
               mapM_ (generateStmt regBank) stmts
               generateTerm regBank term
               regBankValue <- G.readReg regBank
               let retStruct = G.App $ C.MkStruct argState (empty `extend` regBankValue)
               G.returnFromFunction retStruct


generateStmt :: (G.Reg s RegBank)
             -> Stmt
             -> G.Generator s t MachineState ()
generateStmt rb (m := e) = case m of
  S.Register (N.GPReg i) ->
    do rbVal <- G.readReg rb
       let v = undefined
           idx = G.App $ C.BVLit n4 (fromIntegral i)
           rbVal' = G.App $ C.InsertWordMap n4 (C.BVRepr n64) idx v rbVal
       G.assignReg rb rbVal'

  _ -> error $ unwords [ "generateStmt: unimplemented assignment target:"
                       , show (ppStmt (m := e))
                       ]

generateStmt _ stmt = error $ unwords [ "generateStmt: unimplemented instruction type:"
                                      , show (ppStmt stmt)
                                      ]


generateTerm :: (G.Reg s RegBank)
             -> Term
             -> G.Generator s t MachineState a
generateTerm = undefined


