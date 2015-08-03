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
import           Data.Parameterized.Context as Ctx
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
import qualified Lang.Crucible.Types as Cr
import           Reopt.BasicBlock.FunctionCFG
import           Reopt.Object.Memory
import qualified Reopt.CFG.Representation as R
import           Reopt.Semantics.FlexdisMatcher (execInstruction)
import           Reopt.Semantics.Monad
  ( Type(..)
  , TypeRepr(..)
  , BoolType
  , bvLit
  )
import qualified Reopt.Semantics.Monad as S
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types ( FloatInfo(..), FloatInfoRepr, FloatType
                                     , TypeBits, floatInfoBits, n1, n4, n5
                                     , n32, n64, type_width
                                     )
import           Reopt.Reified.Semantics


------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------
newtype Env s = Env { unEnv :: MapF Var (G.Atom s) }

insertEnv :: Var tp -> G.Atom s tp -> Env s -> Env s
insertEnv v a env = Env $ MapF.insert v a (unEnv env)

data Var tp = Var !(Cr.TypeRepr tp) !Name
type Name = String

instance TestEquality Var where
  (Var tp1 n1) `testEquality` (Var tp2 n2) = do
    Refl <- testEquality tp1 tp2
    return Refl

instance MapF.OrdF Var where
  (Var tp1 n1) `compareF` (Var tp2 n2) =
    case (tp1 `compareF` tp2, n1 `compare` n2) of
      (LTF,_) -> LTF
      (GTF,_) -> GTF
      (EQF,o) -> fromOrdering o

instance Ord (Var tp) where
  (Var _ n1) `compare` (Var _ n2) =
    n1 `compare` n2

instance Eq (Var tp) where
  (Var _ n1) == (Var _ n2) =
    n1 == n2


------------------------------------------------------------------------
-- Stmt -> RegCFG translation
------------------------------------------------------------------------
type GPRs = C.WordMapType 4 (C.BVType 64)
type Flags = C.WordMapType 5 (C.BVType 1)
type PC = C.BVType 64

type MachineCtx = EmptyCtx ::> GPRs ::> Flags ::> PC
type MachineState = C.StructType MachineCtx
type MachineStateCtx = EmptyCtx ::> MachineState

machineCtx = C.ctxRepr :: C.CtxRepr MachineCtx
machineState = C.StructRepr machineCtx :: C.TypeRepr MachineState
machineStateCtx = C.ctxRepr :: C.CtxRepr MachineStateCtx


-- | Instruction pointer / program counter
curIP :: Simple Lens (G.Expr s MachineState) (G.Expr s PC)
curIP = lens getter setter
  where
    getter st     = G.App $ C.GetStruct st idx (C.BVRepr n64)
    setter st val = G.App $ C.SetStruct machineCtx st idx val
    idx :: Ctx.Index MachineCtx PC
    idx = nextIndex (incSize $ incSize zeroSize)

-- | The 16 general-purpose registers.
reg64Regs :: Simple Lens (G.Expr s MachineState) (G.Expr s GPRs)
reg64Regs = lens getter setter
  where
    getter st     = G.App $ C.GetStruct st idx (C.WordMapRepr n4 (C.BVRepr n64))
    setter st val = G.App $ C.SetStruct machineCtx st idx val
    idx :: Ctx.Index MachineCtx GPRs
    idx = skip . skip $ nextIndex zeroSize

-- | 32 individual bits in the flags register.
flagRegs :: Simple Lens (G.Expr s MachineState) (G.Expr s Flags)
flagRegs = lens getter setter
  where
    getter st     = G.App $ C.GetStruct st idx (C.WordMapRepr n5 (C.BVRepr n1))
    setter st val = G.App $ C.SetStruct machineCtx st idx val
    idx :: Ctx.Index MachineCtx Flags
    idx = skip . nextIndex $ incSize zeroSize

-- | Lens for WordMaps
wordMap :: forall s w n
         . (1 <= w, KnownNat w, KnownNat n)
        => Int
        -> Simple Lens (G.Expr s (C.WordMapType w (C.BVType n))) (G.Expr s (C.BVType n))
wordMap i = lens getter setter
  where
    getter st     = G.App $ C.LookupWordMap (C.BVRepr nr_n) idx st
    setter st val = G.App $ C.InsertWordMap nr_w (C.BVRepr nr_n) idx val st
    idx = G.App $ C.BVLit nr_w (fromIntegral i)
    nr_w = knownNat :: NatRepr w
    nr_n = knownNat :: NatRepr n

type RegisterType (cl :: N.RegisterClass) = C.BVType (N.RegisterClassBits cl)

register :: forall f cl s
          . N.RegisterName cl
         -> Simple Lens (G.Expr s MachineState) (G.Expr s (RegisterType cl))
register reg =
   case reg of
      N.IPReg           -> curIP
      N.GPReg n         -> reg64Regs . wordMap n
      N.FlagReg n       -> flagRegs . wordMap n
      _                 -> error $ "unimplemented RegisterName" ++ show reg


translateBlock :: HandleAllocator s
               -> Block
               -> ST s C.AnyCFG
translateBlock halloc block = do
  fnH <- mkHandle' halloc "testBlock" machineStateCtx machineState
  (g,[]) <- G.defineFunction halloc InternalPos fnH $ mkDefn block
  case toSSA g of
    C.SomeCFG g_ssa -> return (C.AnyCFG g_ssa)

mkDefn :: Block -> G.FunctionDef Env MachineStateCtx MachineState
mkDefn (Block stmts term) assn = (s, f)
  where s = Env $ MapF.empty
        f = do
          ms <- G.newReg $ G.AtomExpr $ assn^._1
          mapM_ (generateStmt ms) stmts
          generateTerm ms term
          G.returnFromFunction =<< G.readReg ms

generateStmt :: forall s.
                (G.Reg s MachineState)
             -> Stmt
             -> G.Generator s Env MachineState ()
generateStmt ms (l := e) = case l of
  S.Register reg -> do
    e' <- fmap (runReader (translateExpr' e)) get
    G.modifyReg ms (register reg .~ e')

  S.MemoryAddr _ _ -> error "assign meem addr unimplemented"

  _ -> return ()
--
generateStmt ms (Get v l) = case l of
  S.Register reg -> do
    a <- G.mkAtom . (^.(register reg)) =<< G.readReg ms
    modify (insertEnv (translateVar v) a)

  S.MemoryAddr _ _ -> error "get mem addr unimplemented"

  _ -> return ()
--
generateStmt _ms (MakeUndefined v tr) = case tr of
  BVTypeRepr nr -> case testLeq S.n1 nr of
    Nothing -> error "boo!"
    Just LeqProof -> do
      a <- G.mkAtom . G.App $ C.BVUndef nr
      modify (insertEnv (translateVar v) a)
--
generateStmt _ stmt = error $ unwords [ "generateStmt: unimplemented Stmt case:"
                                      , show (ppStmt stmt)
                                      ]


generateTerm :: (G.Reg s MachineState)
             -> Term
             -> G.Generator s t MachineState a
generateTerm = error "generateTerm is unimplemented"



type family F (tp :: Type) :: C.CrucibleType where
  F (BVType n) = Cr.BVType n

-- This is necessary because traverseApp requires its argument function to return
-- something which is a functor over tp. So the newtype wraps the (F tp) type function
-- application inside of it.
newtype GExpr s tp = GExpr { unGExpr :: G.Expr s (F tp) }

translateExpr' :: (MonadReader (Env s) m, Applicative m) => Expr tp -> m (G.Expr s (F tp))
translateExpr' = fmap unGExpr . translateExpr

translateExpr :: (MonadReader (Env s) m, Applicative m) => Expr tp -> m (GExpr s tp) -- m (G.Expr s (F tp))
-- FIXME: I'm specializing to BVType because I know that's the only (currently)
-- possible `tp`. But that could change.
translateExpr (LitExpr nr i) = return . GExpr $ G.App (C.BVLit nr i)
--
translateExpr (VarExpr var) = do
  let var'@(Var _ name) = translateVar var
  mA <- reader (MapF.lookup var' . unEnv)
  case mA of
    Just atom -> return . GExpr $ G.AtomExpr atom
    Nothing   -> error $ "translateExpr bug: unbound variable " ++ name ++ " in expr"
--
-- TruncExpr and SExtExpr differ from App's Trunc & SExt only in the types.
-- These allow for trivial truncations & extensions, where App does not.
-- Crucible does not support trivial truncations, so we just drop the trivial
-- truncation by returning the contained Expr.
translateExpr (TruncExpr nr e) = do
  ge@(GExpr e') <- translateExpr e
  let width = S.bv_width e
  case testStrictLeq nr width of
    Right Refl -> return ge
    Left prf -> return . GExpr . G.App $ withLeqProof prf (C.BVTrunc nr width e')
--
translateExpr (SExtExpr nr e) = do
  ge@(GExpr e') <- translateExpr e
  let width = S.bv_width e
  case testStrictLeq width nr of
    Right Refl -> return ge
    Left prf -> return . GExpr . G.App $ withLeqProof prf (C.BVSext nr width e')
--
translateExpr (AppExpr a) = do
  a' <- R.traverseApp translateExpr a
  return $ case a' of
    R.BVBit (GExpr e1) (GExpr e2)
      | Cr.BVRepr nr1 <- G.exprType e1
      , Cr.BVRepr nr2 <- G.exprType e2 ->
        GExpr $ bvBit nr1 nr2 e1 e2

    R.BVMul nr (GExpr e1) (GExpr e2) ->
      GExpr . G.App $ asPosNat nr (C.BVMul nr e1 e2)

    R.BVEq (GExpr e1) (GExpr e2)
      | Cr.BVRepr nr <- G.exprType e1 ->
        GExpr . G.App . C.BoolToBV n1 . G.App $ asPosNat nr (C.BVEq nr e1 e2)

    R.BVComplement nr (GExpr e) ->
      GExpr . G.App $ asPosNat nr (C.BVNot nr e)

    _ -> error $ unwords [ "translateExpr App case unimplemented:"
                         , show (ppExpr (AppExpr a))
                         ]
--
translateExpr expr = error $ unwords [ "translateExpr: unimplemented Expr case:"
                                     , show (ppExpr expr)
                                     ]


asPosNat :: forall n s a
           . NatRepr n
          -> ((1 <= n) => a)
          -> a
asPosNat nr x = case isPosNat nr of
  Nothing -> error "zero-width NatRepr"
  Just prf -> withLeqProof prf x


bvBit :: forall s n log_n
       . NatRepr n
      -> NatRepr log_n
      -> G.Expr s (C.BVType n)
      -> G.Expr s (C.BVType log_n)
      -> G.Expr s (C.BVType 1)
bvBit nr1 nr2 val idx =
  case testNatCases nr1 n1 of
    NatCaseLT prf -> error "zero-width NatRepr"
    -- NatCaseEQ -> val
    NatCaseGT prf -> withLeqProof prf (G.App $ C.BVTrunc n1 nr1 shiftedVal)
  where
    shiftedVal :: G.Expr s (C.BVType n)
    shiftedVal = G.App $ asPosNat nr1 (C.BVAshr nr1 val idx')
    idx' :: G.Expr s (C.BVType n)
    idx' = case testNatCases nr2 nr1 of
      NatCaseLT prf ->
        withLeqProof prf (asPosNat nr2 (G.App $ C.BVZext nr1 nr2 idx))
      NatCaseEQ -> idx
      NatCaseGT prf ->
        withLeqProof prf (asPosNat nr1 (G.App $ C.BVTrunc nr1 nr2 idx))


translateVar :: Variable tp -> Var (F tp)
translateVar (Variable tr name) =
  case tr of
    BVTypeRepr nr -> Var (Cr.BVRepr nr) name


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
