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
       , machineCtx
       , machineState
       , machineStateCtx
       , asPosNat
       ) where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Control.Monad.Writer
  (censor, execWriterT, listen, tell, MonadWriter, WriterT)
import           Data.Binary.IEEE754
import           Data.BitVector (BV)
import qualified Data.BitVector as BV
import           Data.Bits
import qualified Data.Foldable as Fold
import           Data.Functor
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid (mempty)
import           Data.Parameterized.Classes (OrderingF(..), OrdF, compareF, fromOrdering)
import           Data.Parameterized.Context as Ctx
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some
import Data.Proxy
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Vector as V
import           Text.PrettyPrint.ANSI.Leijen (pretty)

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
                                     , TypeBits, floatInfoBits, n1, n4, n5, n8
                                     , n32, n64, type_width
                                     )
import           Reopt.Reified.Semantics

import           Debug.Trace


------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------
data Env s = Env { vars :: MapF Var (G.Atom s), blocks :: Map Word64 (G.Label s), trackedRip :: Word64}

insertVar :: Var tp -> G.Atom s tp -> Env s -> Env s
insertVar v a env = env {vars = MapF.insert v a (vars env)}
-- FIXME: lenses?

getLabel :: Word64 -> G.End s Env init MachineState (G.Label s)
getLabel addr = do
  env <- get
  let bm = blocks env
  case Map.lookup addr bm of
    Just l -> return l
    Nothing -> do
      l <- G.newLabel
      put $ env {blocks = Map.insert addr l bm}
      return l

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
type Heap = C.ArrayType (C.BVType 64) (C.BVType 8)

type MachineCtx = EmptyCtx ::> Heap ::> GPRs ::> Flags ::> PC
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
    idx = nextIndex (incSize . incSize . incSize $ zeroSize)

-- | 32 individual bits in the flags register.
flagRegs :: Simple Lens (G.Expr s MachineState) (G.Expr s Flags)
flagRegs = lens getter setter
  where
    getter st     = G.App $ C.GetStruct st idx (C.WordMapRepr n5 (C.BVRepr n1))
    setter st val = G.App $ C.SetStruct machineCtx st idx val
    idx :: Ctx.Index MachineCtx Flags
    idx = skip $ nextIndex $ incSize . incSize $ zeroSize

-- | The 16 general-purpose registers.
reg64Regs :: Simple Lens (G.Expr s MachineState) (G.Expr s GPRs)
reg64Regs = lens getter setter
  where
    getter st     = G.App $ C.GetStruct st idx (C.WordMapRepr n4 (C.BVRepr n64))
    setter st val = G.App $ C.SetStruct machineCtx st idx val
    idx :: Ctx.Index MachineCtx GPRs
    idx = skip . skip $ nextIndex $ incSize zeroSize


heap :: Simple Lens (G.Expr s MachineState) (G.Expr s Heap)
heap = lens getter setter
  where
    getter st     = G.App $ C.GetStruct st idx (Cr.typeRepr :: Cr.TypeRepr Heap) --  (C.ArrayRepr (C.BVRepr n64) (C.BVRepr n8))
    setter st val = G.App $ C.SetStruct machineCtx st idx val
    idx :: Ctx.Index MachineCtx Heap
    idx = skip . skip . skip $ nextIndex $ zeroSize

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
  where s = Env {vars = MapF.empty, blocks = Map.empty, trackedRip = 0}
        f = do
          ms <- G.newReg $ G.AtomExpr $ assn^._1
          trace (show $ ppStmts stmts) $ mapM_ (generateStmt ms) stmts
          generateTerm ms term
          G.returnFromFunction =<< G.readReg ms

generateStmt :: forall s.
                (G.Reg s MachineState)
             -> Stmt
             -> G.Generator s Env MachineState ()

generateStmt ms (S.Register N.IPReg := LitExpr nr i) = do
  G.modifyReg ms (curIP .~ G.App (C.BVLit nr i))
  modify $ \env -> env {trackedRip = fromIntegral i}
--
generateStmt ms (l := e) = case l of
  S.Register reg -> do
    e' <- fmap (runReader (translateExpr' e)) get
    G.modifyReg ms (register reg .~ e')

  S.MemoryAddr addr (BVTypeRepr nr) ->
    case testEquality nr S.n8 of
      Nothing -> error "not a byte write!"
      Just Refl -> do
        e' <- fmap (runReader (translateExpr' e)) get
        addr' <- fmap (runReader (translateExpr' addr)) get
        let upd hp = G.App $ C.SymArrayUpdate C.typeRepr C.typeRepr hp addr' e'
        G.modifyReg ms (heap %~ upd)

  _ -> return ()
--
generateStmt ms (Get v l) = do
  a <- G.mkAtom =<< translateLocGet ms l
  modify $ insertVar (translateVar v) a
--
generateStmt _ms (Let v e) = do
  e' <- G.mkAtom =<< fmap (runReader (translateExpr' e)) get  
  modify $ insertVar (translateVar v) e'
--
generateStmt _ms (MakeUndefined v tr) = case tr of
  BVTypeRepr nr -> do
    a <- G.mkAtom . G.App $ asPosNat nr (C.BVUndef nr)
    modify (insertVar (translateVar v) a)
--
generateStmt _ms (Ifte_ e s1s s2s) =
  case (s1s, s2s) of
    -- This is the pattern we generate for Jcc, so we know that it's the end of
    -- a block... it's a bit of a hack
    ([Get v (S.Register N.IPReg), S.Register N.IPReg := AppExpr (R.BVAdd _ (VarExpr v') (LitExpr nr i))], []) | v == v' -> do
      e' <- fmap (G.App . C.BVNonzero n1 . runReader (translateExpr' e)) get
      a <- G.mkAtom e'
      G.endNow $ \c -> do
        branchTrampoline <- G.newLabel
        nextRip <- fmap trackedRip get
        fallthroughLabel <- getLabel nextRip
        
        G.endCurrentBlock (G.Br a branchTrampoline fallthroughLabel)
        G.defineBlock branchTrampoline $ do
          mapM_ (generateStmt _ms) s1s
          G.endNow $ \c -> do
            branchLabel <- getLabel $ nextRip + fromIntegral i
            G.defineBlock branchLabel $ G.returnFromFunction =<< G.readReg _ms
            G.endCurrentBlock (G.Jump branchLabel)
        G.defineBlock fallthroughLabel $ G.returnFromFunction =<< G.readReg _ms

    _ -> do 
      e' <- fmap (G.App . C.BVNonzero n1 . runReader (translateExpr' e)) get
      G.ifte_ e' (mapM_ (generateStmt _ms) s1s) (mapM_ (generateStmt _ms) s2s)
    
--
generateStmt _ stmt = error $ unwords [ "generateStmt: unimplemented Stmt case:"
                                      , show (ppStmt stmt)
                                      ]


translateLocGet :: G.Reg s MachineState -> S.Location (Expr (S.BVType 64)) tp
                -> G.Generator s Env MachineState (G.Expr s (F tp))
translateLocGet ms (S.Register reg) = do
  regs <- G.readReg ms
  return $ regs ^. register reg
translateLocGet ms (S.MemoryAddr addr (S.BVTypeRepr nr)) = do
  h <- (^. heap) <$> G.readReg ms
  addr' <- fmap (runReader (translateExpr' addr)) get
  asPosNat nr $ do
     let v  = C.SymArrayLookup C.typeRepr C.typeRepr h addr'
         v' = case testNatCases nr n8 of
                NatCaseLT LeqProof -> (C.BVTrunc nr n8 (G.App v))
                NatCaseEQ     -> v
                NatCaseGT LeqProof -> (C.BVZext nr n8 (G.App v))
     return $ G.App v'

translateLocGet ms (S.TruncLoc l nr) = do
  e <- translateLocGet ms l
  return $ G.App $ C.BVTrunc nr (S.loc_width l) e
translateLocGet ms ll@(S.LowerHalf l) = do
  e <- translateLocGet ms l
  let w = S.loc_width l
  let hw = S.loc_width ll
  withLeqProof (leqAdd2 (leqRefl hw) (leqProof (knownNat::NatRepr 1) hw)) $
      return $ G.App $ C.BVTrunc hw w e
translateLocGet ms ll@(S.UpperHalf l) = do
  e <- translateLocGet ms l
  let w = S.loc_width l
  let hw = S.loc_width ll
  withLeqProof (leqAdd (leqProof (knownNat::NatRepr 1) hw) hw) $ return $ G.App $ C.BVSelect hw hw w e

generateTerm :: (G.Reg s MachineState)
             -> Term
             -> G.Generator s Env MachineState ()
generateTerm ms Ret = return ()
generateTerm ms (Cond _ _) = return () -- handled in a special case for Ifte
generateTerm ms (Fallthrough w) = do
  G.endNow $ \c -> do
    l <- getLabel w
    G.endCurrentBlock (G.Jump l)
generateTerm ms (Direct w) = do
  G.endNow $ \c -> do
    l <- getLabel w
    G.endCurrentBlock (G.Jump l)
  
--
generateTerm ms t = error $ "generateTerm: unimplemented case: " ++ show t


type family F (tp :: Type) :: C.CrucibleType where
  F (BVType n) = Cr.BVType n

-- This is necessary because traverseApp requires its argument function to return
-- something which is a functor over tp. So the newtype wraps the (F tp) type function
-- application inside of it.
newtype GExpr s tp = GExpr { unGExpr :: G.Expr s (F tp) }

translateExpr' :: (MonadReader (Env s) m, Applicative m) => Expr tp -> m (G.Expr s (F tp))
translateExpr' e = trace (show $ ppExpr e) (fmap unGExpr $ translateExpr e)

-- This is an identity function intended to be a workaround for GHC bug #10507
--   https://ghc.haskell.org/trac/ghc/ticket/10507
nonLoopingCoerce :: (x :~: y) -> v (BVType x) -> v (BVType y)
nonLoopingCoerce Refl x = x

nonLoopingCoerce' :: (y :~: x) -> v (BVType x) -> v (BVType y)
nonLoopingCoerce' Refl x = x

-- This is an identity function intended to be a workaround for GHC bug #10742
--   https://ghc.haskell.org/trac/ghc/ticket/10742
ghcBugWorkAround :: proxy n -> ((1 <=? n) ~ (1 <=? n) => a) -> a
ghcBugWorkAround _ x = x
  
translateExpr :: (MonadReader (Env s) m, Applicative m) => Expr tp -> m (GExpr s tp) -- m (G.Expr s (F tp))
-- FIXME: I'm specializing to BVType because I know that's the only (currently)
-- possible `tp`. But that could change.
translateExpr (LitExpr nr i) = return . GExpr $ G.App (C.BVLit nr i)
--
translateExpr (VarExpr var) = do
  let var'@(Var _ name) = translateVar var
  mA <- reader (MapF.lookup var' . vars)
  case mA of
    Just atom -> return . GExpr $ G.AtomExpr atom
    Nothing   -> error $ "translateExpr bug: unbound variable " ++ name ++ " in expr"
--
translateExpr (AppExpr a) = translateApp <$> R.traverseApp translateExpr a
--
translateExpr expr = error $ unwords [ "translateExpr: unimplemented Expr case:"
                                     , show (ppExpr expr)
                                     ]

translateApp :: R.App (GExpr s) tp -> GExpr s tp -- m (G.Expr s (F tp))
translateApp a = case a of
  R.BVAdd nr e1 e2 -> binop nr e1 e2 C.BVAdd
  R.BVSub nr e1 e2 -> binop nr e1 e2 C.BVSub
  R.BVMul nr e1 e2 -> binop nr e1 e2 C.BVMul
  R.BVTestBit (GExpr e1) (GExpr e2)
    | Cr.BVRepr nr1 <- G.exprType e1
    , Cr.BVRepr nr2 <- G.exprType e2 ->
      GExpr $ bvBit nr1 nr2 e1 e2

  R.BVComplement nr e -> unop nr e C.BVNot
  R.BVEq e1 e2 -> boolBinop e1 e2 C.BVEq
  R.BVSignedLt e1 e2 -> boolBinop e1 e2 C.BVSlt
  R.BVUnsignedLt e1 e2 -> boolBinop e1 e2 C.BVUlt
  R.Mux nr (GExpr b) (GExpr e1) (GExpr e2) ->
      GExpr $ G.App $ asPosNat nr $ C.BVIte (G.App $ C.BVEq n1 (G.App $ C.BVLit n1 1) b) nr e1 e2

  R.BVAnd nr e1 e2 -> binop nr e1 e2 C.BVAnd
  R.BVOr nr e1 e2 -> binop nr e1 e2 C.BVOr
  R.BVXor nr e1 e2 -> binop nr e1 e2 C.BVXor

  R.BVShl nr e1 e2 -> binop nr e1 e2 C.BVShl
  R.BVShr nr e1 e2 -> binop nr e1 e2 C.BVLshr
  R.BVSar nr e1 e2 -> binop nr e1 e2 C.BVAshr

  R.Trunc e w
    | Cr.BVRepr nr <- G.exprType (unGExpr e) -> unop nr e (C.BVTrunc w)

  -- FIXME: crucible swaps the order of the bytes ...
  R.ConcatV n1 n2 (GExpr e1) (GExpr e2) ->
    case plusComm n1 n2 of
      Refl -> GExpr . G.App $ asPosNat n1 $ asPosNat n2 $ C.BVConcat n2 n1 e2 e1
  R.UExt e w
    | Cr.BVRepr nr <- G.exprType (unGExpr e) -> unop nr e (C.BVZext w)

  R.SExt e w
    | Cr.BVRepr nr <- G.exprType (unGExpr e) -> unop nr e (C.BVSext w)

  -- TODO: Actually implement this
  R.EvenParity e ->
    GExpr . G.App $ C.BVLit n1 0

  -- TODO: Actually implement this
  R.SadcOverflows nr (GExpr e1) (GExpr e2) (GExpr carry) ->
    GExpr . G.App $ C.BVLit n1 0

  -- TODO: Actually implement this
  R.UadcOverflows nr (GExpr e1) (GExpr e2) (GExpr carry) ->
    GExpr . G.App $ C.BVLit n1 0

  -- TODO: Actually implement this
  R.UsbbOverflows nr (GExpr e1) (GExpr e2) (GExpr borrow) ->
    GExpr . G.App $ C.BVLit n1 0

  -- TODO: Actually implement this
  R.SsbbOverflows nr (GExpr e1) (GExpr e2) (GExpr borrow) ->
    GExpr . G.App $ C.BVLit n1 0

  _ -> error $ unwords [ "translateExpr App case unimplemented:"
                       , show (R.ppApp (pretty . unGExpr) a)
                       ]
  where
    -- For some reason the booleans don't have their size, so we
    -- figure it out here ...
    boolBinop :: forall s tp.
                 GExpr s tp
                 -> GExpr s tp
                 -> (forall n. (1 <= n) =>
                     NatRepr n
                     -> G.Expr s (F (BVType n))
                     -> G.Expr s (F (BVType n))
                     -> C.App (G.Expr s) C.BoolType)
                 -> GExpr s (BVType 1)
    boolBinop (GExpr e1) (GExpr e2) f
      | Cr.BVRepr nr <- G.exprType e1 =
         GExpr . G.App . C.BoolToBV n1 . G.App $ asPosNat nr (f nr e1 e2)

    binop :: forall s n tp.
             NatRepr n
             -> GExpr s (BVType n)
             -> GExpr s (BVType n)
             -> ((1 <= n) => NatRepr n
                 -> G.Expr s (F (BVType n))
                 -> G.Expr s (F (BVType n))
                 -> C.App (G.Expr s) (F tp))
             -> GExpr s tp
    binop nr (GExpr e1) (GExpr e2) f =
          GExpr . G.App $ asPosNat nr (f nr e1 e2)

    unop :: forall s n tp.
            NatRepr n
            -> GExpr s (BVType n)
            -> ((1 <= n) => NatRepr n -> G.Expr s (F (BVType n)) -> C.App (G.Expr s) (F tp))
            -> GExpr s tp
    unop nr (GExpr e1) f =
          GExpr . G.App $ asPosNat nr (f nr e1)


asPosNat :: forall n s a
           . NatRepr n
          -> ((1 <= n) => a)
          -> a
asPosNat nr x = case isPosNat nr of
  Nothing -> error "zero-width NatRepr"
  Just prf -> withLeqProof prf x

tracer x y = id -- trace (unwords ["target width:", show (natValue x), "current width:", show (natValue y)])

bvBit :: forall s n log_n
       . NatRepr n
      -> NatRepr log_n
      -> G.Expr s (C.BVType n)
      -> G.Expr s (C.BVType log_n)
      -> G.Expr s (C.BVType 1)
bvBit nr1 nr2 val idx =
  case testNatCases nr1 n1 of
    NatCaseLT prf -> error "zero-width NatRepr"
    NatCaseEQ -> val
    NatCaseGT prf -> withLeqProof prf $ tracer n1 nr1 $ (G.App $ C.BVTrunc n1 nr1 shiftedVal)
  where
    shiftedVal :: G.Expr s (C.BVType n)
    shiftedVal = G.App $ asPosNat nr1 (C.BVAshr nr1 val idx')
    idx' :: G.Expr s (C.BVType n)
    idx' = case testNatCases nr2 nr1 of
      NatCaseLT prf ->
        withLeqProof prf (asPosNat nr2 (G.App $ C.BVZext nr1 nr2 idx))
      NatCaseEQ -> idx
      NatCaseGT prf ->
        withLeqProof prf (asPosNat nr1 (G.App $ tracer nr1 nr2 $ C.BVTrunc nr1 nr2 idx))


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
