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
       , translateSingleBlock
       , translateFunction
       , MachineCtx
       , MachineState
       , MachineStateCtx
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
import           Numeric (showHex)
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
type Heap = C.SymbolicArrayType (C.BVIndexType 64) (C.BaseBVType 8)

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


translateSingleBlock :: HandleAllocator s
                     -> Block
                     -> ST s (C.SomeCFG MachineStateCtx MachineState)
translateSingleBlock halloc block = do
  fnH <- mkHandle' halloc "testBlock" machineStateCtx machineState
  (g,[]) <- G.defineFunction halloc InternalPos fnH $ mkDefn block
  traceM "**** <<<<<<<< ****"
  traceM (show (pretty g))
  traceM "**** >>>>>>>> ****"
  return $ toSSA g

mkDefn :: Block -> G.FunctionDef Env MachineStateCtx MachineState
mkDefn (Block stmts term) assn = (s, f)
  where s = Env {vars = MapF.empty, blocks = Map.empty, trackedRip = 0}
        f = do
          ms <- G.newReg $ G.AtomExpr $ assn^._1
          let getLabel w = do
                l <- G.newLabel
                G.defineBlock l $ G.returnFromFunction =<< G.readReg ms
                return l
          trace (show $ ppStmts stmts) $ mapM_ (generateStmt ms getLabel) stmts
          generateTerm ms getLabel term
          G.returnFromFunction =<< G.readReg ms

translateFunction :: HandleAllocator s
                  -> Map Word64 Block
                  -> Word64
                  -> ST s (C.SomeCFG MachineStateCtx MachineState)
translateFunction halloc blockMap entry = do
  fnH <- mkHandle' halloc "entryBlock" machineStateCtx machineState
  (g,[]) <- G.defineFunction halloc InternalPos fnH $ mkFunctionDefn entry blockMap
  traceM "**** <<<<<<<< ****"
  traceM (show (pretty g))
  traceM "**** >>>>>>>> ****"
  return $ toSSA g

mkFunctionDefn :: Word64
               -> Map Word64 Block
               -> G.FunctionDef Env MachineStateCtx MachineState
mkFunctionDefn entry  map assn = (s,f)
  where s = Env {vars = MapF.empty, blocks = Map.empty, trackedRip = 0}
        f = do
          ms <- G.newReg $ G.AtomExpr $ assn^._1
          let getLabel w = do
              env <- get
              let labelMap = blocks env
              case Map.lookup w labelMap of
                Just label -> return label
                Nothing ->
                  case Map.lookup w map of
                    Just b -> do
                      label <- G.newLabel
                      put $ env{blocks = Map.insert w label labelMap}
                      G.defineBlock label $ do
                        G.setPosition $ BinaryPos "__BINARY__" w
                        generateBlock ms getLabel b
                      return label
                    Nothing -> error $ "Unknown block entry 0x" ++ showHex w " in getLabel!"
          G.setPosition $ BinaryPos "__BINARY__" entry
          generateBlock ms getLabel (map Map.! entry)
          G.readReg ms


generateBlock :: (G.Reg s MachineState)
              -> (Word64 -> G.End s Env init MachineState (G.Label s))
              -> Block
              -> G.Generator s Env MachineState ()
generateBlock ms getLabel (Block stmts term) = do
  mapM_ (generateStmt ms getLabel) stmts
  generateTerm ms getLabel term
  

generateStmt :: (G.Reg s MachineState)
             -> (Word64 -> G.End s Env init MachineState (G.Label s))
             -> Stmt
             -> G.Generator s Env MachineState ()

generateStmt ms getLabel (S.Register rv := LitExpr nr i)
  | Just (N.IPReg, Refl, Refl) <- S.registerViewAsFullRegister rv
  = do
  G.modifyReg ms (curIP .~ G.App (C.BVLit nr i))
  modify $ \env -> env {trackedRip = fromIntegral i}
--
generateStmt ms getLabel (l := e) = case l of
  S.Register rv
    | Just (reg, Refl, Refl) <- S.registerViewAsFullRegister rv -> do
    e' <- fmap (runReader (translateExpr' e)) get
    G.modifyReg ms (register reg .~ e')

  S.MemoryAddr _ _ -> return () -- error "assign mem addr unimplemented"

  _ -> return () -- error "assign subregister unimplemented"
--
generateStmt ms getLabel (Get v l) = do
  a <- G.mkAtom =<< translateLocGet ms l
  modify $ insertVar (translateVar v) a
--
generateStmt _ms getLabel (Let v e) = do
  e' <- G.mkAtom =<< fmap (runReader (translateExpr' e)) get  
  modify $ insertVar (translateVar v) e'
--
generateStmt _ms getLabel (MakeUndefined v tr) = case tr of
  BVTypeRepr nr -> do
    a <- G.mkAtom . G.App $ asPosNat nr (C.BVUndef nr)
    modify (insertVar (translateVar v) a)
--
generateStmt _ms getLabel (Ifte_ e s1s s2s) =
  case (s1s, s2s) of
    -- This is the pattern we generate for Jcc, so we know that it's the end of
    -- a block... it's a bit of a hack
    ([Get v (S.Register rv1), S.Register rv2 := AppExpr (R.BVAdd _ (VarExpr v') (LitExpr nr i))], [])
      | Just (N.IPReg, Refl, Refl) <- S.registerViewAsFullRegister rv1
      , Just (N.IPReg, Refl, Refl) <- S.registerViewAsFullRegister rv2
      , v == v' -> do
      e' <- fmap (G.App . C.BVNonzero n1 . runReader (translateExpr' e)) get
      a <- G.mkAtom e'
      G.endNow $ \c -> do
        branchTrampoline <- G.newLabel
        nextRip <- fmap trackedRip get
        fallthroughLabel <- getLabel nextRip
        G.endCurrentBlock (G.Br a branchTrampoline fallthroughLabel)
        G.defineBlock branchTrampoline $ do
          G.setPosition $ InternalPos
          mapM_ (generateStmt _ms getLabel) s1s
          G.endNow $ \c -> do
            branchLabel <- getLabel $ nextRip + fromIntegral i
            G.endCurrentBlock (G.Jump branchLabel)

    _ -> do 
      e' <- fmap (G.App . C.BVNonzero n1 . runReader (translateExpr' e)) get
      G.setPosition InternalPos
      G.ifte_ e' (mapM_ (generateStmt _ms getLabel) s1s) (mapM_ (generateStmt _ms getLabel) s2s)

--
generateStmt _ _ stmt = error $ unwords [ "generateStmt: unimplemented Stmt case:"
                                      , show (ppStmt stmt)
                                      ]

-- TODO: implement translation for x87 stack register reads
translateLocGet ::
  G.Reg s MachineState ->
  S.Location (Expr (S.BVType 64))tp ->
  G.Generator s Env MachineState (G.Expr s (F tp))
translateLocGet ms (S.Register rv) = do
  let reg = S.registerViewReg rv
  regs <- G.readReg ms
  let v = regs ^. register reg
  return $ unGExpr . S.registerViewRead rv . GExpr $ v
translateLocGet ms (S.MemoryAddr addr (S.BVTypeRepr nr)) = 
  return $ G.App $ asPosNat nr (C.BVLit nr 0)

generateTerm :: (G.Reg s MachineState)
             -> (Word64 -> G.End s Env init MachineState (G.Label s))
             -> Term
             -> G.Generator s Env MachineState ()
generateTerm ms getLabel Ret = do
  G.returnFromFunction =<< G.readReg ms
generateTerm ms getLabel (Cond _ _) = return () -- handled in a special case for Ifte
generateTerm ms getLabel (Fallthrough w) = do
  G.endNow $ \c -> do
    l <- getLabel w
    G.endCurrentBlock (G.Jump l)
generateTerm ms getLabel (Direct w) = do
  G.endNow $ \c -> do
    l <- getLabel w
    G.endCurrentBlock (G.Jump l)
generateTerm ms getLabel (Call _ ret) = do
  G.endNow $ \c -> do 
    l <- getLabel ret
    G.endCurrentBlock (G.Jump l)
generateTerm ms getLabel (Indirect) = do
  traceM $ "Assuming indirect is a return"
  G.returnFromFunction =<< G.readReg ms

  -- G.endNow $ \c -> do
  --   G.endCurrentBlock (G.Return )
  
--
generateTerm ms getLabel t = error $ "generateTerm: unimplemented case: " ++ show t


type family F (tp :: Type) :: C.CrucibleType where
  F (BVType n) = Cr.BVType n

-- This is necessary because traverseApp requires its argument function to return
-- something which is a functor over tp. So the newtype wraps the (F tp) type function
-- application inside of it.
newtype GExpr s tp = GExpr { unGExpr :: G.Expr s (F tp) }

translateExpr' :: (MonadReader (Env s) m, Applicative m) => Expr tp -> m (G.Expr s (F tp))
translateExpr' e = trace (show $ ppExpr e) (fmap unGExpr $ translateExpr e)

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

  R.UExt (GExpr e1) nr2
    | Cr.BVRepr nr1 <- G.exprType e1 -> GExpr . G.App $ C.BVZext nr2 nr1 e1

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

-- | An 'S.IsValue' instance for interpreting the 'S.RegisterView'
-- reads and writes.
--
-- We only implement the operations we need for the reads and writes.
--
-- See documentation on 'Reopt.Semantics.Monad.registerViewRead' for
-- ideas about factoring these ops out into a separate class of "basic
-- bitvector operations".
instance S.IsValue (GExpr s) where
  bvLit nr i = GExpr . G.App $ C.BVLit nr (fromIntegral i)
  bvShl (GExpr e1) (GExpr e2)
    | Cr.BVRepr nr <- G.exprType e1
    = GExpr . G.App $ C.BVShl nr e1 e2
  bvShr (GExpr e1) (GExpr e2)
    | Cr.BVRepr nr <- G.exprType e1
    = GExpr . G.App $ C.BVLshr nr e1 e2
  bvTrunc' nr2 (GExpr e1)
    | Cr.BVRepr nr1 <- G.exprType e1
    = GExpr . G.App $ C.BVTrunc nr2 nr1 e1
  bv_width (GExpr e1)
    | Cr.BVRepr nr <- G.exprType e1
    = nr
  complement (GExpr e1)
    | Cr.BVRepr nr <- G.exprType e1
    = GExpr . G.App $ C.BVNot nr e1
  uext' nr2 (GExpr e1)
    | Cr.BVRepr nr1 <- G.exprType e1
    = GExpr . G.App $ C.BVZext nr2 nr1 e1
  GExpr e1 .|. GExpr e2
    | Cr.BVRepr nr <- G.exprType e1
    = GExpr . G.App $ C.BVOr nr e1 e2

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
