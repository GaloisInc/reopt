------------------------------------------------------------------------
-- |
-- Module           : Reopt.CFG.LLVM
-- Description      : Defines basic data types used for representing Reopt CFG.
-- Copyright        : (c) Galois, Inc 2015
-- Maintainer       : Joe Hendrix <jhendrix@galois.com>
-- Stability        : provisional
--
-- Functions which convert the types in Representaiton to their
-- analogues in LLVM
------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Reopt.CFG.LLVM where

import           Control.Applicative
import           Control.Monad
import           Data.Text (Text)
import qualified Data.Text as Text
import           Text.LLVM (BB)
import qualified Text.LLVM as L
import           Text.PrettyPrint.ANSI.Leijen (pretty)

import           Reopt.Machine.Types
import           Reopt.CFG.Representation

blockToLLVM :: Block -> BB () -- L.BasicBlock
blockToLLVM b = do L.label (L.Ident . show $ blockLabel b)
                   mapM_ stmtToLLVM $ blockStmts b -- ++ [termStmtToLLVM $ blockTerm b]

stmtToLLVM :: Stmt -> BB ()
stmtToLLVM stmt = do
  L.comment (show $ pretty stmt)
  case stmt of
   AssignStmt (Assignment lhs rhs) ->
     void $ L.assign (assignIdToLLVMIdent lhs) (rhsToLLVM rhs)
   Write loc v -> do let align = Nothing
                     l_loc <- stmtLocToLLVM loc
                     l_v   <- valueToLLVM v
                     L.store l_v l_loc align
   Comment str -> L.comment $ Text.unpack str
   _           -> L.comment "UNIMPLEMENTED"

assignIdToLLVMIdent :: AssignId -> L.Ident
assignIdToLLVMIdent aid = L.Ident $ "R" ++ show aid

unimplementedInstr :: BB (L.Typed L.Value)
unimplementedInstr = do L.comment "UNIMPLEMENTED"
                        return (L.Typed L.voidT L.ValUndef)

rhsToLLVM :: AssignRhs tp -> BB (L.Typed L.Value)
rhsToLLVM rhs =
  case rhs of
   EvalApp app -> appToLLVM app
   SetUndefined sz -> let typ = natReprToLLVMType sz
                      in  return (L.Typed typ L.ValUndef)
   Read loc -> do let align = Nothing
                  l_loc <- stmtLocToLLVM loc
                  L.load l_loc align
   MemCmp{} -> unimplementedInstr

--------------------------------------------------------------------------------
-- reopt intrinsics:
--------------------------------------------------------------------------------   

-- FIXME: is False ok here??
intrinsic :: String -> L.Type -> [L.Type] -> L.Typed L.Value 
intrinsic name res args =
  (L.ptrT $ L.FunTy res args False) L.-: L.Symbol name
  
iEvenParity :: L.Typed L.Value
iEvenParity = intrinsic "reopt.EvenParity" (L.iT 1) [L.iT 8]

--------------------------------------------------------------------------------
-- LLVM intrinsics
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- conversion to LLVM 
--------------------------------------------------------------------------------

appToLLVM :: App Value tp -> BB (L.Typed L.Value)
appToLLVM app =
  case app of
   Mux _sz b l r ->
     join $ L.select <$> valueToLLVM b <*> valueToLLVM l <*> valueToLLVM r
   MMXExtend v -> unimplementedInstr
   ConcatV sz low high -> do
     low'  <- join $ flip L.zext typ <$> valueToLLVM low
     high' <- join $ flip L.zext typ <$> valueToLLVM high
     s_high <- L.shl high' (natValue sz)
     L.bor low' s_high
   UpperHalf sz v -> do
     v' <- join $ flip L.lshr (natValue sz) <$> valueToLLVM v
     L.trunc v' (natReprToLLVMType sz)
   Trunc v sz -> join $ flip L.trunc (natReprToLLVMType sz) <$> valueToLLVM v
   SExt v sz -> join $ flip L.sext (natReprToLLVMType sz) <$> valueToLLVM v
   UExt v sz -> join $ flip L.zext (natReprToLLVMType sz) <$> valueToLLVM v
   AndApp{}     -> unimplementedInstr  
   OrApp{}      -> unimplementedInstr
   NotApp{}     -> unimplementedInstr
   BVAdd _sz x y -> binop L.add x y
   BVSub _sz x y -> binop L.sub x y
   BVMul _sz x y -> binop L.mul x y
   BVDiv _sz x y -> binop L.udiv x y
   BVMod _sz x y -> binop L.urem x y   -- FIXME: mod?
   BVSignedDiv _sz x y -> binop L.sdiv x y
   BVSignedMod _sz x y -> binop L.srem x y
   BVUnsignedLt x y    -> binop (L.icmp L.Iult) x y
   BVUnsignedLe x y    -> binop (L.icmp L.Iule) x y
   BVSignedLt x y    -> binop (L.icmp L.Islt) x y
   BVSignedLe x y    -> binop (L.icmp L.Isle) x y
   BVTestBit v n     -> do -- FIXME
     let in_typ = typeToLLVMType $ valueType v
     v' <- valueToLLVM v
     n' <- valueToLLVM n
     mask <- L.shl (in_typ L.-: (1 :: Int)) n'
     r <- L.band v' mask
     L.icmp L.Ine r (0 :: Int)
   BVComplement _sz v ->
     -- xor x -1 == complement x, according to LLVM manual.
     join $ flip L.bxor (-1 :: Int) <$> valueToLLVM v
   BVAnd _sz x y -> binop L.band x y
   BVOr _sz x y -> binop L.bor x y
   BVXor _sz x y -> binop L.bxor x y
   BVShl _sz x y -> binop L.shl x y
   BVShr _sz x y -> binop L.lshr x y
   BVSar _sz x y -> binop L.ashr x y
   BVEq x y      -> binop (L.icmp L.Ieq) x y
   EvenParity v  -> do v' <- valueToLLVM v
                       L.call iEvenParity [v']
   ReverseBytes{} -> unimplementedInstr
   -- FIXME: do something more efficient?
   -- Basically does let (r, over)  = llvm.add.with.overflow(x,y)
   --                    (_, over') = llvm.add.with.overflow(r,c)
   --                in over'
   -- and we rely on llvm optimisations to throw away identical adds
   -- and adds of 0
   UadcOverflows sz x y c -> intrinsicOverflows "uadd" sz x y c
   SadcOverflows sz x y c -> intrinsicOverflows "sadd" sz x y c
   UsbbOverflows sz x y c -> intrinsicOverflows "usub" sz x y c
   SsbbOverflows sz x y c -> intrinsicOverflows "ssub" sz x y c
   Bsf sz v -> do
     let op = intrinsic ("llvm.cttz." ++ show (L.ppType typ)) typ [typ, L.iT 1]
     v' <- valueToLLVM v
     L.call op [v', L.iT 1 L.-: L.int 1]
   Bsr sz v -> do
     let op = intrinsic ("llvm.ctlz." ++ show (L.ppType typ)) typ [typ, L.iT 1]
     v' <- valueToLLVM v
     L.call op [v', L.iT 1 L.-: L.int 1]
   
   _ -> unimplementedInstr
  where
    intrinsicOverflows op sz x y c = do
      let in_typ = typeToLLVMType $ valueType x
          op_with_overflow =
            intrinsic ("llvm." ++ op ++ ".with.overflow." ++ show (L.ppType in_typ))
            (L.Struct [in_typ, L.iT 1]) [in_typ, in_typ]
      x' <- valueToLLVM x
      y' <- valueToLLVM y
      c' <- join $ flip L.zext in_typ <$> valueToLLVM c
      r_tuple  <- L.call op_with_overflow [x', y']
      r    <- L.extractValue r_tuple 0
      over <- L.extractValue r_tuple 1
      r' <- L.call op_with_overflow [r, c']
      over' <- L.extractValue r_tuple 1
      L.bor over over'
    
    unop :: (L.Typed L.Value -> BB (L.Typed L.Value))
            -> Value (BVType n)
            -> BB (L.Typed L.Value)
    unop f x = join $ f <$> valueToLLVM x
    
    binop :: (L.Typed L.Value -> L.Typed L.Value -> BB (L.Typed L.Value))
             -> Value (BVType n) -> Value (BVType m)
             -> BB (L.Typed L.Value)
    binop f x y = join $ f <$> valueToLLVM x <*> valueToLLVM y
    typ = typeToLLVMType $ appType app

natReprToLLVMType :: NatRepr n -> L.Type
natReprToLLVMType = L.PrimType . L.Integer . fromIntegral . natValue

typeToLLVMType :: TypeRepr tp -> L.Type
typeToLLVMType (BVTypeRepr n) = natReprToLLVMType n

stmtLocToLLVM :: StmtLoc (Value (BVType 64)) tp -> BB (L.Typed L.Value)
stmtLocToLLVM sloc =
  case sloc of
   MemLoc ptr typ -> do p <- valueToLLVM ptr
                        L.inttoptr p (L.ptrT (typeToLLVMType typ))
   _ -> unimplementedInstr

valueToLLVM :: Value tp -> BB (L.Typed L.Value)
valueToLLVM val =
  case val of
   BVValue _sz n -> mk $ L.integer n
   AssignedValue (Assignment lhs _rhs) ->
     mk $ L.ValIdent $ assignIdToLLVMIdent lhs
   Initial reg   -> mk . L.ValIdent . L.Ident . show $ reg     
  where
    mk :: L.Value -> BB (L.Typed L.Value)
    mk  = return . L.Typed typ
    typ = typeToLLVMType $ valueType val
