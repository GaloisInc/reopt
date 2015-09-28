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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reopt.CFG.LLVM where

import           Control.Monad
import           Control.Monad.Reader
import           Numeric (showHex)
import           Text.LLVM (BB, LLVM)
import qualified Text.LLVM as L
import           Text.PrettyPrint.ANSI.Leijen (pretty)

import           Data.Parameterized.Some

import           Reopt.CFG.FnRep
import           Reopt.CFG.Representation
import           Reopt.Machine.Types

--------------------------------------------------------------------------------
-- reopt intrinsics
--------------------------------------------------------------------------------   

-- FIXME: is False ok here??
intrinsic :: String -> L.Type -> [L.Type] -> L.Typed L.Value 
intrinsic name res args =
  (L.ptrT $ L.FunTy res args False) L.-: L.Symbol name
  
iEvenParity :: L.Typed L.Value
iEvenParity = intrinsic "reopt.EvenParity" (L.iT 1) [L.iT 8]

iRead_X87_RC :: L.Typed L.Value
iRead_X87_RC = intrinsic "reopt.Read_X87_RC" (L.iT 2) []

iWrite_X87_RC :: L.Typed L.Value
iWrite_X87_RC = intrinsic "reopt.Write_X87_RC" L.voidT [L.iT 2]

iRead_X87_PC :: L.Typed L.Value
iRead_X87_PC = intrinsic "reopt.Read_X87_PC" (L.iT 2) []

iWrite_X87_PC :: L.Typed L.Value
iWrite_X87_PC = intrinsic "reopt.Read_X87_PC" L.voidT [L.iT 2]

iRead_FS :: L.Typed L.Value
iRead_FS = intrinsic "reopt.Read_FS" (L.iT 16) []

iWrite_FS :: L.Typed L.Value
iWrite_FS = intrinsic "reopt.Write_FS" L.voidT [L.iT 16]

iRead_GS :: L.Typed L.Value
iRead_GS = intrinsic "reopt.Read_GS" (L.iT 16) []

iWrite_GS :: L.Typed L.Value
iWrite_GS = intrinsic "reopt.Write_GS" L.voidT [L.iT 16]

iMemCopy :: L.Typed L.Value
iMemCopy = intrinsic "reopt.MemCopy" L.voidT [L.iT 64, L.iT 64
                                             , L.iT 64, L.iT 64
                                             , L.iT 1]

iMemCmp :: L.Typed L.Value
iMemCmp = intrinsic "reopt.MemCmp" (L.iT 64) [L.iT 64, L.iT 64
                                             , L.iT 64, L.iT 64
                                             , L.iT 1]

reoptIntrinsics :: [L.Typed L.Value]
reoptIntrinsics = [ iEvenParity
                  , iRead_X87_RC
                  , iWrite_X87_RC
                  , iRead_X87_PC
                  , iWrite_X87_PC
                  , iRead_FS
                  , iWrite_FS
                  , iRead_GS
                  , iWrite_GS
                  , iMemCopy
                  , iMemCmp                    
                  ]
                  
--------------------------------------------------------------------------------
-- LLVM intrinsics
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- conversion to LLVM 
--------------------------------------------------------------------------------

data LLVMState = LLVMState { llvmIntArgs   :: [L.Typed L.Value]
                           , llvmFloatArgs :: [L.Typed L.Value] }

newtype ToLLVM a  = ToLLVM { runToLLVM :: ReaderT LLVMState BB a }
                    deriving (Applicative, Functor, Monad, MonadReader LLVMState)

liftBB :: BB a -> ToLLVM a
liftBB = ToLLVM . lift

liftBBF :: (BB a -> BB b) -> ToLLVM a -> ToLLVM b
liftBBF f v = ToLLVM $ mapReaderT f (runToLLVM v)

functionName :: CodeAddr -> L.Symbol
functionName addr = L.Symbol $ "F" ++ showHex addr ""

blockName :: BlockLabel -> L.Ident
blockName = L.Ident . show 

funReturnType :: L.Type
funReturnType = L.Struct [ L.PrimType (L.Integer 64)
                         , L.PrimType (L.FloatType L.Fp128) ]

functionToLLVM :: Function -> LLVM (L.Typed L.Value)
functionToLLVM f = L.define' L.emptyFunAttrs funReturnType symbol argTypes False go
  where
    argTypes      = map (viewSome typeToLLVMType) (fnIntArgTypes f)
                    ++ map (viewSome typeToLLVMType) (fnFloatArgTypes f)
    symbol        = functionName (fnAddr f)
    go args =
      let nint = length (fnIntArgTypes f)
          st   = LLVMState { llvmIntArgs   = take nint args
                           , llvmFloatArgs = drop nint args }
      in runReaderT (runToLLVM (mapM_ blockToLLVM (fnBlocks f))) st

blockToLLVM :: FnBlock -> ToLLVM () -- L.BasicBlock
blockToLLVM b = do liftBB $ L.label (blockName $ fbLabel b)
                   mapM_ stmtToLLVM $ fbStmts b -- ++ [termStmtToLLVM $ blockTerm b]
                   termStmtToLLVM (fbTerm b)

termStmtToLLVM :: FnTermStmt -> ToLLVM ()
termStmtToLLVM tm =
  case tm of
     FnJump lbl -> liftBB $ L.jump (blockName lbl)
     FnRet iret fret -> do
       iret' <- valueToLLVM iret
       fret' <- valueToLLVM fret
       liftBB $ L.ret (L.struct False [iret', fret'])
     FnBranch cond tlbl flbl -> do
       cond' <- valueToLLVM cond
       liftBB $ L.br cond' (blockName tlbl) (blockName flbl)
       
     FnCall dest args iretv fretv contlbl -> do
       dest' <- valueToLLVM dest
       let arg_tys = map (viewSome (typeToLLVMType . fnValueType)) args
           fun_ty = L.ptrT (L.FunTy funReturnType arg_tys False)
       dest_f <- liftBB $ L.bitcast dest' fun_ty 
       args' <- mapM (viewSome valueToLLVM) args
       rvar  <- liftBB $ L.call dest_f args'
       void $ liftBB $ L.assign (assignIdToLLVMIdent $ frAssignId iretv)
                                (L.extractValue rvar 0)
       void $ liftBB $ L.assign (assignIdToLLVMIdent $ frAssignId fretv)
                                (L.extractValue rvar 1)
     FnTermStmtUndefined -> void $ unimplementedInstr    

stmtToLLVM :: FnStmt -> ToLLVM ()
stmtToLLVM stmt = do
  liftBB $ L.comment (show $ pretty stmt)
  case stmt of
   FnAssignStmt (FnAssignment lhs rhs) ->
     void $ liftBBF (L.assign (assignIdToLLVMIdent lhs)) (rhsToLLVM rhs)
   FnWriteMem ptr v -> do
     v' <- valueToLLVM v
     p  <- valueToLLVM ptr
     liftBB $ do 
       -- FIXME: this should be the same as using typeToLLVMType on v
       p' <- L.inttoptr p (L.ptrT (L.typedType v'))
       let align = Nothing                           
       L.store v' p' align
       -- FS     -> L.call_ iWrite_FS [v']
       -- GS     -> L.call_ iWrite_GS [v']
       -- X87_PC -> L.call_ iWrite_X87_PC [v'] 
       -- X87_RC -> L.call_ iWrite_X87_RC [v']
       -- ControlLoc {} -> void $ unimplementedInstr
       -- DebugLoc {}   -> void $ unimplementedInstr
      
   -- MemCopy bytesPerCopy nValues src dest direction -> do
   --   nValues' <- valueToLLVM nValues
   --   src'     <- valueToLLVM src
   --   dest'    <- valueToLLVM dest
   --   case direction of
   --    BVValue _ 0 -> do
   --      let typ = L.iT (fromIntegral $ 8 * bytesPerCopy)
   --          op = intrinsic ("llvm.memcpy.p0"
   --                          ++ show (L.ppType typ)
   --                          ++ ".p0" ++ show (L.ppType typ)
   --                          ++ ".i64") L.voidT
   --               [L.ptrT typ, L.ptrT typ, L.iT 64, L.iT 32, L.iT 1]
   --      src_ptr  <- L.bitcast src'  (L.ptrT typ)
   --      dest_ptr <- L.bitcast dest' (L.ptrT typ)
   --      L.call_ op [dest_ptr, src_ptr, nValues'
   --                 , L.iT 32 L.-: L.int 0
   --                 , L.iT 1  L.-: L.int 0 ]
   --    _ -> do
   --      direction' <- valueToLLVM direction
   --      L.call_ iMemCopy [ L.iT 64 L.-: L.integer bytesPerCopy
   --                       , nValues', src', dest', direction' ]

   -- MemSet count v ptr -> do
   --   count' <- valueToLLVM count
   --   v'     <- valueToLLVM v
   --   ptr'   <- valueToLLVM ptr
   --   let typ = typeToLLVMType $ valueType v
   --       op = intrinsic ("llvm.memset.p0"
   --                          ++ show (L.ppType typ)
   --                          ++ ".i64") L.voidT
   --            [L.ptrT typ, typ, L.iT 64, L.iT 32, L.iT 1]
   --   ptr_ptr <- L.bitcast ptr' (L.ptrT typ)
   --   L.call_ op [ptr_ptr, v', count', L.iT 32 L.-: L.int 0, L.iT 1 L.-: L.int 0]

   FnComment str -> return () -- L.comment $ Text.unpack str
   -- PlaceHolderStmt {} -> void $ unimplementedInstr
   -- _           -> void $ unimplementedInstr

assignIdToLLVMIdent :: AssignId -> L.Ident
assignIdToLLVMIdent aid = L.Ident $ "R" ++ show aid

unimplementedInstr :: ToLLVM (L.Typed L.Value)
unimplementedInstr = do liftBB $ L.comment "UNIMPLEMENTED"
                        return (L.Typed L.voidT L.ValUndef)

rhsToLLVM :: FnAssignRhs tp -> ToLLVM (L.Typed L.Value)
rhsToLLVM rhs =
  case rhs of
   FnEvalApp app -> appToLLVM app
   FnSetUndefined sz -> let typ = natReprToLLVMType sz
                        in  return (L.Typed typ L.ValUndef)
   FnReadMem ptr typ -> do
     p <- valueToLLVM ptr
     p' <- liftBB $ L.inttoptr p (L.ptrT (typeToLLVMType typ))
     let align = Nothing                                    
     liftBB $ L.load p' align
   FnAlloca v -> do
     v' <- valueToLLVM v
     liftBB $ L.alloca (L.iT 8) (Just v') Nothing
   --     FS     -> L.call iRead_FS []
   --     GS     -> L.call iRead_GS []
   --     X87_PC -> L.call iRead_X87_PC [] 
   --     X87_RC -> L.call iRead_X87_RC [] 
   --     _      -> unimplementedInstr
   -- -- there doesn't seem to be a llvm.memcmp.* intrinsic
   -- MemCmp bytesPerCopy nValues src dest direction -> do
   --   nValues'   <- valueToLLVM nValues
   --   src'       <- valueToLLVM src
   --   dest'      <- valueToLLVM dest
   --   direction' <- valueToLLVM direction
   --   L.call iMemCmp [ L.iT 64 L.-: L.integer bytesPerCopy
   --                  , nValues', src', dest', direction' ]

appToLLVM :: App FnValue tp -> ToLLVM (L.Typed L.Value)
appToLLVM app =
  case app of
   Mux _sz b l r -> do
     b' <- valueToLLVM b
     l' <- valueToLLVM l
     r' <- valueToLLVM r
     liftBB $ L.select b' l' r'
   MMXExtend _v -> unimplementedInstr
   ConcatV sz _sz' low high -> do
     low'  <- liftBB . flip L.zext typ =<< valueToLLVM low
     high' <- liftBB . flip L.zext typ =<< valueToLLVM high
     s_high <- liftBB $ L.shl high' (natValue sz)
     liftBB $ L.bor low' s_high
   UpperHalf sz v -> do
     v' <- liftBB . flip L.lshr (natValue sz) =<< valueToLLVM v
     liftBB $ L.trunc v' (natReprToLLVMType sz)
   Trunc v sz -> liftBB . flip L.trunc (natReprToLLVMType sz) =<< valueToLLVM v
   SExt v sz -> liftBB . flip L.sext (natReprToLLVMType sz) =<< valueToLLVM v
   UExt v sz -> liftBB . flip L.zext (natReprToLLVMType sz) =<< valueToLLVM v
   AndApp{}     -> unimplementedInstr  
   OrApp{}      -> unimplementedInstr
   NotApp{}     -> unimplementedInstr
   BVAdd _sz x y -> binop L.add x y
   BVSub _sz x y -> binop L.sub x y
   BVMul _sz x y -> binop L.mul x y

   -- The x86 documentation for @idiv@ (Intel x86 manual volume 2A,
   -- page 3-393) says that results should be rounded towards
   -- zero. These operations are called @quot@ and @rem@ in Haskell,
   -- whereas @div@ and @mod@ in Haskell round towards negative
   -- infinity. The LLVM @srem@ and @sdiv@ also round towards negative
   -- infinity, and so are the correct operations to use here.  The
   -- LLVM documentation
   -- (http://llvm.org/releases/2.5/docs/LangRef.html) describes the
   -- semantics of @srem@ with "the result has the same sign as the
   -- dividend", which is equivalent to rounding towards zero.
   BVQuot _sz x y       -> binop L.udiv x y
   BVRem _sz x y        -> binop L.urem x y
   BVSignedQuot _sz x y -> binop L.sdiv x y
   BVSignedRem _sz x y  -> binop L.srem x y

   BVUnsignedLt x y    -> binop (L.icmp L.Iult) x y
   BVUnsignedLe x y    -> binop (L.icmp L.Iule) x y
   BVSignedLt x y    -> binop (L.icmp L.Islt) x y
   BVSignedLe x y    -> binop (L.icmp L.Isle) x y
   BVTestBit v n     -> do -- FIXME
     v' <- valueToLLVM v
     let in_typ = L.typedType v'
     n' <- valueToLLVM n
     liftBB $ do 
       mask <- L.shl (in_typ L.-: (1 :: Int)) n'
       r <- L.band v' mask
       L.icmp L.Ine r (0 :: Int)
   BVComplement _sz v ->
     -- xor x -1 == complement x, according to LLVM manual.
     liftBB . flip L.bxor (-1 :: Int) =<< valueToLLVM v
   BVAnd _sz x y -> binop L.band x y
   BVOr _sz x y -> binop L.bor x y
   BVXor _sz x y -> binop L.bxor x y
   BVShl _sz x y -> binop L.shl x y
   BVShr _sz x y -> binop L.lshr x y
   BVSar _sz x y -> binop L.ashr x y
   BVEq x y      -> binop (L.icmp L.Ieq) x y
   EvenParity v  -> do v' <- valueToLLVM v
                       liftBB $ L.call iEvenParity [v']
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
     liftBB $ L.call op [v', L.iT 1 L.-: L.int 1]
   Bsr _sz v -> do
     let op = intrinsic ("llvm.ctlz." ++ show (L.ppType typ)) typ [typ, L.iT 1]
     v' <- valueToLLVM v
     liftBB $ L.call op [v', L.iT 1 L.-: L.int 1]

   FPIsQNaN frep v -> do
     let op = intrinsic ("reopt.isQNaN." ++ show (pretty frep)) (L.iT 1) [typ]
     v' <- valueToLLVM v         
     liftBB $ L.call op [v']

   FPIsSNaN frep v -> do
     let op = intrinsic ("reopt.isSNaN." ++ show (pretty frep)) (L.iT 1) [typ]
     v' <- valueToLLVM v         
     liftBB $ L.call op [v']

   FPAdd frep x y -> fpbinop L.fadd frep x y
   FPAddRoundedUp _frep _x _y -> unimplementedInstr   
   FPSub frep x y -> fpbinop L.fsub frep x y
   FPSubRoundedUp _frep _x _y -> unimplementedInstr
   FPMul frep x y -> fpbinop L.fmul frep x y
   FPMulRoundedUp _frep _x _y -> unimplementedInstr
   FPDiv frep x y -> fpbinop L.fdiv frep x y
   -- FIXME: do we want ordered or unordered here?  The differ in how
   -- they treat QNaN
   FPLt  frep x y -> fpbinop (L.fcmp L.Fult) frep x y
   -- FIXME: similarly, we probably want oeq here (maybe?)
   FPEq  frep x y -> fpbinop (L.fcmp L.Foeq) frep x y
   FPCvt from_rep x to_rep -> do
     x' <- valueToLLVM x
     let from_typ  = floatReprToLLVMType from_rep
         to_typ    = floatReprToLLVMType to_rep
         from_bits = natValue $ floatInfoBits from_rep
         to_bits   = natValue $ floatInfoBits to_rep         
     fp_x <- liftBB $ L.bitcast x' from_typ
     case compare from_bits to_bits of
      LT -> liftBB $ L.fpext fp_x to_typ
      EQ -> return fp_x
      GT -> liftBB $ L.fptrunc fp_x to_typ
   -- FIXME
   FPCvtRoundsUp _from_rep _x _to_rep -> unimplementedInstr
   FPFromBV v frepr -> do
     v' <- valueToLLVM v
     liftBB $ L.sitofp v' (floatReprToLLVMType frepr)
   -- FIXME: side-conditions here
   TruncFPToSignedBV frepr v sz -> do
     v' <- valueToLLVM v
     let typ = floatReprToLLVMType frepr
     flt_v <- liftBB $ L.bitcast v' typ
     liftBB $ L.fptosi flt_v (natReprToLLVMType sz)
  where
    intrinsicOverflows op _sz x y c = do
      x' <- valueToLLVM x
      y' <- valueToLLVM y
      let in_typ = L.typedType x'
          op_with_overflow =
            intrinsic ("llvm." ++ op ++ ".with.overflow." ++ show (L.ppType in_typ))
            (L.Struct [in_typ, L.iT 1]) [in_typ, in_typ]
      c' <- liftBB . flip L.zext in_typ =<< valueToLLVM c
      liftBB $ do
        r_tuple  <- L.call op_with_overflow [x', y']
        r        <- L.extractValue r_tuple 0
        over     <- L.extractValue r_tuple 1
        r_tuple' <- L.call op_with_overflow [r, c']
        over'    <- L.extractValue r_tuple' 1
        L.bor over over'

    fpbinop :: (L.Typed L.Value -> L.Typed L.Value -> BB (L.Typed L.Value))
             -> FloatInfoRepr flt -> FnValue (FloatType flt) -> FnValue (FloatType flt)
             -> ToLLVM (L.Typed L.Value)
    fpbinop f frepr x y = do
      x' <- valueToLLVM x
      y' <- valueToLLVM y
      let typ = floatReprToLLVMType frepr
      liftBB $ do
        flt_x <- L.bitcast x' typ
        flt_y <- L.bitcast y' typ
        f flt_x flt_y

    -- unop :: (L.Typed L.Value -> BB (L.Typed L.Value))
    --         -> Value (BVType n)
    --         -> BB (L.Typed L.Value)
    -- unop f x = join $ f <$> valueToLLVM x
    
    binop :: (L.Typed L.Value -> L.Typed L.Value -> BB (L.Typed L.Value))
             -> FnValue (BVType n) -> FnValue (BVType m)
             -> ToLLVM (L.Typed L.Value)
    binop f x y = do
      x' <- valueToLLVM x
      y' <- valueToLLVM y
      liftBB $ f x' y'

    typ = typeToLLVMType $ appType app

natReprToLLVMType :: NatRepr n -> L.Type
natReprToLLVMType = L.PrimType . L.Integer . fromIntegral . natValue

typeToLLVMType :: TypeRepr tp -> L.Type
typeToLLVMType (BVTypeRepr n) = natReprToLLVMType n

floatReprToLLVMType :: FloatInfoRepr flt -> L.Type
floatReprToLLVMType fir = L.PrimType . L.FloatType $
  case fir of
    HalfFloatRepr         -> L.Half
    SingleFloatRepr       -> L.Float
    DoubleFloatRepr       -> L.Double
    QuadFloatRepr         -> L.Fp128
    X86_80FloatRepr       -> L.X86_fp80
   

-- stmtLocToLLVM :: StmtLoc (Value (BVType 64)) tp -> BB (L.Typed L.Value)
-- stmtLocToLLVM sloc =
--   case sloc of
--    MemLoc ptr typ -> do p <- valueToLLVM ptr
--                         L.inttoptr p (L.ptrT (typeToLLVMType typ))
--    _ -> unimplementedInstr

valueToLLVM :: FnValue tp -> ToLLVM (L.Typed L.Value)
valueToLLVM val =  
  case val of
    FnValueUnsupported _  -> unimplementedInstr
    -- A value that is actually undefined, like a non-argument register at
    -- the start of a function.
    FnUndefined _ -> mk L.ValUndef
    FnConstantValue _sz n -> mk $ L.integer n
    -- Value from an assignment statement.
    FnAssignedValue (FnAssignment lhs _rhs) ->
      mk $ L.ValIdent $ assignIdToLLVMIdent lhs
    -- Value from a phi node
    FnPhiValue (FnPhiVar lhs _tp) ->
      mk $ L.ValIdent $ assignIdToLLVMIdent lhs      
    -- A value returned by a function call (rax/xmm0)
    FnReturn (FnReturnVar lhs _tp) ->
      mk $ L.ValIdent $ assignIdToLLVMIdent lhs

    -- The entry pointer to a function.
    FnFunctionEntryValue addr ->
      mk $ L.ValSymbol (functionName addr)
      
    -- A pointer to an internal block at the given address.
    FnBlockValue addr ->
      mk $ L.ValLabel $ L.Named $ blockName $ GeneratedBlock addr 0
      
    -- Value is an interget argument passed via a register.
    FnIntArg n -> asks ((!! n) . llvmIntArgs)
                     
    -- Value is a function argument passed via a floating point XMM
    -- register.
    FnFloatArg n -> asks ((!! n) . llvmFloatArgs)
    -- A global address
    FnGlobalDataAddr addr -> mk $ L.integer (fromIntegral addr)
  where
    mk :: L.Value -> ToLLVM (L.Typed L.Value)
    mk  = return . L.Typed typ
    typ = typeToLLVMType $ fnValueType val
