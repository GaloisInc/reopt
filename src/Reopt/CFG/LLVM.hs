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

module Reopt.CFG.LLVM (functionToLLVM) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector as V 
import           Numeric (showHex)
import           Text.LLVM (BB, LLVM)
import qualified Text.LLVM as L
import           Text.PrettyPrint.ANSI.Leijen (pretty)

import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some

import           Reopt.CFG.FnRep
import           Reopt.CFG.Representation
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types

--------------------------------------------------------------------------------
-- reopt runtime
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
iWrite_X87_PC = intrinsic "reopt.Write_X87_PC" L.voidT [L.iT 2]

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

-- FIXME: use personalities
iSystemCall :: L.Typed L.Value
iSystemCall = intrinsic "reopt.SystemCall" (L.Struct [L.iT 64, L.iT 1]) argTypes
  where
    -- the +1 is for the additional syscall no. register, which is
    -- passed via the stack.
    argTypes = replicate (length x86SyscallArgumentRegisters + 1) (L.iT 64)

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
                  , iSystemCall
                  ]
                  
--------------------------------------------------------------------------------
-- LLVM intrinsics
--------------------------------------------------------------------------------

llvmIntrinsics :: [L.Typed L.Value]
llvmIntrinsics = [ intrinsic ("llvm." ++ bop ++ ".with.overflow." ++ show (L.ppType in_typ))
                   (L.Struct [in_typ, L.iT 1]) [in_typ, in_typ]
                   | bop <- [ "uadd", "sadd", "usub", "ssub" ]
                   , in_typ <- map L.iT [8, 16, 32, 64] ]
                 ++
                 [ intrinsic ("llvm." ++ uop ++ "." ++ show (L.ppType typ)) typ [typ, L.iT 1]
                 | uop  <- ["cttz", "ctlz"]
                 , typ <- map L.iT [8, 16, 32, 64] ]

declareIntrinsic :: L.Typed L.Value -> LLVM ()
declareIntrinsic (L.Typed (L.PtrTo (L.FunTy rty argtys _)) (L.ValSymbol sym))
  = void $ L.declare rty sym argtys False
declareIntrinsic _ = error "Not an intrinsic"    

--------------------------------------------------------------------------------
-- conversion to LLVM 
--------------------------------------------------------------------------------

data LLVMState = LLVMState { llvmIntArgs   :: [L.Typed L.Value]
                           , llvmFloatArgs :: [L.Typed L.Value]
                           }

newtype ToLLVM a  = ToLLVM { runToLLVM :: StateT LLVMState BB a }
                    deriving (Applicative, Functor, Monad, MonadState LLVMState)

liftBB :: BB a -> ToLLVM a
liftBB = ToLLVM . lift

liftBBF :: (BB a -> BB b) -> ToLLVM a -> ToLLVM b
liftBBF f v = ToLLVM $ mapStateT (\bb -> do { (a, s) <- bb; b <- f (return a); return (b, s) }) (runToLLVM v)

functionName :: CodeAddr -> L.Symbol
functionName addr = L.Symbol $ "F" ++ showHex addr ""

blockName :: BlockLabel -> L.Ident
blockName = L.Ident . show 

-- The type of FP arguments and results.  We actually want fp128, but
-- it looks like llvm (at least as of version 3.6.2) doesn't put fp128
-- into xmm0 on a return, whereas it does for <2 x double>

functionFloatType :: L.Type
functionFloatType = L.Vector 2 (L.PrimType $ L.FloatType L.Double)

castFromFunctionFloatType, castToFunctionFloatType :: L.Typed L.Value -> BB (L.Typed L.Value)
castToFunctionFloatType v = L.bitcast v functionFloatType
castFromFunctionFloatType v = L.bitcast v (L.iT 128)

functionTypeArgTypes :: FunctionType -> [L.Type]
functionTypeArgTypes ft = replicate (fnNIntArgs ft) (L.iT 64)
                          ++ replicate (fnNFloatArgs ft) functionFloatType

functionTypeReturnType :: FunctionType -> L.Type
functionTypeReturnType _ = funReturnType

functionTypeToLLVM :: FunctionType -> L.Type
functionTypeToLLVM ft = L.ptrT (L.FunTy (functionTypeReturnType ft) (functionTypeArgTypes ft) False)

funReturnType :: L.Type
funReturnType = L.Struct $ (map (typeToLLVMType . N.registerType) x86ResultRegisters)
                            ++ (replicate (length x86FloatResultRegisters) functionFloatType)

-- | This is a special label used for e.g. table lookup defaults (where we should never reach).
-- For now it will just loop.
failLabel :: L.Ident
failLabel = L.Ident "failure"

makeFailBlock :: BB ()
makeFailBlock = do L.label failLabel
                   L.jump  failLabel

entryLabel :: L.Ident
entryLabel = L.Ident "entry"

makeEntryBlock :: [FnBlock] -> [L.Typed L.Value] -> ToLLVM ()
-- No blocks, do nothing (should we return?)
makeEntryBlock [] _ = return ()
makeEntryBlock (first:_) fargs = do
  liftBB $ L.label entryLabel
  fargs' <- mapM (liftBB . castFromFunctionFloatType) fargs
  modify (\s -> s { llvmFloatArgs = fargs' })
  liftBB $ L.jump (blockName $ fbLabel first)
  
-- We have each function return all possible results, although only the ones that are actually
-- used (we use undef for the others).  This makes the LLVM conversion slightly simpler.
functionToLLVM :: Function -> LLVM (L.Typed L.Value)
functionToLLVM f = do
  mapM_ declareIntrinsic reoptIntrinsics
  mapM_ declareIntrinsic llvmIntrinsics
  let refs = Map.delete (fnAddr f) (foldFnValue findReferencedFunctions f)
  itraverse_ (\addr ft ->
               L.declare (functionTypeReturnType ft) (functionName addr) (functionTypeArgTypes ft) False) refs
  L.define' L.emptyFunAttrs retType symbol argTypes False go
  where
    argTypes      = functionTypeArgTypes (fnType f)
    retType       = functionTypeReturnType (fnType f)
    
    symbol        = functionName (fnAddr f)
    go args =
      let nint = fnNIntArgs $ fnType f
          st   = LLVMState { llvmIntArgs   = take nint args
                           , llvmFloatArgs = error "uninitialised float args"
                           }
          makeBlocks = do makeEntryBlock (fnBlocks f) (drop nint args)
                          mapM_ blockToLLVM (fnBlocks f)
                          liftBB makeFailBlock 
      in evalStateT (runToLLVM makeBlocks) st

findReferencedFunctions :: FnValue tp -> Map CodeAddr FunctionType
findReferencedFunctions (FnFunctionEntryValue ft addr) = Map.singleton addr ft
findReferencedFunctions _ = mempty

blockToLLVM :: FnBlock -> ToLLVM () -- L.BasicBlock
blockToLLVM b = do liftBB $ L.label (blockName $ fbLabel b)
                   MapF.foldrWithKey phiToLLVM (return ()) (fbPhiNodes b)
                   mapM_ stmtToLLVM $ fbStmts b -- ++ [termStmtToLLVM $ blockTerm b]
                   termStmtToLLVM (fbTerm b)
  where
    phiToLLVM phi ni l =
      do void l
         void $ liftBBF (L.assign (assignIdToLLVMIdent $ unFnPhiVar phi))
                        (liftBB . L.phi (typeToLLVMType $ fnPhiVarType phi) =<< mapM goLbl (unFnPhiNodeInfo ni))
         return ()

    goLbl (lbl, node) = do v <- valueToLLVM node
                           return (L.from v (L.Named $ blockName lbl))

-- Pads the given list of values to be the target lenght using undefs
padUndef :: L.Type -> Int -> [L.Typed L.Value] -> [L.Typed L.Value]
padUndef typ len xs = xs ++ (replicate (len - length xs) (L.Typed typ L.ValUndef))

makeRet :: [ L.Typed L.Value ] -> [ L.Typed L.Value ] -> ToLLVM ()
makeRet grets frets = do
  -- clang constructs something like
  -- %3 = insertvalue { i64, i64 } undef, i64 %1, 0
  -- %4 = insertvalue { i64, i64 } %3, i64 %2, 1
  -- ret { i64, i64 } %4
  -- which we will duplicate, with undef padding where required.

  -- cast fp results to the required type
  cfrets <- mapM (liftBB . castToFunctionFloatType) frets
  let frets' = padUndef functionFloatType (length x86FloatResultRegisters) cfrets
  -- construct the return result struct
  v <- ifoldlM (\n acc fld -> liftBB $ L.insertValue acc fld (fromIntegral n)) initUndef (grets' ++ frets')
  liftBB $ L.ret v
  where
    initUndef = L.Typed funReturnType L.ValUndef
    grets'    = padUndef (L.iT 64) (length x86ResultRegisters) grets

termStmtToLLVM :: FnTermStmt -> ToLLVM ()
termStmtToLLVM tm =
  case tm of
     FnJump lbl -> liftBB $ L.jump (blockName lbl)
     FnRet (grets, frets) -> do
       grets' <- mapM valueToLLVM grets
       frets' <- mapM valueToLLVM frets
       makeRet grets' frets'
     FnBranch cond tlbl flbl -> do
       cond' <- valueToLLVM cond
       liftBB $ L.br cond' (blockName tlbl) (blockName flbl)
       
     FnCall dest (gargs, fargs) (gretvs, fretvs) contlbl -> do
       let arg_tys = replicate (length gargs) (L.iT 64)
                     ++ replicate (length fargs) functionFloatType
           ret_tys = funReturnType
           fun_ty  = L.ptrT (L.FunTy ret_tys arg_tys False)
           
       dest_f <- case dest of
                   -- FIXME: use ft here instead?
                   FnFunctionEntryValue _ft addr -> 
                     return $ L.Typed fun_ty (L.ValSymbol (functionName addr))
                          
                   _ -> do dest' <- valueToLLVM dest
                           liftBB $ L.inttoptr dest' fun_ty

       gargs' <- mapM valueToLLVM gargs
       fargs' <- mapM (\x -> valueToLLVM x >>= liftBB . castToFunctionFloatType) fargs
       let args' = gargs' ++ fargs'

       retv <- liftBB $ L.call dest_f args'
                 
       case contlbl of
         Nothing -> liftBB $ L.ret retv
         Just lbl -> liftBB $ do
           -- Assign all return variables to the extracted result
           itraverse_ (\i v -> L.assign (assignIdToLLVMIdent $ frAssignId v)
                                        (L.extractValue retv (fromIntegral i)))
                      gretvs
           itraverse_ (\i v -> do retv' <- L.extractValue retv
                                              (fromIntegral (i + length x86ResultRegisters))
                                  L.assign (assignIdToLLVMIdent $ frAssignId v)
                                           (castFromFunctionFloatType retv'))
                      fretvs
           L.jump (blockName lbl)

     FnSystemCall call_no name args rets lbl -> do
       args'  <- mapM valueToLLVM args
     -- We put the call no at the end (on the stack) so we don't need to shuffle all the args.       
       let allArgs = padUndef (L.iT 64) (length x86SyscallArgumentRegisters) args'
                     ++ [ L.Typed (L.iT 64) (L.integer $ fromIntegral call_no) ]
                     
       liftBB $ do
           L.comment name
           rvar <- L.call iSystemCall allArgs
           -- Assign all return variables to the extracted result
           itraverse_ (\i (Some v) ->
                        void $ L.assign (assignIdToLLVMIdent $ frAssignId v)
                                        (L.extractValue rvar $ fromIntegral i))
                      rets
           L.jump (blockName lbl)

     FnLookupTable idx vec -> do
         idx' <- valueToLLVM idx
         let dests = map (blockName . mkRootBlockLabel) $ V.toList vec
         liftBB $ L.switch idx' failLabel (zip [0..] dests)
         
     FnTermStmtUndefined -> void $ unimplementedInstr L.voidT "FnTermStmtUndefined"

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

   FnComment _str -> return () -- L.comment $ Text.unpack str
   -- PlaceHolderStmt {} -> void $ unimplementedInstr
   -- _           -> void $ unimplementedInstr

assignIdToLLVMIdent :: AssignId -> L.Ident
assignIdToLLVMIdent aid = L.Ident $ "R" ++ show aid

unimplementedInstr :: L.Type -> String -> ToLLVM (L.Typed L.Value)
unimplementedInstr typ reason = do liftBB $ L.comment ("UNIMPLEMENTED: " ++ reason)
                                   return (L.Typed typ L.ValUndef)

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
     alloc_ptr <- liftBB $ L.alloca (L.iT 8) (Just v') Nothing
     liftBB $ L.ptrtoint alloc_ptr (L.iT 64)
     
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
   MMXExtend _v -> unimplementedInstr typ "MMXExtend"
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
   AndApp{}     -> unimplementedInstr typ "AndApp"
   OrApp{}      -> unimplementedInstr typ "OrApp"
   NotApp{}     -> unimplementedInstr typ "NotApp"
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
   ReverseBytes{} -> unimplementedInstr typ "ReverseBytes"
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
   Bsf _sz v -> do
     let cttz = intrinsic ("llvm.cttz." ++ show (L.ppType typ)) typ [typ, L.iT 1]
     v' <- valueToLLVM v
     liftBB $ L.call cttz [v', L.iT 1 L.-: L.int 1]
   Bsr _sz v -> do
     let ctlz = intrinsic ("llvm.ctlz." ++ show (L.ppType typ)) typ [typ, L.iT 1]
     v' <- valueToLLVM v
     liftBB $ L.call ctlz [v', L.iT 1 L.-: L.int 1]

   FPIsQNaN frep v -> do
     let isQNaN = intrinsic ("reopt.isQNaN." ++ show (pretty frep)) (L.iT 1) [typ]
     v' <- valueToLLVM v         
     liftBB $ L.call isQNaN [v']

   FPIsSNaN frep v -> do
     let isSNaN = intrinsic ("reopt.isSNaN." ++ show (pretty frep)) (L.iT 1) [typ]
     v' <- valueToLLVM v         
     liftBB $ L.call isSNaN [v']

   FPAdd frep x y -> fpbinop L.fadd frep x y
   FPAddRoundedUp _frep _x _y -> unimplementedInstr typ "FPAddRoundedUp"   
   FPSub frep x y -> fpbinop L.fsub frep x y
   FPSubRoundedUp _frep _x _y -> unimplementedInstr typ "FPSubRoundedUp"
   FPMul frep x y -> fpbinop L.fmul frep x y
   FPMulRoundedUp _frep _x _y -> unimplementedInstr typ "FPMulRoundedUp"
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
   FPCvtRoundsUp _from_rep _x _to_rep -> unimplementedInstr typ "FPCvtRoundsUp"
   FPFromBV v frepr -> do
     v' <- valueToLLVM v
     liftBB $ L.sitofp v' (floatReprToLLVMType frepr)
   -- FIXME: side-conditions here
   TruncFPToSignedBV frepr v sz -> do
     v' <- valueToLLVM v
     let ftyp = floatReprToLLVMType frepr
     flt_v <- liftBB $ L.bitcast v' ftyp
     liftBB $ L.fptosi flt_v (natReprToLLVMType sz)
  where
    intrinsicOverflows bop _sz x y c = do
      x' <- valueToLLVM x
      y' <- valueToLLVM y
      let in_typ = L.typedType x'
          op_with_overflow =
            intrinsic ("llvm." ++ bop ++ ".with.overflow." ++ show (L.ppType in_typ))
            (L.Struct [in_typ, L.iT 1]) [in_typ, in_typ]
      c' <- liftBB . flip L.zext in_typ =<< valueToLLVM c
      liftBB $ do
        r_tuple    <- L.call op_with_overflow [x', y']
        r          <- L.extractValue r_tuple 0
        overflows  <- L.extractValue r_tuple 1
        r_tuple'   <- L.call op_with_overflow [r, c']
        overflows' <- L.extractValue r_tuple' 1
        L.bor overflows overflows'

    fpbinop :: (L.Typed L.Value -> L.Typed L.Value -> BB (L.Typed L.Value))
             -> FloatInfoRepr flt -> FnValue (FloatType flt) -> FnValue (FloatType flt)
             -> ToLLVM (L.Typed L.Value)
    fpbinop f frepr x y = do
      x' <- valueToLLVM x
      y' <- valueToLLVM y
      let typ' = floatReprToLLVMType frepr
      liftBB $ do
        flt_x <- L.bitcast x' typ'
        flt_y <- L.bitcast y' typ'
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
    FnValueUnsupported reason _
      -> unimplementedInstr typ ("FnValueUnsupported: " ++ reason)
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

    -- The entry pointer to a function.  We do the cast as a const
    -- expr as function addresses appear as constants in e.g. phi
    -- nodes
    FnFunctionEntryValue ft addr ->
      let fptr :: L.Typed L.Value
          fptr = L.Typed (functionTypeToLLVM ft) (L.ValSymbol (functionName addr))
      in mk $ L.ValConstExpr (L.ConstConv L.PtrToInt fptr typ)
         
    -- A pointer to an internal block at the given address.
    FnBlockValue addr ->
      mk $ L.ValLabel $ L.Named $ blockName $ mkRootBlockLabel addr
      
    -- Value is an interget argument passed via a register.
    FnIntArg n -> gets ((!! n) . llvmIntArgs)
                     
    -- Value is a function argument passed via a floating point XMM
    -- register.
    FnFloatArg n -> gets ((!! n) . llvmFloatArgs)
    -- A global address
    FnGlobalDataAddr addr -> mk $ L.integer (fromIntegral addr)
  where
    mk :: L.Value -> ToLLVM (L.Typed L.Value)
    mk  = return . L.Typed typ
    typ = typeToLLVMType $ fnValueType val
