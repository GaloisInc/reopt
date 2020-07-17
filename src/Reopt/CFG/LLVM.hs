{-|
This provides functionality for converting the
architecture-independent components of @FnReop@ functions into LLVM.

It exports a fairly large set of capabilities so that all
architecture-specific functionality can be implemented on top of this
layer.

-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
module Reopt.CFG.LLVM
  ( moduleForFunctions
  , LLVMGenOptions(..)
    -- * Internals for implement architecture specific functions
  , LLVMArchSpecificOps(..)
  , LLVMArchConstraints
  , HasValue
  , functionTypeToLLVM
  , Intrinsic
  , intrinsic
  , FunLLVMContext(archFns, funLLVMGenOptions)
  , BBLLVM
  , typeToLLVMType
  , setAssignIdValue
  , padUndef
  , arithop
  , convop
  , bitcast
  , band
  , bor
  , shl
  , icmpop
  , llvmAsPtr
  , extractValue
  , insertValue
  , call
  , call_
  , mkLLVMValue
  , ret
  , effect
  , AsmAttrs
  , noSideEffect
  , sideEffect
  , callAsm
  , callAsm_
  , llvmITypeNat
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable
import           Data.Int
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Parameterized.Classes
import qualified Data.Parameterized.List as PL
import           Data.Parameterized.Some
import           Data.Parameterized.TraversableF
import           Data.Proxy
import qualified Data.Text as Text
import qualified Data.Vector as V
import           Data.Word
import           GHC.Stack
import           GHC.TypeLits
import           Numeric.Natural
import qualified Text.LLVM as L
import qualified Text.LLVM.PP as L (ppType)
import           Text.PrettyPrint.ANSI.Leijen (pretty)
import           Text.Printf

import           Data.Macaw.AbsDomain.StackAnalysis (BoundLoc(..))
import           Data.Macaw.CFG
import           Data.Macaw.Types
import           Data.Macaw.X86

import           Reopt.CFG.FnRep
import qualified Reopt.VCG.Annotations as Ann

-- | Return a LLVM type for a integer with the given width.
llvmITypeNat :: Natural -> L.Type
llvmITypeNat w | w <= fromIntegral (maxBound :: Int32) = L.PrimType (L.Integer wn)
               | otherwise = error $ "llvmITypeNat given bad width " ++ show w
  where wn :: Int32
        wn = fromIntegral w

natReprToLLVMType :: NatRepr n -> L.Type
natReprToLLVMType = llvmITypeNat . natValue

------------------------------------------------------------------------
-- HasValue

class HasValue v where
  valueOf :: v -> L.Typed L.Value

instance HasValue (L.Typed L.Value) where
  valueOf = id

------------------------------------------------------------------------
-- Intrinsic

-- | An LLVM intrinsic
data Intrinsic = Intrinsic { intrinsicName :: !L.Symbol
                           , intrinsicRes  :: L.Type
                           , intrinsicArgs  :: [L.Type]
                           , intrinsicAttrs :: [L.FunAttr]
                           }

instance HasValue Intrinsic where
  valueOf i = L.Typed tp (L.ValSymbol (intrinsicName i))
    where tp = L.ptrT $ L.FunTy (intrinsicRes i) (intrinsicArgs i) False

-- | Define an intrinsic that has no special attributes
intrinsic :: String -> L.Type -> [L.Type] -> Intrinsic
intrinsic name res args = intrinsic' name res args []

-- | Define an intrinsic that has no special attributes
intrinsic' :: String -> L.Type -> [L.Type] -> [L.FunAttr] -> Intrinsic
intrinsic' name res args attrs = Intrinsic { intrinsicName = L.Symbol name
                                           , intrinsicRes  = res
                                           , intrinsicArgs = args
                                           , intrinsicAttrs = attrs
                                           }

--------------------------------------------------------------------------------
-- LLVM intrinsics
--------------------------------------------------------------------------------

-- | LLVM arithmetic with overflow intrinsic.
overflowOp :: String -> L.Type -> Intrinsic
overflowOp bop in_typ =
  intrinsic ("llvm." ++ bop ++ ".with.overflow." ++ show (L.ppType in_typ))
            (L.Struct [in_typ, L.iT 1])
            [in_typ, in_typ]

-- | @llvm.masked.load.*@ intrinsic
llvmMaskedLoad :: Int32 -- ^ Number of vector elements
                -> String -- ^ Type name (e.g. i32)
                -> L.Type -- ^ Element type (should match string)
                -> Intrinsic
llvmMaskedLoad n tp tpv = do
 let vstr = "v" ++ show n ++ tp
     mnem = "llvm.masked.load." ++ vstr ++ ".p0" ++ vstr
     args = [ L.PtrTo (L.Vector n tpv), L.iT 32, L.Vector n (L.iT 1), L.Vector n tpv ]
  in intrinsic mnem (L.Vector n tpv) args

-- | @llvm.masked.store.*@ intrinsic
llvmMaskedStore :: Int32 -- ^ Number of vector elements
                -> String -- ^ Type name (e.g. i32)
                -> L.Type -- ^ Element type (should match string)
                -> Intrinsic
llvmMaskedStore n tp tpv = do
 let vstr = "v" ++ show n ++ tp
     mnem = "llvm.masked.store." ++ vstr ++ ".p0" ++ vstr
     args = [ L.PtrTo (L.Vector n tpv), L.iT 32, L.Vector n (L.iT 1), L.Vector n tpv ]
  in intrinsic mnem (L.Vector n tpv) args

llvmIntrinsics :: [Intrinsic]
llvmIntrinsics = [ overflowOp bop in_typ
                   | bop <- [ "uadd", "sadd", "usub", "ssub" ]
                   , in_typ <- map L.iT [4, 8, 16, 32, 64] ]
                 ++
                 [ intrinsic ("llvm." ++ uop ++ "." ++ show (L.ppType typ)) typ [typ, L.iT 1]
                 | uop  <- ["cttz", "ctlz"]
                 , typ <- map L.iT [8, 16, 32, 64] ]

--------------------------------------------------------------------------------
-- conversion to LLVM
--------------------------------------------------------------------------------

-- The type of FP arguments and results.  We actually want fp128, but
-- it looks like llvm (at least as of version 3.6.2) doesn't put fp128
-- into xmm0 on a return, whereas it does for <2 x double>

llvmFloatType :: FloatInfoRepr flt -> L.FloatType
llvmFloatType flt =
  case flt of
    HalfFloatRepr -> L.Half
    SingleFloatRepr -> L.Float
    DoubleFloatRepr -> L.Double
    QuadFloatRepr -> L.Fp128
    X86_80FloatRepr -> L.X86_fp80

typeToLLVMType :: TypeRepr tp -> L.Type
typeToLLVMType BoolTypeRepr   = L.iT 1
typeToLLVMType (BVTypeRepr n) = llvmITypeNat (natValue n)
typeToLLVMType (FloatTypeRepr flt) = L.PrimType $ L.FloatType $ llvmFloatType flt
typeToLLVMType (TupleTypeRepr s) = L.Struct (toListFC typeToLLVMType s)
typeToLLVMType (VecTypeRepr w tp)
   | toInteger (natValue w) <= toInteger (maxBound :: Int32) =
     L.Vector (fromIntegral (natValue w)) (typeToLLVMType tp)
   | otherwise = error $ "Vector width of " ++ show w ++ " is too large."


-- | This is a special label used for switch statement defaults.
--
-- We should never actually reach these blocks, and with some
-- additional analysis we can probably eliminate the need for ever
-- generating them, however at the moment we use them.
switchFailLabel :: String
switchFailLabel = "failure"

-- | Block for fail label.
failBlock :: L.BasicBlock
failBlock = L.BasicBlock { L.bbLabel = Just $ L.Named $ L.Ident switchFailLabel
                         , L.bbStmts = [L.Effect L.Unreachable []]
                         }

------------------------------------------------------------------------
-- Return handing

-- | Get LLVM type for a function that returns the given values.
llvmFunctionReturnType :: FunctionType arch -> L.Type
llvmFunctionReturnType ftp =
  case fnReturnType ftp of
    Nothing -> L.PrimType L.Void
    Just (Some rtp) -> typeToLLVMType rtp

------------------------------------------------------------------------

functionTypeToLLVM :: FunctionType arch -> L.Type
functionTypeToLLVM ft = L.ptrT $
  L.FunTy (llvmFunctionReturnType ft)
          (viewSome typeToLLVMType <$> fnArgTypes ft)
          (fnVarArgs ft)

declareFunction :: FunctionDecl arch
                -> L.Declare
declareFunction d =
  let ftp = funDeclType d
   in L.Declare { L.decRetType = llvmFunctionReturnType ftp
                , L.decName    = L.Symbol (BSC.unpack (funDeclName d))
                , L.decArgs    = viewSome typeToLLVMType <$> fnArgTypes ftp
                , L.decVarArgs = fnVarArgs ftp
                , L.decAttrs   = []
                , L.decComdat  = Nothing
                }

-- Pads the given list of values to be the target lenght using undefs
padUndef :: L.Type -> Int -> [L.Typed L.Value] -> [L.Typed L.Value]
padUndef typ len xs = xs ++ (replicate (len - length xs) (L.Typed typ L.ValUndef))

-- | Result obtained by printing a block to LLVM
data LLVMBlockResult arch =
  LLVMBlockResult { fnBlock :: !(FnBlock arch)
                    -- ^ Fn block to create this.
                  , llvmPhiVars :: !(V.Vector (PhiBinding arch))
                    -- ^ Vector phi variables
                  , finalBBState :: !(BBLLVMState arch)
                  }


type PhiValues = Map L.BlockLabel L.Value

-- | Contains a vector with one element for each LLVM blocks to the
-- results for that label.
type BlockPhiAssignment = V.Vector PhiValues

-- | Maps LLVM blocks to the phi assignment for them.
type ResolvePhiMap w = Map (FnBlockLabel w) BlockPhiAssignment

-- | Create an initial phi map from the function.
--
-- This does not propagate values.
initResolvePhiMap :: Function arch -> ResolvePhiMap (ArchAddrWidth arch)
initResolvePhiMap f =
  Map.fromList [ (fbLabel b, V.replicate phiCnt Map.empty)
               | b <- fnBlocks f
               , let phiCnt = V.length (fbPhiVars b)
               ]

-- | Return the phi assignment associated with the given target label.
--
-- Note. This needs the expected number of phi variables in case
-- the phi variables have not yet been initialized.
phiAssignmentForBlock :: ResolvePhiMap w
                      -> FnBlockLabel w
                      -> BlockPhiAssignment
phiAssignmentForBlock m l =
    Map.findWithDefault (error emsg) l m
  where emsg = "Could not find phi assignment."

------------------------------------------------------------------------
-- IntrinsicMap

-- | Map from intrinsic name to intrinsic seen so far
--
-- Globally maintained when translating model so that we know which
-- definitions to include.
type IntrinsicMap = Map L.Symbol Intrinsic

------------------------------------------------------------------------
-- FunStateAnn

-- | Map from assign ID to LLVM value
type AssignValMap = Map FnAssignId (L.Typed L.Value)

-- | State relative to a function.
data FunState arch =
  FunState { nmCounter :: !Int
             -- ^ Counter for generating new identifiers
           , funIntrinsicMap :: !IntrinsicMap
             -- ^ Collection used for tracking which intrinsics we need.
           , needSwitchFailLabel :: !Bool
             -- ^ Flag that indicates if we need the fail block for a
             -- switch statement.
           , funBlockPhiMap :: !(ResolvePhiMap (ArchAddrWidth arch))
             -- ^ Maps blocks to the phi nodes for that block.
           }

funIntrinsicMapLens :: Simple Lens (FunState arch) IntrinsicMap
funIntrinsicMapLens = lens funIntrinsicMap (\s v -> s { funIntrinsicMap = v })

funBlockPhiMapLens :: Lens' (FunState arch) (ResolvePhiMap (ArchAddrWidth arch))
funBlockPhiMapLens = lens funBlockPhiMap (\s v -> s { funBlockPhiMap = v })

------------------------------------------------------------------------
-- Functions for creating LLVM identifiers.

tempIdent :: Int -> L.Ident
tempIdent c = L.Ident ('t' : show c)

------------------------------------------------------------------------
-- LLVMGEneration options

-- | Options for generating LLVM
data LLVMGenOptions =
  LLVMGenOptions { mcExceptionIsUB :: !Bool
                   -- ^ Code with side effects is allowed to result in
                   -- LLVM undefined behavior if machine code would
                   -- have raised an exception.
                 }

------------------------------------------------------------------------
-- BBLLVM

-- | Architecture-specific operations for generating LLVM
data LLVMArchSpecificOps arch = LLVMArchSpecificOps
  { archEndianness :: !Endianness
    -- ^ Endianness of architecture
    --
    -- Some architectures allow endianness to change dynamically,
    -- Macaw does not support this.
  , archFnCallback :: !(forall tp . ArchFn arch (FnValue arch) tp
                        -> BBLLVM arch (L.Typed L.Value))
    -- ^ Callback for architecture-specific functions
  , archStmtCallback :: !(FnArchStmt arch (FnValue arch) -> BBLLVM arch ())
    -- ^ Callback for architecture-specific statements
  , popCountCallback :: !(forall w . NatRepr w -> FnValue arch (BVType w)
                          -> BBLLVM arch (L.Typed L.Value))
    -- ^ Callback for emitting a popcount instruction
    --
    -- This is architecture-specific so that we can choose machine code.
  }

type LLVMArchConstraints arch
   = ( FnArchConstraints arch
     , ShowF (ArchReg arch)
     , OrdF (ArchReg arch)
     , IsArchStmt (FnArchStmt arch)
     , Eq (FunctionType arch)
     )

-- | Information used to generate LLVM for a specific function.
--
-- This information is the same for all blocks within the function.
data FunLLVMContext arch = FunLLVMContext
  { archFns  :: !(LLVMArchSpecificOps arch)
    -- ^ Architecture-specific functions
  , funLLVMGenOptions :: !LLVMGenOptions
    -- ^ Options for generating LLVM
  , funArgs      :: !(V.Vector (L.Typed L.Value))
     -- ^ Arguments to this function.
  , funType :: !(FunctionType arch)
    -- ^ Type for this function
  , funAllocaCount    :: !Int
  , withArchConstraints :: forall a . (LLVMArchConstraints arch => a) -> a
  }

-- | State specific to translating a single basic block.
data BBLLVMState arch = BBLLVMState
  { bbThisLabel :: !L.BlockLabel
    -- ^ The label for the block we are translating
  , bbAssignValMap :: !AssignValMap
    -- ^ Map assignment ids created in this block to the LLVM value.
  , bbStmts :: ![L.Stmt]
    -- ^ Statements added so far with last statement at head of list.
  }


data BBLLVMPair arch = BBLLVMPair { funState :: !(FunState arch)
                                  , bbLLVMState :: !(BBLLVMState arch)
                                  }

funStateLens :: Simple Lens (BBLLVMPair arch) (FunState arch)
funStateLens = lens funState (\s v -> s { funState = v })

newtype BBLLVM arch a = BBLLVM { unBBLLVM :: ReaderT (FunLLVMContext arch) (State (BBLLVMPair arch)) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader (FunLLVMContext arch)
           )

instance MonadState (BBLLVMState arch) (BBLLVM arch) where
  get = BBLLVM $ bbLLVMState <$> get
  put s = BBLLVM $ modify $ \t -> t { bbLLVMState = s }

-- | Generate a fresh identifier with the name 'rX'
freshName :: BBLLVM arch L.Ident
freshName = BBLLVM $ do
  c <- uses funStateLens nmCounter
  modify' $ funStateLens %~ \fs -> fs { nmCounter = c + 1 }
  pure $! tempIdent c

addIntrinsic :: Intrinsic -> BBLLVM arch ()
addIntrinsic i = BBLLVM $ do
  m <- use $ funStateLens . funIntrinsicMapLens
  when (Map.notMember (intrinsicName i) m) $ do
    funStateLens . funIntrinsicMapLens .= Map.insert (intrinsicName i) i m

markNeedSwitchFailLabel :: BBLLVM arch ()
markNeedSwitchFailLabel = BBLLVM $ do
  funStateLens %= \s -> s { needSwitchFailLabel = True }

-- | Append a statement to the list of statements
emitStmt :: L.Stmt -> BBLLVM arch ()
emitStmt stmt = seq stmt $ do
  s <- get
  put $! s { bbStmts = stmt : bbStmts s }

-- | Evaluate an instruction and return the result.
evalInstr :: L.Instr -> BBLLVM arch L.Value
evalInstr i = do
  nm <- freshName
  emitStmt $! L.Result nm i []
  pure $! L.ValIdent nm

-- | Emit an instruction as an effect
effect :: L.Instr -> BBLLVM arch ()
effect i = emitStmt $ L.Effect i []

-- | Add a comment instruction
comment :: String -> BBLLVM arch ()
comment msg = effect (L.Comment msg)

ret :: L.Typed L.Value -> BBLLVM arch ()
ret = effect . L.Ret

unimplementedInstr' :: L.Type -> String -> BBLLVM arch (L.Typed L.Value)
unimplementedInstr' typ reason = do
  comment ("UNIMPLEMENTED: " ++ reason)
  return (L.Typed typ L.ValUndef)

-- | Sets the value of the function assign id.
setAssignIdValue :: HasCallStack
                 => FnAssignId
                 -> L.Typed L.Value
                 -> BBLLVM arch ()
setAssignIdValue fid v = do
  m <- gets $ bbAssignValMap
  case Map.lookup fid m of
    Just{} -> error $ "internal: Assign id " ++ show (pretty fid) ++ " already assigned."
    Nothing -> pure ()
  modify' $ \s -> s { bbAssignValMap = Map.insert fid v (bbAssignValMap s) }

------------------------------------------------------------------------
-- Convert a value to LLVM

-- | Map a function value to a LLVM value with no change.
valueToLLVM :: forall arch tp
            .  HasCallStack
            => FunLLVMContext arch -- ^ Context
            -> AssignValMap -- ^ Map from assignment ids to value.
            -> FnValue arch tp
            -> L.Typed L.Value
valueToLLVM ctx avmap val = withArchConstraints ctx $ do
  let ptrWidth = addrWidthNatRepr (addrWidthRepr (Proxy :: Proxy (ArchAddrWidth arch)))
  case val of
    -- A value that is actually undefined, like a non-argument register at
    -- the start of a function.
    FnUndefined typ -> L.Typed (typeToLLVMType typ) L.ValUndef
    FnConstantBool b -> L.Typed (L.iT 1) $ L.integer (if b then 1 else 0)
    FnConstantValue sz n -> L.Typed (natReprToLLVMType sz) $ L.integer n
    -- Value from an assignment statement.
    FnAssignedValue (FnAssignment lhs _rhs) ->
      case Map.lookup lhs avmap of
        Just v -> v
        Nothing ->
          error $ "Could not find assignment value " ++ show (pretty lhs)
    -- Value from a phi node
    FnPhiValue phiVar -> do
      case Map.lookup (unFnPhiVar phiVar) avmap of
        Just v -> v
        Nothing ->
          error $ "Could not find phi value " ++ show (unFnPhiVar phiVar)
    -- A value returned by a function call (rax/xmm0)
    FnReturn (FnReturnVar lhs _tp) ->
      case Map.lookup lhs avmap of
        Just v -> v
        Nothing ->
          error $ "Could not find return variable " ++ show (pretty lhs)
    -- The entry pointer to a function.  We do the cast as a const
    -- expr as function addresses appear as constants in e.g. phi
    -- nodes
    FnFunctionEntryValue ftp nm -> do
      let typ = natReprToLLVMType ptrWidth
      let fptr :: L.Typed L.Value
          fptr = L.Typed (functionTypeToLLVM ftp) (L.ValSymbol (L.Symbol (BSC.unpack nm)))
      L.Typed typ $ L.ValConstExpr (L.ConstConv L.PtrToInt fptr typ)
    -- Value is an argument passed via a register.
    FnArg i _tp | 0 <= i, i < V.length (funArgs ctx) -> funArgs ctx V.! i
                | otherwise -> error $ "Illegal argument index " ++ show i

mkLLVMValue :: HasCallStack
            => FnValue arch tp
            -> BBLLVM arch (L.Typed L.Value)
mkLLVMValue val = do
  ctx <- ask
  m   <- gets bbAssignValMap
  pure $! valueToLLVM ctx m val

arithop :: L.ArithOp -> L.Typed L.Value -> L.Value -> BBLLVM arch (L.Typed L.Value)
arithop f val s = L.Typed (L.typedType val) <$> evalInstr (L.Arith f val s)

bitop :: L.BitOp -> L.Typed L.Value -> L.Value -> BBLLVM arch (L.Typed L.Value)
bitop f val s = L.Typed (L.typedType val) <$> evalInstr (L.Bit f val s)

-- | Conversion operation
convop :: L.ConvOp -> L.Typed L.Value -> L.Type -> BBLLVM arch (L.Typed L.Value)
convop f val tp = L.Typed tp <$> evalInstr (L.Conv f val tp)

-- | Compare two LLVM values using the given operator.
icmpop :: L.ICmpOp -> L.Typed L.Value -> L.Value -> BBLLVM arch (L.Typed L.Value)
icmpop f val s = do
  L.Typed (L.iT 1) <$> evalInstr (L.ICmp f val s)

-- | Extract a value from a struct
extractValue :: L.Typed L.Value -> Int32 -> BBLLVM arch (L.Typed L.Value)
extractValue ta i = do
  let etp = case L.typedType ta of
              L.Struct fl -> fl !! fromIntegral i
              L.Array _l etp' -> etp'
              _ -> error "extractValue not given a struct or array."
  L.Typed etp <$> evalInstr (L.ExtractValue ta [i])

-- | Insert a valiue into an aggregate
insertValue :: L.Typed L.Value -- ^ Aggregate
            -> L.Typed L.Value -- ^ Value to insert
            -> Int32 -- ^ Index to insert
            -> BBLLVM arch (L.Typed L.Value)
insertValue ta tv i =
  L.Typed (L.typedType ta) <$> evalInstr (L.InsertValue ta tv [i])

-- | Do a bitcast
bitcast :: L.Typed L.Value -> L.Type -> BBLLVM arch (L.Typed L.Value)
bitcast = convop L.BitCast

-- | Unsigned extension
zext :: L.Typed L.Value -> L.Type -> BBLLVM arch (L.Typed L.Value)
zext = convop L.ZExt

band :: L.Typed L.Value -> L.Value -> BBLLVM arch (L.Typed L.Value)
band = bitop L.And

-- | Bitwise inclusive  or
bor :: L.Typed L.Value -> L.Value -> BBLLVM arch (L.Typed L.Value)
bor = bitop L.Or

bxor :: L.Typed L.Value -> L.Value -> BBLLVM arch (L.Typed L.Value)
bxor = bitop L.Xor

shl :: L.Typed L.Value -> L.Value -> BBLLVM arch (L.Typed L.Value)
shl = bitop (L.Shl False False)

ashr :: L.Typed L.Value -> L.Value -> BBLLVM arch (L.Typed L.Value)
ashr = bitop (L.Ashr False)

lshr :: L.Typed L.Value -> L.Value -> BBLLVM arch (L.Typed L.Value)
lshr = bitop (L.Lshr False)

-- | Generate a non-tail call that returns a value
call :: (HasCallStack, HasValue v)
     => v
     -> [L.Typed L.Value]
     -> BBLLVM arch (L.Typed L.Value)
call (valueOf -> f) args =
  case L.typedType f of
    L.PtrTo (L.FunTy res _argTypes _varArgs) -> do
      case res of
        L.PrimType L.Void -> do
          error $ "Call expected to return a value, but returns void."
        _ -> do
          fmap (L.Typed res) $ evalInstr $ L.Call False (L.typedType f) (L.typedValue f) args
    _ -> error $ "Call given non-function pointer argument:\n" ++ show f

-- | Generate a non-tail call that does not return a value
call_ :: HasValue v => v -> [L.Typed L.Value] -> BBLLVM arch ()
call_ (valueOf -> f) args =
  case L.typedType f of
    L.PtrTo (L.FunTy (L.PrimType L.Void) _argTypes _varArgs) -> do
      effect $ L.Call False (L.typedType f) (L.typedValue f) args
    _ -> error $ "call_ given non-function pointer argument\n" ++ show f

-- | Sign extend a boolean value to the given width.
carryValue :: (HasCallStack, 1 <= w)
           => NatRepr w
           -> FnValue arch BoolType
           -> BBLLVM arch (L.Typed L.Value)
carryValue w x = do
  val <- mkLLVMValue x
  if natValue w == 1 then
    pure val
   else
    zext val (llvmITypeNat (natValue w))

-- | Handle an intrinsic overflows
intrinsicOverflows :: (HasCallStack, 1 <= w)
                   => String
                   -> FnValue arch (BVType w)
                   -> FnValue arch (BVType w)
                   -> FnValue arch BoolType
                   -> BBLLVM arch (L.Typed L.Value)
-- Special case where carry/borrow flag is 0.
intrinsicOverflows bop x y (FnConstantBool False) = do
  x' <- mkLLVMValue x
  y' <- mkLLVMValue y
  let in_typ = L.typedType x'
  r_tuple    <- call (overflowOp bop in_typ) [x', y']
  extractValue r_tuple 1
-- General case involves two calls
intrinsicOverflows bop x y c = bbArchConstraints $ do
  x' <- mkLLVMValue x
  y' <- mkLLVMValue y
  let in_typ = L.typedType x'
  r_tuple    <- call (overflowOp bop in_typ) [x', y']
  r          <- extractValue r_tuple 0
  overflows  <- extractValue r_tuple 1
  -- Check for overflow in carry flag
  c' <- carryValue (typeWidth x) c
  r_tuple'   <- call (overflowOp bop in_typ) [r, c']
  overflows' <- extractValue r_tuple' 1
  bor overflows (L.typedValue overflows')


appToLLVM :: forall arch tp
          .  HasCallStack
          => App (FnValue arch) tp
          -> BBLLVM arch (L.Typed L.Value)
appToLLVM app = bbArchConstraints $ do
  let typ = typeToLLVMType $ typeRepr app
  let binop :: (L.Typed L.Value -> L.Value -> BBLLVM arch (L.Typed L.Value))
            -> FnValue arch utp
            -> FnValue arch utp
            -> BBLLVM arch (L.Typed L.Value)
      binop f x y = do
        x' <- mkLLVMValue x
        y' <- mkLLVMValue y
        f x' (L.typedValue y')
  case app of
    Eq x y ->  binop (icmpop L.Ieq) x y
    Mux _tp c t f -> do
      l_c <- mkLLVMValue c
      l_t <- mkLLVMValue t
      l_f <- mkLLVMValue f
      fmap (L.Typed (L.typedType l_t)) $ evalInstr $ L.Select l_c l_t (L.typedValue l_f)
    -- Construct a tuple from fields.
    MkTuple fieldTypes fields -> do
      let structType = L.Struct (toListFC typeToLLVMType fieldTypes)
      let initUndef = L.Typed structType L.ValUndef
      let f :: forall utp
            .  FnValue arch utp
            -> (Int32 -> L.Typed L.Value -> BBLLVM arch (L.Typed L.Value))
            -> (Int32 -> L.Typed L.Value -> BBLLVM arch (L.Typed L.Value))
          f fld c i s = do
            llvmFieldValue <- mkLLVMValue fld
            s' <- insertValue s llvmFieldValue i
            c (i+1) s'
      foldrFC f (\_ r -> pure r) fields 0 initUndef

    -- :: !(P.List TypeRepr l) -> !(f (TupleType l)) -> !(P.Index l r) -> App f r
    TupleField _fieldTypes macawStruct idx -> do
      -- Make a struct
      llvmStruct <- mkLLVMValue macawStruct
      -- Get index as an Int32
      let idxVal :: Integer
          idxVal = PL.indexValue idx
      when (idxVal >= toInteger (maxBound :: Int32)) $
        error $ "Index out of range " ++ show idxVal ++ "."
      -- Extract a value
      extractValue llvmStruct (fromInteger idxVal :: Int32)
    Trunc v sz -> mkLLVMValue v >>= \u -> convop L.Trunc u (natReprToLLVMType sz)
    SExt v sz  -> flip (convop L.SExt)  (natReprToLLVMType sz) =<< mkLLVMValue v
    UExt v sz  -> flip (convop L.ZExt)  (natReprToLLVMType sz) =<< mkLLVMValue v
    Bitcast x tp -> do
      llvmVal <- mkLLVMValue x
      convop L.BitCast llvmVal (typeToLLVMType (widthEqTarget tp))
    AndApp x y -> binop band x y
    OrApp  x y -> binop bor x y
    NotApp x   -> do
      -- x = 0 == complement x, according to LLVM manual.
      llvm_x <- mkLLVMValue x
      icmpop L.Ieq llvm_x (L.ValInteger 0)
    XorApp x y    -> binop bxor x y
    BVAdd _sz x y -> binop (arithop (L.Add False False)) x y
    BVAdc _sz x y (FnConstantBool False) -> do
      binop (arithop (L.Add False False)) x y
    BVAdc _sz x y c -> do
      r <- binop (arithop (L.Add False False)) x y
      arithop (L.Add False False) r . L.typedValue =<< carryValue (typeWidth x) c

    BVSub _sz x y -> binop (arithop (L.Sub False False)) x y
    BVSbb _sz x y (FnConstantBool False) -> do
      binop (arithop (L.Sub False False)) x y
    BVSbb _sz x y b -> do
      d <- binop (arithop (L.Sub False False)) x y
      arithop (L.Sub False False) d . L.typedValue =<< carryValue (typeWidth x) b

    BVMul _sz x y -> binop (arithop (L.Mul False False)) x y
    BVUnsignedLt x y     -> binop (icmpop L.Iult) x y
    BVUnsignedLe x y     -> binop (icmpop L.Iule) x y
    BVSignedLt x y       -> binop (icmpop L.Islt) x y
    BVSignedLe x y       -> binop (icmpop L.Isle) x y
    BVTestBit v n -> do -- TODO: Test this it had a FIX comment with no details.
      llvm_v <- mkLLVMValue v
      let in_typ = L.typedType llvm_v
      n' <- mkLLVMValue n
      n_ext <-
        case compareF (typeWidth v) (typeWidth n) of
          LTF -> error "BVTestBit expected second argument to be at least first"
          EQF -> pure n'
          GTF -> zext n' in_typ
      mask <- shl (L.Typed in_typ (L.ValInteger 1)) (L.typedValue n_ext)
      r <- bitop L.And llvm_v (L.typedValue mask)
      icmpop L.Ine r (L.ValInteger 0)
    BVComplement _sz v -> do
      -- xor x -1 == complement x, according to LLVM manual.
      llvm_v <- mkLLVMValue v
      bitop L.Xor llvm_v (L.ValInteger (-1))
    BVAnd _sz x y -> binop band x y
    BVOr _sz x y  -> binop bor  x y
    BVXor _sz x y -> binop bxor x y
    BVShl _sz x y -> binop shl  x y
    BVSar _sz x y -> binop ashr x y
    BVShr _sz x y -> binop lshr x y
    PopCount w v  -> do
      fn <- asks $ popCountCallback  . archFns
      fn w v
    ReverseBytes{} -> unimplementedInstr' typ "ReverseBytes"
    UadcOverflows x y c -> intrinsicOverflows "uadd" x y c
    SadcOverflows x y c -> intrinsicOverflows "sadd" x y c
    UsbbOverflows x y c -> intrinsicOverflows "usub" x y c
    SsbbOverflows x y c -> intrinsicOverflows "ssub" x y c

    Bsf _sz v -> do
      let cttz = intrinsic ("llvm.cttz." ++ show (L.ppType typ)) typ [typ, L.iT 1]
      v' <- mkLLVMValue v
      call cttz [v', L.iT 1 L.-: L.int 1]
    Bsr _sz v -> do
      let ctlz = intrinsic ("llvm.ctlz." ++ show (L.ppType typ)) typ [typ, L.iT 1]
      v' <- mkLLVMValue v
      call ctlz [v', L.iT 1 L.-: L.int 1]

-- | Evaluate a value as a pointer
llvmAsPtr :: HasCallStack
          => FnValue arch (BVType (ArchAddrWidth arch))
             -- ^ Value to evaluate
          -> L.Type
             -- ^ Type of value pointed to
          -> BBLLVM arch (L.Typed L.Value)
llvmAsPtr ptr tp = do
  llvmPtrAsBV  <- mkLLVMValue ptr
  convop L.IntToPtr llvmPtrAsBV (L.PtrTo tp)

-- | Create a singleton vector from a value.
singletonVector :: L.Typed L.Value -> BBLLVM arch (L.Typed L.Value)
singletonVector val = do
  let eltType = L.typedType val
  let arrayType = L.Vector 1 eltType
  let vec0 = L.Typed arrayType L.ValUndef
  L.Typed arrayType
    <$> evalInstr (L.InsertElt vec0 val (L.ValInteger 0))

checkEndian :: Endianness -> BBLLVM arch ()
checkEndian end = do
  req <- asks $ archEndianness . archFns
  when (end /= req) $
    error $ "Memory accesses must have byte order " ++ show req

llvmFloatName :: FloatInfoRepr flt -> String
llvmFloatName flt =
  case flt of
    HalfFloatRepr -> "f16"
    SingleFloatRepr -> "f32"
    DoubleFloatRepr -> "f64"
    QuadFloatRepr -> "f128"
    X86_80FloatRepr -> "f80"

-- | Given a `MemRepr`, return the name of the type for annotating
-- intrinsics and the type itself.
resolveLoadNameAndType :: MemRepr tp -> BBLLVM arch (String, L.Type)
resolveLoadNameAndType memRep =
  case memRep of
    BVMemRepr w end -> do
      checkEndian end
      let bvw = 8 * natValue w
      pure ("i" ++ show bvw, llvmITypeNat bvw)
    FloatMemRepr f end -> do
      checkEndian end
      pure (llvmFloatName f, L.PrimType (L.FloatType (llvmFloatType f)))
    PackedVecMemRepr n eltRep -> do
      (eltName, eltType) <- resolveLoadNameAndType eltRep
      unless (toInteger (natValue n) <= toInteger (maxBound :: Int32)) $ do
        error $ "Vector width of " ++ show n ++ " is too large."
      pure ("v" ++ show n ++ eltName, L.Vector (fromIntegral (natValue n)) eltType)

-- | Convert an assignment to a llvm expression
rhsToLLVM :: HasCallStack
          => FnAssignId -- ^ Value being assigned.
          -> FnAssignRhs arch (FnValue arch) tp
          -> BBLLVM arch ()
rhsToLLVM lhs rhs =
  case rhs of
    FnEvalApp app -> do
      llvmRhs <- appToLLVM app
      setAssignIdValue lhs llvmRhs
    FnSetUndefined tp -> do
      setAssignIdValue lhs (L.Typed (typeToLLVMType tp) L.ValUndef)
    FnReadMem ptr typ -> do
      p <- llvmAsPtr ptr (typeToLLVMType typ)
      v <- evalInstr (L.Load p Nothing Nothing)
      setAssignIdValue lhs (L.Typed (typeToLLVMType typ) v)
    FnCondReadMem memRepr cond addr passthru -> do
      (loadName, eltType) <- resolveLoadNameAndType memRepr
      let intr = llvmMaskedLoad 1 loadName eltType
      addIntrinsic intr
      llvmAddr     <- llvmAsPtr addr (L.Vector 1 eltType)
      let llvmAlign = L.Typed (L.iT 32) (L.ValInteger 0)
      llvmCond     <- singletonVector =<< mkLLVMValue cond
      llvmPassthru <- singletonVector =<< mkLLVMValue passthru
      rv <- call intr [ llvmAddr, llvmAlign, llvmCond, llvmPassthru ]
      r <- evalInstr $ L.ExtractElt rv (L.ValInteger 0)
      setAssignIdValue lhs (L.Typed eltType r)
    FnEvalArchFn f -> do
      fn <- asks $ archFnCallback  . archFns
      setAssignIdValue lhs  =<< fn f

resolveFunctionEntry :: FnValue arch (BVType (ArchAddrWidth arch))
                     -> BBLLVM arch (L.Typed L.Value)
resolveFunctionEntry dest =
  case dest of
    FnFunctionEntryValue dest_ftp nm -> do
      let sym = L.Symbol (BSC.unpack nm)
      return $ L.Typed (functionTypeToLLVM dest_ftp) (L.ValSymbol sym)
    _ -> do
      error "Do not support indirect calls."

{-# INLINE bbArchConstraints #-}
bbArchConstraints :: (LLVMArchConstraints arch => BBLLVM arch a) -> BBLLVM arch a
bbArchConstraints m = do
  ctx <- ask
  withArchConstraints ctx m

stmtToLLVM :: forall arch
           .  HasCallStack
           => FnStmt arch
           -> BBLLVM arch ()
stmtToLLVM stmt = bbArchConstraints $ do
  comment (show $ pretty stmt)
  case stmt of
   FnComment _ -> return ()
   FnAssignStmt (FnAssignment lhs rhs) -> do
     rhsToLLVM lhs rhs
   FnWriteMem addr v -> do
     llvmVal <- mkLLVMValue v
     llvmPtr <- llvmAsPtr addr (L.typedType llvmVal)
     -- Cast LLVM point to appropriate type
     effect $ L.Store llvmVal llvmPtr Nothing Nothing
   FnCondWriteMem cond addr v memRepr -> do
     -- Obtain llvm.masked.store intrinsic and ensure it will be declared.
     (loadName, eltType) <- resolveLoadNameAndType memRepr
     let intr = llvmMaskedStore 1 loadName eltType
     addIntrinsic intr
     -- Compute value to write
     llvmValue <- singletonVector =<< mkLLVMValue v
     -- Convert addr to appropriate pointer.
     llvmAddr <- llvmAsPtr addr (L.Vector 1 eltType)
     -- Just use zero alignment
     let llvmAlign = L.Typed (L.iT 32) (L.ValInteger 0)
     -- Construct mask
     llvmMask <- singletonVector =<< mkLLVMValue cond
     -- Call llvmn.masked.store intrinsic
     call_ intr [ llvmValue, llvmAddr, llvmAlign, llvmMask ]
   FnCall dest args mretv -> do
     llvmFn <- resolveFunctionEntry dest
     llvmArgs <- mapM (\(Some v) -> mkLLVMValue v) args
     -- Assign return values
     case mretv of
       Nothing -> do
         call_ llvmFn llvmArgs
       Just (Some rvar) -> do
         retv <- call llvmFn llvmArgs
         setAssignIdValue (frAssignId rvar) retv
   FnArchStmt astmt -> do
     fn <- asks $ archStmtCallback . archFns
     fn astmt

llvmBlockLabel :: FnBlockLabel w -> L.BlockLabel
llvmBlockLabel = L.Named . L.Ident . fnBlockLabelString

addTargetPhiValues :: forall arch . FnJumpTarget arch -> BBLLVM arch ()
addTargetPhiValues tgt = do
  thisLabel <- gets bbThisLabel :: BBLLVM arch L.BlockLabel
  let tgtLbl :: FnBlockLabel (ArchAddrWidth arch)
      tgtLbl = fnJumpLabel tgt
  let values = fnJumpPhiValues tgt
  -- Add this block value to phi assignment
  let updateVar :: Map L.BlockLabel L.Value
                -> Some (FnValue arch)
                -> BBLLVM arch (Map L.BlockLabel L.Value)
      updateVar prevVars (Some v) = do
        thisVal <- mkLLVMValue v
        pure $! Map.insert thisLabel (L.typedValue thisVal) prevVars
  m <- BBLLVM $ use $ funStateLens . funBlockPhiMapLens
  let curEntries = phiAssignmentForBlock m tgtLbl
  newEntries <- V.zipWithM updateVar curEntries values
  BBLLVM $ funStateLens . funBlockPhiMapLens .= Map.insert tgtLbl newEntries m

termStmtToLLVM :: HasCallStack
               => FnTermStmt arch
               -> BBLLVM arch ()
termStmtToLLVM tm =
  case tm of
    FnJump tgt -> do
      addTargetPhiValues tgt
      let lbl = llvmBlockLabel $ fnJumpLabel tgt
      effect $ L.Jump lbl
    FnBranch macawCond ttgt ftgt -> do
      llvmCond <- mkLLVMValue macawCond
      addTargetPhiValues ttgt
      addTargetPhiValues ftgt
      let tlbl = llvmBlockLabel $ fnJumpLabel ttgt
      let flbl = llvmBlockLabel $ fnJumpLabel ftgt
      effect $ L.Br llvmCond tlbl flbl
    FnLookupTable idx vec -> do
      idx' <- mkLLVMValue idx
      traverse_ addTargetPhiValues vec
      let dests = V.toList $ llvmBlockLabel . fnJumpLabel <$> vec
      markNeedSwitchFailLabel
      effect $ L.Switch idx' (L.Named (L.Ident switchFailLabel)) (zip [0..] dests)
    FnRet mret -> do
      ftp <- asks funType
      case (fnReturnType ftp, mret) of
        (Nothing, Nothing) -> do
          effect L.RetVoid
        (Just _, Nothing) -> error "Expected return value when none found."
        (Nothing, Just _) -> error "Unexpected return value."
        (Just _rtp, Just (Some rval)) -> do
          llvmRetVal <- mkLLVMValue rval
          effect $! L.Ret llvmRetVal
    FnTailCall dest args -> do
      callTarget <- resolveFunctionEntry dest
      llvmArgs <- mapM (viewSome mkLLVMValue) args
      let callTargetType = L.typedType callTarget
      let callTargetValue = L.typedValue callTarget
      case callTargetType of
        L.PtrTo (L.FunTy res _argTypes _varArgs) ->
          case res of
            L.PrimType L.Void -> do
              effect $ L.Call False callTargetType callTargetValue llvmArgs
              effect L.RetVoid
            _ -> do
              retv <- evalInstr $ L.Call False callTargetType callTargetValue llvmArgs
              effect (L.Ret (L.Typed res retv))
        _ -> do
          error "Expected function type to tail call target."

resolvePhiStmt :: PhiValues
               -> PhiBinding arch
               -> L.Stmt
resolvePhiStmt phiValues b =
  case phiFnRepVar b of
    Some phiVar ->
      let lnm = L.Ident (phiLLVMIdent b)
          llvmType :: L.Type
          llvmType  = typeToLLVMType (fnPhiVarType phiVar)
          llvmPhiValues :: [(L.Value, L.BlockLabel)]
          llvmPhiValues = [ (llvmVal, lbl) | (lbl, llvmVal) <- Map.toList phiValues ]
       in L.Result lnm (L.Phi llvmType llvmPhiValues) []

-- | Construct a basic block from a block result
toBasicBlock :: (LLVMArchConstraints arch, HasCallStack)
             => ResolvePhiMap (ArchAddrWidth arch)
             -> LLVMBlockResult arch -- ^ This block
             -> L.BasicBlock
toBasicBlock phiMap res
  | V.length phiVars /= V.length phiAssignment =
    error "Phi variables length does not match phi assignment."
  | otherwise =
    L.BasicBlock { L.bbLabel = Just lbl
                 , L.bbStmts = phiStmts ++ reverse (bbStmts finalState)
                 }
  where b = fnBlock res
        phiVars = llvmPhiVars res
        phiAssignment = phiAssignmentForBlock phiMap (fbLabel b)
        lbl = L.Named (L.Ident (fnBlockLabelString (fbLabel b)))
        phiStmts = V.toList $ V.zipWith resolvePhiStmt phiAssignment phiVars
        finalState = finalBBState res

-- | Information relating a Macaw funcation variable and LLVM identifier.
data PhiBinding arch =
  PhiBinding { phiFnRepVar :: !(Some (FnPhiVar arch))
               -- ^ Function representation variable.
             , phiLLVMIdent :: !String
               -- ^ LLVM identifier for Phi variable
             }

-- | Add a phi var with the node info so that we have a ident to
-- reference it by and queue up work to assign the value later.
addPhiBinding :: HasCallStack
              => Some (FnPhiVar arch)
              -> BBLLVM arch (PhiBinding arch)
addPhiBinding (Some phiVar) = do
  L.Ident nm <- freshName
  let llvmType = typeToLLVMType (fnPhiVarType phiVar)
  setAssignIdValue (unFnPhiVar phiVar) (L.Typed llvmType (L.ValIdent (L.Ident nm)))
  pure $! PhiBinding { phiFnRepVar = Some phiVar
                     , phiLLVMIdent = nm
                     }

addLLVMBlock :: forall arch
            .  FunLLVMContext arch
               -- ^ Context for block
            -> FunState arch
            -> FnBlock arch
               -- ^ Block to generate
            -> (FunState arch, LLVMBlockResult arch)
addLLVMBlock ctx fs b = (finFS, res)
  where s0 = BBLLVMState { bbThisLabel = llvmBlockLabel (fbLabel b)
                         , bbAssignValMap = Map.empty
                         , bbStmts        = []
                         }
        go :: BBLLVM arch (V.Vector (PhiBinding arch))
        go = do
          -- Add statements for Phi nodes
          bindings <- traverse addPhiBinding (fbPhiVars b)
          -- Add statements
          mapM_ stmtToLLVM (fbStmts b)
          -- Add term statement
          termStmtToLLVM (fbTerm b)
          -- Results
          pure bindings
        (phiBindings, BBLLVMPair finFS s) =
          runState (runReaderT (unBBLLVM go) ctx) (BBLLVMPair fs s0)

        res = LLVMBlockResult { fnBlock = b
                              , llvmPhiVars = phiBindings
                              , finalBBState = s
                              }

------------------------------------------------------------------------
-- Inline assembly

-- | Attributes to annote inline assembly.
data AsmAttrs = AsmAttrs { asmSideeffect :: !Bool }

-- | Indicates all side effects  of inline assembly are captured in the constraint list.
noSideEffect :: AsmAttrs
noSideEffect = AsmAttrs { asmSideeffect = False }

-- | Indicates asm may have side effects that are not captured in the
-- constraint list.
sideEffect :: AsmAttrs
sideEffect = AsmAttrs { asmSideeffect = True }

-- | Call some inline assembly
callAsm :: AsmAttrs -- ^ Asm attrs
        -> L.Type   -- ^ Return type
        -> String   -- ^ Code
        -> String   -- ^ Args code
        -> [L.Typed L.Value] -- ^ Arguments
        -> BBLLVM arch (L.Typed L.Value)
callAsm attrs resType asmCode asmArgs args = do
  let argTypes = L.typedType <$> args
      ftp = L.PtrTo (L.FunTy resType argTypes False)
      f = L.ValAsm (asmSideeffect attrs) False asmCode asmArgs
  L.Typed resType <$> evalInstr (L.Call False ftp f args)

-- | Call some inline assembly that does not return a value.
callAsm_ :: AsmAttrs -- ^ Asm attrs
         -> String   -- ^ Code
         -> String   -- ^ Args code
         -> [L.Typed L.Value] -- ^ Arguments
         -> BBLLVM arch ()
callAsm_ attrs asmCode asmArgs args = do
  let argTypes = L.typedType <$> args
  let ftp = L.PtrTo (L.FunTy (L.PrimType L.Void) argTypes False)
  let f :: L.Value
      f = L.ValAsm (asmSideeffect attrs) False asmCode asmArgs
  call_ (L.Typed ftp f) args

------------------------------------------------------------------------
-- Translating functions

-- | The LLVM translate monad records all the intrinsics detected when
-- adding LLVM.
newtype LLVMTrans a = LLVMTrans (State IntrinsicMap a)
  deriving (Functor, Applicative, Monad, MonadState IntrinsicMap)

-- | Run the translation returning result and intrinsics.
runLLVMTrans :: LLVMTrans a -> ([Intrinsic], a)
runLLVMTrans (LLVMTrans action) =
  let (r, m) = runState action Map.empty
   in (Map.elems m, r)

argIdent :: Int -> L.Ident
argIdent i = L.Ident ("arg" ++ show i)

memAnn :: Ann.MCAddr -> (Word64, FnMemAccessType) -> Ann.MCMemoryEvent
memAnn a (o, tp) =
  Ann.MCMemoryEvent
  { Ann.eventAddr = fromIntegral a + fromIntegral o
  , Ann.eventInfo =
      case tp of
        HeapAccess -> Ann.HeapAccess
        StackAccess -> Ann.BinaryOnlyAccess
  }

mkStackExpr :: Integer -> Ann.Expr Ann.BlockVar
mkStackExpr o
  | o < 0 =
    let oExpr = Ann.BVDecimal (fromInteger (negate o)) 64
     in Ann.BVSub (Ann.Var Ann.StackHigh) oExpr
  | o == 0 = Ann.Var Ann.StackHigh
  | otherwise =
    let oExpr :: Ann.Expr Ann.BlockVar
        oExpr = Ann.BVDecimal (fromInteger o) 64
     in Ann.BVAdd (Ann.Var Ann.StackHigh) oExpr

newtype BlockAnnGen a = BlockAnnGen (Except String a)
  deriving (Functor, Applicative, Monad, MonadError String)

runBlockAnnGen :: BSC.ByteString -> Ann.MCAddr -> BlockAnnGen a -> Either String a
runBlockAnnGen fnm a (BlockAnnGen m) =
  case runExcept m of
    Left e -> Left $ printf "Missing annotation generation feature in %s(%s): %s" (BSC.unpack fnm) (show a) e
    Right r -> Right r


mkBoundLocExpr :: HasCallStack => BoundLoc X86Reg tp -> BlockAnnGen (Ann.Expr Ann.BlockVar)
mkBoundLocExpr (RegLoc xr) =
  case xr of
    X86_GP r ->
      pure $! Ann.Var (Ann.InitGPReg64 r)
    X86_FlagReg r->
      pure $! Ann.Var (Ann.InitFlagReg r)
    _ ->
      throwError $ "Do not support register " ++ show xr
mkBoundLocExpr (StackOffLoc o tp) =
  if o < 0 then
    case tp of
      BVMemRepr byteCount LittleEndian -> do
        let stackExpr = mkStackExpr (toInteger o)
            bitCount = 8 * natValue byteCount
        pure $! Ann.Var (Ann.MCStack stackExpr bitCount)
      _ ->
        throwError $ "Do not support stack references with type " ++ show tp
   else
    throwError $ "Do not support positive stack offsets."

-- | Generate preconditions for a phi variable to associate LLVM phi
-- variables with machine code.
addPhiPrecond :: PhiBinding X86_64 -> [Ann.Expr Ann.BlockVar] -> BlockAnnGen [Ann.Expr Ann.BlockVar]
addPhiPrecond b prev0 =
  case phiFnRepVar b of
    Some phiVar -> do
          -- Get expression representing LLVM value.
      let phiExpr :: Ann.Expr Ann.BlockVar
          phiExpr = Ann.Var (Ann.LLVMVar (Text.pack (phiLLVMIdent b)))
          -- Assert pfi expression is equal to each machine location.
          vars = fnPhiVarRep phiVar : fnPhiVarLocations phiVar
      let fn v prev = seq prev $ do
            e <- mkBoundLocExpr v
            let expr = Ann.Eq phiExpr e
            seq expr $ pure (expr : prev)
      foldrM fn prev0 vars

-- | Create assertions from block preconditions.
mkInvPrecond :: FnBlockInvariant X86_64 -> BlockAnnGen (Ann.Expr Ann.BlockVar)
mkInvPrecond (FnCalleeSavedReg r x) = do
  xExpr <- mkBoundLocExpr x
  regExpr <-
    case r of
      X86_GP rgp -> pure $! Ann.Var (Ann.FnStartGPReg64 rgp)
      _ -> throwError "Only general purpose registers are supported."
  pure $! Ann.Eq xExpr regExpr
mkInvPrecond (FnEqualLocs x y) =
  Ann.Eq <$> mkBoundLocExpr x <*> mkBoundLocExpr y
mkInvPrecond (FnStackOff o x) =
  Ann.Eq <$> mkBoundLocExpr x <*> pure (mkStackExpr (toInteger o))

-- | Generate pair containing block label in LLVM and annotations from
-- block.
getBlockAnn :: BSC.ByteString -- ^ Name of function
            -> LLVMBlockResult X86_64
            -> Either String (String, Ann.BlockAnn)
getBlockAnn fnm blockRes = do
  let b = fnBlock blockRes
  let lbl = fbLabel b
  let addr  = fromIntegral $ addrOffset $ segoffAddr $ fnBlockLabelAddr lbl
  runBlockAnnGen fnm addr $ do
    let pr = fbPrecond b
    invPreconds <- mapM mkInvPrecond (fbInvariants b)

    -- Preconditions that relate phi variable with Macaw location.
    preconds <- foldrM addPhiPrecond invPreconds (llvmPhiVars blockRes)

    -- Generate memory event annotation given base address, offset and address type.
    let ann = Ann.ReachableBlockAnn { Ann.blockAddr = addr
                                    , Ann.blockCodeSize = fbSize b
                                    , Ann.blockX87Top = blockInitX87TopReg pr
                                    , Ann.blockDFFlag = blockInitDF pr
                                    , Ann.blockPreconditions = preconds
                                    , Ann.blockAllocas = Map.empty
                                    , Ann.mcMemoryEvents = V.toList $ memAnn addr <$> fbMemInsnAddrs b
                                    }
    pure $! (fnBlockLabelString lbl, Ann.ReachableBlock ann)

-- | This translates the function to LLVM and returns the define.
--
-- We have each function return all possible results, although only
-- the ones that are actually used (we use undef for the others).
-- This makes the LLVM conversion slightly simpler.
--
-- Users should declare intrinsics via 'declareIntrinsics' before
-- using this function.  They should also add any referenced
-- functions.
defineFunction :: forall arch
               .  (LLVMArchConstraints arch, arch ~ X86_64)
               => LLVMArchSpecificOps arch
                  -- ^ Architecture specific operations
               -> LLVMGenOptions
                  -- ^ Options for generating LLVM
               -> Function arch
                  -- ^ Function to translate
               -> LLVMTrans (L.Define, Either String Ann.FunctionAnn)
defineFunction archOps genOpts f = do
  let mkInputReg :: Some TypeRepr -> Int -> L.Typed L.Ident
      mkInputReg (Some tp) i = L.Typed (typeToLLVMType tp) (argIdent i)

  let inputArgs :: [L.Typed L.Ident]
      inputArgs = zipWith mkInputReg (fnArgTypes (fnType f)) [0..]
  let ctx :: FunLLVMContext arch
      ctx = FunLLVMContext { archFns = archOps
                           , funLLVMGenOptions = genOpts
                           , funArgs = V.fromList $ fmap L.ValIdent <$> inputArgs
                           , funType = fnType f
                           , funAllocaCount = 0
                           , withArchConstraints = \x -> x
                           }

  -- Create ordinary blocks
  m0 <- get
  let initFunState :: FunState arch
      initFunState = FunState { nmCounter = 0
                              , funIntrinsicMap = m0
                              , needSwitchFailLabel = False
                              , funBlockPhiMap = initResolvePhiMap f
                              }
  let (postEntryFunState, entryBlockRes) =
        addLLVMBlock ctx initFunState (fnEntryBlock f)

  let (finalFunState, finalBlocks) =
        mapAccumL (addLLVMBlock ctx) postEntryFunState (fnRestBlocks f)

  -- Update intrins map
  put (funIntrinsicMap finalFunState)

  let entryLLVMBlock :: L.BasicBlock
      entryLLVMBlock = toBasicBlock (funBlockPhiMap finalFunState) entryBlockRes

  let blocks :: [L.BasicBlock]
      blocks = toBasicBlock (funBlockPhiMap finalFunState) <$> finalBlocks

  let finBlocks
        | needSwitchFailLabel finalFunState = entryLLVMBlock : (blocks ++ [failBlock])
        | otherwise = entryLLVMBlock : blocks
  let funDef = L.Define { L.defLinkage  = Nothing
                        , L.defRetType  = llvmFunctionReturnType (fnType f)
                        , L.defName     = L.Symbol (BSC.unpack (fnName f))
                        , L.defArgs     = inputArgs
                        , L.defVarArgs  = False
                        , L.defAttrs    = []
                        , L.defSection  = Nothing
                        , L.defGC       = Nothing
                        , L.defBody     = finBlocks
                        , L.defMetadata = Map.empty
                        , L.defComdat   = Nothing
                        }
  let funAnn :: Either String Ann.FunctionAnn
      funAnn = do
        blockAnnEntries <- mapM (getBlockAnn (fnName f)) (V.fromList (entryBlockRes : finalBlocks))
        let finBlockAnnMap
              | needSwitchFailLabel finalFunState =
                  V.snoc blockAnnEntries (switchFailLabel, Ann.UnreachableBlock)
              | otherwise =
                  blockAnnEntries
        let blockObjMap = uncurry Ann.blockAnnToJSON <$> finBlockAnnMap
        pure $! Ann.FunctionAnn { Ann.llvmFunName = BSC.unpack (fnName f)
                                , Ann.blocks = blockObjMap
                                }
  pure (funDef, funAnn)

------------------------------------------------------------------------
-- Other

declareIntrinsic :: Intrinsic -> L.Declare
declareIntrinsic i =
  L.Declare { L.decRetType = intrinsicRes i
            , L.decName    = intrinsicName i
            , L.decArgs    = intrinsicArgs i
            , L.decVarArgs = False
            , L.decAttrs   = intrinsicAttrs i
            , L.decComdat  = Nothing
            }

-- | Generate LLVM module from a list of functions describing the
-- behavior.
moduleForFunctions :: forall arch
                   .  ( LLVMArchConstraints arch
                      , Show (FunctionType arch)
                      , FoldableFC (ArchFn arch)
                      , FoldableF (FnArchStmt arch)
                      , arch ~ X86_64
                      )
                   => LLVMArchSpecificOps arch
                      -- ^ Architecture specific functions
                   -> LLVMGenOptions
                      -- ^ Options for generating LLVM
                   -> RecoveredModule arch
                      -- ^ Module to generate
                   -> (L.Module, [Either String Ann.FunctionAnn])
moduleForFunctions archOps genOpts recMod =
  let (dynIntrinsics, definesAndAnn) = runLLVMTrans $
        traverse (defineFunction archOps genOpts) (recoveredDefs recMod)
      llvmMod =  L.Module { L.modSourceName = Nothing
                          , L.modDataLayout = []
                          , L.modTypes      = []
                          , L.modNamedMd    = []
                          , L.modUnnamedMd  = []
                          , L.modGlobals    = []
                          , L.modDeclares   = fmap declareIntrinsic llvmIntrinsics
                                           ++ fmap declareIntrinsic dynIntrinsics
                                           ++ fmap declareFunction (recoveredDecls recMod)
                          , L.modDefines    = fst <$> definesAndAnn
                          , L.modInlineAsm  = []
                          , L.modAliases    = []
                          , L.modComdat     = Map.empty
                          }
   in (llvmMod, snd <$> definesAndAnn)
