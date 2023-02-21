{-|
This module provides methods for constructing functions from the basic
blocks discovered by 'Data.Macaw.Discovery'.
-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
-- {-# LANGUAGE TemplateHaskell            #-}
-- {-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Reopt.CFG.Recovery
  ( recoverFunction
  , RecoveredFunction(..)
  , LLVMLogEvent(..)
  , llvmLogEventHeader
  , llvmLogEventToStrings
  , RecoverError(..)
  , Data.Macaw.Analysis.RegisterUse.BlockInvariantMap
  , x86BlockInvariants
  , x86CallRegs
    -- * X86 type info
  , X86FunTypeInfo(..)
  , mkX86FunctionType
  , X86ArgInfo(..)
  , ZMMType(..)
  , X86RetInfo(..)
  , retReg
  ) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Bits
import qualified Data.ByteString.Char8            as BSC
import           Data.Foldable
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromJust, fromMaybe)
import           Data.Parameterized.Classes
import qualified Data.Parameterized.List          as P
import           Data.Parameterized.Map           (MapF, Pair (..))
import qualified Data.Parameterized.Map           as MapF
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some
import           Data.Parameterized.TraversableF  (TraversableF (traverseF))
import           Data.Parameterized.TraversableFC
import           Data.Sequence                    (Seq)
import qualified Data.Sequence                    as Seq
import qualified Data.Text                        as Text
import qualified Data.Vector                      as V
import           Data.Word
import qualified Flexdis86                        as F
import           GHC.Stack
import           Prettyprinter
import           Text.Printf

import           Data.Macaw.Analysis.RegisterUse
import           Data.Macaw.CFG
import           Data.Macaw.Discovery.State
import qualified Data.Macaw.Memory.Permissions    as Perm
import           Data.Macaw.Types

import           Data.Macaw.X86                   (x86DemandContext,
                                                   x86_64CallParams)
import           Data.Macaw.X86.ArchTypes
import           Data.Macaw.X86.SyscallInfo
import           Data.Macaw.X86.X86Reg

import           Reopt.CFG.FnRep
import           Reopt.CFG.FnRep.X86
import           Reopt.CFG.LLVM                   (LLVMBitCastInfo (..),
                                                   LLVMLogEvent (..),
                                                   LLVMLogEventInfo (..),
                                                   llvmITypeNat,
                                                   llvmLogEventHeader,
                                                   llvmLogEventToStrings,
                                                   typeToLLVMType)
import           Reopt.CFG.LLVM.X86               (x86ArchFnToLLVM)
import           Reopt.Events                     (RecoverError (..),
                                                   ReoptErrorTag (..))
import qualified Reopt.Utils.Printf               as Printf

-------------------------------------------------------------------------------
--


fromSomeList :: [Some f] -> Some (P.List f)
fromSomeList = P.fromListWith id

fromSomeListWithM :: forall f g m
                  .  Monad m
                  => (forall tp . f tp -> m (g tp))
                  -> [Some f]
                  -> m (Some (P.List g))
fromSomeListWithM f = foldrM g (Some P.Nil)
  where g :: Some f -> Some (P.List g) -> m (Some (P.List g))
        g (Some x) (Some r) = Some . (P.:< r) <$> f x

data ZMMType tp where
  ZMMDouble :: ZMMType (FloatType DoubleFloat)
  ZMM512D :: ZMMType (VecType 8 (FloatType DoubleFloat))

instance TestEquality ZMMType where
  testEquality ZMMDouble ZMMDouble = Just Refl
  testEquality ZMM512D ZMM512D     = Just Refl
  testEquality _ _                 = Nothing

deriving instance Show (ZMMType tp)

instance HasRepr ZMMType TypeRepr where
  typeRepr ZMMDouble = knownRepr
  typeRepr ZMM512D   = knownRepr

-- | This identifies how a argument is passed into a function, or
-- a return value is passed out.
data X86ArgInfo where
  -- | This identifies a 64-bit value passed as a register.
  --
  -- The register should be compatible with the ABI.
  ArgBV64 :: !F.Reg64 -> X86ArgInfo
  -- | A single double is passed into one of the ZMM registers.
  ArgZMM :: !(ZMMType tp) -> !Word8 -> X86ArgInfo

deriving instance Show X86ArgInfo

instance Eq X86ArgInfo where
  ArgBV64 i == ArgBV64 j         = i == j
  ArgZMM xtp xi == ArgZMM ytp yi = xi == yi && isJust (testEquality xtp ytp)
  _ == _                         = False

-- | This returns the value associate with a argument for demand computation purposes.
argValue :: RegState X86Reg (Value X86_64 ids) ->  X86ArgInfo -> Some (Value X86_64 ids)
argValue regs (ArgBV64 r)  = Some $ regs^.boundValue (X86_GP r)
argValue regs (ArgZMM _ i) = Some $ regs^.boundValue (X86_ZMMReg i)

-- $(pure [])

-- | This identifies how a return value is passed from a callee to
-- the callee.  The type indicates the type of the return value as opposed
-- to the type of the register.
data X86RetInfo tp where
  RetBV64 :: !F.Reg64 -> X86RetInfo (BVType 64)
   -- ^ This identifies a 64-bit value returned as a register (RAX/RDX)
   --
   -- The register should be compatible with the ABI.
  RetZMM :: !(ZMMType tp) -> !Word8 -> X86RetInfo tp
   -- ^ This identifies one of the two zmm registers used as argument (zmm0/1).

deriving instance (Show (X86RetInfo tp))

instance ShowF X86RetInfo

-- | The register types this return value is associated with.
retReg :: X86RetInfo tp -> Some X86Reg
retReg (RetBV64   r) = Some $! X86_GP r
retReg (RetZMM _  i) = Some $! X86_ZMMReg i

instance TestEquality X86RetInfo where
  testEquality (RetBV64   x) (RetBV64   y) = if x == y then Just Refl else Nothing
  testEquality (RetZMM xtp x) (RetZMM ytp y) = do
    Refl <- testEquality xtp ytp
    if x == y then Just Refl else Nothing
  testEquality _ _ = Nothing

instance HasRepr X86RetInfo TypeRepr where
  typeRepr (RetBV64 _)   = knownRepr
  typeRepr (RetZMM tp _) = typeRepr tp

-- $(pure [])

-- | This describes the registers and return value of an x86_64 function.
--
-- This representation does not support arguments that spilled on the stack, but
-- this would be a good feature to add.
--
-- It uses a list for arguments so that we can use C headers and ensure the
-- arguments appear in a particular order (e.g. from the binary perspective a
-- function that takes two integers followed by a float is ABI compatible with a
-- function that takes a float followed by two integers).
data X86FunTypeInfo
   = X86NonvarargFunType [X86ArgInfo] [Some X86RetInfo]
   | X86PrintfFunType !Int
     -- ^ @X86PrintfFunType n@ denotes a function that takes @n@ 64-bit
     -- bitvectors as arguments followed by a printf-style format string that
     -- defines the number of subsequent arguments.
     --
     -- The function must return an int.
   | X86OpenFunType
     -- ^ @X86OpenFunType@ denotes the rtype of open.
  deriving (Eq, Show)

------------------------------------------------------------------------
-- Embedding


-- | @Embedding i o@ describes how to convert from a value of type @i@
-- to a value of type @o@.
data Embedding i o where
  EqEmbedding :: !(WidthEqProof i o) -> Embedding i o
  UExtEmbedding :: (1 <= s, s+1 <= l, 1 <= l)
                => !(WidthEqProof i (BVType s))
                -> !(NatRepr l)
                -> Embedding i (BVType l)

embeddingSource :: Embedding i o -> TypeRepr i
embeddingSource (EqEmbedding p)     = widthEqSource p
embeddingSource (UExtEmbedding p _) = widthEqSource p

embeddingRefl :: TypeRepr tp -> Embedding tp tp
embeddingRefl tp = EqEmbedding (WidthEqRefl tp)

-- | Apply an embedding to a type.
data EmbeddingApp f tp where
  -- | Apply an embedding a parameterized value.
  EmbeddingApp :: !(f i)
               -> !(Embedding i o)
               -> EmbeddingApp f o

instance PrettyF f => Pretty (EmbeddingApp f tp) where
  pretty (EmbeddingApp v _)    = prettyF v

mkIdentEmbeddingApp :: HasRepr f TypeRepr => f tp -> EmbeddingApp f tp
mkIdentEmbeddingApp v = EmbeddingApp v (EqEmbedding (WidthEqRefl (typeRepr v)))

instance ShowF f => Show (EmbeddingApp f tp) where
  show (EmbeddingApp v _) = showF v
instance ShowF f => ShowF (EmbeddingApp f)

-- | Apply an embedding to a type.
data EmbeddingInv f tp where
  -- | Apply an embedding a parameterized value.
  EmbeddingInv :: !(Embedding i o)
               -> !(f o)
               -> EmbeddingInv f i

instance HasRepr (EmbeddingInv f) TypeRepr where
  typeRepr (EmbeddingInv emb _) = embeddingSource emb

------------------------------------------------------------------------
-- FnRegValue

type FnRegValue arch = EmbeddingApp (FnValue arch)

-- $(pure [])

------------------------------------------------------------------------
-- Function recover monad

-- | Information for function recovery common to a function or module.
data FunRecoverContext ids =
  FRC { frcMemory :: !(Memory 64)
      , frcInterp :: !(DiscoveryFunInfo X86_64 ids)
      , frcSyscallPersonality  :: !SyscallPersonality
        -- ^ System call personality
      , frcCurrentFunctionReturns :: ![Some X86RetInfo]
        -- ^ The type of the function being recovered.
      , frcBlockDepMap :: !(Map (MemSegmentOff 64) (BlockInvariants X86_64 ids))
        -- ^ Maps the starting address of blocks to the dependency set
        -- for the block.
      }

-- | State for function recovery common to all blocks in a function.
data FunRecoverState = FRS { frsNextAssignId :: !FnAssignId,
                             frsLogEvents    :: ![LLVMLogEvent]
                           }

-- | Monad for function recovery
newtype FunRecover ids a =
    FR { runFR :: ReaderT (FunRecoverContext ids) (StateT FunRecoverState (Except (RecoverError 64))) a }
  deriving (Functor, Applicative, Monad, MonadState FunRecoverState, MonadError (RecoverError 64))

runFunRecover :: FunRecoverContext ids
              -> FunRecover ids a
              -> Either (RecoverError 64) a
runFunRecover ctx m =
  let s0 = FRS { frsNextAssignId = FnAssignId 0,
                 frsLogEvents = []
               }
   in runExcept (evalStateT (runReaderT (runFR m) ctx) s0)

-- | Create a fresh assign id.
funFreshId :: FunRecover ids FnAssignId
funFreshId = FR $ do
  FnAssignId nextId <- gets frsNextAssignId
  modify' $ \frs -> frs { frsNextAssignId = FnAssignId (nextId + 1) }
  return $! FnAssignId nextId

-- $(pure [])

------------------------------------------------------------------------
-- RecoverState

-- | This represents a value writen to the stack and the memory
-- representation used to write it.
data StackWriteVal arch =
  forall tp . StackWriteVal !(FnRegValue arch tp) !(MemRepr tp)

-- | State used for recovering a block
data RecoverState arch ids =
  RS { rsStartAddr :: !(ArchSegmentOff arch)
       -- ^ Initial block
     , rsBlockInvariants :: !(BlockInvariants arch ids)
       -- ^ Invariants inferred about block
     , rsPredBlockAddrs :: ![ArchSegmentOff arch]
       -- ^ Predecessors for this block
     , rsPhiLocMap :: !(MapF (BoundLoc (ArchReg arch)) (FnRegValue arch))
       -- ^ Maps representative locations to the associated variable.
       -- | Index of instruction recover is processing.
     , rsInsnIndex :: !Int
     , rsBlockOff :: !(ArchAddrWord arch)
     -- ^ The offset in the block of the current code.
     , rsBlockSize :: !Int
     , _rsCurStmts  :: !(Seq (FnStmt arch))
       -- ^ Statements added to block so far
       -- | Maps assignments processed so far to their values
     , _rsAssignMap    :: !(MapF (AssignId ids) (FnValue arch))
       -- | Map the index of processed used memory writes to the
       -- value stored in memory after they execute.
     , rsWriteMap  :: !(Map StmtIndex (StackWriteVal arch))
       -- | Instruction offset and type of previous accesses
     , rsSeenMemAccessTypes :: ![(Word64, FnMemAccessType)]
       -- | Memory accesses that have not yet been processed.
     , rsPendingMemAccesses :: ![(StmtIndex, MemAccessInfo arch ids)]
     , rsLogEvents :: ![LLVMLogEvent]
     }

-- | List of statements accumulated so far.
rsCurStmts :: Lens' (RecoverState arch ids) (Seq (FnStmt arch))
rsCurStmts = lens _rsCurStmts (\s v -> s { _rsCurStmts = v })

-- | Map from assignments that have been evaluated to the value bound
-- to them.
rsAssignMap :: Lens' (RecoverState arch ids)
                     (MapF (AssignId ids) (FnValue arch))
rsAssignMap = lens _rsAssignMap (\s v -> s { _rsAssignMap = v })

------------------------------------------------------------------------
-- Recover

-- | Monad for recovering code in a block.
newtype Recover ids a = Recover {
    runRecover :: StateT (RecoverState X86_64 ids) (FunRecover ids) a
  }
  deriving ( Functor, Applicative, Monad
           , MonadError (RecoverError 64)
           , MonadState (RecoverState X86_64 ids)
           )

liftFunRecover :: FunRecover ids a -> Recover ids a
liftFunRecover m = Recover $ lift m

getFunCtx :: Recover ids (FunRecoverContext ids)
getFunCtx = liftFunRecover $ FR ask

logEvent :: LLVMLogEvent -> Recover ids ()
logEvent e = modify $ \s -> s { rsLogEvents = e:rsLogEvents s }

-- | Create a fresh variable for the left-hand-side of an assignment.
freshId :: Recover ids FnAssignId
freshId = liftFunRecover funFreshId

-- | Create a fresh return variable given the type id.
mkReturnVar :: TypeRepr tp -> Recover ids (FnReturnVar tp)
mkReturnVar tp = (`FnReturnVar` tp) <$> freshId

-- | Add a statement to the list of statements in current function block.
addFnStmt :: FnStmt X86_64 -> Recover ids ()
addFnStmt stmt = rsCurStmts %= (Seq.|> stmt)

throwErrorAt :: ReoptErrorTag -> String -> Recover ids a
throwErrorAt tag msg = do
  sa <- gets rsStartAddr
  sz <- gets rsBlockSize
  idx <- gets rsInsnIndex
  throwError $ RecoverErrorAt {
    recoverErrorTag = tag,
    recoverErrorBlock = sa,
    recoverErrorBlockSize = sz,
    recoverErrorInsnIndex = idx,
    recoverErrorMessage = Text.pack msg
  }

-- $(pure [])

------------------------------------------------------------------------
-- X86FunctionType

-- | Type used in passing this argument to a function.
argRegTypeRepr :: X86ArgInfo -> Some TypeRepr
argRegTypeRepr (ArgBV64 _) = Some (BVTypeRepr n64)
argRegTypeRepr (ArgZMM tp _) =
  case tp of
    ZMMDouble -> Some (FloatTypeRepr  DoubleFloatRepr)
    ZMM512D   -> Some (VecTypeRepr n8 (FloatTypeRepr DoubleFloatRepr))

-- $(pure [])

------------------------------------------------------------------------
-- evalAssign

-- | This evaluates the right-hand side of an assignment and returns
-- the value.
evalAssignRhs :: FnAssignRhs X86_64 (FnValue X86_64) tp -> Recover ids (FnValue X86_64 tp)
evalAssignRhs rhs = do
  fnAssign <- (`FnAssignment` rhs) <$> freshId
  seq fnAssign $ do
    addFnStmt $ FnAssignStmt fnAssign
    return $ FnAssignedValue fnAssign

-- $(pure [])

bitcast :: String
        -> FnValue X86_64 i
        -> WidthEqProof i o
        -> Recover ids (FnValue X86_64 o)
bitcast ctx x p = do
  logEvent event
  evalAssignRhs $ FnEvalApp (Bitcast x p)
  where src = typeToLLVMType $ widthEqSource p
        dst = typeToLLVMType $ widthEqTarget p
        event = LLVMLogEvent ctx $ LogInfoBitCast $ LLVMBitCastInfo src dst

-- $(pure [])

checkedBitcast :: String
               -> FnValue X86_64 i
               -> WidthEqProof i o
               -> Recover ids (FnValue X86_64 o)
checkedBitcast ctx v pr =
  case testEquality (widthEqSource pr) (widthEqTarget pr) of
    Just Refl -> pure v
    Nothing   -> bitcast ctx v pr

-- $(pure [])

-- | This turns a @FnRegValue@ into a @FnValue@.
coerceRegValue :: FnRegValue X86_64 tp
               -> Recover ids (FnValue X86_64 tp)
coerceRegValue (EmbeddingApp v e) =
  case e of
    EqEmbedding pr -> checkedBitcast "coerceRegValue" v pr
    UExtEmbedding pr w -> do
      vw <- checkedBitcast "coerceRegValue" v pr
      evalAssignRhs $ FnEvalApp (UExt vw w)

-- | This takes a value written to memory (e.g., the stack),
-- and coerces it to a value of the read type while bypassing
-- memory.
coerceWriteToRead :: FnRegValue X86_64 wtp -- ^ Value written to memory
                  -> MemRepr wtp -- ^ Type of write
                  -> MemRepr rtp -- ^ Type of read
                  -> Recover ids (FnValue X86_64 rtp)
coerceWriteToRead (EmbeddingApp v emb) writeRepr readRepr
  | Just Refl <- testEquality writeRepr readRepr =
      coerceRegValue (EmbeddingApp v emb)
  | BVMemRepr writeWidth writeEnd <- writeRepr
  , FloatMemRepr readFloatRepr readEnd <- readRepr
  , EqEmbedding pr <- emb
  , Just Refl <- testEquality writeWidth (floatInfoBytes readFloatRepr)
  , writeEnd == readEnd =
      bitcast "coerceWriteToRead" v (WidthEqTrans pr (ToFloat readFloatRepr))
  | otherwise =
      throwErrorAt ReoptTypeMismatchTag $
        printf "Cannot read type %s from write of type %s."
               (show readRepr) (show writeRepr)

-- $(pure [])
------------------------------------------------------------------------
-- Return conversion

bv512ToVec8Double :: WidthEqProof (BVType 512) (VecType 8 (FloatType DoubleFloat))
bv512ToVec8Double = WidthEqTrans (UnpackBits n8 n64) (VecEqCongruence n8 (ToFloat DoubleFloatRepr))

vec8DoubleToBV512 :: WidthEqProof (VecType 8 (FloatType DoubleFloat)) (BVType 512)
vec8DoubleToBV512 = WidthEqTrans (VecEqCongruence n8 (FromFloat DoubleFloatRepr)) (PackBits n8 n64)

-- $(pure [])

------------------------------------------------------------------------
-- recoverValue

-- | Recover a constant.
recoverCValue :: HasCallStack
              => CValue X86_64 tp
              -> Recover ids (FnValue X86_64 tp)
recoverCValue cv = do
  mem <- frcMemory <$> getFunCtx
  case cv of
    BVCValue w i
      | Just Refl <- testEquality w (knownNat :: NatRepr (ArchAddrWidth X86_64))
        -> emitNewAssign i
      | otherwise -> pure $ FnConstantValue w i
    BoolCValue b ->
      pure $ FnConstantBool b
    RelocatableCValue _w addr
      | Just addrRef <- asSegmentOff mem addr
      , Perm.isExecutable (segmentFlags (segoffSegment addrRef)) -> do
        throwErrorAt ReoptUnsupportedFnValueTag "Cannot lift code pointers."
      | otherwise ->
        case asAbsoluteAddr addr of
          Just absAddr -> emitNewAssign (toInteger absAddr)
          Nothing ->
            throwErrorAt ReoptUnsupportedFnValueTag $ "Cannot lift relative addr " ++ show addr
    SymbolCValue _w sym -> do
      throwErrorAt ReoptUnsupportedFnValueTag $ "Cannot lift symbols " ++ show sym
  where
    -- We want to name constants that could be pointers to globals to
    -- make giving them types easier later.
    emitNewAssign = evalAssignRhs . FnAddrWidthConstant


-- $(pure [])

recoverAssignId :: AssignId ids tp -> Recover ids (FnValue X86_64 tp)
recoverAssignId aid = do
  inv <- gets rsBlockInvariants
  case MapF.lookup aid (biAssignMap inv) of
    -- Skip
    Just (IVDomain d s) -> do
      case d of
        InferredStackOffset _ ->
          throwErrorAt ReoptStackOffsetEscapeTag "Stack offset escaped."
        FnStartRegister _ ->
          throwErrorAt ReoptRegisterEscapeTag "Callee-saved register escaped."
        RegEqualLoc lr -> do
          case s of
            MemSlice{} -> throwErrorAt ReoptUnsupportedFnValueTag "Do not support memslice."
            NoMemSlice -> do
              m <- gets rsPhiLocMap
              case MapF.lookup lr m of
                Nothing -> do
                  throwErrorAt ReoptMissingVariableValue $
                    printf "Missing value for %s." (show (pretty lr))
                Just rv ->
                  coerceRegValue rv
    Just (IVAssignValue prevAid) -> do
      recoverAssignId prevAid
    Just (IVCValue cv) -> do
      recoverCValue cv
    _ -> do
      assignMap <- use rsAssignMap
      case MapF.lookup aid assignMap of
        Just rval -> pure rval
        Nothing ->
          throwErrorAt ReoptUninitializedAssignmentTag $
            "Encountered uninitialized assignment: r" ++ show aid ++ "\n"
            ++ show (MapF.keys assignMap)

-- $(pure [])

prefixFailure :: String -> Recover ids a -> Recover ids a
prefixFailure nm m = m `catchError` h
  where h e =
          throwError $ e { recoverErrorMessage = "Recover " <> Text.pack nm <> " failed: " <> recoverErrorMessage e }

recoverAssignId' :: String -> AssignId ids tp -> Recover ids (FnValue X86_64 tp)
recoverAssignId' nm aid = prefixFailure nm (recoverAssignId aid)

-- $(pure [])

recoverInitialReg :: X86Reg tp -> Recover ids (FnRegValue X86_64 tp)
recoverInitialReg reg = do
  cns <- gets $ biStartConstraints . rsBlockInvariants
  case locDomain cns (RegLoc reg) of
    InferredStackOffset _ ->
      throwErrorAt ReoptUnsupportedFnValueTag "Stack offset escaped."
    FnStartRegister _ ->
      throwErrorAt ReoptUnsupportedFnValueTag "Callee-saved register escaped."
    RegEqualLoc lr -> do
      m <- gets rsPhiLocMap
      case MapF.lookup lr m of
        Nothing -> do
          let var  = show (pretty lr)
          throwErrorAt ReoptUnsupportedFnValueTag $ printf "Uninitialized value %s" var
        Just rv ->
          pure rv

-- | Recover a value without basing it.
recoverRegValue :: HasCallStack
             => Value X86_64 ids tp
             -> Recover ids (FnRegValue X86_64 tp)
recoverRegValue val = do
  case val of
    CValue cv ->
      mkIdentEmbeddingApp <$> recoverCValue cv
    Initial reg ->
      recoverInitialReg reg
    AssignedValue asgn -> do
      mkIdentEmbeddingApp <$> recoverAssignId (assignId asgn)

-- $(pure [])

-- | Recover a stack value
recoverValue :: Value X86_64 ids tp
             -> Recover ids (FnValue X86_64 tp)
recoverValue v = do
  case v of
    CValue cv          -> recoverCValue cv
    Initial reg        -> coerceRegValue =<< recoverInitialReg reg
    AssignedValue asgn -> recoverAssignId (assignId asgn)

-- | Recover a stack value
recoverValue' :: String
              -> Value X86_64 ids tp
              -> Recover ids (FnValue X86_64 tp)
recoverValue' nm v = do
  case v of
    CValue cv -> prefixFailure nm $ recoverCValue cv
    Initial reg -> coerceRegValue =<< prefixFailure nm (recoverInitialReg reg)
    AssignedValue asgn -> recoverAssignId' nm (assignId asgn)

-- $(pure [])

------------------------------------------------------------------------
-- recoverRegister

recoverRegister :: HasCallStack
                => StmtIndex
                -> RegState X86Reg (Value X86_64 ids)
                -> X86Reg tp
                -> Recover ids (FnValue X86_64 tp)
recoverRegister _ regs r = recoverValue' (show r) (regs^. boundValue r)

-- $(pure [])

------------------------------------------------------------------------
-- Return register/value handling

zmmEmbedding :: ZMMType tp -> Embedding tp (BVType 512)
zmmEmbedding ZMMDouble = UExtEmbedding (FromFloat DoubleFloatRepr) n512
zmmEmbedding ZMM512D   = EqEmbedding vec8DoubleToBV512

-- | The returns the type a function return type of this register should return.
--
-- Note. This is different for ZMM registers with the underlying type
-- of the ZMM register returned by `retReg`.
retInfoEmbedding :: X86RetInfo tp -> EmbeddingInv X86Reg tp
retInfoEmbedding (RetBV64 r) = EmbeddingInv (embeddingRefl knownRepr) (X86_GP r)
retInfoEmbedding (RetZMM tp i) =
  let emb = zmmEmbedding tp
   in EmbeddingInv emb (X86_ZMMReg i)

recoverZMMRegValue :: StmtIndex
                   -> ZMMType tp
                   -> Word8
                   -> RegState X86Reg (Value X86_64 ids)
                   -> Recover ids (FnValue X86_64 tp)
recoverZMMRegValue tstmtIdx tp i regs = do
  v512 <- recoverRegister tstmtIdx regs (X86_ZMMReg i)
  case tp of
    ZMMDouble -> do
      logEvent $ LLVMLogEvent "recoverZMMRegValue"
               $ LogInfoTrunc
               $ LLVMBitCastInfo (llvmITypeNat 512) (llvmITypeNat 64)
      v64 <- evalAssignRhs $ FnEvalApp (Trunc v512 n64)
      bitcast "recoverZMMRegValue" v64 (ToFloat DoubleFloatRepr)
    ZMM512D -> do
      bitcast "recoverZMMRegValue" v512 bv512ToVec8Double

returnValueForX86RetInfo :: StmtIndex
                         -> X86RetInfo tp
                         -> RegState X86Reg (Value X86_64 ids)
                         -> Recover ids (FnValue X86_64 tp)
returnValueForX86RetInfo tstmtIdx (RetBV64 r) regs = do
  recoverRegister tstmtIdx regs (X86_GP r)
returnValueForX86RetInfo tstmtIdx (RetZMM tp i) regs = do
  recoverZMMRegValue tstmtIdx tp i regs

-- | Obtain return value informaton for function given register values.
mkReturnValueFromRegs :: forall ids
                      . StmtIndex
                         -> [Some X86RetInfo] -- ^ Return type information
                      -> RegState X86Reg (Value X86_64 ids)
                      -> Recover ids (Maybe (Some (FnValue X86_64)))
mkReturnValueFromRegs _ [] _ = pure Nothing
mkReturnValueFromRegs tstmtIdx [Some r] regs = do
  Just . Some <$> returnValueForX86RetInfo tstmtIdx r regs
mkReturnValueFromRegs tstmtIdx retInfoList regs = do
  Some fields <- fromSomeListWithM (\r -> returnValueForX86RetInfo tstmtIdx r regs) retInfoList
  r <- evalAssignRhs $ FnEvalApp (MkTuple (fmapFC typeRepr fields) fields)
  pure (Just (Some r))

-- $(pure [])

-- | This contains a reference to the function to call, the arguments and return register.
type instance ArchFunType X86_64 = ( FnValue X86_64 (BVType 64)
                                   , [X86ArgInfo]
                                   , [Some X86RetInfo]
                                   )

------------------------------------------------------------------------
-- recoverCallTarget

recoverCallTarget :: Recover ids ( FnValue X86_64 (BVType 64)
                                 , [X86ArgInfo]
                                 , [Some X86RetInfo]
                                 )
recoverCallTarget = do
  mp <- gets $ biCallFunType . rsBlockInvariants
  case mp of
    Just p  -> pure p
    Nothing -> error "Expected call return type."

------------------------------------------------------------------------
-- recoverStmt

-- $(pure [])

checkAssignmentUnused :: HasCallStack => AssignId ids tp -> Recover ids ()
checkAssignmentUnused aid = do
  mSeen <- uses rsAssignMap (MapF.lookup aid)
  case mSeen of
    Nothing -> return ()
    Just _  -> error $ "internal: Assign " ++ show aid ++ " already seen"

-- | Associate assign id with the reuslt
setAssignVal :: HasCallStack
             => AssignId ids tp
             -> FnValue X86_64 tp
             -> Recover ids ()
setAssignVal aid rval = do
  checkAssignmentUnused aid
  rsAssignMap %= MapF.insert aid rval

setAssignRhs :: HasCallStack
             => AssignId ids tp
             -> FnAssignRhs X86_64 (FnValue X86_64) tp
             -> Recover ids ()
setAssignRhs aid rhs = do
  rval <- evalAssignRhs rhs
  setAssignVal aid rval

-- $(pure [])

whenAssignUsed :: AssignId ids tp -> Recover ids () -> Recover ids ()
whenAssignUsed aid m = do
  inv <- gets rsBlockInvariants
  when (biAssignIdUsed aid inv) m

-- | Get information about next memory access
popMemAccessInfo :: StmtIndex -> Recover ids (MemAccessInfo X86_64 ids)
popMemAccessInfo n = do
  s <- get
  case rsPendingMemAccesses s of
    [] -> error "popMemAccessInfo invalid"
    (i,a):r -> do
      put $! s { rsPendingMemAccesses = r }
      if i < n then
        popMemAccessInfo n
       else if i == n then
        pure a
       else do
        error "popMemAccessInfo missed index"

pushMemAccessType :: FnMemAccessType -> Recover ids ()
pushMemAccessType tp = do
  o <- gets $ fromIntegral . rsBlockOff
  modify $ \s -> s { rsSeenMemAccessTypes = (o,tp):rsSeenMemAccessTypes s }

recoverReadMem :: StmtIndex
               -> AssignId ids tp
               -> Value X86_64 ids (BVType 64)
              -> MemRepr tp -> Recover ids ()
recoverReadMem stmtIdx aid addr memRepr = do
  let nm = "r" ++ show aid
  access <- popMemAccessInfo stmtIdx
  case access of
    NotFrameAccess -> do
      pushMemAccessType HeapAccess
      addrVal <- recoverValue' nm addr
      setAssignRhs aid (FnReadMem addrVal (typeRepr memRepr))
    FrameReadInitAccess _o d -> do
      pushMemAccessType StackAccess
      whenAssignUsed aid $ do
        case d of
          InferredStackOffset _ -> do
            throwErrorAt ReoptStackOffsetEscapeTag "Stack pointer escape."
          FnStartRegister _r -> do
            error "Read callee saved register."
          RegEqualLoc l ->
            case testEquality (typeRepr l) (typeRepr memRepr) of
              Nothing -> error "Incorrect type"
              Just Refl -> do
                m <- gets rsPhiLocMap
                case MapF.lookup l m of
                  Nothing ->
                    throwErrorAt ReoptUninitializedPhiVarTag "Uninitialized phi variable."
                  Just rv -> do
                    v <- coerceRegValue rv
                    setAssignVal aid v
    FrameReadWriteAccess writeIdx -> do
      pushMemAccessType StackAccess
      whenAssignUsed aid $ do
        m <- gets rsWriteMap
        case Map.lookup writeIdx m of
          Nothing -> error "Could not find value."
          Just (StackWriteVal v writeRepr) ->
            setAssignVal aid =<< coerceWriteToRead v writeRepr memRepr
    FrameReadOverlapAccess _ -> do
      pushMemAccessType StackAccess
      whenAssignUsed aid $ do
        throwErrorAt ReoptStackReadOverlappingOffsetTag "Stack read at an overlapping offset."
    FrameWriteAccess{} -> error "Expected read access"
    FrameCondWriteAccess{} -> error "Expected read access"
    FrameCondWriteOverlapAccess{} -> error "Expected read access"

-- | This records a stack access if the assignment is a memory
-- read.  It is used so annotations include all memory accesses.
recordPotentialStackMemAccess ::
  Assignment X86_64 ids tp ->
  Recover ids ()
recordPotentialStackMemAccess asgn = do
  case assignRhs asgn of
    ReadMem _addr _memRepr -> do
      pushMemAccessType StackAccess
    CondReadMem _tp _cond _addr _def -> do
      throwErrorAt ReoptUnimplementedFeatureTag "Conditional reads are not yet supported."
    _ -> do
      pure ()

-- | Recover computation needed to assign correct value to assignment
-- identifier.
recoverAssign :: HasCallStack
              => StmtIndex
              -> Assignment X86_64 ids tp
              -> Recover ids ()
recoverAssign stmtIdx asgn = do
  inv <- gets rsBlockInvariants
  let aid = assignId asgn
  case MapF.lookup aid (biAssignMap inv) of
    -- Skip
    Just (IVDomain _ _) -> do
      recordPotentialStackMemAccess asgn
    Just (IVAssignValue _) -> do
      recordPotentialStackMemAccess asgn
    Just (IVCValue _) -> do
      recordPotentialStackMemAccess asgn
    _ -> do
      let nm = "r" ++ show aid
      case assignRhs asgn of
        EvalApp app -> do
          whenAssignUsed aid $ do
            setAssignRhs aid . FnEvalApp =<< traverseFC (recoverValue' nm) app
        SetUndefined tp ->
          whenAssignUsed aid $ do
            setAssignRhs aid (FnSetUndefined tp)
        EvalArchFn f _ -> do
          whenAssignUsed aid $ do
            fval <- traverseFC (recoverValue' nm) f
            case x86ArchFnToLLVM fval of
              Just _ ->
                pure ()
              Nothing -> do
                throwErrorAt ReoptUnimplementedLLVMBackendFeatureTag $
                  "LLVM backend does not yet support: "
                  ++ show (runIdentity (ppArchFn (pure . pretty) f))
            setAssignRhs aid (FnEvalArchFn fval)
        ReadMem addr memRepr -> do
          recoverReadMem stmtIdx aid addr memRepr
        CondReadMem _tp _cond _addr _def -> do
          throwErrorAt ReoptUnimplementedFeatureTag "Conditional reads are not yet supported."

-- $(pure [])

whenWriteUsed :: StmtIndex -> Recover ids () -> Recover ids ()
whenWriteUsed idx m = do
  inv <- gets rsBlockInvariants
  when (biWriteUsed idx inv) m

-- | This should add code as needed to support the statement.
recoverStmt :: StmtIndex -- ^ Index of statement
            -> Stmt X86_64 ids
            -> Recover ids ()
recoverStmt stmtIdx stmt = do
  case stmt of
    AssignStmt asgn -> do
      recoverAssign stmtIdx asgn
    WriteMem addr memRepr val -> do
      ainfo <- popMemAccessInfo stmtIdx
      case ainfo of
        NotFrameAccess -> do
          pushMemAccessType HeapAccess
          rAddr <- recoverValue' "write address" addr
          rVal  <- recoverValue' "write value" val
          addFnStmt $ FnWriteMem rAddr rVal
        FrameReadInitAccess{} -> error "Expected write access"
        FrameReadWriteAccess _ -> error "Expected write access"
        FrameReadOverlapAccess{} -> error "Expected write access"
        FrameCondWriteAccess{} -> error "Expected write access"
        FrameCondWriteOverlapAccess{} -> error "Expected write access"
        FrameWriteAccess _o -> do
          pushMemAccessType StackAccess
          whenWriteUsed stmtIdx $ do
            rval <- recoverRegValue val
            let v = StackWriteVal rval memRepr
            modify $ \s -> s { rsWriteMap = Map.insert stmtIdx v (rsWriteMap s) }

    CondWriteMem _cond _addr _memRepr _val -> do
      error "Conditional writes are not yet supported."
    Comment msg -> do
      addFnStmt $ FnComment msg
    ExecArchStmt astmt0 -> do
      -- Architecture-specific statements are assumed to always
      -- have side effects.
      astmt <- traverseF recoverValue astmt0
      addFnStmt (FnArchStmt (X86FnStmt astmt))
    InstructionStart o _ -> do
      -- Set recovery instruction offset
      modify $ \s -> s { rsBlockOff = o }
    ArchState _ _ -> do
      pure ()

------------------------------------------------------------------------
-- Jump target

-- | Resolve a phi value from an inferred value.
resolveInitInferValue :: HasCallStack
                         => MemSegmentOff 64 -- ^ Address to jump to
                         -> InitInferValue X86_64 tp
                         -> Recover ids (FnRegValue X86_64 tp)
resolveInitInferValue tgt vd =
  case vd of
    InferredStackOffset _ ->
      throwErrorAt ReoptStackOffsetEscapeTag "Stack offset escaped."
    FnStartRegister _ ->
      throwErrorAt ReoptRegisterEscapeTag "Callee-saved register escaped."
    RegEqualLoc lr -> do
      m <- gets rsPhiLocMap
      case MapF.lookup lr m of
        Nothing -> do
          src <- gets rsStartAddr
          let var  = show (pretty lr)
          throwErrorAt ReoptMissingVariableValue $
            printf "%s should provide value of %s to %s" (show src) var (show tgt)
        Just rv ->
          pure rv

-- | Resolve a phi value from an inferred value.
resolveInferValue :: HasCallStack
                  => MemSegmentOff 64 -- ^ Address to jump to
                  -> PostValueMap X86_64 ids
                  -> BoundLoc X86Reg tp
                  -> Recover ids (Some (FnValue X86_64))
resolveInferValue tgt pvm l =
  case pvmFind l pvm of
    IVDomain _ ms@MemSlice{} -> do
      throwErrorAt ReoptUnsupportedMemOpTag $ "Mem slice not supported: " ++ show ms
    IVDomain d NoMemSlice -> do
      fmap Some $ coerceRegValue =<< resolveInitInferValue tgt d
    IVAssignValue aid -> do
      Some <$> recoverAssignId aid
    IVCValue cv -> Some <$> recoverCValue cv
    IVCondWrite idx repr -> do
      m <- gets rsWriteMap
      case Map.lookup idx m of
        Nothing -> error "Could not find write value."
        Just (StackWriteVal rv writeRepr) ->
          case testEquality repr writeRepr of
            Nothing   -> error "Invalid type"
            Just Refl -> Some <$> coerceRegValue rv

-- | Get the phi variable information for the block that starts at the
-- given address.
getBlockInvariants :: MemSegmentOff 64
                   -> FunRecover ids (BlockInvariants X86_64 ids)
getBlockInvariants addr = do
  ctx <- FR ask
  let emsg = "internal: Could not find block invariants for " ++ show addr ++ "."
  pure $! Map.findWithDefault (error emsg) addr (frcBlockDepMap ctx)

-- | Given an address to jump to (which should be the start of a
-- block), this generates a jump target with both the address and phi
-- values to initialize the phi variables in the target.
recoverJumpTarget :: forall ids
                  .  HasCallStack
                  => MapF X86Reg (FnValue X86_64)
                  -> MemSegmentOff 64 -- ^ Address to jump to
                  -> Recover ids (FnJumpTarget X86_64)
recoverJumpTarget retVarMap tgtAddr = do
  thisAddr <- gets rsStartAddr
  -- Get invariant info for target address.
  tgtInv <- liftFunRecover $ getBlockInvariants tgtAddr
  let postValues =
        let emsg = "Could not find post values for target."
         in Map.findWithDefault (error emsg) thisAddr (biPredPostValues tgtInv)
  let recoverVec :: Some (BoundLoc X86Reg)
                 -> Recover ids (Some (FnValue X86_64))
      recoverVec (Some (RegLoc r))
        | Just v <- MapF.lookup r retVarMap = pure (Some v)
      recoverVec (Some l) = resolveInferValue tgtAddr postValues l
  values <- traverse recoverVec (V.fromList (biPhiLocs tgtInv))
  pure $! FnJumpTarget { fnJumpLabel = fnBlockLabelFromAddr tgtAddr
                       , fnJumpPhiValues = values
                       }

-- $(pure [])

------------------------------------------------------------------------
-- recoverBlock

recoverX86TermStmt :: forall ids
                   .  StmtIndex
                   -> X86TermStmt (Value X86_64 ids)
                   -> RegState (ArchReg X86_64) (Value X86_64 ids)
                      -- ^ Register value at start of block
                   -> Maybe (MemSegmentOff 64)
                   -> Recover ids (FnTermStmt X86_64)
recoverX86TermStmt _tstmtIdx tstmt _regs _mnext_addr =
  case tstmt of
    Hlt ->
      throwErrorAt ReoptUnsupportedInFnRecoveryTag "hlt is not supported in function recovery."
    UD2 -> do
      throwErrorAt ReoptUnsupportedInFnRecoveryTag "ud2 is not supported in function recovery."

-- | This parses the arguments that we are going to pass to a function call.
evalFunctionArg :: StmtIndex -- ^ Index of terminal statement call is at.
                -> RegState X86Reg (Value X86_64 ids)
                   -- ^ Values in registers at time of call.
                -> X86ArgInfo -- ^ Information about argument.
                -> Recover ids (Some (FnValue X86_64))
evalFunctionArg tstmtIdx regs argInfo =
  case argInfo of
    ArgBV64 r -> do
      Some <$> recoverRegister tstmtIdx regs (X86_GP r)
    ArgZMM tp i -> do
      Some <$> recoverZMMRegValue tstmtIdx tp i regs

-- $(pure [])

{-
retTypeReprList :: [X86RetInfo] -> Some (P.List TypeRepr)
retTypeReprList [] = P.Nil
retTypeReprList (r:l) =
  case (retTypeRepr r, retTypeReprList l) of
    (Some rtp, Some ltp) -> Some (rtp P.:< ltp)

-- | Analyze list of return values to create information needed to
--  resolve return.
evalReturnVar :: forall ids
               . CallTarget args
               [X86RetInfo]
               -> Recover ids (Maybe (Some FnReturnVar), MapF X86Reg (FnValue X86_64))
evalReturnVar [] = do
  pure (Nothing, MapF.empty)
evalReturnVar [RetBV64 r] = do
aid <- freshId
  let v = FnReturnVar { frAssignId = aid
                      , frReturnType = knownRepr
                      }
  pure (Just (Some v), MapF.singleton (X86_GP r) (FnReturn v))
evalReturnVar [RetMM512D i] = do
  aid <- freshId
  let v = FnReturnVar { frAssignId = aid
                      , frReturnType = knownRepr
                      }
  r <- bitcast (FnReturn v) vec8DoubleToBV512
  pure (Just (Some v), MapF.singleton (X86_ZMMReg i) r)
evalReturnVar l = do
  aid <- freshId
  Some fieldTypes <- pure $ retTypeReprList l
  let v = FnReturnVar { frAssignId = aid
                      , frReturnType = TupleTypeRepr fieldTypes
                      }
  let mkRetReg :: X86RetInfo
               -> ReturnInfo X86_64
               -> Recover ids (ReturnInfo X86_64)
      mkRetReg ri (vars,m) = do
        aid <- freshId
        Some reg <- pure (retReg ri)
        let v = FnReturnVar { frAssignId = aid
                            , frReturnType = typeRepr reg
                            }
        pure (Some v:vars, MapF.insert reg (FnReturn v) m)
  foldrM mkRetReg ([], MapF.empty) l
-}

-- $(pure [])

recoverStmts :: [Stmt X86_64 ids] -> Recover ids ()
recoverStmts [] = pure ()
recoverStmts (n:r) = do
  stmtIdx <- gets rsInsnIndex
  recoverStmt stmtIdx n
  modify $ \s -> s { rsInsnIndex = stmtIdx + 1 }
  recoverStmts r

-- $(pure [])

data RetFieldRelation retType fieldType where
  IdentField :: RetFieldRelation retType retType
  IndexField :: !(P.Index flds tp) -> RetFieldRelation (TupleType flds) tp

data FieldRegRelation ftype rtype where
  GPFieldRegRel :: FieldRegRelation (BVType 64) (BVType 64)
  ZmmV8DFieldRegRel :: FieldRegRelation (VecType 8 (FloatType DoubleFloat)) (BVType 512)
  ZmmDFieldRegRel :: FieldRegRelation (FloatType DoubleFloat) (BVType 512)


-- | Denotes a value returned by a function return.
data RetRegRelation retType regType where
  RetRegRelation :: !(RetFieldRelation retType fieldType)
                 -> !(FieldRegRelation fieldType regType)
                 -> RetRegRelation retType regType

-- | List of return values
data RetRegRelations retType =
  RetRegRelations { fnRetValuesType :: !(TypeRepr retType)
                  -- ^ Type actually returned.
                  , fnRetValueMap   :: !(MapF X86Reg (RetRegRelation retType))
                  }

retInfoRegPair :: X86RetInfo fieldType
               -> Pair X86Reg (FieldRegRelation fieldType)
retInfoRegPair (RetBV64   r)         = Pair (X86_GP r)     GPFieldRegRel
retInfoRegPair (RetZMM ZMMDouble  i) = Pair (X86_ZMMReg i) ZmmDFieldRegRel
retInfoRegPair (RetZMM ZMM512D i)    = Pair (X86_ZMMReg i) ZmmV8DFieldRegRel

-- | Infer return values from ret info.
inferRetRegRelations :: forall fields . P.List X86RetInfo fields -> Maybe (Some RetRegRelations)
inferRetRegRelations P.Nil = Nothing
inferRetRegRelations (retInfo P.:< P.Nil) =
  case retInfoRegPair retInfo of
    Pair reg rel ->
      let frv = RetRegRelations { fnRetValuesType = typeRepr retInfo
                                , fnRetValueMap = MapF.singleton reg (RetRegRelation IdentField rel)
                                }
       in Just (Some frv)
inferRetRegRelations fields =
  let insField :: P.Index fields tp
               -> X86RetInfo tp
               -> MapF X86Reg (RetRegRelation (TupleType fields))
               -> MapF X86Reg (RetRegRelation (TupleType fields))
      insField idx retInfo m =
        case retInfoRegPair retInfo of
          Pair reg fieldRegRel ->
            MapF.insert reg (RetRegRelation (IndexField idx) fieldRegRel) m
      frv = RetRegRelations { fnRetValuesType = TupleTypeRepr (fmapFC typeRepr fields)
                            , fnRetValueMap = P.ifoldr insField MapF.empty fields
                            }
   in Just (Some frv)

getRetField :: FnValue X86_64 retType
            -> RetFieldRelation retType fieldType
            -> Recover ids (FnValue X86_64 fieldType)
getRetField retVal IdentField = pure $! retVal
getRetField retVal (IndexField idx) = do
  case typeRepr retVal of
    TupleTypeRepr fieldTypes ->
      evalAssignRhs (FnEvalApp (TupleField fieldTypes retVal idx))

resolveRetInfoValue :: MapF X86Reg (RetRegRelation retType)
                    -> FnValue X86_64 retType
                    -> X86RetInfo fieldType
                    -> Recover ids (FnValue X86_64 fieldType)
resolveRetInfoValue regRel retVal (RetBV64 r) =
  case MapF.lookup (X86_GP r) regRel of
    Just (RetRegRelation retFieldRel GPFieldRegRel) -> do
      getRetField retVal retFieldRel
    Nothing -> throwErrorAt ReoptUnresolvedReturnValTag "Could not resolve return value."
resolveRetInfoValue regRel retVal (RetZMM ZMM512D i) =
  case MapF.lookup (X86_ZMMReg i) regRel of
    Just (RetRegRelation retFieldRel ZmmV8DFieldRegRel) -> do
      getRetField retVal retFieldRel
    Just (RetRegRelation retFieldRel ZmmDFieldRegRel) -> do
      vecVal <- evalAssignRhs $ FnSetUndefined knownRepr
      fv <- getRetField retVal retFieldRel
      evalAssignRhs $ FnEvalApp (InsertElement knownRepr vecVal 0 fv)
    Nothing -> throwErrorAt ReoptUnresolvedReturnValTag "Could not resolve return value."
resolveRetInfoValue regRel retVal (RetZMM ZMMDouble i) =
  case MapF.lookup (X86_ZMMReg i) regRel of
    Just (RetRegRelation retFieldRel ZmmDFieldRegRel) -> do
      getRetField retVal retFieldRel
    Just (RetRegRelation retFieldRel ZmmV8DFieldRegRel) -> do
      fv <- getRetField retVal retFieldRel
      evalAssignRhs $ FnEvalApp (ExtractElement knownRepr fv 0)
    Nothing -> throwErrorAt ReoptUnresolvedReturnValTag "Could not resolve return value."

resolveRetValueFields :: MapF X86Reg (RetRegRelation retType)
                      -> FnValue X86_64 retType
                      -> V.Vector (Some X86RetInfo)
                      -> P.List TypeRepr flds
                      -> P.List (FnValue X86_64) flds
                      -> Int
                      -> Recover ids (Some (FnValue X86_64))
resolveRetValueFields _ _ _ types vals 0 = do
  fmap Some $ evalAssignRhs $ FnEvalApp (MkTuple types vals)
resolveRetValueFields regRel retVal callerRets types vals i =
  case callerRets V.! (i-1) of
    Some ri -> do
      v <- resolveRetInfoValue regRel retVal ri
      resolveRetValueFields regRel retVal callerRets (typeRepr v P.:< types) (v P.:< vals) (i-1)

-- | Resolve the return value of a function
resolveRetValue :: MapF X86Reg (RetRegRelation retType)
                   -- ^ Maps registers with information for retrieving value
                   -- from return value.
                -> FnValue X86_64 retType
                   -- ^ Return value of tail call.
                -> V.Vector (Some X86RetInfo)
                   -- ^ Information aboout return values expected by return.
                -> Recover ids (Maybe (Some (FnValue X86_64)))
resolveRetValue regRel retVal callerRets =
  case V.length callerRets of
    0 -> pure Nothing
    1 ->
      case callerRets V.! 0 of
        Some ri -> do
          val <- resolveRetInfoValue regRel retVal ri
          pure $! (Just $! Some val)
    n ->
      Just <$> resolveRetValueFields regRel retVal callerRets P.Nil P.Nil n

-- | Compute the return type (if any) of this function.
retReturnType :: [Some X86RetInfo] -> Maybe (Some TypeRepr)
retReturnType [] = Nothing
retReturnType [r] = Just $! mapSome typeRepr r
retReturnType l =
 case fromSomeList l of
   Some rets -> Just $! Some $ TupleTypeRepr (fmapFC typeRepr rets)

-- | Generate FnBlock from parsed block
recoverBlock :: forall ids
             .  ParsedBlock X86_64 ids
                -- ^ Entire block.
             -> Recover ids (FnTermStmt X86_64)
recoverBlock b = do
  -- Note that this will process the write of the return address of the stack,
  -- but this should not count as a demanded value and not affect the LLVM.
  -- However, it is needed to ensure annotations are generated for all memory
  -- events.
  recoverStmts (pblockStmts b)
  let tstmtIdx = length (pblockStmts b)
  -- Block recovery may need to strip statements, so we case split on terminal statement.
  case pblockTermStmt b of
    -- Handle tail call.
    ParsedCall regs Nothing -> do
      -- Get call target
      (callTarget, callArgLocs, callReturnLocs) <- recoverCallTarget
      -- Evaluate call arguments
      args <- mapM (evalFunctionArg tstmtIdx regs) callArgLocs

      -- Check this is a legal tail call.
      thisReturnLocs <- frcCurrentFunctionReturns <$> getFunCtx
      if thisReturnLocs == callReturnLocs then
        pure $ FnTailCall callTarget args
       else do
        Some callReturnList <- pure $ fromSomeList callReturnLocs
        case inferRetRegRelations callReturnList of
          Nothing ->
            -- If we don't have a return value, then we make it up.
            case retReturnType thisReturnLocs of
              Nothing        -> pure $ FnRet Nothing
              Just (Some tp) -> pure $ FnRet $ Just $ Some (FnUndefined tp)
--            throwErrorAt "Tail call returns no values, but function expects return values."
          Just (Some callRetInfo) -> do
            v <- mkReturnVar (fnRetValuesType callRetInfo)
            addFnStmt $! FnCall callTarget args (Just (Some v))
            mretValue <- resolveRetValue (fnRetValueMap callRetInfo) (FnReturn v) (V.fromList thisReturnLocs)
            pure $ FnRet mretValue

    -- Handle non-tail call.
    ParsedCall regs (Just retAddr) -> do
      -- Get call target
      (callTarget, callArgLocs, callReturnLocs) <- recoverCallTarget
      -- Evaluate call arguments
      args <- mapM (evalFunctionArg tstmtIdx regs) callArgLocs
      retMap <-
        case callReturnLocs of
          [] -> do
            addFnStmt (FnCall callTarget args Nothing)
            pure MapF.empty
          [Some ret] -> do
            case retInfoEmbedding ret of
              EmbeddingInv emb r -> do
                v <- mkReturnVar (embeddingSource emb)
                addFnStmt (FnCall callTarget args (Just (Some v)))
                let rv = FnReturn v
                v' <- coerceRegValue (EmbeddingApp rv emb)
                pure $! MapF.singleton r v'
          _ -> do
            case P.fromListWith (mapSome retInfoEmbedding) callReturnLocs of
              Some retEmbeddings -> do
                let fieldTypes = fmapFC typeRepr retEmbeddings
                let tp = TupleTypeRepr fieldTypes
                v <- mkReturnVar tp
                addFnStmt (FnCall callTarget args (Just (Some v)))
                let rv = FnReturn v
                let g :: forall fields tp
                      .  P.List TypeRepr fields
                      -> FnValue X86_64 (TupleType fields)
                      -> MapF X86Reg (FnValue X86_64)
                      -> P.Index fields tp
                      -> EmbeddingInv X86Reg tp
                      -> Recover ids (MapF X86Reg (FnValue X86_64))
                    g flds s m idx (EmbeddingInv emb r) = do
                      fv <- evalAssignRhs $ FnEvalApp $ TupleField flds s idx
                      v' <- coerceRegValue (EmbeddingApp fv emb)
                      pure $! MapF.insert r v' m
                P.ifoldlM (g fieldTypes rv) MapF.empty retEmbeddings
      FnJump <$> recoverJumpTarget retMap retAddr
    PLTStub{} -> do
      throwErrorAt ReoptCannotRecoverFnWithPLTStubsTag "Cannot recover functions with PLT stubs."
    ParsedTranslateError{} -> do
      error "Functions with translate error should not appear."
    ClassifyFailure{} ->
      error "Functions with classify failure should not appear."

    ParsedJump _regs tgtAddr -> do
      -- Get target
      tgt  <- recoverJumpTarget MapF.empty tgtAddr
      pure (FnJump tgt)

    ParsedBranch _regs cond trueAddr falseAddr -> do
      -- Translate condition
      condVal <- recoverValue cond
      -- Get registers that may be needed by one of the two branch targets.
      trueTgt  <- recoverJumpTarget MapF.empty trueAddr
      falseTgt <- recoverJumpTarget MapF.empty falseAddr
      pure (FnBranch condVal trueTgt falseTgt)

    ParsedLookupTable _layout _regs idx vec -> do
      -- Recover term statement
      idx'   <- recoverValue idx
      tgtVec <- traverse (recoverJumpTarget MapF.empty) vec
      pure (FnLookupTable idx' tgtVec)

    ParsedReturn regs -> do
      ftr <- frcCurrentFunctionReturns <$> getFunCtx
      FnRet <$> mkReturnValueFromRegs tstmtIdx ftr regs

    ParsedArchTermStmt ts regs nextAddr -> do
      -- Recover term statement
      recoverX86TermStmt tstmtIdx ts regs nextAddr

-- $(pure [])

ppInvariant :: Pair (BoundLoc (ArchReg arch)) (InitInferValue arch) -> FnBlockInvariant arch
ppInvariant (Pair l d) =
  case d of
    InferredStackOffset o -> FnStackOff o l
    FnStartRegister r     -> FnCalleeSavedReg r l
    RegEqualLoc r         -> FnEqualLocs r l

evalRecover :: forall ids
            .  ParsedBlock X86_64 ids
            -> BlockInvariants X86_64 ids
               -- ^ Invariants for block
            -> [MemSegmentOff 64]
               -- ^ Predecessors of current
            -> V.Vector (Some (FnPhiVar X86_64))
            -> MapF (BoundLoc X86Reg) (FnRegValue X86_64)
               -- ^ Map from locations to values.
            -> FunRecover ids (FnBlock X86_64)
evalRecover b inv preds phiVars locMap = do
  let s0 = RS { rsStartAddr = pblockAddr b
              , rsBlockInvariants = inv
              , rsPredBlockAddrs = preds
              , rsPhiLocMap = locMap
              , rsInsnIndex = 0
              , rsBlockOff = 0
              , rsBlockSize = blockSize b
              , _rsCurStmts = Seq.empty
              , _rsAssignMap = MapF.empty
              , rsWriteMap = Map.empty
              , rsSeenMemAccessTypes = []
              , rsPendingMemAccesses = biMemAccessList inv
              , rsLogEvents = []
              }

  let pr =
        case pblockPrecond b of
          Left _e -> error "Attempt to recover function with error in block precondition."
          Right r -> r

  (tm, s) <- runStateT (runRecover (recoverBlock b)) s0

  modify' $ \fs -> fs {frsLogEvents = rsLogEvents s ++ frsLogEvents fs}

  return $! FnBlock { fbLabel = fnBlockLabelFromAddr (pblockAddr b)
                    , fbPrecond = pr
                    , fbSize = fromIntegral (blockSize b)
                    , fbPrevBlocks = fnBlockLabelFromAddr <$> preds
                    , fbInvariants = ppInvariant <$> locMapToList (bscLocMap (biStartConstraints inv))
                    , fbPhiVars = phiVars
                    , fbStmts = toList (s^.rsCurStmts)
                    , fbTerm = tm
                    , fbMemInsnAddrs = V.fromList (reverse (rsSeenMemAccessTypes s))
                    }

-- $(pure [])

------------------------------------------------------------------------
-- recoverFunction

-- | Introduce a new phi var for a class, and update location map.
addPhiVarForClass :: BlockInvariants X86_64 ids
                  -> ([Some (FnPhiVar X86_64)], MapF (BoundLoc X86Reg) (FnRegValue X86_64))
                     -- ^ Previously constructed location map.
                  -> Some (BoundLoc X86Reg)
                     -- ^ Class to add
                  -> FunRecover ids ([Some (FnPhiVar X86_64)], MapF (BoundLoc X86Reg) (FnRegValue X86_64))
addPhiVarForClass inv (vl,m0) (Some phiLoc) = do
  vnm <- funFreshId
  let tp = typeRepr phiLoc
  let ll = llValues (MapF.findWithDefault (LL []) phiLoc (biLocMap inv))
  let phiVar = FnPhiVar { unFnPhiVar = vnm
                        , fnPhiVarType = tp
                        , fnPhiVarRep  = phiLoc
                        , fnPhiVarLocations = ll
                        }
  let v = mkIdentEmbeddingApp (FnPhiValue phiVar)
  let isReg (RegLoc _)        = True
      isReg (StackOffLoc _ _) = False
  let ins m l = MapF.insert l v m
  let m' = foldl' ins m0 (phiLoc : filter isReg ll)
  seq m' $ pure (Some phiVar:vl, m')

recoverInnerBlock :: ParsedBlock X86_64 ids
                  -> FunRecover ids (FnBlock X86_64)
recoverInnerBlock b = do
  let addr = pblockAddr b
  fInfo <- frcInterp <$> FR ask
  inv <- getBlockInvariants addr
  (phiVars, locMap) <- foldlM (addPhiVarForClass inv) ([],MapF.empty) (biPhiLocs inv)
   -- Get predecessors for this block.
  -- let preds = fromJust (error "recoverInnerBlock: Nothing") (Map.lookup addr (funBlockPreds fInfo))
  -- Temporary hack, likely unsound
  let preds = fromMaybe [] (Map.lookup addr (funBlockPreds fInfo))
   -- Generate phi nodes from predecessors and registers that this block refers to.
  let phiVarVec = V.fromList (reverse phiVars)
  evalRecover b inv preds phiVarVec locMap

-- $(pure [])

x86TermStmtNext :: StartInferContext X86_64
                -> InferState X86_64 ids
                -> Int
                -> X86TermStmt (Value X86_64 ids)
                -> RegState X86Reg (Value X86_64 ids)
                -> Either (RegisterUseError X86_64)
                          (PostValueMap X86_64 ids, BlockStartConstraints X86_64)
x86TermStmtNext _ _ _ Hlt _ = error "Hlt has no successor."
x86TermStmtNext _ _ _ UD2 _ = error "UD2 has no successor."

x86TermStmtUsage :: SyscallPersonality
                 -> X86TermStmt (Value X86_64 ids)
                 -> RegState (ArchReg X86_64) (Value X86_64 ids)
                 -> BlockUsageSummary X86_64 ids
                 -> Either (RegisterUseError X86_64) (RegDependencyMap X86_64 ids)
x86TermStmtUsage _ Hlt _ _ = pure mempty
x86TermStmtUsage _ UD2 _ _ = pure mempty

type IsSigned = Bool

-- | Type for argument expected by printf.
data PrintfArgType
  = Char     !IsSigned
  | Short    !IsSigned
  | Int      !IsSigned
  | Long     !IsSigned
  | LongLong !IsSigned
  | IntMaxT  !IsSigned
  | SizeT    !IsSigned
  | PtrDiffT
  | WIntT -- ^ A wint_t character
  | Double
  | LongDouble
  | VoidPtrT -- ^ A void pointer
  | CharPtrT -- ^ A character pointer
  | WCharPtrT -- ^ A wchar_t pointer
  | NoArg

printfIntArgType :: IsSigned -> Printf.Length -> Either String PrintfArgType
printfIntArgType signed len =
  case len of
    Printf.HH         -> pure $ Char signed
    Printf.H          -> pure $ Short signed
    Printf.LongInt    -> pure $ Long signed
    Printf.LLongInt   -> pure $ LongLong signed
    Printf.LongDouble -> Left "L prefix only applicable to floating point."
    Printf.Q          -> pure $ LongLong signed
    Printf.J          -> pure $ IntMaxT signed
    Printf.Z          -> pure $ SizeT signed
    Printf.T          -> pure PtrDiffT
    Printf.NoLength   -> pure $ Int signed

printfFloatLength :: Printf.Length -> Either String PrintfArgType
printfFloatLength len =
  case len of
    Printf.LongDouble -> pure LongDouble
    Printf.NoLength -> pure Double
    _ -> Left $ Printf.ppLength len ++ " prefix not applicable to floating point."

printfArgType :: Printf.Specifier -> Either String PrintfArgType
printfArgType sp = do
  let len = Printf.specifierLength sp
  case Printf.specifierConv sp of
    Printf.IntDecimal          -> printfIntArgType True len
    Printf.UnsignedOctal       -> printfIntArgType False len
    Printf.UnsignedDecimal     -> printfIntArgType False len
    Printf.UnsignedHexadecimal -> printfIntArgType False len
    Printf.FloatExponent    -> printfFloatLength len
    Printf.FloatDecimal     -> printfFloatLength len
    Printf.FloatGeneral     -> printfFloatLength len
    Printf.FloatHexadecimal -> printfFloatLength len
    Printf.Char ->
      case len of
        Printf.LongInt -> pure WIntT
        _              -> pure $ Int True
    Printf.String ->
      case len of
        Printf.LongInt -> pure WCharPtrT
        _              -> pure CharPtrT
    Printf.Pointer -> pure VoidPtrT
    Printf.CharCount   -> pure NoArg
    Printf.PrintStderr -> pure NoArg


data PrintfArgState = PrintfArgState
  { pasRemainingIntArgs :: [F.Reg64]
  , pasFloatArgCount    :: !Word8
  , pasArgRegs          :: [X86ArgInfo]
    -- ^ Argument registers  in reverse order.
  }

-- | Initial printf arg state.
initPrintfArgState :: PrintfArgState
initPrintfArgState =
  PrintfArgState { pasRemainingIntArgs = x86GPPArgumentRegs
                 , pasFloatArgCount = 0
                 , pasArgRegs = []
                 }

getPrintfIntArg :: PrintfArgState -> Maybe (F.Reg64, PrintfArgState)
getPrintfIntArg pas =
  case pasRemainingIntArgs pas of
    [] -> Nothing
    (h:r) ->
      let pas' = pas { pasArgRegs = ArgBV64 h : pasArgRegs pas
                     , pasRemainingIntArgs = r
                     }
       in seq pas' $ Just (h, pas')


addIntArg :: PrintfArgState -> Either String PrintfArgState
addIntArg pas =
  case getPrintfIntArg pas of
    Nothing       -> Left "Too many integer arguments."
    Just (_,pas') -> Right pas'

addDoubleArg :: PrintfArgState -> Either String PrintfArgState
addDoubleArg pas = do
  let cnt = pasFloatArgCount pas
  when (cnt >= 8) $ Left "Too many float arguments."
  pure $ pas { pasArgRegs = ArgZMM ZMMDouble cnt : pasArgRegs pas
             , pasFloatArgCount = cnt+1
             }

addArg :: PrintfArgState -> PrintfArgType -> Either String PrintfArgState
addArg pas tp =
  case tp of
    Char _     -> addIntArg pas
    Short _    -> addIntArg pas
    Int _      -> addIntArg pas
    Long _     -> addIntArg pas
    LongLong _ -> addIntArg pas
    IntMaxT _  -> addIntArg pas
    SizeT _    -> addIntArg pas
    PtrDiffT   -> addIntArg pas
    WIntT      -> addIntArg pas
    Double     -> addDoubleArg pas
    LongDouble -> Left "Do not support long double arguments."
    VoidPtrT   -> addIntArg pas
    CharPtrT   -> addIntArg pas
    WCharPtrT  -> addIntArg pas
    NoArg      -> pure pas

parseUnpackedFormat :: PrintfArgState -> Printf.UnpackedRep -> Either String PrintfArgState
parseUnpackedFormat pas (Printf.UnpackedTerm _) =
  Right pas
parseUnpackedFormat pas (Printf.UnpackedLiteral _ r) =
  parseUnpackedFormat pas r
parseUnpackedFormat pas (Printf.UnpackedSpecifier s r) = do
  argType <- printfArgType s
  pas' <- addArg pas argType
  parseUnpackedFormat pas' r
parseUnpackedFormat _ (Printf.UnpackedError e) =
  Left ("printf error " ++ show e)

inferPrintfArgs :: Memory 64 -- ^ Memory state
                -> BSC.ByteString -- ^ Name of function
                -> RegState X86Reg (Value X86_64 ids) -- Register values
                -> PrintfArgState -- ^ Initial printf arg state
                -> Either String (CallRegs X86_64 ids)
inferPrintfArgs mem nm regs initState = do
  (formatStringReg, initState') <-
    case getPrintfIntArg initState of
      Nothing -> Left "Could not get printf format string register."
      Just p  -> Right p
  let printfFormatValue = regs^.boundValue (X86_GP formatStringReg)
  printfFormatRawAddr <-
    case addrWidthClass (memAddrWidth mem) (valueAsMemAddr printfFormatValue) of
      Just addr -> pure addr
      Nothing -> Left $ printf "Could not resolve printf format string address (value = %s)." (show printfFormatValue)
  printfFormatAddr <-
    case asSegmentOff mem printfFormatRawAddr of
      Just addr -> pure addr
      Nothing -> Left $ printf "Could not resolve printf format address %s in memory." (show printfFormatRawAddr)
  s <- case readNullTermString printfFormatAddr of
         NullTermString s -> pure s
         NoNullTerm -> Left "Could not read printf format string."
         NullTermMemoryError e -> Left $ "Access error for printf format sting: " ++ show e
         RelocationBeforeNull _ -> Left "Encountered relocation in format string."
  let uf = Printf.unpackFormat s
  case parseUnpackedFormat initState' uf of
    Left msg -> Left $ "printf error: " ++ show s ++ "\n" ++ msg
    Right pas -> do
      -- Type in declaration
      let funType :: FunctionType X86_64
          funType = do
            let declArgTypes = argRegTypeRepr <$> reverse (pasArgRegs initState')
            FunctionType { fnArgTypes = declArgTypes
                         , fnReturnType = Just (Some (BVTypeRepr n64))
                         , fnVarArgs = True
                         }
      let fnEntry :: FnValue X86_64 (BVType 64)
          fnEntry = FnFunctionEntryValue funType nm

      let x86ArgInfo :: [X86ArgInfo]
          x86ArgInfo = reverse (pasArgRegs pas)
      let x86RetInfo :: [Some X86RetInfo]
          x86RetInfo = [Some (RetBV64 F.RAX)]
      Right $ CallRegs { callRegsFnType = (fnEntry, x86ArgInfo, x86RetInfo)
                       , callArgValues = argValue regs <$> x86ArgInfo
                       , callReturnRegs = viewSome retReg <$> x86RetInfo
                       }


-- | Create flag value.
oCreat :: Integer
oCreat = 64

-- | Create function type from arg info and ret info
mkX86FunctionType ::
  X86FunTypeInfo ->
  FunctionType X86_64
mkX86FunctionType (X86NonvarargFunType args rets) =
  FunctionType {
    fnArgTypes = argRegTypeRepr <$> args,
    fnReturnType = retReturnType rets,
    fnVarArgs = False
  }
mkX86FunctionType (X86PrintfFunType icnt0) =
  let intType = Some (BVTypeRepr n64)
   in FunctionType {
          fnArgTypes = replicate (icnt0+1) intType,
          fnReturnType = Just intType,
          fnVarArgs = True
        }
mkX86FunctionType X86OpenFunType =
  let stringPtrType = BVTypeRepr n64
      intType = BVTypeRepr n64
   in FunctionType {
          fnArgTypes = [ Some stringPtrType, Some intType ],
          fnReturnType = Just (Some intType),
          fnVarArgs = True
        }

x86TranslateCallType ::
  Memory 64 ->
  -- | Name of function
  BSC.ByteString ->
  -- | Registers when call occurs.
  RegState X86Reg (Value X86_64 ids) ->
  X86FunTypeInfo ->
  Either RegisterUseErrorReason (CallRegs X86_64 ids)
x86TranslateCallType _mem nm regs x86Ftp@(X86NonvarargFunType args rets) = do
  let ftp = mkX86FunctionType x86Ftp
  let v = FnFunctionEntryValue ftp nm
  Right CallRegs { callRegsFnType = (v, args, rets)
                 , callArgValues = argValue regs <$> args
                 , callReturnRegs = viewSome retReg <$> rets
                 }
x86TranslateCallType mem nm regs (X86PrintfFunType icnt0) = do
  let resolveInitArgs 0 s = Right s
      resolveInitArgs n s = do
        (_, s') <-
          case getPrintfIntArg s of
            Nothing ->
              Left $
                Reason
                  ResolutonFailureCallToKnownVarArgsFunction
                  "Too many printf initial args."
            Just r ->
              Right r
        resolveInitArgs (n-1) s'
  s <- resolveInitArgs icnt0 initPrintfArgState
  case inferPrintfArgs mem nm regs s of
    Left msg ->
      Left $ Reason ResolutonFailureCallToKnownVarArgsFunction msg
    Right r ->
      Right r
x86TranslateCallType _mem nm regs x86Ftp@X86OpenFunType = do
  let ftp = mkX86FunctionType x86Ftp
  let v = FnFunctionEntryValue ftp nm

  -- Register for storing flags
  let flagsReg = F.RSI
  let flagsValue = regs^.boundValue (X86_GP flagsReg)
  isCreat <-
      case flagsValue of
        BVValue _ val -> Right (val .&. oCreat == oCreat)
        _ -> Left $ Reason ResolutonFailureCallToKnownVarArgsFunction "Could not resolve flags as constant."
  -- Get number of arguments for open
  let argCnt = if isCreat then 3 else 2

  let args :: [X86ArgInfo]
      args = fmap ArgBV64 (take argCnt x86GPPArgumentRegs)
  let rets :: [Some X86RetInfo]
      rets = [Some (RetBV64 F.RAX)]
  Right CallRegs { callRegsFnType = (v, args, rets)
                  , callArgValues  = argValue regs <$> args
                  , callReturnRegs = [Some (X86_GP F.RAX)]
                  }

-- | Compute map from block starting addresses to the dependencies required to
-- run block.
x86CallRegs ::
  forall ids .
  Memory 64 ->
  -- | Maps start address of function to the function name.
  (MemSegmentOff 64 -> Maybe BSC.ByteString) ->
  -- | Maps function names to be exported to the type associated with that name.
  (BSC.ByteString -> Maybe X86FunTypeInfo) ->
  -- | Address of the call statement.
  MemSegmentOff 64 ->
  -- | Registers when call occurs.
  RegState X86Reg (Value X86_64 ids) ->
  Either RegisterUseErrorReason (CallRegs X86_64 ids)
x86CallRegs mem funNameMap funTypeMap _callSite regs = do
  nm <- do
    let ipVal = regs^.boundValue ip_reg
    case ipVal of
      BVValue _ val -> do
        let faddr = absoluteAddr (fromInteger val)
        callTarget <-
          case asSegmentOff mem faddr  of
            Just r -> pure r
            Nothing -> Left $ Reason InvalidCallTargetAddress (memWordValue (addrOffset faddr))
        case funNameMap callTarget of
          Just r ->
            Right r
          Nothing ->
            Left $ Reason CallTargetNotFunctionEntryPoint (memWordValue (addrOffset faddr))
      RelocatableValue _ faddr -> do
        callTarget <-
          case asSegmentOff mem faddr of
            Just r -> pure r
            Nothing -> Left $ Reason InvalidCallTargetAddress (memWordValue (addrOffset faddr))
        case funNameMap callTarget of
          Just r ->
            Right r
          Nothing ->
            Left $ Reason CallTargetNotFunctionEntryPoint (memWordValue (addrOffset faddr))
      SymbolValue _ (SymbolRelocation nm _ver) -> do
        pure nm
      _ ->
        Left $ Reason IndirectCallTarget ()
  case funTypeMap nm of
    Just tp -> x86TranslateCallType mem nm regs tp
    Nothing -> Left $ Reason UnknownCallTargetArguments nm

uninitRegs :: [Pair X86Reg (FnRegValue X86_64)]
uninitRegs =
  [ Pair (X86_ZMMReg i) (mkIdentEmbeddingApp (FnUndefined knownRepr))
  | i <- [0..31]
  ]

-- @initializeArgumentValues args locMap@ initializes the @locMap@
-- mapping from locations to values with the initial values associated
-- with each function argument.
initializeArgumentValues
  :: [X86ArgInfo]
  -> MapF (BoundLoc X86Reg) (FnRegValue X86_64)
  -> MapF (BoundLoc X86Reg) (FnRegValue X86_64)
initializeArgumentValues args locMap = ifoldr insArg locMap args
  where insArg :: Int -- ^ Index of argument
               -> X86ArgInfo -- ^ Information about source of argument.
               -> MapF (BoundLoc X86Reg) (FnRegValue X86_64)
               -> MapF (BoundLoc X86Reg) (FnRegValue X86_64)
        insArg i (ArgBV64 r) m =
          let tp = BVTypeRepr n64
           in MapF.insert (RegLoc (X86_GP r)) (mkIdentEmbeddingApp (FnArg i tp)) m
        insArg i (ArgZMM zmmType zmmIdx) m = do
          let loc = RegLoc (X86_ZMMReg zmmIdx)
          let emb = zmmEmbedding zmmType
          let itp = embeddingSource emb
          MapF.insert loc (EmbeddingApp (FnArg i itp) emb) m

x86BlockInvariants
  :: SyscallPersonality
  -> Memory 64
  -> (MemSegmentOff 64 -> Maybe BSC.ByteString)
     -- ^ Map from addresses that correspond to function
     -- entry points to the name of the function at that
     -- addresses.
  -> (BSC.ByteString -> Maybe X86FunTypeInfo)
     -- ^ Map from function name to the type information at that address.
  -> DiscoveryFunInfo X86_64 ids
  -> [Some X86RetInfo]
     -- ^ return arguments for function
  -> Either (RegisterUseError X86_64) (BlockInvariantMap X86_64 ids)
x86BlockInvariants sysp mem funNameMap funTypeMap fInfo rets = do
  let useCtx = RegisterUseContext
              { archCallParams = x86_64CallParams
              , archPostTermStmtInvariants = x86TermStmtNext
              , calleeSavedRegisters =
                  [ Some RBP, Some RBX, Some R12, Some R13, Some R14, Some R15 ]
              , callScratchRegisters = []
              , returnRegisters = viewSome retReg <$> rets
              , reguseTermFn = x86TermStmtUsage sysp
              , callDemandFn = x86CallRegs mem funNameMap funTypeMap
              , demandContext = x86DemandContext
              }
  runExcept (registerUse useCtx fInfo)

data RecoveredFunction a =
  RecoveredFunction
  { recoveredFunction :: !(Function a)
  , llvmLogEvents     :: ![LLVMLogEvent]
  }

-- | Recover the function at a given address.
--
-- Returns either an error message with a fatal error, or a list of
-- warnings and a function.
recoverFunction :: forall ids
                .  SyscallPersonality
                -> Memory 64
                -> DiscoveryFunInfo X86_64 ids
                -> Map (MemSegmentOff 64) (BlockInvariants X86_64 ids)
                   -- ^ Inferred block invariants
                -> BSC.ByteString
                   -- ^ Name of function
                -> [X86ArgInfo] -- ^ Type of arguments
                -> [Some X86RetInfo] -- ^ Type of return types
                -> Either (RecoverError 64) (RecoveredFunction X86_64)
recoverFunction sysp mem fInfo invMap nm curArgs curRets = do
  -- Get address of function entry point
  let entryAddr = discoveredFunAddr fInfo
  let funCtx = FRC { frcMemory = mem
                   , frcInterp = fInfo
                   , frcSyscallPersonality = sysp
                   , frcCurrentFunctionReturns = curRets
                   , frcBlockDepMap = invMap
                   }
  runFunRecover funCtx $ do
    let entryBlk = fromJust $ Map.lookup entryAddr (fInfo^.parsedBlocks)

    -- Insert uninitialized register into initial block location map.
    let insUninit :: Pair X86Reg (FnRegValue X86_64)
                  -> MapF (BoundLoc X86Reg) (FnRegValue X86_64)
                  -> MapF (BoundLoc X86Reg) (FnRegValue X86_64)
        insUninit (Pair r v) m = MapF.insertWith (\_n old -> old) (RegLoc r) v m
    -- Compute registers for first block
    let locMap :: MapF (BoundLoc X86Reg) (FnRegValue X86_64)
        locMap
          = MapF.empty
            -- Set df to 0 at function start.
          & MapF.insert (RegLoc DF) (mkIdentEmbeddingApp (FnConstantBool False))
            -- Populate used arguments.
          & initializeArgumentValues curArgs
            -- Populate unused registers with default values.
          & flip (foldr insUninit) uninitRegs

    entryInv <- getBlockInvariants entryAddr
    recoveredEntryBlk <- evalRecover entryBlk entryInv [] V.empty locMap

    -- Recover all blocks after first.
    let recoverBlk :: MemSegmentOff 64
                   -> ParsedBlock X86_64 ids
                   -> FunRecover ids (Maybe (FnBlock X86_64))
        recoverBlk a blk
          | a == entryAddr = pure Nothing
          | otherwise = Just <$> recoverInnerBlock blk
    blks <- Map.traverseMaybeWithKey recoverBlk (fInfo^.parsedBlocks)

    let fn = Function { fnAddr = entryAddr
                      , fnType =
                          FunctionType { fnArgTypes   = argRegTypeRepr <$> curArgs
                                       , fnReturnType = retReturnType curRets
                                       , fnVarArgs    = False
                                       }
                      , fnName = nm
                      , fnEntryBlock = recoveredEntryBlk
                      , fnRestBlocks = Map.elems blks
                      }
    events <- gets frsLogEvents
    pure $! seq fn $ RecoveredFunction fn events
