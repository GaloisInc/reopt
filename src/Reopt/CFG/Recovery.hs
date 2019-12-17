{-|
This module provides methods for constructing functions from the basic
blocks discovered by 'Data.Macaw.Discovery'.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Reopt.CFG.Recovery
  ( recoverFunction
    -- * X86 type info
  , X86FunTypeInfo(..)
  , X86ArgInfo(..)
  , argReg
  , X86RetInfo(..)
  , retReg
  ) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Parameterized.Classes
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
import           Data.Parameterized.TraversableF
import           Data.Parameterized.TraversableFC
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Vector as V
import           Data.Word
import qualified Flexdis86 as F
import           GHC.Stack
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import           Text.Printf

import           Data.Macaw.AbsDomain.StackAnalysis
import           Data.Macaw.AbsDomain.JumpBounds (InitJumpBounds(..))
import           Data.Macaw.Analysis.RegisterUse
import           Data.Macaw.CFG
import           Data.Macaw.Discovery.State
import           Data.Macaw.Types

import           Data.Macaw.X86.ArchTypes
import           Data.Macaw.X86.SyscallInfo
import           Data.Macaw.X86.X86Reg
import           Data.Macaw.X86 (x86DemandContext)

import           Reopt.CFG.FnRep
import           Reopt.CFG.FnRep.X86

-- | This identifies how a argument is passed into a function, or
-- a return value is passed out.
data X86ArgInfo where
  ArgBV64 :: !F.Reg64 -> X86ArgInfo
  -- ^ This identifies a 64-bit value passed as a register.
  --
  -- The register should be compatible with the ABI.
  ArgMM512D :: !Word8 -> X86ArgInfo
  -- ^ This identifies one of the zmm registers used as arguments (zmm0-7).

-- | The register types this return value is associated with.
argReg :: X86ArgInfo -> Some X86Reg
argReg (ArgBV64 r) = Some (X86_GP r)
argReg (ArgMM512D i) = Some (X86_ZMMReg i)

-- | This identifies how a return value is passed from a callee to
-- the callee.
data X86RetInfo where
  RetBV64 :: !F.Reg64 -> X86RetInfo
  -- ^ This identifies a 64-bit value returned as a register (RAX/RDX)
  --
  -- The register should be compatible with the ABI.
  RetMM512D :: !Word8 -> X86RetInfo
  -- ^ This identifies one of the two zmm registers used as argument (zmm0/1).

-- | The register types this return value is associated with.
retReg :: X86RetInfo -> Some X86Reg
retReg (RetBV64 r) = Some (X86_GP r)
retReg (RetMM512D i) = Some (X86_ZMMReg i)

-- | This describes the registers and return value of an x86_64 ABI
-- compliant function.
--
-- This representation does not support arguments that spilled on the
-- stack, but this would be a good feature to add.
--
-- It uses a list for arguments so that we can use C headers and
-- ensure the arguments appear in a particular order (e.g. from the
-- binary perspective a function that takes two integers followed by a
-- float is indistinguishable from a function that takes a float
-- followed by two integers.
data X86FunTypeInfo
   = X86FunTypeInfo { ftiArgRegs :: [X86ArgInfo]
                    , ftiRetRegs :: [X86RetInfo]
                    }

-- |  Maximum function type.
maximumFunTypeInfo :: X86FunTypeInfo
maximumFunTypeInfo =
  X86FunTypeInfo { ftiArgRegs = fmap ArgBV64 x86GPPArgumentRegs
                             ++ fmap ArgMM512D [0..7]
                 , ftiRetRegs = fmap RetBV64   [ F.RAX, F.RDX ]
                             ++ fmap RetMM512D [0,1]
                 }

toFunctionTypeRegs :: X86FunTypeInfo -> FunctionTypeRegs X86Reg
toFunctionTypeRegs ftp =
  FunctionTypeRegs { fnArgRegs    = argReg <$> ftiArgRegs ftp
                   , fnReturnRegs = retReg <$> ftiRetRegs ftp
                   }

------------------------------------------------------------------------
-- FnRegValue

data FnRegValue arch tp where
  CalleeSaved :: !(ArchReg arch tp)
              -> FnRegValue arch tp
  -- ^ This is a callee saved register
  FnValue :: !(FnValue arch i)
          -> !(WidthEqProof i o)
          -> FnRegValue arch o
     -- ^ A value assigned to a register

instance (ShowF (ArchReg arch), MemWidth (ArchAddrWidth arch)) => Pretty (FnRegValue arch tp) where
  pretty (CalleeSaved r)     = text "calleeSaved" <> parens (text $ showF r)
  pretty (FnValue v _)    = pretty v

$(pure [])

------------------------------------------------------------------------
-- BlockEqClass (used for phi variables)

-- | List of locations that should have the same value when a block
-- begins execution.
data BlockEqClass r tp =
  BlockEqClass { blockEqInitLocation :: !(BoundLoc r tp)
                 -- ^ The main location to use for argument passing purposes.
               , blockEqLaterLocations :: ![BoundLoc r tp]
                 -- ^ Set of demanded locations considered equivalent.
               , blockEqClassType :: !(TypeRepr tp)
               }

-- | Create a class from a initial value.
initClass :: HasRepr r TypeRepr => BoundLoc r tp -> BlockEqClass r tp
initClass loc = BlockEqClass { blockEqInitLocation = loc
                             , blockEqLaterLocations = []
                             , blockEqClassType = typeRepr loc
                             }

-- | Fold over all locations  in class.
foldBlockEqClasses :: (a -> BoundLoc r tp -> a)
                   -> a
                   -> BlockEqClass r tp
                   -> a
foldBlockEqClasses f a0 cl =
  let a1 = f a0 (blockEqInitLocation cl)
   in seq a1 (foldr' (flip f) a1 (blockEqLaterLocations cl))

-- | Add a location to the class.
addLocToClass :: BoundLoc r tp
              -> BlockEqClass r tp
              -> BlockEqClass r tp
addLocToClass l cl = cl { blockEqLaterLocations = l : blockEqLaterLocations cl }

instance ShowF r => Show (BlockEqClass r tp) where
  showsPrec _ eqCl =
    let locs = blockEqInitLocation eqCl : blockEqLaterLocations eqCl
     in showList (pretty <$> locs)

instance ShowF r => ShowF (BlockEqClass r)

-- | Vector of all equivalence classes of locations in a block that
-- are demanded by the block.
--
-- There is one phi node for each element  of this vector.
type BlockEqClassVec r = V.Vector (Some (BlockEqClass r))

$(pure [])

------------------------------------------------------------------------
-- Function recover monad

-- | Information for function recovery common to a function or module.
data FunRecoverContext ids =
  FRC { frcMemory :: !(Memory 64)
      , frcInterp :: !(DiscoveryFunInfo X86_64 ids)
      , frcSyscallPersonality  :: !SyscallPersonality
        -- ^ System call personality
      , frcFunctionArgs :: !(Map (MemSegmentOff 64) (BSC.ByteString, X86FunTypeInfo))
        -- ^ Maps addresses to their name and type information.
      , frcCurrentFunctionType :: !X86FunTypeInfo
        -- ^ The type of the function being recovered.
      , frcPhiVarMap :: !(Map (MemSegmentOff 64) (BlockEqClassVec X86Reg))
        -- ^ Maps block starting addresses to the phi variable locations.
      , frcBlockDepMap :: (Map (MemSegmentOff 64) (DependencySet X86Reg ids))
        -- ^ Maps the starting address of blocks to the dependency set
        -- for the block.
      }

-- | State for function recovery common to all blocks in a function.
data FunRecoverState = FRS { frsNextAssignId :: !FnAssignId
                           , frsWarnings :: ![String]
                             -- ^ List of warnings added so far.
                           }

-- | Monad for function recovery
newtype FunRecover ids a =
    FR { runFR :: ReaderT (FunRecoverContext ids) (StateT FunRecoverState (Except String)) a }
  deriving (Functor, Applicative, Monad, MonadError String)

runFunRecover :: FunRecoverContext ids
              -> FunRecover ids a
              -> Either String ([String], a)
runFunRecover ctx m =
  let s0 = FRS { frsNextAssignId = FnAssignId 0
               , frsWarnings = []
               }
   in case runExcept (runStateT (runReaderT (runFR m) ctx) s0) of
        Left e -> Left e
        Right (a,s) -> Right (frsWarnings s, a)

--  | Report a warning
funAddWarning :: String -> FunRecover ids ()
funAddWarning msg = do
  FR $ modify' $ \frs -> frs { frsWarnings = msg : frsWarnings frs }

-- | Create a fresh assign id.
funFreshId :: FunRecover ids FnAssignId
funFreshId = FR $ do
  FnAssignId nextId <- gets $ frsNextAssignId
  modify' $ \frs -> frs { frsNextAssignId = FnAssignId (nextId + 1) }
  return $! FnAssignId nextId

------------------------------------------------------------------------
-- RecoverState

-- | State used for recovering a block
data RecoverState arch ids =
  RS { rsBlock :: !(ParsedBlock arch ids)
       -- ^ Initial block
     , rsPredBlockAddrs :: ![MemSegmentOff (ArchAddrWidth arch)]
       -- ^ Predecessors for this block
     , rsPhiVars :: !(V.Vector (Some (FnPhiVar arch)))
       -- ^ List of phi variables in order blocks jumping to
       -- this block should define them.
     , rsDependencySet :: !(DependencySet (ArchReg arch)  ids)
       -- ^ Dependencies computed for current block.
     , rsCurRegs   :: !(MapF (ArchReg arch) (FnRegValue arch))
       --
       -- ^ This maps registers to the associated value at the start
       -- of the block after any stack allocations have been
       -- performed.
     , rsBlockOff :: !(ArchAddrWord arch)
       -- ^ The offset in the block of the current code.
     , _rsAssignStackOffsetMap
       :: !(AssignStackOffsetMap (ArchAddrWidth arch) ids )
       -- ^ Maps assignments that refer to stack to the corresponding
       -- stack offset.
     , _rsStackMap :: !(StackMap (ArchAddrWidth arch) (FnValue arch))
       -- ^ Maps stack offsets to the register value set there.
     , _rsCurStmts  :: !(Seq (FnStmt arch))
     , _rsAssignMap    :: !(MapF (AssignId ids) (FnValue arch))
     , _rsMemInsnAddrs :: ![(Word64, FnMemAccessType)]
       -- ^ Pairs for offsets and mem access types of instructions in reverse order.
     }


-- | This maps registers to the associated value at the start of the block after
-- any stack allocations have been performed.
rsAssignStackOffsetMap
  :: Simple Lens (RecoverState arch ids)
                 (AssignStackOffsetMap (ArchAddrWidth arch)  ids)
rsAssignStackOffsetMap =
  lens _rsAssignStackOffsetMap (\s v -> s { _rsAssignStackOffsetMap = v })

-- | Maps stack offsets to the register value set there.
rsStackMap :: Lens' (RecoverState arch ids)
                    (StackMap (ArchAddrWidth arch) (FnValue arch))
rsStackMap = lens _rsStackMap (\s v -> s { _rsStackMap = v })

-- | List of statements accumulated so far.
rsCurStmts :: Lens' (RecoverState arch ids) (Seq (FnStmt arch))
rsCurStmts = lens _rsCurStmts (\s v -> s { _rsCurStmts = v })

-- | Map from assignments that have been evaluated to the value bound
-- to them.
rsAssignMap :: Lens' (RecoverState arch ids)
                     (MapF (AssignId ids) (FnValue arch))
rsAssignMap = lens _rsAssignMap (\s v -> s { _rsAssignMap = v })

-- | Pairs for offsets and mem access types of instructions in reverse
-- order.
rsMemInsnAddrs :: Lens' (RecoverState arch ids) [(Word64, FnMemAccessType)]
rsMemInsnAddrs = lens _rsMemInsnAddrs (\s v -> s { _rsMemInsnAddrs = v })

------------------------------------------------------------------------
-- Recover

-- | Monad for recovering code in a block.
newtype Recover ids a = Recover {
    runRecover :: StateT (RecoverState X86_64 ids) (FunRecover ids) a
  }
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadState (RecoverState X86_64 ids)
           )

evalRecover :: forall ids a
            .  ParsedBlock X86_64 ids
            -> [MemSegmentOff 64]
               -- ^ Predecessors of current block
            -> V.Vector (Some (FnPhiVar X86_64))
               -- ^ Phi variables in expected order.
            -> MapF X86Reg (FnRegValue X86_64)
               -- ^ Initial register values
            -> StackMap 64 (FnValue X86_64)
            -> Recover ids a
            -> FunRecover ids a
evalRecover b preds phiVars regs initStackMap m = do
  depMap <- FR $ asks frcBlockDepMap

  -- Get dependencies for this block.
  let ds = case Map.lookup (pblockAddr b) depMap of
             Just ds' -> ds'
             Nothing -> error $ "Could not find block dependencies."

  let s0 = RS { rsBlock = b
              , rsPredBlockAddrs = preds
              , rsPhiVars = phiVars
              , rsDependencySet = ds
              , rsCurRegs = regs
              , rsBlockOff = 0
              , _rsAssignStackOffsetMap = Map.empty
              , _rsStackMap = initStackMap
              , _rsCurStmts = Seq.empty
              , _rsAssignMap = MapF.empty
              , _rsMemInsnAddrs = []
              }
  evalStateT (runRecover m) s0

liftFunRecover :: FunRecover ids a -> Recover ids a
liftFunRecover m = Recover $ lift m

getFunCtx :: Recover ids (FunRecoverContext ids)
getFunCtx = liftFunRecover $ FR ask

--  | Report a warning
addWarning :: String -> Recover ids ()
addWarning msg = liftFunRecover $ funAddWarning msg

-- | Create a fresh assign id.
freshId :: Recover ids FnAssignId
freshId = liftFunRecover funFreshId

-- | Create a fresh return variable given the type id.
mkReturnVar :: TypeRepr tp -> Recover ids (FnReturnVar tp)
mkReturnVar tp = (`FnReturnVar` tp) <$> freshId

-- | Add a statement to the list of statements in current function block.
addFnStmt :: FnStmt X86_64 -> Recover ids ()
addFnStmt stmt = rsCurStmts %= (Seq.|> stmt)

-- | Return address of instruction we are translating.
getCurRecoveryAddr :: Recover ids (MemAddr 64)
getCurRecoveryAddr = do
  b <- gets rsBlock
  o <- gets rsBlockOff
  pure $ incAddr (toInteger o) (segoffAddr (pblockAddr b))

------------------------------------------------------------------------

mkBlock :: FnTermStmt X86_64
           -- ^ Terminator for this block
        -> Recover ids (FnBlock X86_64)
mkBlock tm = do
  b  <- gets rsBlock
  pr <- case pblockPrecond b of
          Left _e -> throwError "Could not resolve block precondition."
          Right pr -> pure pr
  preds    <- gets rsPredBlockAddrs
  phiVars <- gets rsPhiVars
  curStmts <- use rsCurStmts
  memInfo <- use rsMemInsnAddrs
  return $! FnBlock { fbLabel = fnBlockLabelFromAddr (pblockAddr b)
                    , fbPrecond = pr
                    , fbSize  = fromIntegral (blockSize b)
                    , fbPrevBlocks = fnBlockLabelFromAddr <$> preds
                    , fbPhiVars = phiVars
                    , fbStmts  = toList curStmts
                    , fbTerm   = tm
                    , fbMemInsnAddrs = V.fromList (reverse memInfo)
                    }

$(pure [])

------------------------------------------------------------------------
-- X86FunctionType

-- | Type used in passing this argument to a function.
argRegTypeRepr :: X86ArgInfo -> Some TypeRepr
argRegTypeRepr ArgBV64{} = Some (BVTypeRepr n64)
argRegTypeRepr ArgMM512D{} = Some (VecTypeRepr n8 (FloatTypeRepr DoubleFloatRepr))

$(pure [])

-- | The register types this return value is associated with.
retRegTypeRepr :: X86RetInfo -> Some TypeRepr
retRegTypeRepr RetBV64{} = Some (BVTypeRepr n64)
retRegTypeRepr RetMM512D{} = Some (VecTypeRepr n8 (FloatTypeRepr DoubleFloatRepr))

$(pure [])

-- | Construct a generatic function type from the x86-specific type rep.
resolveX86FunctionType :: X86FunTypeInfo -> FunctionType X86_64
resolveX86FunctionType ft =
  FunctionType { fnArgTypes = argRegTypeRepr <$> ftiArgRegs ft
               , fnReturnTypes = retRegTypeRepr <$> ftiRetRegs ft
               }

$(pure [])

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

$(pure [])

bitcast :: FnValue X86_64 i -> WidthEqProof i o -> Recover ids (FnValue X86_64 o)
bitcast x p = evalAssignRhs $ FnEvalApp (Bitcast x p)

$(pure [])

------------------------------------------------------------------------
-- recoverValue

unsupportedFnValue :: String -> TypeRepr tp -> Recover ids (FnValue X86_64 tp)
unsupportedFnValue nm _repr = do
  throwError $ "Cannot create value: " ++ nm


-- | Recover a constant.
recoverCValue :: HasCallStack
              => CValue X86_64 tp
              -> Recover ids (FnValue X86_64 tp)
recoverCValue cv =
  case cv of
    BVCValue w i ->
      pure $ FnConstantValue w i
    BoolCValue b ->
      pure $ FnConstantBool b
    RelocatableCValue w addr -> do
      funCtx <- getFunCtx
      let mem = frcMemory funCtx
      let interpState = frcInterp funCtx
      case asSegmentOff mem addr of
        Nothing ->
          case asAbsoluteAddr addr of
            Just absAddr -> pure $ FnConstantValue (addrWidthNatRepr w) (toInteger absAddr)
            Nothing -> unsupportedFnValue ("Relative addr " ++ show addr) (addrWidthTypeRepr w)

        Just addrRef -> do
          case () of
            _ | Just (nm, ft) <- Map.lookup addrRef (frcFunctionArgs funCtx) -> do
                  pure $! FnFunctionEntryValue (resolveX86FunctionType ft) nm

            _ | Map.member addrRef (interpState^.parsedBlocks) -> do
                  let msg = "Do not support functions that reference block addresses."
                  unsupportedFnValue msg (addrWidthTypeRepr w)

                -- Turn address into hardcoded constant.
              | Just fixedAddr <- segoffAsAbsoluteAddr addrRef -> do
                  pure $ FnConstantValue n64 (fromIntegral fixedAddr)

              | otherwise -> do
                  unsupportedFnValue ("segment pointer " ++ show addr) (typeRepr cv)

    SymbolCValue w sym ->
      unsupportedFnValue ("Symbol references " ++ show sym) (addrWidthTypeRepr w)

$(pure [])

-- | Recover a stack value
recoverValue :: HasCallStack
             => Value X86_64 ids tp
             -> Recover ids (FnValue X86_64 tp)
recoverValue v = do
  case v of
    CValue cv -> do
      recoverCValue cv
    AssignedValue asgn -> do
      assignMap <- use rsAssignMap
      case MapF.lookup (assignId asgn) assignMap of
        Just rval -> pure rval
        Nothing -> do
          usedAssigns <- gets $ dsAssignSet . rsDependencySet
          -- Only add assignment if it is used.
          if Some (assignId asgn) `Set.member` usedAssigns then
            error $ "Encountered uninitialized assignment: " ++ show (assignId asgn) ++ "\n"
              ++ show (MapF.keys assignMap)
           else
            error $ "Asked to recover unused assignment: " ++ show (assignId asgn) ++ "\n"
    Initial reg -> do
      s <- get
      case MapF.lookup reg (rsCurRegs s) of
        Nothing ->
          unsupportedFnValue ("Initial register " ++ show reg) (typeRepr reg)
        Just (CalleeSaved _) ->
          unsupportedFnValue ("Initial (callee) register " ++ show reg) (typeRepr reg)
        Just (FnValue v' pr) -> do
          case testEquality (widthEqSource pr) (widthEqTarget pr) of
            Just Refl -> do
              pure v'
            Nothing -> do
              bitcast v' pr

$(pure [])

------------------------------------------------------------------------
-- recoverRegister

recoverRegister :: HasCallStack
                => RegState X86Reg (Value X86_64 ids)
                -> X86Reg tp
                -> Recover ids (FnValue X86_64 tp)
recoverRegister regs r = do
  recoverValue (regs^. boundValue r)

$(pure [])

------------------------------------------------------------------------
-- recoverCallTarget

recoverCallTarget :: HasCallStack
                  => RegState X86Reg (Value X86_64 ids)
                  -> Recover ids (FnValue X86_64 (BVType 64), X86FunTypeInfo)
recoverCallTarget regState = do
  funCtx <- getFunCtx
  let mem = frcMemory funCtx
  let v = regState ^. boundValue ip_reg
  case v of
    RelocatableValue _ addr
      | Just addrRef <- asSegmentOff mem addr
      , Just (nm, ft) <- Map.lookup addrRef (frcFunctionArgs funCtx) -> do
          pure (FnFunctionEntryValue (resolveX86FunctionType ft) nm, ft)
    _ -> do
      callTgt <- recoverValue v
      pure (callTgt, maximumFunTypeInfo)

------------------------------------------------------------------------
-- recoverStmt

$(pure [])

checkAssignmentUnused :: HasCallStack => AssignId ids tp -> Recover ids ()
checkAssignmentUnused aid = do
  mSeen <- uses rsAssignMap (MapF.lookup aid)
  case mSeen of
    Nothing -> return ()
    Just _ -> error $ "internal: Assign " ++ show aid ++ " already seen"

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

whenUsed :: AssignId ids tp -> Recover ids () -> Recover ids ()
whenUsed aid m = do
  usedAssigns <- gets $ dsAssignSet . rsDependencySet
  -- Only add assignment if it is used.
  when (Some aid `Set.member` usedAssigns) m

recordMemAccess :: FnMemAccessType -> Recover ids ()
recordMemAccess tp = do
  -- Get offset of block for recording read.
  o <- fromIntegral <$> gets rsBlockOff
  rsMemInsnAddrs %= ((o, tp):)

-- | Add statements for the assignment
recoverAssign :: HasCallStack => Assignment X86_64 ids tp -> Recover ids ()
recoverAssign asgn = do
  let aid = assignId asgn

  case assignRhs asgn of
    EvalApp app -> do
      -- Check if this is a stack value
      initStackCns <- gets $ initBndsMap . blockJumpBounds . rsBlock
      assignStackMap <- use rsAssignStackOffsetMap
      let stackFn v = toInteger <$> valueStackOffset initStackCns assignStackMap v
      case appAsStackOffset stackFn app of
        Just (StackOffsetView o) -> do
          rsAssignStackOffsetMap %= Map.insert (Some aid) (fromInteger o)
        Nothing -> do
          whenUsed aid $ do
            setAssignRhs aid =<< (FnEvalApp <$> traverseFC recoverValue app)
    SetUndefined tp -> do
      whenUsed aid $
        setAssignRhs aid (FnSetUndefined tp)
    ReadMem addr memRepr -> do
      initStackCns <- gets $ initBndsMap . blockJumpBounds . rsBlock
      assignStackMap <- use rsAssignStackOffsetMap
      case valueStackOffset initStackCns assignStackMap addr of
        Just o -> do
          recordMemAccess StackAccess
          whenUsed aid $ do
            -- Get values on stack
            curStack <- use rsStackMap
            -- Lookup offset repr pair in current stack.
            case stackMapLookup o memRepr curStack of
              SMLResult v -> do
                setAssignVal aid v
              SMLOverlap _overOff _overRepr _overVal -> do
                error "Stack read at an overlapping offset."
              SMLNone -> do
                curInsnAddr <- getCurRecoveryAddr
                error $ show curInsnAddr
                  ++ ": Stack read at an uninitialized location."
        Nothing -> do
          -- Note. Are we allowed to deleted unused heap accesses?
          whenUsed aid $ do
            recordMemAccess HeapAccess
            addrVal <- recoverValue addr
            setAssignRhs aid (FnReadMem addrVal (typeRepr memRepr))
    CondReadMem tp cond addr def -> do
      whenUsed aid $ do
        rhs <- FnCondReadMem tp
               <$> recoverValue cond
               <*> recoverValue addr
               <*> recoverValue def
        setAssignRhs aid rhs
    EvalArchFn f _ -> do
      whenUsed aid $ do
        fval <- traverseFC recoverValue f
        setAssignRhs aid (FnEvalArchFn fval)

-- | Run the given computation if a write statement is used.
whenStackWriteIsUsed :: StmtIndex
                     -> Recover ids ()
                     -> Recover ids ()
whenStackWriteIsUsed idx m = do
  idxSet <- gets $ dsWriteStmtIndexSet . rsDependencySet
  when (Set.member idx idxSet) m

-- | This should add code as needed to support the statement.
recoverStmt :: StmtIndex -- ^ Index of statement
            -> Stmt X86_64 ids
            -> Recover ids ()
recoverStmt stmtIdx stmt = do
  --regs <- use rsCurRegs
  case stmt of
    AssignStmt asgn -> do
      recoverAssign asgn
    WriteMem addr memRepr val -> do
      initStackCns <- gets $ initBndsMap . blockJumpBounds . rsBlock
      assignStackMap <- use rsAssignStackOffsetMap
      case valueStackOffset initStackCns assignStackMap addr of
        Just o -> do
          -- Check if written address is actually read
          whenStackWriteIsUsed stmtIdx $ do
            rval <- recoverValue val
            -- Overwrite value in stack.
            rsStackMap %= stackMapOverwrite o memRepr rval
          recordMemAccess StackAccess
        Nothing -> do
          rAddr <- recoverValue addr
          rVal  <- recoverValue val
          addFnStmt $ FnWriteMem rAddr rVal
          recordMemAccess HeapAccess
    CondWriteMem cond addr memRepr val -> do
      rCond <- recoverValue cond
      rAddr <- recoverValue addr
      rVal  <- recoverValue val
      addFnStmt $ FnCondWriteMem rCond rAddr rVal memRepr
    Comment msg -> do
      addFnStmt $ FnComment msg
    ExecArchStmt astmt0 -> do
      astmt <- traverseF recoverValue astmt0
      addFnStmt (FnArchStmt (X86FnStmt astmt))
    InstructionStart o _ -> do
      modify $ \s -> s { rsBlockOff = o }
    ArchState _ _ -> do
      pure ()

------------------------------------------------------------------------
-- Jump target

recoverJumpTarget :: forall ids
                  .  HasCallStack
                  => (forall tp . X86Reg tp -> Recover ids (FnValue X86_64 tp))
                     -- ^ Function for getting  register value.
                  -> MemSegmentOff 64 -- ^ Address to jump to
                  -> Recover ids (FnJumpTarget X86_64)
recoverJumpTarget regFn tgtAddr = do
  varMap <- frcPhiVarMap <$> getFunCtx

  -- Get phi variables
  let varLocs :: BlockEqClassVec X86Reg
      varLocs =
        let emsg = "internal: Could not find phi variables for "
                   ++ show tgtAddr ++ "."
         in Map.findWithDefault (error emsg) tgtAddr varMap

  let recoverVec :: Some (BlockEqClass X86Reg)
                 -> Recover ids (Some (FnValue X86_64))
      recoverVec (Some cl) = do
        case blockEqInitLocation cl of
          RegLoc r -> Some <$> regFn r
          StackOffLoc o repr -> do
            sm <- use rsStackMap
            case stackMapLookup o repr sm of
              SMLResult v -> pure (Some v)
              _ -> do
                curInsnAddr <- getCurRecoveryAddr
                error $ printf "%s: Could not resolve value at stack offset %s."
                               (show curInsnAddr) (show o)
  values <- traverse recoverVec varLocs
  pure $! FnJumpTarget { fnJumpLabel = fnBlockLabelFromAddr tgtAddr
                       , fnJumpPhiValues = values
                       }

$(pure [])

------------------------------------------------------------------------
-- RecoveredBlockInfo

-- | Builds a mapping from labels to recovered function blocks and a
-- mapping from the block label to the register map resulting from
-- block.
type RecoveredBlockInfo = Map (MemSegmentOff 64) (FnBlock X86_64)

emptyRecoveredBlockInfo :: RecoveredBlockInfo
emptyRecoveredBlockInfo = Map.empty

addFnBlock :: FnBlock X86_64
           -> RecoveredBlockInfo
           -> RecoveredBlockInfo
addFnBlock b info =
  let a = fnBlockLabelAddr (fbLabel b)
   in Map.insert a b info

$(pure [])

------------------------------------------------------------------------
-- recoverBlock

-- | Return the value associated with a register after a function call.
getPostCallValue :: RegState X86Reg (Value X86_64 ids)
                     -- ^ Value of registers before syscall
                 -> MapF X86Reg (FnValue X86_64)
                    -- ^ Return values
                 -> X86Reg tp
                 -> Recover ids (FnValue X86_64 tp)
getPostCallValue regs retRegMap r = do
  case r of
    _ | Just v <- MapF.lookup r retRegMap -> do
          pure v
    _ | Just Refl <- testEquality r sp_reg -> do
          spv <- recoverRegister regs sp_reg
          evalAssignRhs $ FnEvalApp $ BVAdd knownNat spv (FnConstantValue n64 8)

   -- df is 0 after a function call.
    _ | Just Refl <- testEquality r DF -> return $ FnConstantBool False
    -- Callee-saved registers are preserved.
    _ | Some r `Set.member` x86CalleeSavedRegs ->
        recoverRegister regs r
    _ -> do
      addr <- gets $ pblockAddr . rsBlock
      addWarning ("WARNING: Nothing known about register "
                   ++ show r ++ " at " ++ show addr ++ "\n"
                 )
      let nm = "post-call register " ++ show r
      unsupportedFnValue nm (typeRepr r)

$(pure [])

-- | Get value for register after a system call.
--
-- This is subtly different to that for function calls.
getPostSyscallValue :: RegState X86Reg (Value X86_64 ids)
                       -- ^ Value of registers before syscall
                    -> X86Reg tp
                    -> Recover ids (FnValue X86_64 tp)
getPostSyscallValue regs r =
  case r of
    _ | Just Refl <- testEquality r sp_reg -> do
          recoverRegister regs sp_reg
      | Some r `Set.member` x86CalleeSavedRegs ->
        recoverRegister regs r

    _ | Just Refl <- testEquality r DF -> return $ FnConstantBool False

    _ -> do
      addr <- gets $ pblockAddr . rsBlock
      addWarning ("WARNING: Nothing known about register " ++ show r ++ " at " ++ show addr)
      unsupportedFnValue ("post-syscall register " ++ show r) (typeRepr r)

recoverX86TermStmt :: forall ids
                   .  X86TermStmt ids
                   -> RegState (ArchReg X86_64) (Value X86_64 ids)
                      -- ^ Register value at start of block
                   -> Maybe (MemSegmentOff 64)
                   -> Recover ids (FnBlock X86_64)
recoverX86TermStmt tstmt regs mnext_addr =
  case tstmt of
    Hlt ->
      throwError "hlt is not supported in function recovery."
    UD2 -> do
      throwError "ud2 is not supported in function recovery."
    X86Syscall -> do
      sysp <- frcSyscallPersonality <$> getFunCtx

      let syscallRegs :: [ArchReg X86_64 (BVType 64)]
          syscallRegs = syscallArgumentRegs

      let args
            | BVValue _ this_call_no <- regs^.boundValue syscall_num_reg
            , Just (_,_,argtypes) <- Map.lookup (fromInteger this_call_no) (spTypeInfo sysp) =
              take (length argtypes) syscallRegs
            | otherwise =
              syscallRegs

      let rregs = spResultRegisters sysp


      let mkRet :: MapF X86Reg (FnValue X86_64)
                -> Some X86Reg
                -> Recover ids (MapF X86Reg (FnValue X86_64))
          mkRet m (Some r) = do
            rv <- mkReturnVar (typeRepr r)
            return $ MapF.insert r (FnReturn rv) m

      initMap <- foldM mkRet MapF.empty rregs

      -- pull the return variables out of initMap (in order of rregs)
      let getVar :: Maybe (FnValue X86_64 tp) -> FnReturnVar tp
          getVar (Just (FnReturn rv)) = rv
          getVar _ = error "impossible"

      let rets :: [Some FnReturnVar]
          rets = map f rregs
            where f (Some r) = Some $ getVar $ MapF.lookup r initMap


      call_num <- recoverRegister regs syscall_num_reg
      args'  <- mapM (recoverRegister regs) args
      addFnStmt (FnArchStmt (X86FnSystemCall call_num args' rets))
      case mnext_addr of
        Nothing -> do
          -- TODO: Fix this by adding a
          error "Recovery: Could not find system call return label"
        Just nextAddr -> do
          tgt <- recoverJumpTarget (getPostSyscallValue regs) nextAddr
          mkBlock (FnJump tgt)

-- | Given a register state this interprets the function arguments.
evalFunctionArgs :: RegState X86Reg (Value X86_64 ids)
                 -> X86FunTypeInfo
                 -> Recover ids [Some (FnValue X86_64)]
evalFunctionArgs regs ft = do
  forM (ftiArgRegs ft) $ \argInfo -> do
    case argInfo of
      ArgBV64 r -> do
        Some <$> recoverRegister regs (X86_GP r)
      ArgMM512D i -> do
        v512 <- recoverRegister regs (X86_ZMMReg i)
        vv <- bitcast v512 (UnpackBits n8 n64)
        Some <$> bitcast vv (VecEqCongruence n8 (ToFloat DoubleFloatRepr))

-- | This removes the instruction that pushes the return address to the stack.
stripPushReturn :: MemSegmentOff 64 -- ^ Expected return address
                -> BVValue X86_64 ids 64 -- ^ Value in stack pointer next.
                -> [Stmt X86_64 ids] -- ^ Statements in reverse order
                -> Either String [Stmt X86_64 ids]
stripPushReturn _addr _next_sp [] =
  Left "No instructions with push of return address."
stripPushReturn _addr _next_sp (AssignStmt{}:_) =
  Left "Assignment before push of return address."
stripPushReturn retAddr next_sp (WriteMem addr valRepr val:l)
  -- Check for a call statement by determining if the last statement
  -- writes an executable address to the stack pointer.
  | Just _ <- testEquality addr next_sp
    -- Check this is the right length.
  , Just Refl <- testEquality (BVMemRepr n8 LittleEndian) valRepr
    -- Check if value is a valid literal address
  , Just val_a <- valueAsMemAddr val
    -- Check if segment of address is marked as executable.
  , segoffAddr retAddr == val_a =
    Right l
  | otherwise =
    Left "Unexpected right before push of return address."
stripPushReturn _addr _next_sp (CondWriteMem{}:_) =
  Left "Conditional memory write before push of return address."
stripPushReturn _addr _ (InstructionStart{}:_) =
  Left "Instruction start before return address."
stripPushReturn addr next_sp (s@Comment{}:l)= do
  (s:) <$> stripPushReturn addr next_sp l
stripPushReturn _addr _ (ExecArchStmt{}:_) =
  Left "Arch statement before push of return address."
stripPushReturn addr next_sp (s@ArchState{}:l) =
  (s:) <$> stripPushReturn addr next_sp l

-- | This contains the list of return variables for a call and a map from registers
-- to value returned.
--
-- This does not support stack return values.
type ReturnInfo arch = ([Some FnReturnVar], MapF (ArchReg arch) (FnValue arch))

-- | Analyze list of return values to create information needed to
--  resolve return.
evalReturnVars :: forall ids
               .  [X86RetInfo]
               -> Recover ids (ReturnInfo X86_64)
evalReturnVars retInfo = do
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
  foldrM mkRetReg ([], MapF.empty) retInfo

$(pure [])

recoverStmts :: StmtIndex -> [Stmt X86_64 ids] -> Recover ids ()
recoverStmts _ [] = pure ()
recoverStmts stmtIdx (n:r) = do
  recoverStmt stmtIdx n
  recoverStmts (stmtIdx+1) r

-- | Generate FnBlock fro mparsed block
recoverBlock :: forall ids
             .  ParsedBlock X86_64 ids
                -- ^ Entire block.
             -> Recover ids (FnBlock X86_64)
recoverBlock b = do
  -- Block recovery may need to strip statements, so we case split on terminal statement.
  case pblockTermStmt b of
    -- Case split on whether this is a tail call or not so we can decide which
    -- type of statement to emit.
    ParsedCall regs (Just retAddr) -> do
      -- Recover statements
      let next_sp = regs^.boundValue sp_reg
      case stripPushReturn retAddr next_sp (reverse (pblockStmts b)) of
        Left e -> throwError e
        Right l -> recoverStmts 0 (reverse l)
      -- Get call target
      (call_tgt, fti) <- recoverCallTarget regs
      -- Evaluate call arguments
      let ftp = resolveX86FunctionType fti
      args <- evalFunctionArgs regs fti
      -- Get list of return variables for call, and map from register to value for
      -- needed return values.
      (ri, retRegMap) <- evalReturnVars (ftiRetRegs fti)
      -- Add call statement
      addFnStmt (FnCall call_tgt ftp args ri)
      -- Create block that ends with jump to return address.
      retTgt <- recoverJumpTarget (getPostCallValue regs retRegMap) retAddr
      mkBlock (FnJump retTgt)
    -- Handle tail call.
    ParsedCall regs Nothing -> do
      thisFunType <- frcCurrentFunctionType <$> getFunCtx
      -- Recover statements
      recoverStmts 0 (pblockStmts b)
      -- Get call target
      (callTarget, callFunTypeInfo) <- recoverCallTarget regs
      -- Evaluate call arguments
      let callFunType = resolveX86FunctionType callFunTypeInfo
      args <- evalFunctionArgs regs callFunTypeInfo
      (ri,retRegMap) <- evalReturnVars (ftiRetRegs callFunTypeInfo)
      -- Add function call
      addFnStmt (FnCall callTarget callFunType args ri)
      let resolveReg :: X86RetInfo
                     -> Some (FnValue X86_64)
          resolveReg retInfo =
            case retReg retInfo of
              Some r ->
                case MapF.lookup r retRegMap of
                  Just v -> Some v
                  Nothing -> Some (FnUndefined (typeRepr r))
      -- Create block
      let ri2 = resolveReg <$> ftiRetRegs thisFunType
      mkBlock (FnRet ri2)
    PLTStub _ _ _ -> do
      error $ "Cannot recover functions with PLT stubs."
    ParsedTranslateError{} -> do
      error "Cannot recover function in blocks where translation error occurs."
    ClassifyFailure{} ->
      error $ "Classification failed in Recovery"

    ParsedJump regs tgtAddr -> do
      -- Recover statements
      recoverStmts 0 (pblockStmts b)
      -- Get target
      tgt  <- recoverJumpTarget (recoverRegister regs) tgtAddr
      mkBlock (FnJump tgt)

    ParsedBranch regs cond trueAddr falseAddr -> do
      -- Recover statements
      recoverStmts 0 (pblockStmts b)
      -- Translate condition
      condVal <- recoverValue cond
      -- Get registers that may be needed by one of the two branch targets.
      trueTgt  <- recoverJumpTarget (recoverRegister regs) trueAddr
      falseTgt <- recoverJumpTarget (recoverRegister regs) falseAddr
      let br = FnBranch condVal trueTgt falseTgt
      mkBlock br

    ParsedReturn regs -> do
      -- Recover statements
      recoverStmts 0 (pblockStmts b)
      -- Recover term stmt
      fti <- frcCurrentFunctionType <$> getFunCtx
      let evalRet :: X86RetInfo -> Recover ids (Some (FnValue X86_64))
          evalRet (RetBV64 r) = do
            Some <$> recoverRegister regs (X86_GP r)
          evalRet (RetMM512D i) = do
            x <- recoverRegister regs (X86_ZMMReg i)
            v <- bitcast x (UnpackBits n8 n64)
            Some <$> bitcast v (VecEqCongruence n8 (ToFloat DoubleFloatRepr))
      ri <- traverse evalRet (ftiRetRegs fti)
      mkBlock (FnRet ri)

    ParsedArchTermStmt ts regs next_addr -> do
      -- Recover statements
      recoverStmts 0 (pblockStmts b)
      -- Recover term statement
      recoverX86TermStmt ts regs next_addr

    ParsedLookupTable regs idx vec -> do
      -- Recover statements
      recoverStmts 0 (pblockStmts b)
      -- Recover term statement
      idx'   <- recoverValue idx
      tgtVec <- traverse (recoverJumpTarget (recoverRegister regs)) vec
      mkBlock (FnLookupTable idx' tgtVec)

------------------------------------------------------------------------
-- recoverFunction

$(pure [])


-- | Introduce a new phi var for a class, and updae location map.
addPhiVarForClass :: ([Some (FnPhiVar X86_64)], LocMap X86Reg (FnPhiVar X86_64))
                     -- ^ Previously constructed pfi variables and location map.
                  -> Some (BlockEqClass X86Reg)
                     -- ^ Class to add
                  -> FunRecover ids ( [Some (FnPhiVar X86_64)]
                                    , LocMap X86Reg (FnPhiVar X86_64)
                                    )
addPhiVarForClass (vl,m0) (Some eqCl) = do
  vnm <- funFreshId
  let phiVar = FnPhiVar { unFnPhiVar = vnm
                        , fnPhiVarType = blockEqClassType eqCl
                        , fnPhiVarRep = blockEqInitLocation eqCl
                        , fnPhiVarLocations = blockEqLaterLocations eqCl
                        }
  let ins m l = nonOverlapLocInsert l phiVar m
  let m' = foldBlockEqClasses ins m0 eqCl
  pure (Some phiVar:vl, m')

recoverInnerBlock :: DiscoveryFunInfo X86_64 ids
                  -> FunPredMap 64
                  -> Map (MemSegmentOff 64) (BlockEqClassVec X86Reg)
                     -- ^ Maps block starting address to demand information
                  -> RecoveredBlockInfo
                  -> MemSegmentOff 64
                  -> FunRecover ids RecoveredBlockInfo
recoverInnerBlock fInfo blockPreds blockUsageInfo blockInfo addr = do

  let eqClasses =
        let emsg = "internal: Could not find usage info for "
                   ++ show addr ++ "."
         in Map.findWithDefault (error emsg) addr blockUsageInfo

  (phiVarList, locPhiVarMap) <- foldlM addPhiVarForClass ([], locMapEmpty) eqClasses

  let phiVars :: V.Vector (Some (FnPhiVar X86_64))
      phiVars = V.fromList (reverse phiVarList)

  let regs :: MapF X86Reg (FnRegValue X86_64)
      regs = MapF.fromList
         [ MapF.Pair r (FnValue (FnPhiValue phiVar) pr)
         | MapF.Pair r phiVar <- MapF.toList $ locMapRegs locPhiVarMap
         , let pr = WidthEqRefl (fnPhiVarType phiVar)
         ]
  let initStackMap :: StackMap 64 (FnValue X86_64)
      initStackMap = fmapF FnPhiValue (locMapStack locPhiVarMap)

   -- Get predecessors for this block.
  let Just preds = Map.lookup addr blockPreds
   -- Generate phi nodes from predecessors and registers that this block refers to.
  let Just b = Map.lookup addr (fInfo^.parsedBlocks)
  fb <- evalRecover b preds phiVars regs initStackMap $
          recoverBlock b
  return $! blockInfo & addFnBlock fb

$(pure [])

-- | Maps Initial state used to build class vector.
type EqClassState r = MapF (BoundLoc r) (BlockEqClass r)

eqClassVec :: EqClassState r -> BlockEqClassVec r
eqClassVec m = V.fromList (toListF Some m)

emptyClassState :: EqClassState r
emptyClassState = MapF.empty

-- | Adds location to equivalence state.
addDemandedLocation :: ( MemWidth (ArchAddrWidth arch)
                       , OrdF (ArchReg arch)
                       , HasRepr (ArchReg arch) TypeRepr
                       )
                    => BlockStartStackConstraints arch
                    -> EqClassState (ArchReg arch)
                    -> Some (BoundLoc (ArchReg arch))
                    -> EqClassState (ArchReg arch)
addDemandedLocation cns m (Some loc) =
  case blockStartLocRepAndCns cns loc of
    (_,Just _) -> m
    (rep,Nothing) ->
      let f _ old = addLocToClass loc old
       in MapF.insertWith f rep (initClass loc) m

-- | This cross references the locations demanded by a block with the
-- inferred equivalences between locations to generate a vector
-- describing each Phi nodes a block will use for inputs.
eqClassVecFromDeps :: ( MemWidth (ArchAddrWidth arch)
                      , OrdF (ArchReg arch)
                      , HasRepr (ArchReg arch) TypeRepr
                      )
                   => Map (MemSegmentOff (ArchAddrWidth arch))
                          (DependencySet (ArchReg arch) ids)
                      -- ^ Map from block addresses to dependencies.
                   -> ParsedBlock arch ids
                      -- ^ block address
                   -> BlockEqClassVec (ArchReg arch)
eqClassVecFromDeps depMap b =
  -- Lookup dependencies for block we are jumping to.
  case Map.lookup (pblockAddr b) depMap of
    Nothing ->
      error $ "No dependencies for " ++ show (pblockAddr b) ++ "."
    Just ds ->
      let bnds = blockJumpBounds b
       in eqClassVec $ foldl (addDemandedLocation (initBndsMap bnds)) emptyClassState (dsLocSet ds)

summarizeX86ArchTermStmt :: SyscallPersonality
                         -> X86TermStmt ids
                         -> RegState (ArchReg X86_64) (Value X86_64 ids)
                         -> RegisterUseM X86_64 ids ()
summarizeX86ArchTermStmt sysp X86Syscall regs = do
  addRegisterUses regs (Some sp_reg : (Set.toList x86CalleeSavedRegs))
  traverse_ clearDependencySet (spResultRegisters sysp)
summarizeX86ArchTermStmt _ Hlt _ = pure ()
summarizeX86ArchTermStmt _ UD2 _ = pure ()

-- | Recover the function at a given address.
--
-- Returns either an error message with a fatal error, or a list of
-- warnings and a function.
recoverFunction :: forall ids
                .  SyscallPersonality
                -> Map (MemSegmentOff 64) (BSC.ByteString, X86FunTypeInfo)
                   -- ^ Map from address to the name at that address along with type.
                -> Memory 64
                -> DiscoveryFunInfo X86_64 ids
                -> Either String ([String], Function X86_64)
recoverFunction sysp funTypeMap mem fInfo = do
  -- Get address of function entry point
  let entryAddr = discoveredFunAddr fInfo

  -- Get name and type information inferred about fthis function.
  (nm,cfti) <-
    case Map.lookup entryAddr funTypeMap of
      Just p -> pure p
      Nothing -> Left $ "Missing type for " ++ show entryAddr ++ "."

  -- Get map from block start addresses to start addresses of blocks
  -- that jump to them.
  let blockPreds = funBlockPreds fInfo

  -- Compute map from block starting addresses to the dependicies required to run block.
  depMap <- runExcept $ do
    let ctx = RegisterUseContext
              { functionArgFn = \a -> do
                  segOff <- asSegmentOff mem a
                  toFunctionTypeRegs . snd <$> Map.lookup segOff funTypeMap
              , defaultCallRegs = toFunctionTypeRegs maximumFunTypeInfo
              , returnRegisters = retReg <$> ftiRetRegs cfti
              , calleeSavedRegisters = Set.toList x86CalleeSavedRegs
              , callScratchRegisters = [Some DF]
              , summarizeTermFn = summarizeX86ArchTermStmt sysp
              , demandContext = x86DemandContext
              }
    registerUse ctx fInfo blockPreds

  let blockUsageInfo :: Map (MemSegmentOff 64) (BlockEqClassVec X86Reg)
      blockUsageInfo = eqClassVecFromDeps depMap <$> fInfo^.parsedBlocks


  let funCtx = FRC { frcMemory = mem
                   , frcInterp = fInfo
                   , frcSyscallPersonality = sysp
                   , frcCurrentFunctionType = cfti
                   , frcFunctionArgs = funTypeMap
                   , frcPhiVarMap = blockUsageInfo
                   , frcBlockDepMap = depMap
                   }
  runFunRecover funCtx $ do
    let Just b = Map.lookup entryAddr (fInfo^.parsedBlocks)

    let insArg :: Int
               -> X86ArgInfo
               -> MapF X86Reg (FnRegValue X86_64)
               -> MapF X86Reg (FnRegValue X86_64)
        insArg i (ArgBV64 r) m =
          let tp = BVTypeRepr n64
           in MapF.insert (X86_GP r) (FnValue (FnArg i tp) (WidthEqRefl tp)) m
        insArg i (ArgMM512D r) m =
          let itp = VecTypeRepr n8 (BVTypeRepr n64)
              pr = PackBits n8 n64
           in MapF.insert (X86_ZMMReg r) (FnValue (FnArg i itp) pr) m

    -- This marks all the registers in the ABI that should save their
    -- value as callee saved
    let insCalleeSaved (Some r) = MapF.insert r (CalleeSaved r)

    -- Note. We currently do not support arguments passed by stack.
    let initStackMap = emptyStackMap

    -- Compute registers for first block
    let initRegs :: MapF X86Reg (FnRegValue X86_64)
        initRegs
          = MapF.empty
          & flip (ifoldr insArg) (ftiArgRegs cfti)
          & flip (foldr insCalleeSaved) x86CalleeSavedRegs
          -- Set df to 0 at function start.
          & MapF.insert DF (FnValue (FnConstantBool False) (WidthEqRefl (typeRepr DF)))

    r0 <-
      evalRecover b [] V.empty initRegs initStackMap $ do
        fb <- recoverBlock b
        return $! emptyRecoveredBlockInfo & addFnBlock fb

    rf <- foldM (recoverInnerBlock fInfo blockPreds blockUsageInfo)
                r0
                (Map.keys blockPreds)

    let (Just entry,restBlocks) =
          Map.updateLookupWithKey (\_ _ -> Nothing) entryAddr rf

    pure $! Function { fnAddr = entryAddr
                     , fnType = resolveX86FunctionType cfti
                     , fnName = nm
                     , fnEntryBlock = entry
                     , fnRestBlocks = Map.elems restBlocks
                     }
