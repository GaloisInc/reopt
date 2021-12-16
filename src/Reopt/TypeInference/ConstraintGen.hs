{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- This module provides a constraint generation pass for inferring the
-- types of Reopt (FnRep) programs, inspired by TIE.
--

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module Reopt.TypeInference.ConstraintGen
  ( genModule
  , FunType
  , Constraint
  , ModuleConstraints(..)
  ) where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.ByteString.Char8      as BSC
import           Data.Foldable              (traverse_)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (isJust)
import qualified Data.Vector                as V

import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some

import           Reopt.CFG.FnRep

import           Data.Macaw.CFG             (App (..), ArchAddrWidth,
                                             ArchSegmentOff)
import           Data.Macaw.Memory          (Memory, absoluteAddr,
                                             addrWidthClass, asSegmentOff,
                                             memAddrWidth, memWidth)
import           Data.Macaw.Types
import Data.List (intercalate)

-- This algorithm proceeds in stages:
-- 1. Give type variables to the arguments to all functions
-- 2. For each function:
--  a. Give type variables to the arguments to all blocks
--  b. Generate constraints per-block

-- Note we need register use information at the function level (to
-- generate function types) and at the block level (basically phi
-- nodes, so we don't unify the types of dead values).

-- ------------------------------------------------------------
-- Core types

-- type RegType arch = Map (Some (ArchReg arch)) IType

-- FIXME: arch not required here
data FunType arch = FunType {
    funArgs :: [ IType ]
  , funRet  :: Maybe IType
}

instance Show (FunType arch) where
  show ft =
    "(" ++ intercalate ", " (map show (funArgs ft)) ++ ") -> " ++
    maybe "_|_" show (funRet ft)

newtype TyVar = TyVar { _getTyVar :: Int }
  deriving (Eq, Ord)

instance Show TyVar where
  show (TyVar i) = "α" ++ show i

data IType = ITyVar TyVar

instance Show IType where
  show (ITyVar tv) = show tv

-- This is the free type for constraints, should be replace by the
--  type from the constraint solver.

data Constraint =
  CEq IType IType -- ^ The types must be equal
  | CAddrWidthAdd IType IType IType
  -- ^ At most one type may be a ptr, first type is the result
  | CAddrWidthSub IType IType IType
  -- ^ The rhs may not be a ptr if the lhs is a bv, first type is the result
  | CBVNotPtr IType -- ^ The argument cannot be a ptr (so should be a bv)
  | CIsPtr IType    -- ^ The type must point to something

instance Show Constraint where
  show c =
    case c of
      CEq t1 t2 -> show t1 ++ " = " ++ show t2
      CAddrWidthAdd t1 t2 t3 -> show t1 ++ " = " ++ show t2 ++ " + " ++ show t3
      CAddrWidthSub t1 t2 t3 -> show t1 ++ " = " ++ show t2 ++ " - " ++ show t3
      CBVNotPtr t -> "non-ptr " ++ show t
      CIsPtr t    -> "ptr " ++ show t

-- -----------------------------------------------------------------------------
-- Monad

-- This tracks e.g. fresh type variable names, along with known
-- types/function types.

newtype Warning = Warning String

instance Show Warning where
  show (Warning w) = w

type PhiType arch = Map (Some (FnPhiVar arch)) IType

data CGenContext arch = CGenContext {
  -- Global state
  cgenFunTypes        :: Map (ArchSegmentOff arch) (FunType arch)
  -- | External functions (only named).
  , cgenExtFunTypes   :: Map BSC.ByteString (FunType arch)

  , cgenMemory        :: Memory (ArchAddrWidth arch)
  -- Function state
  , cgenCurrentFun    :: FunType arch
  , cgenBlockPhiTypes :: Map (FnBlockLabel (ArchAddrWidth arch)) [IType]

  -- Current block state
  , cgenCurrentPhis  :: PhiType arch
}

data CGenState = CGenState {
    _nextFreeTyVar :: Int -- FIXME: use Nonce?
  , _assignTypes   :: Map FnAssignId IType

  -- | Offset of the current instruction, used (not right now) for
  -- tagging constraints and warnings.
  -- , _curOffset     :: ArchAddrWord arch 
  , _warnings      :: [Warning]
  , _constraints   :: [Constraint] -- in reverse gen order
}

makeLenses ''CGenState

newtype CGenM arch a =
  CGenM { _getCGenM :: ReaderT (CGenContext arch) (State CGenState) a }
  deriving (Functor, Applicative, Monad)

runCGenM :: Memory (ArchAddrWidth arch) ->
            CGenM arch a ->
            (a, CGenState)
runCGenM mem (CGenM m) =
  runState (runReaderT m ctxt) st0
  where
    -- FIXME: make this nicer
    ctxt = CGenContext { cgenFunTypes = error "No fun tys"
                       , cgenExtFunTypes = error "No ext fun tys"
                       , cgenMemory   = mem
                       , cgenCurrentFun  = error "No current fun"
                       , cgenBlockPhiTypes = error "No phi types"
                       , cgenCurrentPhis   = error "No current phis"
                       }
    st0 = CGenState { _nextFreeTyVar = 0
                    , _assignTypes = mempty
                    , _warnings    = mempty
                    , _constraints = mempty
                    }


-- ------------------------------------------------------------
-- Monad operations

-- | Warn about a missing feature (for example).
warn :: String -> CGenM arch ()
warn s = CGenM $ warnings <>= [Warning s]

-- '<<+=' increment var, return old value

-- | Returns a fresh type var.
freshTyVar :: CGenM arch TyVar
freshTyVar = CGenM $ TyVar <$> (nextFreeTyVar <<+= 1)

-- | Creates a new type variable and associates it with the assignment
freshForAssignment :: FnAssignment arch tp -> CGenM arch TyVar
freshForAssignment a = do
  tyv <- freshTyVar
  -- update assignment type map
  CGenM $ assignTypes . at (fnAssignId a) ?= ITyVar tyv
  pure tyv

-- freshForCallRet :: FnReturnVar tp -> CGenM arch TyVar
-- freshForCallRet a = do
--   tyv <- freshTyVar
--   -- update assignment type map
--   CGenM $ assignTypes . at (frAssignId a) ?= ITyVar tyv
--   pure tyv

assignmentType :: FnAssignment arch tp -> CGenM arch IType
assignmentType a = do
  m_ty <- CGenM $ use (assignTypes . at (fnAssignId a))
  case m_ty of
    Nothing -> error "Missing assignment type"
    Just ty -> pure ty

-- We lump returns and assigns into the same map
funRetType :: FnReturnVar tp -> CGenM arch IType
funRetType fr = do
  m_ty <- CGenM $ use (assignTypes . at (frAssignId fr))
  case m_ty of
    Nothing -> error "Missing function return type"
    Just ty -> pure ty

updFunRetType :: FnReturnVar tp -> IType -> CGenM arch ()
updFunRetType fr ty =
  CGenM $ assignTypes . at (frAssignId fr) ?= ty

phiType :: FnPhiVar arch tp -> CGenM arch IType
phiType phiv = do
  m_ty <- CGenM $ asks (Map.lookup (Some phiv) . cgenCurrentPhis)
  case m_ty of
    Nothing -> error "Missing phi var"
    Just ty -> pure ty

argumentType :: Int -> CGenM arch IType
argumentType i = do
  tys <- CGenM $ asks (funArgs . cgenCurrentFun)
  case tys ^? ix i of
    Nothing -> error "Missing argument"
    Just ty -> pure ty

emitConstraint :: Constraint -> CGenM arch ()
emitConstraint c = CGenM $ constraints %= (:) c

addrWidth :: CGenM arch (NatRepr (ArchAddrWidth arch))
addrWidth = CGenM $ asks (memWidth . cgenMemory)

-- currentOffset :: CGenM arch (ArchAddrWord arch)
-- currentOffset = CGenM $ use curOffset

-- setCurrentOffset :: ArchAddrWord arch -> CGenM arch ()
-- setCurrentOffset off = CGenM $ curOffset .= off

currentFunType :: CGenM arch (FunType arch)
currentFunType = CGenM $ asks cgenCurrentFun

phisForBlock :: FnBlockLabel (ArchAddrWidth arch)
             -> CGenM arch [IType]
phisForBlock blockAddr = do
  m_rtyp <- CGenM $ asks (Map.lookup blockAddr . cgenBlockPhiTypes)
  case m_rtyp of
    Nothing -> error "Missing phi types"
    Just rtyp -> pure rtyp

funTypeAtAddr :: forall arch. FnValue arch (BVType (ArchAddrWidth arch))
              -> CGenM arch (Maybe (FunType arch))
funTypeAtAddr saddr = do
  ftypes    <- CGenM $ asks cgenFunTypes
  extftypes <- CGenM $ asks cgenExtFunTypes
  aWidth    <- memAddrWidth <$> CGenM (asks cgenMemory)

  case saddr of
    FnConstantValue _ v -> do
      -- c.f. RegisterUse.x86CallRegs
      let faddr = addrWidthClass aWidth $
                  absoluteAddr (fromInteger v)
      -- FIXME
      mem <- CGenM $ asks cgenMemory
      pure (flip Map.lookup ftypes =<< asSegmentOff mem faddr)
    FnFunctionEntryValue _ fn -> pure (Map.lookup fn extftypes)
    _ -> pure Nothing

withContext :: (CGenContext arch -> CGenContext arch) -> CGenM arch a -> CGenM arch a
withContext f (CGenM m) = CGenM (local f m)

-- -----------------------------------------------------------------------------
-- Imported API

-- The representation of constraints is still being figured out, so
-- these functions abstract over the precise representation.

-- ------------------------------------------------------------
-- Constraints

emitEq :: IType -> IType -> CGenM arch ()
emitEq t1 t2 = emitConstraint (CEq t1 t2)

-- | Emits an add which may be a pointer add
emitPtrAdd :: IType -> IType -> IType -> CGenM arch ()
emitPtrAdd rty t1 t2 = emitConstraint (CAddrWidthAdd rty t1 t2)

-- | Emits a sub which may return a pointer
emitPtrSub :: IType -> IType -> IType -> CGenM arch ()
emitPtrSub rty t1 t2 = emitConstraint (CAddrWidthSub rty t1 t2)

-- | Emits a constraint that the argument isn't a pointer
emitNotPtr :: IType -> CGenM arch ()
emitNotPtr t = emitConstraint (CBVNotPtr t)

emitPtr :: IType -> CGenM arch ()
emitPtr t = emitConstraint (CIsPtr t)

-- -----------------------------------------------------------------------------
-- Core algorithm

genFnValue :: FnValue arch tp -> CGenM arch IType
genFnValue v =
  case v of
    FnUndefined {} -> punt
    FnConstantBool {} -> punt
    FnConstantValue {} -> punt
    FnAssignedValue a -> assignmentType a
    FnPhiValue phiv   -> phiType phiv
    FnReturn frv      -> funRetType frv
    FnFunctionEntryValue {} -> punt
    FnArg i _         -> argumentType i
  where
    punt = do
      warn "Punting on FnValue"
      ITyVar <$> freshTyVar

-- | Generate constraints for an App.  The first argument is the
-- output (result) type.
genApp :: IType -> App (FnValue arch) tp -> CGenM arch ()
genApp ty app =
  case app of
    Eq l r    -> vEq l r
    Mux _ _ l r -> vEq l r

    -- We don't generate any further constraints for boolean ops
    AndApp {} -> pure ()
    OrApp  {} -> pure ()
    NotApp {} -> pure ()
    XorApp {} -> pure ()

    -- We currently ignore tuples
    MkTuple {} -> warn "Ignoring MkTyple"
    TupleField {} -> warn "Ignoring TupleField"

    -- We currently ignore vectors
    ExtractElement {} -> warn "Ignoring ExtractElement"
    InsertElement {}  -> warn "Ignoring InsertElement"

    -- BV size operations
    -- FIXME: for now we assume we can only do these on bitvecs (not pointers)
    Trunc v _ -> nonptrUnOp v
    SExt  v _ -> nonptrUnOp v
    UExt  v _ -> nonptrUnOp v

    -- FIXME: Not sure what to do with these, for now leave underconstrained.
    Bitcast {} -> warn "Ignoring Bitcast"

    -- BV Arithmetic
    --
    -- In general we don't need to do much, as most are just bv ops.
    -- Add and Sub are the main exceptions
    BVAdd sz l r -> do
      addrw <- addrWidth
      when (isJust (testEquality addrw sz))
        (join $ emitPtrAdd ty <$> genFnValue l <*> genFnValue r)
      -- non-64 bit adds can't generate a pointer so we don't need to
      -- generate that constraint

    -- FIXME: should this be considered another add?
    BVAdc _ l r _c -> nonptrBinOp l r

    BVSub sz l r -> do
      addrw <- addrWidth
      when (isJust (testEquality addrw sz))
        (join $ emitPtrSub ty <$> genFnValue l <*> genFnValue r)

    BVSbb _ l r _ -> nonptrBinOp l r
    BVMul _ l r   -> nonptrBinOp l r

    -- We are allowed to compare pointers, and test their bits 
    BVUnsignedLe {} -> pure ()
    BVUnsignedLt {} -> pure ()
    BVSignedLe {} -> pure ()
    BVSignedLt {} -> pure ()
    BVTestBit {}  -> pure ()

    BVComplement _ v -> nonptrUnOp v
    -- FIXME: this could be a pointer op to mask bits off?
    BVAnd _ l r      -> nonptrBinOp l r

    -- FIXME: this could be a pointer op to do add on aligned ptrs?
    BVOr  _ l r      -> nonptrBinOp l r
    BVXor _ l r      -> nonptrBinOp l r
    BVShl _ l r      -> nonptrBinOp l r
    BVShr _ l r      -> nonptrBinOp l r
    BVSar _ l r      -> nonptrBinOp l r

    -- FIXME: e.g. l could be a ptr?  Not sure why you would use this though
    UadcOverflows l r _ -> nonptrBinCmp l r
    SadcOverflows l r _ -> nonptrBinCmp l r
    UsbbOverflows l r _ -> nonptrBinCmp l r
    SsbbOverflows l r _ -> nonptrBinCmp l r

    PopCount _ v     -> nonptrUnOp v
    ReverseBytes _ v -> nonptrUnOp v
    Bsf _ v          -> nonptrUnOp v
    Bsr _ v          -> nonptrUnOp v
  where
    vEq :: forall arch tp tp'. FnValue arch tp -> FnValue arch tp' -> CGenM arch ()
    vEq l r = do
      l_ty <- genFnValue l
      r_ty <- genFnValue r
      emitEq l_ty r_ty
      emitEq ty l_ty -- r_ty == ty by transitivity, so we don't add it

    nonptrUnOp :: forall n arch. FnValue arch (BVType n) -> CGenM arch ()
    nonptrUnOp v = do
      emitNotPtr =<< genFnValue v
      emitNotPtr ty

    -- | The result and arguments have to be bitvecs (i.e., not ptrs).
    -- We don't relate the sizes as that is given by the macaw type at
    -- the moment.
    nonptrBinOp :: forall n m arch.
      FnValue arch (BVType n) -> FnValue arch (BVType m) -> CGenM arch ()
    nonptrBinOp l r = do
      emitNotPtr =<< genFnValue l
      emitNotPtr =<< genFnValue r
      emitNotPtr ty

    nonptrBinCmp :: forall n m arch.
      FnValue arch (BVType n) -> FnValue arch (BVType m) -> CGenM arch ()
    nonptrBinCmp l r = do
      emitNotPtr =<< genFnValue l
      emitNotPtr =<< genFnValue r

genFnAssignment :: FnAssignment arch tp -> CGenM arch ()
genFnAssignment a = do
  ty <- ITyVar <$> freshForAssignment a
  case fnAssignRhs a of
    FnSetUndefined {} -> pure () -- no constraints generated
    FnReadMem ptr _sz -> emitPtr =<< genFnValue ptr
    FnCondReadMem _sz _cond ptr def -> do
      emitPtr =<< genFnValue ptr
      emitEq ty =<< genFnValue def

    FnEvalApp app -> genApp ty app

    FnEvalArchFn _afn -> warn "ignoring EvalArchFn"



-- The offset argument is used by call term stmts
genFnStmt :: FnStmt arch
          -> CGenM arch ()
genFnStmt stmt =
  case stmt of
    FnComment _ -> pure ()
    FnAssignStmt a -> genFnAssignment a
    FnWriteMem addr _v -> emitPtr =<< genFnValue addr
    FnCondWriteMem _cond addr _v _ -> emitPtr =<< genFnValue addr
    FnCall fn args m_rv -> genCall fn args m_rv
    FnArchStmt _astmt   -> warn "Ignoring FnArchStmt"

-- -- | Matches up the argument registers with the destination.
-- genRegType :: RegState (ArchReg arch) (Value arch ids)
--            -> RegType arch
--            -> CGenM arch IType
-- genRegType regs rty = traverse_ go (Map.toList rty)
--   where
--     go (Some r, ty) = emitEq ty =<< genFnValue (regs ^. boundValue r)

-- | Used at the end of a block on a control transfer.  In essence
-- this unifies the block arguments with the values from the current block.
genBlockTransfer :: FnJumpTarget arch
                 -> CGenM arch ()
genBlockTransfer tgt = do
  phiTys <- phisForBlock (fnJumpLabel tgt)
  let phiVals = V.toList (fnJumpPhiValues tgt)
  zipWithM_ go phiVals phiTys
  where
    go (Some v) ty = emitEq ty =<< genFnValue v

genCall :: FnValue arch (BVType (ArchAddrWidth arch)) ->
           -- | arguments
           [ Some (FnValue arch) ] ->
           -- | Name of return value           
           Maybe (Some FnReturnVar) ->
           CGenM arch ()
genCall fn args m_ret = do
  m_ftyp <- funTypeAtAddr fn

  case m_ftyp of
    Nothing -> warn "Couldn't determine target of call"
    Just ftyp -> do
      -- Arguments
      zipWithM_ go args (funArgs ftyp)

      -- Return
      case (m_ret, funRet ftyp) of
        (Just (Some rv), Just rty) -> updFunRetType rv rty
        (Just _, _) -> warn "Missing return type"
        _ -> pure ()

  where
    go (Some v) ty = emitEq ty =<< genFnValue v

genFnBlock :: FnBlock arch -- ^ Current block
           -> CGenM arch ()
genFnBlock b = do
  -- let blockAddr = pblockAddr b

  -- Generate constraints (and type vars) for the stmts
  mapM_ genFnStmt (fbStmts b)
  case fbTerm b of
    FnJump tgtAddr ->
      genBlockTransfer tgtAddr

    FnBranch _cond trueAddr falseAddr -> do
      genBlockTransfer trueAddr
      genBlockTransfer falseAddr

    FnLookupTable _lookupIdx tgtV ->
      traverse_ genBlockTransfer tgtV

    FnRet m_v -> do
      fty <- currentFunType
      case (m_v, funRet fty) of
        (Just (Some v), Just ty) -> emitEq ty =<< genFnValue v
        (Nothing, Nothing) -> pure ()
        _ -> warn "Mismatch between return type and return value"

    FnTailCall fn args -> do
      fty <- currentFunType
      m_calledFTy <- funTypeAtAddr fn
      genCall fn args Nothing
      case (funRet fty, funRet <$> m_calledFTy) of
        (_, Nothing) -> warn "Unknown function"
        (Just rty, Just (Just rty')) -> emitEq rty rty'
        (Just _, _) -> warn "Mismatch between return type and return type in tail call"
        _ -> pure ()

genFunction :: Function arch -> CGenM arch ()
genFunction fn = do
  -- allocated tyvars for phi nodes.
  let mkPhis b = (,) (fbLabel b) . map ITyVar <$> replicateM (V.length (fbPhiVars b)) freshTyVar
  bphis <- Map.fromList <$> mapM mkPhis blocks

  -- FIXME: abstract over
  m_cfun <- CGenM $ asks (Map.lookup (fnAddr fn) . cgenFunTypes)
  let cfun = case m_cfun of
        Nothing -> error "Missing function"
        Just cfun' -> cfun'


  let goBlock b = do
        let cphis = case Map.lookup (fbLabel b) bphis of
              Nothing -> error "Missing block"
              Just phis -> phis
            cphisMap = Map.fromList (zip (V.toList (fbPhiVars b)) cphis)

        withContext (\c -> c { cgenCurrentFun = cfun
                             , cgenBlockPhiTypes = bphis
                             , cgenCurrentPhis = cphisMap
                             }) (genFnBlock b)
  mapM_ goBlock blocks
  where
    blocks = fnBlocks fn

-- Allocates new tyvars
functionTypeToFunType :: FunctionType arch -> CGenM arch (FunType arch)
functionTypeToFunType ft = do
  args <- replicateM (length (fnArgTypes ft)) (ITyVar <$> freshTyVar)
  ret  <- traverse (\_ -> ITyVar <$> freshTyVar) (fnReturnType ft)
  pure (FunType args ret)

data ModuleConstraints arch = ModuleConstraints {
    mcFunTypes :: Map (ArchSegmentOff arch) (FunType arch)
  , mcExtFunTypes :: Map BSC.ByteString (FunType arch)
  , mcAssignTypes :: Map FnAssignId IType
  , mcWarnings    :: [Warning]
  , mcConstraints :: [Constraint]
}

genModule :: RecoveredModule arch ->
             Memory (ArchAddrWidth arch) ->
             ModuleConstraints arch            
genModule m mem = fst $ runCGenM mem $ do
  -- allocate type variables for functions without types
  -- FIXME: we currently ignore hints

  let doDecl d = do
        fty <- functionTypeToFunType (funDeclType d)
        pure ((funDeclAddr d, fty), (funDeclName d, fty))

  (declAddrs, declSyms) <- unzip <$> mapM doDecl (recoveredDecls m)
  -- FIXME: how do symbolic calls work?
  defAddrs <- mapM (\f -> (,) (fnAddr f) <$> functionTypeToFunType (fnType f)) (recoveredDefs m)

  let symMap = Map.fromList declSyms -- FIXME: not sure these are the correct functinos
      addrMap = Map.fromList (defAddrs ++ declAddrs)

  withContext (\c -> c { cgenFunTypes = addrMap, cgenExtFunTypes = symMap }) $
    mapM_ genFunction (recoveredDefs m)

  -- FIXME: abstracr
  idMap <- CGenM $ use assignTypes
  warns <- CGenM $ use warnings
  cstrs <- CGenM $ use constraints

  pure ModuleConstraints { mcFunTypes = addrMap
                         , mcExtFunTypes = symMap
                         , mcAssignTypes = idMap
                         , mcWarnings    = warns
                         , mcConstraints = cstrs }

