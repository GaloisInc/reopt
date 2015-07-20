{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Reopt.CFG.Recovery
  ( Function(..)
  , FnBlock(..)
  , FnStmt(..)
  , FnTermStmt(..)
  , FnValue(..)
  , FnAssignment(..)
  , FnAssignRhs(..)

  , recoverFunction
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Error
import Control.Monad.State.Strict
import Data.Int (Int64)
import Data.Foldable as Fold (toList, traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Type.Equality
import Data.Word
import Numeric (showHex)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Reopt.CFG.InterpState
import Reopt.CFG.Representation
import qualified Reopt.Machine.StateNames as N
import Reopt.Machine.Types
import Reopt.Object.Memory

import Debug.Trace

------------------------------------------------------------------------
-- Function definitions

data Function = Function { fnAddr :: CodeAddr
                         , fnBlocks :: [FnBlock]
                         }

instance Pretty Function where
  pretty fn =
    pretty (showHex (fnAddr fn) "") <$$>
    vcat (pretty <$> fnBlocks fn)

data FnBlock
   = FnBlock { fbLabel :: !BlockLabel
             , fbStmts :: ![FnStmt]
             , fbTerm :: !(FnTermStmt)
             }


instance Pretty FnBlock where
  pretty b =
    pretty (fbLabel b) <$$>
    indent 2 (vcat (pretty <$> fbStmts b) <$$> pretty (fbTerm b))

data FnStmt
  = forall tp . FnWriteMem !(FnValue (BVType 64)) !(FnValue tp)
    -- | A comment
  | FnComment !Text
    -- | An assignment statement
  | forall tp . FnAssignStmt !(FnAssignment tp)


instance Pretty FnStmt where
  pretty s =
    case s of
      FnWriteMem addr val -> text "*" <> parens (pretty addr) <+> text "=" <+> pretty val
      FnComment msg -> text "#" <+> text (Text.unpack msg)

data FnTermStmt
   = FnTermStmtUndefined
   | FnBranch !(FnValue BoolType) !BlockLabel !BlockLabel

instance Pretty FnTermStmt where
  pretty s =
    case s of
      FnTermStmtUndefined -> text "undefined term"
      FnBranch c x y -> text "branch" <+> pretty c <+> pretty x <+> pretty y

data FnAssignment tp
   = FnAssignment { fnAssignId :: !AssignId
                  , fnAssignRhs :: !(FnAssignRhs tp)
                  }

-- | The right-hand side of a function assingment statement.
data FnAssignRhs (tp :: Type) where
  -- An expression with an undefined value.
  FnSetUndefined :: !(NatRepr n) -- Width of undefined value.
                 -> FnAssignRhs (BVType n)
  FnReadMem :: !(FnValue (BVType 64))
            -> !(TypeRepr tp)
            -> FnAssignRhs tp
  FnEvalApp :: !(App FnValue tp)
            -> FnAssignRhs tp
  FnAlloca :: !(FnValue (BVType 64))
           -> FnAssignRhs (BVType 64)

-- | A function value.
data FnValue (tp :: Type) where
  FnValueUnsupported :: FnValue tp
  FnConstantValue :: NatRepr n -> Integer -> FnValue (BVType n)
  -- Value from an assignment statement.
  FnAssignedValue :: !(FnAssignment tp) -> FnValue tp
  -- The entry pointer to a function.
  FnEntryValue :: Word64 -> FnValue (BVType 64)
  -- A pointer to an internal block.
  FnBlockValue :: Word64 -> FnValue (BVType 64)
  -- The address at the implicit alloca that occurs at the start of a block.
  FnBlockAllocAddr :: Word64 -> FnValue (BVType 64)


instance Pretty (FnValue tp) where
  pretty FnValueUnsupported = text "unsupported"

------------------------------------------------------------------------
-- StackHeight

-- | Describe the amount of space needed to allocate for the stack.
-- The first parameter is a constant, the remaining values are the
-- set of stack locations accessed.  The height of the stack must be
-- at least as large as each value in the set.
data StackHeight = StackHeight Int64 (Set (Value (BVType 64)))

-- | Create a stack height from a single value.
valueAsStackHeight :: Value (BVType 64) -> StackHeight
valueAsStackHeight x
  | Just xc <- asInt64Constant x = StackHeight (min xc 0) Set.empty
  | otherwise = StackHeight 0 (Set.singleton x)

-- | Conjoin two stack heights to compute the maximum height.
mergeStackHeight :: StackHeight -> StackHeight -> StackHeight
mergeStackHeight (StackHeight xc xs) (StackHeight yc ys) =
  StackHeight (min xc yc) (Set.union xs ys)


-- | This code is reponsible for parsing the statement to determine
-- what code needs to be added to support an alloca at the beginning
-- of the block.
recoverStmtStackHeight :: Stmt -> State StackHeight ()
recoverStmtStackHeight s = do
  case s of
    Write (MemLoc addr _) val
      | Just offset <- asStackAddrOffset addr -> do
        modify $ mergeStackHeight (valueAsStackHeight offset)
    _ -> do
      return ()

recoverTermStmtStackHeight :: TermStmt -> State StackHeight ()
recoverTermStmtStackHeight (FetchAndExecute s) =
  case asStackAddrOffset (s ^. register N.rsp) of
    Nothing ->
      trace "Could not intepret stack height" $ do
        return ()
    Just offset -> do
      modify $ mergeStackHeight (valueAsStackHeight offset)
recoverTermStmtStackHeight _ = do
  return ()

------------------------------------------------------------------------
-- FnStack

-- | This stores information about the current stack when working on function
-- identification.
data FnStack = UndefinedFnStack

initFnStack :: FnStack
initFnStack = UndefinedFnStack

-- | Given information about the stack and a offset into the stack, return
-- a function value denoting the given location.
stackOffsetAddr :: FnStack -> Value (BVType 64) -> FnValue (BVType 64)
stackOffsetAddr _ _ = trace "stackOffsetAddr unsupported" $
 FnValueUnsupported

------------------------------------------------------------------------
-- RecoverState

data FnRegValue cl where
  -- | This is a callee saved register.
  CalleeSaved :: N.RegisterName cl -> FnRegValue cl
  -- | This register comes from an integer argument.
  FnIntArg :: Int -> FnRegValue 'N.GP

data RecoverState = RS { _rsInterp :: !InterpState
                       , _rsBlocks :: !(Map BlockLabel FnBlock)
                       , _rsFrontier :: !(Set BlockLabel)
                       , _rsNextAssignId :: !AssignId
                       , _rsRegMap    :: !(Map CodeAddr (MapF N.RegisterName FnRegValue))
                       , _rsStackMap  :: !(Map CodeAddr FnStack)

                       , _rsCurAddr   :: !CodeAddr
                       , _rsCurStmts  :: !(Seq FnStmt)
                       , _rsAssignMap :: !(MapF Assignment FnAssignment)
                       }


rsInterp :: Simple Lens RecoverState InterpState
rsInterp = lens _rsInterp (\s v -> s { _rsInterp = v })

rsBlocks :: Simple Lens RecoverState (Map BlockLabel FnBlock)
rsBlocks = lens _rsBlocks (\s v -> s { _rsBlocks = v })

rsFrontier :: Simple Lens RecoverState (Set BlockLabel)
rsFrontier = lens _rsFrontier (\s v -> s { _rsFrontier = v })

rsNextAssignId :: Simple Lens RecoverState AssignId
rsNextAssignId = lens _rsNextAssignId (\s v -> s { _rsNextAssignId = v })

rsRegMap :: Simple Lens RecoverState (Map CodeAddr (MapF N.RegisterName FnRegValue))
rsRegMap = lens _rsRegMap (\s v -> s { _rsRegMap = v })

rsStackMap :: Simple Lens RecoverState (Map CodeAddr FnStack)
rsStackMap = lens _rsStackMap (\s v -> s { _rsStackMap = v })

rsCurAddr :: Simple Lens RecoverState CodeAddr
rsCurAddr = lens _rsCurAddr (\s v -> s { _rsCurAddr = v })

-- | List of statements accumulated so far.
rsCurStmts :: Simple Lens RecoverState (Seq FnStmt)
rsCurStmts = lens _rsCurStmts (\s v -> s { _rsCurStmts = v })

-- | Map from assignments in original block to assignment in
rsAssignMap :: Simple Lens RecoverState (MapF Assignment FnAssignment)
rsAssignMap = lens _rsAssignMap (\s v -> s { _rsAssignMap = v })

------------------------------------------------------------------------
-- Recover

type Recover a = StateT RecoverState (ErrorT String Identity) a

runRecover :: RecoverState -> Recover a -> Either String a
runRecover s m = runIdentity $ runErrorT $ evalStateT m s

getCurStack :: Recover FnStack
getCurStack = do
  addr <- use rsCurAddr
  m_stk <- uses rsStackMap (Map.lookup addr)
  case m_stk of
    Nothing -> error "Current stack undefined."
    Just stk -> return stk

mkFnAssign :: FnAssignRhs tp -> Recover (FnAssignment tp)
mkFnAssign rhs = do
  next_id <- use rsNextAssignId
  rsNextAssignId .= next_id + 1
  return $! FnAssignment next_id rhs

addFnStmt :: FnStmt -> Recover ()
addFnStmt stmt = rsCurStmts %= (Seq.|> stmt)

addFrontier :: BlockLabel -> Recover ()
addFrontier lbl = do
  mr <- uses rsBlocks (Map.lookup lbl)
  case mr of
    Nothing -> do
      rsFrontier %= Set.insert lbl
    Just{} -> do
      return ()

-- | Return value bound to register (if any)
lookupInitialReg :: N.RegisterName cl -> Recover (Maybe (FnRegValue cl))
lookupInitialReg reg = do
  addr <- use rsCurAddr
  maybe_map <- uses rsRegMap (Map.lookup addr)
  case maybe_map of
    Nothing -> do
      error $ "Did not define register map for " ++ showHex addr "."
    Just reg_map -> do
      return $! MapF.lookup reg reg_map

------------------------------------------------------------------------
-- recoverFunction

recoverFunction :: InterpState -> CodeAddr -> Either String Function
recoverFunction s a = do
  let initRegs = MapF.empty
               & MapF.insert N.rdi (FnIntArg 0)
               & MapF.insert N.rsi (FnIntArg 1)
               & MapF.insert N.rdx (FnIntArg 2)
               & MapF.insert N.rcx (FnIntArg 3)
               & MapF.insert N.r8  (FnIntArg 4)
               & MapF.insert N.r9  (FnIntArg 5)
               & MapF.insert N.rbx (CalleeSaved N.rbx)
               & MapF.insert N.r12 (CalleeSaved N.r12)
               & MapF.insert N.r13 (CalleeSaved N.r13)
               & MapF.insert N.r14 (CalleeSaved N.r14)
               & MapF.insert N.r15 (CalleeSaved N.r15)
               & MapF.insert N.rbp (CalleeSaved N.rbp)
  let rs = RS { _rsInterp = s
              , _rsBlocks = Map.empty
              , _rsFrontier = Set.singleton (GeneratedBlock a 0)
              , _rsNextAssignId = 0
              , _rsRegMap = Map.singleton a initRegs
              , _rsStackMap = Map.singleton a initFnStack
              , _rsCurAddr = 0
              , _rsCurStmts = Seq.empty
              , _rsAssignMap = MapF.empty
              }
  runRecover rs $ do
    recoverIter
    block_map <- use rsBlocks
    return $! Function { fnAddr = a
                       , fnBlocks = Map.elems block_map
                       }

-- | Explore states until we have reached end of frontier.
recoverIter :: Recover ()
recoverIter = do
  f <- use rsFrontier
  case Set.maxView f of
    Nothing -> return ()
    Just (lbl,f') -> do
      trace ("Exploring " ++ show lbl) $ do
      rsCurAddr .= labelAddr lbl
      rsCurStmts  .= Seq.empty
      rsAssignMap .= MapF.empty
      b <- recoverBlock lbl
      rsFrontier %= Set.delete lbl
      rsBlocks   %= Map.insert lbl b

recoverBlock :: BlockLabel
             -> Recover FnBlock
recoverBlock lbl = do
  Just b <- uses (rsInterp . blocks)  (`lookupBlock` lbl)


  let ht0 = StackHeight 0 Set.empty
      ht = flip execState ht0 $ do
             Fold.traverse_ recoverStmtStackHeight (blockStmts b)
             recoverTermStmtStackHeight (blockTerm b)
  case ht of
    StackHeight c s
      | c == 0 && Set.null s -> return ()
      | Set.null s -> do
        let sz = FnConstantValue n64 (toInteger (negate c))
        fnAssign <- mkFnAssign (FnAlloca sz)
        addFnStmt $ FnAssignStmt fnAssign
      | otherwise ->
        trace "Unsupported stack height" $ return ()

  Fold.traverse_ recoverStmt (blockStmts b)
  term <- recoverTermStmt (blockTerm b)
  stmts <- use rsCurStmts
  return $! FnBlock { fbLabel = lbl
                    , fbStmts = Fold.toList stmts
                    , fbTerm = term
                    }

-- | This should add code as needed to support the statement.
recoverStmt :: Stmt -> Recover ()
recoverStmt s =
  case s of
    AssignStmt assign -> do
      let lhs = assignId assign
      case assignRhs assign of
        EvalApp _ -> return ()
        SetUndefined w -> do
          fnAssign <- mkFnAssign (FnSetUndefined w)
          rsAssignMap %= MapF.insert assign fnAssign
          addFnStmt $ FnAssignStmt fnAssign
        Read (MemLoc addr tp) -> do
          fn_addr <- recoverValue addr
          fnAssign <- mkFnAssign (FnReadMem fn_addr tp)
          rsAssignMap %= MapF.insert assign fnAssign
          addFnStmt $ FnAssignStmt fnAssign
        _ -> trace ("recoverStmt undefined for " ++ show (pretty s)) $ do
          return ()
    Write (MemLoc addr _) val
      | Just int_addr_off <- asStackAddrOffset addr
      , Initial reg <- val -> do
        stk <- getCurStack
        m_reg_val <- lookupInitialReg reg
        case m_reg_val of
          Just (CalleeSaved _) -> do
            -- TODO: Update stack with this information.
            return ()
          _ -> do
            let r_addr = stackOffsetAddr stk int_addr_off
            r_val  <- recoverValue val
            addFnStmt $ FnWriteMem r_addr r_val
      | otherwise -> do
        r_addr <- recoverValue addr
        r_val  <- recoverValue val
        addFnStmt $ FnWriteMem r_addr r_val
    Comment msg -> do
      addFnStmt $ FnComment msg
    _ -> trace ("recoverStmt undefined for " ++ show (pretty s)) $ do
      return ()

recoverTermStmt :: TermStmt -> Recover FnTermStmt
recoverTermStmt s =
  case s of
    Branch c x y -> do
      cv <- recoverValue c
      addFrontier x
      addFrontier y
      return $! FnBranch cv x y
    _ -> trace ("recoverTermStmt undefined for " ++ show (pretty s)) $ do
      return $ FnTermStmtUndefined

recoverValue :: Value tp -> Recover (FnValue tp)
recoverValue v = do
  interpState <- use rsInterp
  mem <- uses rsInterp memory
  case v of
    BVValue w i
      | Just Refl <- testEquality w n64
      , let addr = fromInteger i
      , Just seg <- findSegment addr mem -> do
        case () of
          _ | memFlags seg `hasPermissions` pf_x
            , Set.member addr (interpState^.functionEntries) -> do
              return $ FnEntryValue addr

            | memFlags seg `hasPermissions` pf_x
            , Map.member addr (interpState^.blocks) -> do
              cur_addr <- use rsCurAddr
              when (not (inSameFunction cur_addr addr interpState)) $ do
                trace ("Cross function jump " ++ showHex cur_addr " to " ++ showHex addr ".") $
                  return ()
              return $ FnBlockValue addr

            | otherwise -> do
              trace ("recoverValue given segment pointer: " ++ showHex i "") $ do
                return $ FnValueUnsupported
      | otherwise -> do
        return $ FnConstantValue w i

    AssignedValue assign -> do
      m_seen <- uses rsAssignMap (MapF.lookup assign)
      case m_seen of
        Just fnAssign ->
          return $! FnAssignedValue fnAssign
        Nothing -> do
          case assignRhs assign of
            EvalApp app -> do
              app' <- traverseApp recoverValue app
              fnAssign <- mkFnAssign (FnEvalApp app')
              rsAssignMap %= MapF.insert assign fnAssign
              return $! FnAssignedValue fnAssign
            _ -> do
              trace ("recoverValue does not yet support assignment " ++ show (pretty assign)) $
                return $ FnValueUnsupported
    Initial reg -> do
      m_reg_val <- lookupInitialReg reg
      case m_reg_val of
        Just (CalleeSaved _) -> do
          trace ("recoverValue unexpectedly encountered callee saved register: " ++ show reg) $ do
          return $ FnValueUnsupported
        _ ->
          trace ("recoverValue does not yet support initial value " ++ show reg) $
            return $ FnValueUnsupported