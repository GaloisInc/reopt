{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Werror #-}
module Reopt.CFG.FunctionArgs
  ( functionArgs
  ) where

import           Control.Lens
import           Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable as Fold (traverse_)
import           Data.List (intercalate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Parameterized.Classes
import           Data.Parameterized.Some
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Data.Macaw.CFG
import           Data.Macaw.Discovery.Info
import           Reopt.CFG.FnRep (FunctionType(..))
import           Reopt.Machine.X86State
import           Data.Macaw.DebugLogging

-- -----------------------------------------------------------------------------

-- The algorithm computes the set of direct deps (i.e., from writes)
-- and then iterates, propagating back via the register deps.  It
-- doesn't compute assignment uses (although it could) mainly to keep
-- memory use down.  We recompute assignment use later in RegisterUse.
--
-- The basic question this analysis answers is: what arguments does a
-- function require, and what results does it produce?
--
-- There are 3 phases
-- 1. Block-local summarization
-- 2. Function-local summarization
-- 3. Global fixpoint calculation.
--
-- The first 2 phases calculate, for each function, the following information:
--
-- A. What registers are required by a function (ignoring function
--    calls)?
--
-- B. Given that result register {rax, rdx, xmm0} is demanded, what
--    extra register arguments are required, and what extra result
--    arguments are required?
--
-- C. Given that function f now requires argument r, what extra
--    arguments are required, and what extra result registers are
--    demanded?

type RegisterSet = Set (Some X86Reg)

-- | If we demand a register from a function (or block, for phase 1),
-- this results in both direct argument register demands and function
-- result demands.
data DemandSet = DemandSet { registerDemands       :: !RegisterSet
                           , functionResultDemands :: !(Map CodeAddr RegisterSet)
                           }
                 deriving (Eq, Ord, Show)

instance Monoid DemandSet where
  mempty = DemandSet mempty mempty
  mappend ds1 ds2 =
    DemandSet { registerDemands = registerDemands ds1 `mappend` registerDemands ds2
              , functionResultDemands =
                  Map.unionWith Set.union (functionResultDemands ds1)
                                          (functionResultDemands ds2)
              }

demandSetDifference :: DemandSet -> DemandSet -> DemandSet
demandSetDifference ds1 ds2 =
  DemandSet (registerDemands ds1 `Set.difference` registerDemands ds2)
            (Map.differenceWith setDiff (functionResultDemands ds1)
                                        (functionResultDemands ds2))
  where
    setDiff s1 s2 =
      let s' = s1 `Set.difference` s2
      in if Set.null s' then Nothing else Just s'

-- | The types of demands we can make
data DemandType =
  -- | We just want a local register (e.g., a subsequent block needs
  -- register rax)
  DemandAlways
  -- | A function requires an additional argument
  | forall tp. DemandFunctionArg CodeAddr (X86Reg tp)
  -- | The result of the current function.
  | forall tp. DemandFunctionResult (X86Reg tp)

deriving instance Show DemandType

instance Eq DemandType where
  DemandAlways == DemandAlways = True
  (DemandFunctionArg faddr1 r1) == (DemandFunctionArg faddr2 r2) =
    faddr1 == faddr2 && isJust (testEquality r1 r2)
  (DemandFunctionResult r1) == (DemandFunctionResult r2) =
    isJust (testEquality r1 r2)
  _ == _ = False

instance Ord DemandType where
  DemandAlways `compare` DemandAlways = EQ
  DemandAlways `compare` _  = LT
  _ `compare` DemandAlways  = GT

  (DemandFunctionArg faddr1 r1) `compare` (DemandFunctionArg faddr2 r2)
    | faddr1 == faddr2 = case compareF r1 r2 of LTF -> LT
                                                EQF -> EQ
                                                GTF -> GT
    | otherwise = faddr1 `compare` faddr2

  (DemandFunctionArg {}) `compare` _ = LT
  _ `compare` (DemandFunctionArg {}) = GT

  (DemandFunctionResult r1) `compare` (DemandFunctionResult r2) =
    case compareF r1 r2 of LTF -> LT
                           EQF -> EQ
                           GTF -> GT

type DemandMap = Map DemandType DemandSet

demandMapUnion :: DemandMap -> DemandMap -> DemandMap
demandMapUnion = Map.unionWith mappend

type AssignmentCache ids = Map (Some (AssignId ids)) RegisterSet

-- Generated by phase 1, used by phase 2.
data FunctionArgsState ids = FAS {
  -- | Holds state about the set of registers that a block uses
  -- (required by this block).
  _blockTransfer :: !(Map (BlockLabel Word64) (Map (Some X86Reg) DemandSet))

  -- | If a demand d is demanded of block lbl then the block demands S, s.t.
  -- blockDemandMap ^. at lbl ^. at d = Just S
  , _blockDemandMap    :: !(Map (BlockLabel Word64) DemandMap)

  -- | The list of predecessors for a given block
  , _blockPreds     :: !(Map (BlockLabel Word64) [BlockLabel Word64])

  -- | A cache of the assignments and their deps.  The key is not included
  -- in the set of deps (but probably should be).
  , _assignmentCache :: !(AssignmentCache ids)

  -- | The set of blocks we need to consider.
  , _blockFrontier  :: !(Set (BlockLabel Word64))
  }

initFunctionArgsState :: FunctionArgsState ids
initFunctionArgsState =
  FAS { _blockTransfer     = Map.empty
      , _blockDemandMap    = Map.empty
      , _blockPreds        = Map.empty
      , _assignmentCache   = Map.empty
      , _blockFrontier     = Set.empty }

blockTransfer :: Simple Lens (FunctionArgsState ids)
                   (Map (BlockLabel Word64) (Map (Some X86Reg) DemandSet))
blockTransfer = lens _blockTransfer (\s v -> s { _blockTransfer = v })

blockDemandMap :: Simple Lens (FunctionArgsState ids)
                    (Map (BlockLabel Word64) DemandMap)
blockDemandMap = lens _blockDemandMap (\s v -> s { _blockDemandMap = v })

blockPreds :: Simple Lens (FunctionArgsState ids) (Map (BlockLabel Word64) [BlockLabel Word64])
blockPreds = lens _blockPreds (\s v -> s { _blockPreds = v })

assignmentCache :: Simple Lens (FunctionArgsState ids) (AssignmentCache ids)
assignmentCache = lens _assignmentCache (\s v -> s { _assignmentCache = v })

blockFrontier :: Simple Lens (FunctionArgsState ids) (Set (BlockLabel Word64))
blockFrontier = lens _blockFrontier (\s v -> s { _blockFrontier = v })

-- ----------------------------------------------------------------------------------------

type FunctionArgsM ids a = State (FunctionArgsState ids) a

-- ----------------------------------------------------------------------------------------
-- Phase one functions

-- | This registers a block in the first phase (block discovery).
addEdge :: BlockLabel Word64 -> BlockLabel Word64 -> FunctionArgsM ids ()
addEdge source dest =
  do -- record the edge
     blockPreds    %= Map.insertWith mappend dest [source]
     blockFrontier %= Set.insert dest

valueUses :: Value X86_64 ids tp -> FunctionArgsM ids RegisterSet
valueUses = zoom assignmentCache .
            foldValueCached (\_ _    -> mempty)
                            (\r      -> Set.singleton (Some r))
                            (\_ regs -> regs)


-- Figure out the deps of the given registers and update the state for the current label
recordPropagation :: Ord a
                  => Simple Lens (FunctionArgsState ids) (Map (BlockLabel Word64) (Map a DemandSet))
                  -> BlockLabel Word64
                  -> RegState X86Reg (Value X86_64 ids)
                  -> (forall tp . X86Reg tp -> a)
                  -> [Some X86Reg]
                  -> FunctionArgsM ids () -- Map (Some N.RegisterName) RegDeps
recordPropagation l lbl s mk rs = do
  let doReg (Some r) = do
        rs' <- valueUses (s ^. boundValue r)
        return (mk r, DemandSet rs' mempty)
  vs <- mapM doReg rs
  l %= Map.insertWith (Map.unionWith mappend) lbl (Map.fromListWith mappend vs)

-- | A block requires a value, and so we need to remember which
-- registers are required.
demandValue :: BlockLabel Word64 -> Value X86_64 ids tp -> FunctionArgsM ids ()
demandValue lbl v = do
  regs <- valueUses v
  blockDemandMap %= Map.insertWith demandMapUnion lbl
                        (Map.singleton DemandAlways (DemandSet regs mempty))

nextBlock :: FunctionArgsM ids (Maybe (BlockLabel Word64))
nextBlock = blockFrontier %%= \s -> let x = Set.maxView s in (fmap fst x, maybe s snd x)

-- -----------------------------------------------------------------------------
-- Entry point

-- | Returns the set of argument registers and result registers for each function.
functionArgs :: DiscoveryInfo X86_64 ids -> Map CodeAddr FunctionType
functionArgs ist =
  -- debug' DFunctionArgs (ppSet (text . flip showHex "") seenFuns) $
  debugPrintMap $ finalizeMap $ calculateGlobalFixpoint argDemandsMap resultDemandsMap argsMap
  where
    (argDemandsMap, resultDemandsMap, argsMap)
      = foldl doOneFunction mempty (ist ^. functionEntries)

    -- This function computes the following 3 pieces of information:
    -- 1. Initial function arguments (ignoring function calls)
    -- 2. Function arguments to function arguments
    -- 3. Function results to function arguments.
    doOneFunction acc addr =
      flip evalState initFunctionArgsState $ do
        -- Run the first phase (block summarization)
        summarizeIter ist Set.empty (Just lbl0)
        -- propagate back uses
        new <- use blockDemandMap

        -- debugM DFunctionArgs (">>>>>>>>>>>>>>>>>>>>>>>>" ++ (showHex addr "" ))
        -- debugM' DFunctionArgs (ppMap (text . show) (ppMap (text . show) (text . show)) new)
        -- debugM DFunctionArgs ("------------------------" ++ (showHex addr "" ))
        -- xfer <- use blockTransfer
        -- debugM' DFunctionArgs (ppMap (text . show) (ppMap (text . show) (text . show)) xfer)

        calculateLocalFixpoint new
        -- summary for entry block has what we want.
        -- m <- use (blockDemandMap . ix lbl0)
        -- debugM DFunctionArgs ("*************************"  ++ (showHex addr "" ))
        -- debugM' DFunctionArgs (ppMap (text . show) (text . show) m)
        -- debugM DFunctionArgs ("<<<<<<<<<<<<<<<<<<<<<<<<<" ++ (showHex addr "" ))

        funDemands <- use (blockDemandMap . ix lbl0)
        return (Map.foldlWithKey' (decomposeMap addr) acc funDemands)
      where
        lbl0 = mkRootBlockLabel addr

    -- A function may demand a callee saved register as it will store
    -- it onto the stack in order to use it later.  This will get
    -- recorded as a use, which is erroneous, so we strip out any
    -- reference to them here.
    calleeDemandSet = DemandSet { registerDemands = Set.insert (Some sp_reg)
                                                    x86CalleeSavedRegs
                                , functionResultDemands = mempty }

    decomposeMap :: CodeAddr
                 -> ( Map CodeAddr (Map (Some X86Reg) (Map CodeAddr DemandSet))
                    , Map CodeAddr (Map (Some X86Reg) DemandSet)
                    , Map CodeAddr DemandSet)
                 -> DemandType -> DemandSet
                 -> ( Map CodeAddr (Map (Some X86Reg) (Map CodeAddr DemandSet))
                    , Map CodeAddr (Map (Some X86Reg) DemandSet)
                    , Map CodeAddr DemandSet
                    )
    decomposeMap addr acc (DemandFunctionArg f r) v =
      -- FIXME: A bit of an awkward datatype ...
      acc & _1 %~ Map.insertWith (Map.unionWith (Map.unionWith mappend)) f
                        (Map.singleton (Some r) (Map.singleton addr v))
    decomposeMap addr acc (DemandFunctionResult r) v =
      acc & _2 %~ Map.insertWith (Map.unionWith mappend) addr
                        (Map.singleton (Some r) v)
    -- Strip out callee saved registers as well.
    decomposeMap addr acc DemandAlways v =
      acc & _3 %~ Map.insertWith mappend addr (v `demandSetDifference` calleeDemandSet)

    finalizeMap :: Map CodeAddr DemandSet
                -> Map CodeAddr FunctionType
    finalizeMap dm =
      let go ds = Map.unionWith Set.union (functionResultDemands ds)
          retDemands = foldr go Map.empty dm
      in fmap orderPadArgs
         $ Map.mergeWithKey (\_ ds rets -> Just (registerDemands ds, rets))
                            (fmap (\ds ->  (registerDemands ds, mempty)))
                            (fmap ((,) mempty))
                            dm retDemands

    -- drop the suffix which isn't a member of the arg set.  This
    -- allows e.g. arg0, arg2 to go to arg0, arg1, arg2.
    maximumArgPrefix :: [X86Reg tp] -> RegisterSet -> Int
    maximumArgPrefix regs rs =
      length $ dropWhile (not . (`Set.member` rs) . Some) $ reverse regs

    -- Turns a set of arguments into a prefix of x86ArgumentRegisters and friends
    orderPadArgs :: (RegisterSet, RegisterSet) -> FunctionType
    orderPadArgs (args, rets) =
      FunctionType (maximumArgPrefix x86ArgumentRegs args)
                   (maximumArgPrefix x86FloatArgumentRegs args)
                   (maximumArgPrefix x86ResultRegs rets)
                   (maximumArgPrefix x86FloatResultRegs rets)

    debugPrintMap :: Map CodeAddr FunctionType -> Map CodeAddr FunctionType
    debugPrintMap m = debug DFunctionArgs ("Arguments: \n\t" ++ (intercalate "\n\t" (Map.elems comb))) m
      where
        -- FIXME: ignores those functions we don't have names for.
        comb = Map.intersectionWith doOne (symbolNames ist) m
        doOne n ft = BSC.unpack n ++ ": " ++ show (pretty ft)

-- PERF: we can calculate the return types as we go (instead of doing
-- so at the end).
calculateGlobalFixpoint :: Map CodeAddr (Map (Some X86Reg)
                                             (Map CodeAddr DemandSet))
                        -> Map CodeAddr (Map (Some X86Reg) DemandSet)
                        -> Map CodeAddr DemandSet
                        -> Map CodeAddr DemandSet
calculateGlobalFixpoint argDemandsMap resultDemandsMap argsMap
  = go argsMap argsMap
  where
    go acc new
      | Just ((fun, newDemands), rest) <- Map.maxViewWithKey new =
          let (nexts, acc') = backPropagate acc fun newDemands
          in go acc' (Map.unionWith mappend rest nexts)
      | otherwise = acc

    backPropagate acc fun (DemandSet regs rets) =
      -- We need to push rets through the corresponding functions, and
      -- notify all functions which call fun regs.
      let goRet addr retRegs =
            mconcat [ resultDemandsMap ^. ix addr ^. ix r | r <- Set.toList retRegs ]
          retDemands = Map.mapWithKey goRet rets

          regsDemands =
            Map.unionsWith mappend [ argDemandsMap ^. ix fun ^. ix r | r <- Set.toList regs ]

          newDemands = Map.unionWith mappend regsDemands retDemands

          -- All this in newDemands but not in acc
          novelDemands = Map.differenceWith diff newDemands acc
      in (novelDemands, Map.unionWith mappend acc novelDemands )

    diff ds1 ds2 =
        let ds' = ds1 `demandSetDifference` ds2 in
        if ds' == mempty then Nothing else Just ds'

transferDemands :: Map (Some X86Reg) DemandSet
                   -> DemandSet -> DemandSet
transferDemands xfer (DemandSet regs funs) =
  -- Using ix here means we ignore any registers we don't know about,
  -- e.g. caller-saved registers after a function call.
  -- FIXME: is this the correct behavior?
  mconcat (DemandSet mempty funs : [ xfer ^. ix r | r <- Set.toList regs ])

calculateLocalFixpoint :: Map (BlockLabel Word64) DemandMap -> FunctionArgsM ids ()
calculateLocalFixpoint new
  | Just ((currLbl, newDemands), rest) <- Map.maxViewWithKey new =
      -- propagate backwards any new demands to the predecessors
      do preds <- use (blockPreds . ix (rootBlockLabel currLbl))
         nexts <- filter (not . Map.null . snd) <$> mapM (doOne newDemands) preds
         calculateLocalFixpoint (Map.unionWith demandMapUnion rest
                                 (Map.fromListWith demandMapUnion nexts))
  | otherwise = return ()
  where
    doOne :: DemandMap
          -> BlockLabel Word64
          -> FunctionArgsM ids (BlockLabel Word64, DemandMap)
    doOne newDemands predLbl = do
      xfer   <- use (blockTransfer . ix predLbl)

      let demands' = transferDemands xfer <$> newDemands
          lbl' = rootBlockLabel predLbl

      -- update uses, returning value before this iteration
      seenDemands <- use (blockDemandMap . ix lbl')
      blockDemandMap . at lbl' .= Just (Map.unionWith mappend demands' seenDemands)
      -- seenDemands <- blockDemandMap . ix lbl' <<%= demandMapUnion demands'

      return (lbl', Map.differenceWith diff demands' seenDemands)

    diff ds1 ds2 =
        let ds' = ds1 `demandSetDifference` ds2 in
        if ds' == mempty then Nothing else Just ds'

-- | Explore states until we have reached end of frontier.
summarizeIter :: DiscoveryInfo X86_64 ids
              -> Set (BlockLabel Word64)
              -> Maybe (BlockLabel Word64)
              -> FunctionArgsM ids ()
summarizeIter _   _     Nothing = return ()
summarizeIter ist seen (Just lbl)
  | lbl `Set.member` seen = nextBlock >>= summarizeIter ist seen
  | otherwise = do summarizeBlock ist lbl
                   lbl' <- nextBlock
                   summarizeIter ist (Set.insert lbl seen) lbl'

-- A function call is the only block type that results in the
-- generation of function call demands, so we split that aspect out
-- (callee saved are handled in summarizeBlock).
summarizeCall :: BlockLabel Word64
              -> RegState X86Reg (Value X86_64 ids)
              -> Either CodeAddr (BVValue X86_64 ids 64)
              -> Bool
              -> FunctionArgsM ids ()
summarizeCall lbl proc_state (Left faddr) isTailCall = do
  -- If a subsequent block demands r, then we note that we want r from
  -- function faddr
  -- FIXME: refactor out Some s
  let retRegs = ((Some <$> x86ResultRegs) ++ (Some <$> x86FloatResultRegs))
  if isTailCall
     -- tail call, propagate demands for our return regs to the called function
     then let propMap = map (\(Some r) -> (DemandFunctionResult r, demandSet (Some r))) retRegs
          in  blockDemandMap %= Map.insertWith (Map.unionWith mappend) lbl (Map.fromList propMap)
     else  traverse_ propResult retRegs


  -- If a function wants argument register r, then we note that this
  -- block needs the corresponding state values.  Note that we could
  -- do this for _all_ registers, but this should make the summaries somewhat smaller.
  propArgument [Some rax_reg] -- special var args register.
  propArgument (Some <$> x86ArgumentRegs)
  propArgument (Some <$> x86FloatArgumentRegs)
  where
    -- singleton for now, but propagating back will introduce more deps.
    demandSet sr         = DemandSet mempty (Map.singleton faddr (Set.singleton sr))
    propResult :: Some X86Reg -> FunctionArgsM ids ()
    propResult sr =
      blockTransfer %= Map.insertWith (Map.unionWith mappend) lbl
                                      (Map.singleton sr (demandSet sr))

    -- FIXME: clag from recordPropagation
    propArgument rs = recordPropagation blockDemandMap lbl proc_state (DemandFunctionArg faddr) rs

-- In the dynamic case, we just assume all arguments (FIXME: results?)
summarizeCall lbl proc_state (Right _dynaddr) _isTailCall = do
  demandRegisters [Some ip_reg]
  demandRegisters (Some <$> x86ArgumentRegs)
  demandRegisters (Some <$> x86FloatArgumentRegs) -- FIXME: required?
  where
    demandRegisters = recordPropagation blockDemandMap lbl proc_state (const DemandAlways)

-- | This function figures out what the block requires
-- (i.e., addresses that are stored to, and the value stored), along
-- with a map of how demands by successor blocks map back to
-- assignments and registers.
summarizeBlock :: DiscoveryInfo X86_64 ids
               -> BlockLabel Word64
               -> FunctionArgsM ids ()
summarizeBlock (interp_state :: DiscoveryInfo X86_64 ids) root_label = go root_label
  where
    go :: BlockLabel Word64 -> FunctionArgsM ids ()
    go lbl = do
      -- By default we have no arguments, return nothing
      blockDemandMap %= Map.insertWith demandMapUnion lbl mempty

      Just (b, m_pterm) <- return $ getClassifyBlock lbl interp_state

      let goStmt (WriteMem addr v) = do
            demandValue lbl addr
            demandValue lbl v

          goStmt (ExecArchStmt (MemCopy _sz cnt src dest rev)) = do
            demandValue lbl cnt
            demandValue lbl src
            demandValue lbl dest
            demandValue lbl rev

          goStmt (ExecArchStmt (MemSet cnt v ptr df)) = do
            demandValue lbl cnt
            demandValue lbl v
            demandValue lbl ptr
            demandValue lbl df

          goStmt _ = return ()

          -- FIXME: rsp here?
          recordCallPropagation proc_state =
            recordPropagation blockTransfer lbl proc_state Some
                              (Some sp_reg : (Set.toList x86CalleeSavedRegs))

      case m_pterm of
        Just (ParsedBranch c x y) -> do
          traverse_ goStmt (blockStmts b)
          demandValue lbl c
          go x
          go y

        Just (ParsedCall proc_state stmts' fn m_ret_addr) -> do
          traverse_ goStmt stmts'

          summarizeCall lbl proc_state fn (not $ isJust m_ret_addr)

          case m_ret_addr of
            Nothing       -> return ()
            Just ret_addr -> do addEdge lbl (mkRootBlockLabel ret_addr)
                                recordCallPropagation proc_state

        Just (ParsedJump proc_state tgt_addr) -> do
          traverse_ goStmt (blockStmts b)
          -- record all propagations
          recordPropagation blockTransfer lbl proc_state Some x86StateRegs
          addEdge lbl (mkRootBlockLabel tgt_addr)

        Just (ParsedReturn proc_state stmts') -> do
          traverse_ goStmt stmts'
          recordPropagation blockDemandMap lbl proc_state DemandFunctionResult $
            fmap Some x86ResultRegs ++ fmap Some x86FloatResultRegs

        Just (ParsedSyscall proc_state next_addr _call_no _pname _name argRegs _retRegs) -> do
            -- FIXME: we ignore the return type for now, probably not a problem.
            traverse_ goStmt (blockStmts b)

            recordPropagation blockDemandMap lbl proc_state (const DemandAlways)
               (Some <$> argRegs)

            recordCallPropagation proc_state
            addEdge lbl (mkRootBlockLabel next_addr)

        Just (ParsedLookupTable proc_state idx vec) -> do
            traverse_ goStmt (blockStmts b)

            demandValue lbl idx

            -- record all propagations
            recordPropagation blockTransfer lbl proc_state Some x86StateRegs
            traverse_ (addEdge lbl . mkRootBlockLabel) vec

        Nothing -> debugM DFunctionArgs ("WARNING: No parsed block type at " ++ show lbl) >> return ()

-- -----------------------------------------------------------------------------
-- debug
