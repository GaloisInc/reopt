{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Reopt.CFG.StackArgs
  ( maximumStackArg
  ) where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Int
import           Data.Maybe (catMaybes)
import           Data.Parameterized.Map (MapF)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word

import           Reopt.Analysis.AbsState
import           Reopt.CFG.DiscoveryInfo
import           Reopt.CFG.Representation
import           Reopt.Machine.X86State
import           Reopt.Object.Memory

type StackArgs a = State (Int64, Set (BlockLabel Word64)) a

addBlock :: BlockLabel Word64 -> StackArgs ()
addBlock lbl = _2 %= Set.insert lbl

nextBlock :: StackArgs (Maybe (BlockLabel Word64))
nextBlock = _2 %%= \s -> let x = Set.maxView s in (fmap fst x, maybe s snd x)

addOffset :: Int64 -> StackArgs ()
addOffset v = _1 %= max v

-- | Returns the maximum stack argument used by the function, that is,
-- the highest index above sp0 that is read or written.
maximumStackArg :: MapF (Assignment X86_64) AbsValue
                   -> DiscoveryInfo -> CodeAddr -> Int64
maximumStackArg amap ist addr =
  fst $ execState (recoverIter amap aregs ist Set.empty (Just $ GeneratedBlock addr 0))
                  (0, Set.empty)
  where
    aregs = (lookupAbsBlock addr (ist ^. absState)) ^. absX86State

-- | Explore states until we have reached end of frontier.
recoverIter :: MapF (Assignment X86_64) AbsValue
               -> X86State AbsValue
               -> DiscoveryInfo
               -> Set (BlockLabel Word64)
               -> Maybe (BlockLabel Word64)
               -> StackArgs ()
recoverIter _     _    _   _ Nothing = return ()
recoverIter amap aregs ist seen (Just lbl)
  | lbl `Set.member` seen = nextBlock >>= recoverIter amap aregs ist seen
  | otherwise = do recoverBlock amap aregs ist lbl
                   lbl' <- nextBlock
                   recoverIter amap aregs ist (Set.insert lbl seen) lbl'

recoverBlock :: MapF (Assignment X86_64) AbsValue
                -> X86State AbsValue
                -> DiscoveryInfo
                -> BlockLabel Word64
                -> StackArgs ()
recoverBlock amap aregs interp_state lbl = do
  Just b <- return $ lookupBlock (interp_state ^. blocks) lbl

  let xfer = transferValue' (isCodeAddrOrNull (memory interp_state)) amap aregs
      go = map goStmt . blockStmts
      goStmt (AssignStmt (Assignment _ (ReadMem addr _)))
        | StackOffset _ s <- xfer addr = Just $ Set.findMax s
      goStmt _ = Nothing

  case catMaybes (go b) of
    []  -> return ()
    xs  -> addOffset (maximum xs)

  case blockTerm b of
    Branch _c x y -> do
      addBlock x
      addBlock y

    FetchAndExecute proc_state
        -- The last statement was a call.
      | Just (_, ret_addr) <- identifyCall (memory interp_state) (blockStmts b) proc_state -> do

        let lbl' = GeneratedBlock ret_addr 0
        addBlock lbl'

      -- Jump to concrete offset.
      | BVValue _ (fromInteger -> tgt_addr) <- proc_state^.boundValue ip_reg
        -- Check that we are in the same function
      , inSameFunction (labelAddr lbl) tgt_addr interp_state -> do

        let lbl' = GeneratedBlock tgt_addr 0
        addBlock lbl'

       -- Return
      | Just _assign <- identifyReturn proc_state -> return ()

    _ -> return () -- ???
