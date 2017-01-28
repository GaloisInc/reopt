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
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import           Data.Parameterized.Map (MapF)
import           Data.Set (Set)
import qualified Data.Set as Set

import           Data.Macaw.AbsDomain.AbsState
import           Data.Macaw.CFG
import           Data.Macaw.Discovery.Info
import           Data.Macaw.Memory
import qualified Data.Macaw.Memory.Permissions as Perm
import           Data.Macaw.Types (n64)

import           Reopt.Machine.X86State

type StackArgs a = State (Int64, Set (BlockLabel 64)) a

addBlock :: BlockLabel 64 -> StackArgs ()
addBlock lbl = _2 %= Set.insert lbl

nextBlock :: StackArgs (Maybe (BlockLabel 64))
nextBlock = _2 %%= \s -> let x = Set.maxView s in (fmap fst x, maybe s snd x)

addOffset :: Int64 -> StackArgs ()
addOffset v = _1 %= max v

-- | Returns the maximum stack argument used by the function, that is,
-- the highest index above sp0 that is read or written.
maximumStackArg :: MapF (AssignId ids) (AbsValue 64)
                -> DiscoveryInfo X86_64 ids
                -> SegmentedAddr 64
                -> Int64
maximumStackArg amap ist addr =
  fst $ execState (recoverIter amap aregs ist Set.empty (Just $ GeneratedBlock addr 0))
                  (0, Set.empty)
  where
    aregs =
      case Map.lookup addr (ist ^. codeInfoMap) of
        Just s -> s^.addrAbsBlockState^.absRegState
        Nothing -> error "maximumStackArg could not find value."

-- | Explore states until we have reached end of frontier.
recoverIter :: MapF (AssignId ids) (AbsValue 64)
               -> RegState X86Reg (AbsValue 64)
               -> DiscoveryInfo X86_64 ids
               -> Set (BlockLabel 64)
               -> Maybe (BlockLabel 64)
               -> StackArgs ()
recoverIter _     _    _   _ Nothing = return ()
recoverIter amap aregs ist seen (Just lbl)
  | lbl `Set.member` seen = nextBlock >>= recoverIter amap aregs ist seen
  | otherwise = do recoverBlock amap aregs ist lbl
                   lbl' <- nextBlock
                   recoverIter amap aregs ist (Set.insert lbl seen) lbl'

recoverBlock :: MapF (AssignId ids) (AbsValue 64)
                -> RegState X86Reg (AbsValue 64)
                -> DiscoveryInfo X86_64 ids
                -> BlockLabel 64
                -> StackArgs ()
recoverBlock amap aregs interp_state lbl = do
  Just b <- return $ lookupBlock (interp_state ^. blocks) lbl
  let mem = memory interp_state

  let is_code addr = do
        sa <- absoluteAddrSegment mem addr
        if segmentFlags (addrSegment sa) `Perm.hasPerm` Perm.execute then
          Just $! sa
         else
          Nothing

  let xfer = transferValue' n64 is_code amap aregs
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
      | Just tgt_addr <- asLiteralAddr mem (proc_state^.boundValue ip_reg)
        -- Check that we are in the same function
      , inSameFunction (labelAddr lbl) tgt_addr interp_state -> do

        let lbl' = GeneratedBlock tgt_addr 0
        addBlock lbl'

       -- Return
      | Just _assign <- identifyReturn proc_state x86StackDelta -> return ()

    _ -> return () -- ???
