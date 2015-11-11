{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Reopt.CFG.StackArgs
  ( maximumStackArg
  ) where

import           Control.Lens
import           Control.Monad (join)
import           Control.Monad.Error
import           Control.Monad.State.Strict
import           Data.Foldable as Fold (toList, traverse_)
import           Data.Int
import           Data.Int (Int64)
import           Data.List (elemIndex, elem)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Type.Equality
import           Data.Word
import           Numeric (showHex)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Data.Set (Set)
import qualified Data.Set as Set

import           Data.Maybe (catMaybes)

import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF

import           Reopt.Analysis.AbsState
import           Reopt.CFG.InterpState
import           Reopt.CFG.Representation
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types
import           Reopt.Object.Memory


import           Debug.Trace

type StackArgs a = State (Int64, Set BlockLabel) a

addBlock :: BlockLabel -> StackArgs ()
addBlock lbl = _2 %= Set.insert lbl 

nextBlock :: StackArgs (Maybe BlockLabel)
nextBlock = _2 %%= \s -> let x = Set.maxView s in (fmap fst x, maybe s snd x)

addOffset :: Int64 -> StackArgs ()
addOffset v = _1 %= max v

-- -----------------------------------------------------------------------------
isWriteTo :: Stmt -> Value (BVType 64) -> TypeRepr tp -> Maybe (Value tp)
isWriteTo (Write (MemLoc a _) val) expected tp
  | Just _ <- testEquality a expected
  , Just Refl <- testEquality (valueType val) tp =
    Just val
isWriteTo _ _ _ = Nothing


isCodeAddrWriteTo :: Memory Word64 -> Stmt -> Value (BVType 64) -> Maybe Word64
isCodeAddrWriteTo mem s sp
  | Just (BVValue _ val) <- isWriteTo s sp (knownType :: TypeRepr (BVType 64))
  , isCodeAddr mem (fromInteger val)
  = Just (fromInteger val)
isCodeAddrWriteTo _ _ _ = Nothing

identifyCall :: Memory Word64
             -> [Stmt]
             -> X86State Value
             -> Maybe (Seq Stmt, Word64, [Some Value])
identifyCall mem stmts0 s = go (Seq.fromList stmts0)
  where next_sp = s^.register N.rsp
        go stmts =
          case Seq.viewr stmts of
            Seq.EmptyR -> Nothing
            prev Seq.:> stmt
              | Just ret <- isCodeAddrWriteTo mem stmt next_sp ->
                Just (prev, ret, [])
              | Write{} <- stmt -> Nothing
              | otherwise -> go prev

-- | This is designed to detect returns from the X86 representation.
-- It pattern matches on a X86State to detect if it read its instruction
-- pointer from an address that is 8 below the stack pointer.
identifyReturn :: X86State Value -> Maybe (Assignment (BVType 64))
identifyReturn s = do
  let next_ip = s^.register N.rip
      next_sp = s^.register N.rsp
  case next_ip of
    AssignedValue assign@(Assignment _ (Read (MemLoc ip_addr _)))
      | let (ip_base, ip_off) = asBaseOffset ip_addr
      , let (sp_base, sp_off) = asBaseOffset next_sp
      , (ip_base, ip_off + 8) == (sp_base, sp_off) -> Just assign
    _ -> Nothing

-- | Returns the maximum stack argument used by the function, that is,
-- the highest index above sp0 that is read or written.
maximumStackArg :: MapF Assignment AbsValue
                   -> InterpState -> CodeAddr -> Int64
maximumStackArg amap ist addr =
  fst $ execState (recoverIter amap aregs ist Set.empty (Just $ GeneratedBlock addr 0))
                  (0, Set.empty)
  where
    aregs = (lookupAbsBlock addr (ist ^. absState)) ^. absX86State

-- | Explore states until we have reached end of frontier.
recoverIter :: MapF Assignment AbsValue
               -> X86State AbsValue
               -> InterpState
               -> Set BlockLabel
               -> Maybe BlockLabel
               -> StackArgs ()
recoverIter _     _    _   _ Nothing = return ()
recoverIter amap aregs ist seen (Just lbl)
  | lbl `Set.member` seen = nextBlock >>= recoverIter amap aregs ist seen
  | otherwise = do recoverBlock amap aregs ist lbl
                   lbl' <- nextBlock
                   recoverIter amap aregs ist (Set.insert lbl seen) lbl'

recoverBlock :: MapF Assignment AbsValue
                -> X86State AbsValue
                -> InterpState 
                -> BlockLabel
                -> StackArgs ()
recoverBlock amap aregs interp_state lbl = do
  Just b <- return $ lookupBlock (interp_state ^. blocks) lbl

  let xfer = transferValue' (memory interp_state) amap aregs
      go = map goStmt . blockStmts
      goStmt (AssignStmt (Assignment _ (Read (MemLoc addr _))))
        | StackOffset _ s <- xfer addr = Just $ Set.findMax s
      goStmt _ = Nothing

  case catMaybes (go b) of
    []  -> return ()
    xs  -> addOffset (maximum xs)

  case blockTerm b of
    Branch c x y -> do
      addBlock x
      addBlock y

    FetchAndExecute proc_state
        -- The last statement was a call.
      | Just (_, ret_addr, _) <- identifyCall (memory interp_state) (blockStmts b) proc_state -> do

        let lbl' = GeneratedBlock ret_addr 0
        addBlock lbl'

      -- Jump to concrete offset.
      | BVValue _ (fromInteger -> tgt_addr) <- proc_state^.register N.rip
        -- Check that we are in the same function
      , inSameFunction (labelAddr lbl) tgt_addr interp_state -> do

        let lbl' = GeneratedBlock tgt_addr 0
        addBlock lbl'

       -- Return
      | Just assign <- identifyReturn proc_state -> return ()

    _ -> return () -- ???


