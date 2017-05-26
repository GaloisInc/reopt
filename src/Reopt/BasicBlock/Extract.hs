{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Reopt.BasicBlock.Extract
  ( extractBlock
  , Next(..)
  ) where

import           Control.Monad.State
import qualified Data.List as L
import           Data.Macaw.CFG as R
import           Data.Macaw.Types (TypeBits, n0, n64)
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some
import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.Set as S
import           Data.Set (Set)
import           Data.Word

import           Flexdis86 as F

import           Reopt.Concrete.BitVector (bitVector)
import           Reopt.Concrete.MachineState (Value(..))
import           Reopt.Concrete.Semantics as CS
import qualified Reopt.Machine.StateNames as N
import qualified Reopt.Semantics.Monad as S
import           Reopt.Semantics.FlexdisMatcher

import           Debug.Trace

-- | View a 'RegisterView' as a full register.
--
-- The returned equalities help with type checking, e.g. by
-- constraining the type indices of the 'Location' in which the
-- 'RegisterView' is embedded.
registerViewAsFullRegister :: S.RegisterView tp b n
                           -> Maybe (N.RegisterName tp, b :~: 0, n :~: TypeBits tp)
registerViewAsFullRegister v
  | Just Refl <- S.registerViewBase v `testEquality` n0
  , Just Refl <- S.registerViewSize v `testEquality` N.registerWidth (S.registerViewReg v)
  , S.DefaultView <- S.registerViewType v
  = Just (S.registerViewReg v, Refl, Refl)
  | otherwise = Nothing

data Next =
    Absolute Word64
  | NIndirect
  | NFallthrough Word64
  deriving (Show, Eq)

extractBlock :: (F.ByteReader r) => Word64 -> Set Word64
             -> r (Either Word64 ([Next], Word64, [CS.Stmt]))
extractBlock startAddr breakAddrs =
  extractBlockInner startAddr breakAddrs []

extractBlockInner :: ByteReader m
                  => Word64
                  -> Set Word64
                  -> [CS.Stmt]
                  -> m (Either Word64 ([Next], Word64, [CS.Stmt]))
extractBlockInner startAddr breakAddrs prevStmts =
  do (ii, w) <- runStateT (unWrappedByteReader $ disassembleInstruction) 0
     let iaddr = ValueExpr (Literal (bitVector n64 (fromIntegral (startAddr + w))))
     case execInstruction iaddr ii
       of Just m | stmts <- execSemantics m ->
            case nexts M.empty [Absolute startAddr] stmts
              of [Absolute addr]
                   | addr == startAddr + w && S.member addr breakAddrs ->
                        return $ Right ([NFallthrough addr], startAddr + w, prevStmts ++ stmts)
                   | addr == startAddr + w ->
                       extractBlockInner addr breakAddrs $ prevStmts ++ stmts
                   | otherwise ->
                       return $ Right ([Absolute addr], startAddr + w, prevStmts ++ stmts)
                 []    -> trace "extractBlockInner: empty" $ return $ Left startAddr
                 addrs -> return $ Right (addrs, startAddr + w, prevStmts ++ stmts)
          Nothing -> return $ Left startAddr

newtype WrappedByteReader r a = WrappedByteReader
  {unWrappedByteReader :: StateT Word64 r a} deriving (Functor, MonadTrans, Applicative, Monad)

instance ByteReader r => ByteReader (WrappedByteReader r) where
  readByte = WrappedByteReader {unWrappedByteReader = do
    count <- get
    put (count + 1)
    lift readByte}


nexts :: Map (Some Variable) [Next] -> [Next] -> [CS.Stmt] -> [Next]
nexts _ addrs [] = addrs
nexts vars _addrs (((S.Register rv) := expr) : rest)
  | Just (N.IPReg, Refl, Refl) <- registerViewAsFullRegister rv
  = nexts vars (staticExpr vars expr) rest
nexts vars addrs ((Ifte_ _expr as bs) : rest) =
  let aAddrs = nexts vars addrs as
      bAddrs = nexts vars addrs bs
  in nexts vars (L.union aAddrs bAddrs) rest
nexts vars addrs ((Get v (S.Register rv)) : rest)
  | Just (N.IPReg, Refl, Refl) <- registerViewAsFullRegister rv
  = nexts (M.insert (Some v) addrs vars) addrs rest
nexts vars addrs ((Get v _) : rest) =
  nexts (M.insert (Some v) [NIndirect] vars) addrs rest
nexts vars addrs ((Let v expr) : rest) =
  nexts (M.insert (Some v) (staticExpr vars expr) vars) addrs rest
-- nexts vars addrs ret ((S.MemoryAddr _ (S.BVTypeRepr n) := expr) : rest)
--   | Just Refl <- testEquality n n64
--   , [Absolute addr] <- staticExpr vars expr =
--       nexts vars addrs (Just addr) rest
nexts vars addrs (_ : rest) = nexts vars addrs rest

staticExpr :: Map (Some Variable) [Next] -> Expr a -> [Next]
staticExpr _ (LitExpr _nr i) = [Absolute $ fromIntegral i]
staticExpr vars (AppExpr (BVAdd _ a b))
  | all isAbsolute avs && all isAbsolute bvs =
      [ Absolute (av + bv) | Absolute av <- avs, Absolute bv <- bvs ]
  where
    avs = staticExpr vars a
    bvs = staticExpr vars b
    isAbsolute (Absolute _) = True
    isAbsolute _            = False
staticExpr vars (VarExpr v)
  | Just l <- M.lookup (Some v) vars = l
staticExpr _ _ = [NIndirect] -- an overapproximation of what actually happens, but ...

-- this could be a bunch more aggressive but this is enough to handle non-jump
-- instructions and direct jumps
