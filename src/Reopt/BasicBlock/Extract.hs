{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}

module Reopt.BasicBlock.Extract (extractBlock, Next(..))
where
import           Control.Monad.State
import           Control.Applicative
import           Reopt.Concrete.Semantics as CS
import           Reopt.CFG.Representation as R
import qualified Reopt.Semantics.Monad as S
import           Reopt.Semantics.FlexdisMatcher
import qualified Reopt.Machine.StateNames as N
import           Data.Word
import           Data.Parameterized.NatRepr
import qualified Data.Map as M
import qualified Data.List as L
import           Data.Map(Map)
import           Flexdis86 as F

data Next =
    Absolute Word64
  | Indirect (N.RegisterName 'N.GP)
  | Ret
  deriving Show

instance Eq Next where
  Absolute n == Absolute m = n == m
  Indirect (N.GPReg n) == Indirect (N.GPReg m) = n == m
  Ret == Ret = True


extractBlock :: (F.ByteReader r) => Word64 -> r (Either Word64 ([Next], [CS.Stmt]))
extractBlock startAddr =
  extractBlockInner startAddr []
extractBlockInner startAddr prevStmts =
  do (ii, w) <- runStateT (unWrappedByteReader $ disassembleInstruction defaultX64Disassembler) 0
     case execInstruction (startAddr + w) ii
       of Just m | stmts <- execSemantics m ->
            case nexts M.empty [Absolute startAddr] stmts
              of [Absolute addr] -> 
                   if addr == startAddr + w 
                     then extractBlockInner addr $ prevStmts ++ stmts
                     else return $ Right ([Absolute addr], prevStmts ++ stmts)
                 [] -> return $ Left startAddr
                 addrs -> return$ Right (addrs, prevStmts ++ stmts)
          Nothing -> return $ Left startAddr

newtype WrappedByteReader r a = WrappedByteReader 
  {unWrappedByteReader :: StateT Word64 r a} deriving (Functor, MonadTrans, Applicative, Monad)

instance ByteReader r => ByteReader (WrappedByteReader r) where
  readByte = WrappedByteReader {unWrappedByteReader = do
    count <- get
    put (count + 1)
    lift readByte}


nexts :: Map (Variable (S.BVType 64)) [Next] -> [Next] -> [CS.Stmt] ->  [Next]
nexts _ addrs [] = addrs
nexts vars addrs (((S.Register N.IPReg) := expr) : rest) =
  nexts vars (staticExpr vars expr) rest
nexts vars addrs ((Ifte_ expr as bs) : rest) =
  nexts vars (L.union (nexts vars addrs as) (nexts vars addrs bs)) rest
nexts vars addrs ((Get v (S.Register N.IPReg)) : rest) =
  nexts (M.insert v addrs vars) addrs rest
nexts vars addrs ((Get v (S.Register (N.GPReg i))) : rest) =
  nexts (M.insert v [Indirect (N.GPReg i)] vars) addrs rest
nexts vars addrs ((Get v (S.MemoryAddr (VarExpr _ ) (S.BVTypeRepr n))) : rest)
  | Just Refl <- testEquality n S.n64 = nexts (M.insert v [Ret] vars) addrs rest
nexts vars addrs (_ : rest) = nexts vars addrs rest

staticExpr :: Map (Variable (S.BVType 64)) [Next] -> Expr (S.BVType 64) -> [Next]
staticExpr _ (LitExpr nr i) = [Absolute $ fromIntegral i]
staticExpr vars (AppExpr (BVAdd _ a b)) = 
  let as = toRaw [] $ staticExpr vars a
      bs = toRaw [] $ staticExpr vars b
  in foldl (\ res a' -> (map (Absolute .(+a')) bs) ++ res) [] as
  where
   toRaw acc [] = acc
   toRaw acc (Absolute n : l) = toRaw (n : acc) l
   toRaw acc _ = []
staticExpr vars (VarExpr v) = 
  case M.lookup v vars of
    Just l -> l
    Nothing -> []
staticExpr _ _ = []
-- this could be a bunch more aggressive but this is enough to handle non-jump
-- instructions and direct jumps
