{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# LANGUAGE TypeFamilies #-}

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
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Map(Map)
import qualified Data.Set as S
import           Data.Set(Set)
import           Flexdis86 as F

data Next =
    Absolute Word64
  | NIndirect (N.RegisterName 'N.GP)
  | NFallthrough Word64
  | NRet
  deriving Show

instance Eq Next where
  Absolute n == Absolute m = n == m
  NIndirect (N.GPReg n) == NIndirect (N.GPReg m) = n == m
  NFallthrough n == NFallthrough m = n == m
  NRet == NRet = True


extractBlock :: (F.ByteReader r) => Word64 -> Set Word64 -> r (Either Word64 ([Next], Maybe Word64, [CS.Stmt]))
extractBlock startAddr breakAddrs =
  extractBlockInner startAddr breakAddrs []
extractBlockInner startAddr breakAddrs prevStmts =
  do (ii, w) <- runStateT (unWrappedByteReader $ disassembleInstruction defaultX64Disassembler) 0
     case execInstruction (startAddr + w) ii
       of Just m | stmts <- execSemantics m ->
            case nexts M.empty [Absolute startAddr] Nothing stmts
              of ([Absolute addr], ret)
                   | addr == startAddr + w && S.member addr breakAddrs ->
                        return $ Right ([NFallthrough addr], Nothing, prevStmts ++ stmts)
                   | addr == startAddr + w ->
                       extractBlockInner addr breakAddrs $ prevStmts ++ stmts
                   | otherwise ->
                       return $ Right ([Absolute addr], ret, prevStmts ++ stmts)
                 ([], _) -> return $ Left startAddr
                 (addrs, _) -> return$ Right (addrs, Nothing, prevStmts ++ stmts)
          Nothing -> return $ Left startAddr

newtype WrappedByteReader r a = WrappedByteReader 
  {unWrappedByteReader :: StateT Word64 r a} deriving (Functor, MonadTrans, Applicative, Monad)

instance ByteReader r => ByteReader (WrappedByteReader r) where
  readByte = WrappedByteReader {unWrappedByteReader = do
    count <- get
    put (count + 1)
    lift readByte}


nexts :: Map (Variable (S.BVType 64)) [Next] -> [Next] -> Maybe Word64 ->
  [CS.Stmt] -> ([Next], Maybe Word64)
nexts _ addrs ret [] = (addrs, ret)
nexts vars addrs ret (((S.Register rv) := expr) : rest)
  | Just (N.IPReg, Refl, Refl) <- S.registerViewAsFullRegister rv
  = nexts vars (staticExpr vars expr) ret rest
nexts vars addrs ret ((Ifte_ expr as bs) : rest) =
  let (aAddrs, _) = nexts vars addrs ret as
      (bAddrs, _) = nexts vars addrs ret bs
  in nexts vars (L.union aAddrs bAddrs) ret rest
nexts vars addrs ret ((Get v (S.Register rv)) : rest)
  | Just (N.IPReg, Refl, Refl) <- S.registerViewAsFullRegister rv
  = nexts (M.insert v addrs vars) addrs ret rest
nexts vars addrs ret ((Get v (S.Register rv)) : rest)
  | Just (N.GPReg i, Refl, Refl) <- S.registerViewAsFullRegister rv
  = nexts (M.insert v [NIndirect (N.GPReg i)] vars) addrs ret rest
nexts vars addrs ret ((Get v (S.MemoryAddr (VarExpr _ ) (S.BVTypeRepr n))) : rest)
  | Just Refl <- testEquality n S.n64 = 
      nexts (M.insert v [NRet] vars) addrs ret rest
nexts vars addrs ret ((S.MemoryAddr _ (S.BVTypeRepr n) := expr) : rest)
  | Just Refl <- testEquality n S.n64
  , [Absolute addr] <- staticExpr vars expr = 
      nexts vars addrs (Just addr) rest
nexts vars addrs ret (_ : rest) = nexts vars addrs ret rest

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
