{-# LANGUAGE DataKinds #-}

module Reopt.BasicBlock.FunctionCFG (CFG(..), Block(..), Term(..), findBlocks)
where
import           Reopt.BasicBlock.Extract
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Word
import           Numeric
import           Reopt.Concrete.Semantics as CS
import qualified Reopt.Machine.StateNames as N
import           Reopt.Object.Memory

type CFG = Map Word64 Block

data Block = Block [Stmt] Term

data Term = Cond Word64 Word64 -- true, false
          | Call Word64 Word64 -- call, ret
          | Direct Word64
          | Fallthrough Word64
          | Indirect (N.RegisterName 'N.GP)
          | Ret
instance Show Term where
  show (Cond w1 w2) = "Cond " ++ showHex w1 " " ++ showHex w2 "" 
  show (Call w1 w2) = "Call " ++ showHex w1 " " ++ showHex w2 "" 
  show (Direct w) = "Direct " ++ showHex w ""
  show (Fallthrough w) = "Fallthrough " ++ showHex w ""
  show (Indirect reg) = "Indirect " ++ show reg
  show Ret = "Ret"


-- FIXME: fancy data structures could do this better - keep instruction
-- beginnings around, use a range map...
findBlocks :: Memory Word64 -> Word64 -> Either String CFG
findBlocks mem entry = do
  cfg1 <- findBlocks' mem (S.empty) (S.singleton entry) M.empty
  findBlocks' mem (M.keysSet cfg1) (S.singleton entry) M.empty

findBlocks' :: Memory Word64 
            -> Set Word64 
            -> Set Word64 
            -> CFG 
            -> Either String CFG
findBlocks' mem breaks queue cfg
  | Just (entry, queue') <- S.minView queue = 
    case runMemoryByteReader pf_x mem entry $ extractBlock entry breaks of
      Left err -> Left $ show err
      Right (Left err, _) -> Left $ "Could not disassemble instruction at 0x" ++ showHex err ""
      Right (Right ([Absolute callee], Just ret, stmts), _) -> 
        if M.member ret cfg
          then findBlocks' mem breaks queue' $ 
            M.insert entry (Block stmts $ Call callee ret) cfg
          else findBlocks' mem breaks (S.insert ret queue') 
            (M.insert entry (Block stmts $ Call callee ret) cfg)
      Right (Right ([Absolute next], Nothing, stmts), _) -> 
        if M.member next cfg
          then findBlocks' mem breaks queue' $
            M.insert entry (Block stmts $ Direct next) cfg
          else findBlocks' mem breaks (S.insert next queue')
            (M.insert entry (Block stmts $ Direct next) cfg)
      Right (Right ([NFallthrough next], Nothing, stmts), _) -> 
        if M.member next cfg
          then findBlocks' mem breaks queue' $
            M.insert entry (Block stmts $ Fallthrough next) cfg
          else findBlocks' mem breaks (S.insert next queue')
            (M.insert entry (Block stmts $ Fallthrough next) cfg)
      Right (Right ([Absolute branch, Absolute fallthrough], Nothing, stmts), _) -> 
        let queue'' = if M.member branch cfg 
                        then queue'
                        else S.insert branch queue' 
            queue''' = if M.member fallthrough cfg
                        then queue''
                        else S.insert fallthrough queue''
        in findBlocks' mem breaks queue''' $ 
            M.insert entry (Block stmts $ Cond branch fallthrough) cfg
      Right (Right ([NIndirect reg], Nothing, stmts), _) -> 
        findBlocks' mem breaks queue' $ M.insert entry (Block stmts $ Indirect reg) cfg
      Right (Right ([NRet], Nothing, stmts), _) ->
        findBlocks' mem breaks queue' $ M.insert entry (Block stmts Ret) cfg

  | otherwise = Right cfg
