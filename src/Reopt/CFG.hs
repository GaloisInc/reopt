{-# LANGUAGE GADTs #-}
module Reopt.CFG where

import Data.Map (Map)
import qualified Data.Vector as V
import Data.Word


data CFG addr = CFG { _cfgBlocks :: !(V.Vector (Block addr))
                    , _addrMap :: !(Map addr Int)
                    }

data Block addr = Block { blockOps :: V.Vector (MicroOp addr)
                        }

class IsBVType tp where
instance IsBVType Word8  where
instance IsBVType Word16 where
instance IsBVType Word32 where
instance IsBVType Word64 where


class IsRegType tp where

instance IsRegType Bool   where
instance IsRegType Word8  where
instance IsRegType Word16 where
instance IsRegType Word32 where
instance IsRegType Word64 where

data ExprF tp where
  RegVal :: Reg tp -> ExprF tp

-- | A virtual register in the Micro-op CPU
-- (which has an unbounded number of registers).
newtype Reg tp = Reg Int

-- | A micro op is a simple operation that makes at most one modification
-- to the machine state.
data MicroOp addr where
  AssignReg :: IsRegType tp => Reg tp -> ExprF tp -> MicroOp addr
  Read  :: IsBVType tp => ExprF addr -> ExprF tp -> MicroOp addr
  Write :: IsBVType tp => ExprF addr -> ExprF tp -> MicroOp addr

