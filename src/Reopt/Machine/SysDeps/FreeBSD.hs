
module Reopt.Machine.SysDeps.FreeBSD
       ( syscallTypeInfo
       , SyscallArgType (..)
       , module Reopt.Machine.SysDeps.Types
       ) where

import qualified Data.Map as Map
import           Data.Word

import           Reopt.Machine.SysDeps.FreeBSDGenerated (syscallInfo)
import           Reopt.Machine.SysDeps.Types

-- We might want to export more later, hence we export this function
-- instead of just the map
syscallTypeInfo :: Word64 -> Maybe SyscallTypeInfo
syscallTypeInfo k = Map.lookup k syscallInfo

