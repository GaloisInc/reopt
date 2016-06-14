
module Reopt.Machine.SysDeps.Types where

import qualified Data.Map as Map
import           Data.Parameterized.Some
import           Data.Word

import           Data.Macaw.CFG (ArchReg)

data SyscallArgType = VoidArgType | WordArgType | XMMFloatType
                    deriving (Eq, Show, Read)

type SyscallTypeInfo = (String, SyscallArgType, [SyscallArgType])

data SyscallPersonality arch =
  SyscallPersonality { spName     :: String
                     , spTypeInfo :: Map.Map Word64 SyscallTypeInfo
                     , spResultRegisters :: [Some (ArchReg arch)]
                     }
