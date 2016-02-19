
module Reopt.Machine.SysDeps.Types where

import qualified Data.Map as Map
import           Data.Parameterized.Some
import           Data.Word

import qualified Reopt.Machine.StateNames as N

data SyscallArgType = VoidArgType | WordArgType | XMMFloatType
                    deriving (Eq, Show, Read)

type SyscallTypeInfo = (String, SyscallArgType, [SyscallArgType])

data SyscallPersonality =
  SyscallPersonality { spName     :: String
                     , spTypeInfo :: Map.Map Word64 SyscallTypeInfo
                     , spResultRegisters :: [Some N.RegisterName]
                     }
                       
