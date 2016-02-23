
module Reopt.Machine.SysDeps.Linux
       ( syscallPersonality
       ) where

import           Data.Parameterized.Some

import           Reopt.Machine.SysDeps.LinuxGenerated (syscallInfo)
import           Reopt.Machine.SysDeps.Types
import qualified Reopt.Machine.StateNames as N

syscallPersonality :: SyscallPersonality
syscallPersonality =
  SyscallPersonality { spName = "Linux"
                     , spTypeInfo = syscallInfo
                     , spResultRegisters = [Some N.rax]
                     }
  
