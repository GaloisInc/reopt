
module Reopt.Machine.SysDeps.Linux
       ( syscallPersonality
       ) where

import           Data.Parameterized.Some

import           Reopt.Machine.SysDeps.LinuxGenerated (syscallInfo)
import           Reopt.Machine.SysDeps.Types
import           Reopt.Machine.X86State

syscallPersonality :: SyscallPersonality X86_64
syscallPersonality =
  SyscallPersonality { spName = "Linux"
                     , spTypeInfo = syscallInfo
                     , spResultRegisters = [Some rax_reg]
                     }
