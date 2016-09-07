
module Reopt.Machine.SysDeps.Linux
       ( syscallPersonality
       ) where

import           Data.Parameterized.Some

import           Reopt.Machine.SysDeps.LinuxGenerated (syscallInfo)
import           Data.Macaw.Architecture.Syscall
import           Reopt.Machine.X86State

syscallPersonality :: SyscallPersonality X86_64
syscallPersonality =
  SyscallPersonality { spName = "Linux"
                     , spTypeInfo = syscallInfo
                     , spResultRegisters = [Some rax_reg]
                     }
