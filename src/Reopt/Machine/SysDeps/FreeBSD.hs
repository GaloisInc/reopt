
module Reopt.Machine.SysDeps.FreeBSD
       ( syscallPersonality
       ) where

import           Data.Parameterized.Some

import           Data.Macaw.Architecture.Syscall

import           Reopt.Machine.SysDeps.FreeBSDGenerated (syscallInfo)
import           Reopt.Machine.X86State

syscallPersonality :: SyscallPersonality X86_64
syscallPersonality =
  SyscallPersonality { spName = "FreeBSD"
                     , spTypeInfo = syscallInfo
                     , spResultRegisters = [ Some rax_reg, Some cf_reg ]
                     }
