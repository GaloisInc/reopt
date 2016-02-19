
module Reopt.Machine.SysDeps.FreeBSD
       ( syscallPersonality
       ) where

import           Data.Parameterized.Some

import           Reopt.Machine.SysDeps.FreeBSDGenerated (syscallInfo)
import           Reopt.Machine.SysDeps.Types
import qualified Reopt.Machine.StateNames as N

syscallPersonality :: SyscallPersonality
syscallPersonality =
  SyscallPersonality { spName = "FreeBSD"
                     , spTypeInfo = syscallInfo
                     , spResultRegisters = [ Some N.rax, Some N.cf ]
                     }

