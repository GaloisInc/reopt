
module Reopt.Machine.SysDeps
       ( sysDeps
       , module Reopt.Machine.SysDeps.Types
       ) where

import qualified Data.Map as Map

import           Reopt.Machine.SysDeps.FreeBSD as FreeBSD
import           Reopt.Machine.SysDeps.Linux as Linux
import           Reopt.Machine.SysDeps.Types
import           Reopt.Machine.X86State (X86_64)

sysDeps :: Map.Map String (SyscallPersonality X86_64)
sysDeps = Map.fromList [ (spName sp, sp)
                       | sp <- [ Linux.syscallPersonality
                               , FreeBSD.syscallPersonality
                               ]
                       ]
