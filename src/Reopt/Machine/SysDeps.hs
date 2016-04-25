
module Reopt.Machine.SysDeps
       ( sysDeps
       , module Reopt.Machine.SysDeps.Types
       ) where

import qualified Data.Map as Map

import           Reopt.Machine.SysDeps.FreeBSD as FreeBSD
import           Reopt.Machine.SysDeps.Linux as Linux
import           Reopt.Machine.SysDeps.Types

sysDeps :: Map.Map String SyscallPersonality
sysDeps = Map.fromList [ (spName sp, sp)
                       | sp <- [ Linux.syscallPersonality
                               , FreeBSD.syscallPersonality
                               ]
                       ]
