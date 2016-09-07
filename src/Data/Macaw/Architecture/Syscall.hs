{-|
Copyright  : (c) Galois, Inc 2016
Maintainer : jhendrix@galois.com

Data structures used for defining how system calls are interpreted.
-}
module Data.Macaw.Architecture.Syscall
  ( SyscallPersonality(..)
  , SyscallTypeInfo
  , SyscallArgType(..)
  ) where

import qualified Data.Map as Map
import           Data.Parameterized.Some
import           Data.Word

import           Data.Macaw.CFG (ArchReg)

data SyscallArgType = VoidArgType | WordArgType
  deriving (Eq, Show, Read)

-- | Information about a specific syscall.
--
-- This contains the name, the result type, and the argument types.
type SyscallTypeInfo = (String, SyscallArgType, [SyscallArgType])

data SyscallPersonality arch =
  SyscallPersonality { spName     :: String
                     , spTypeInfo :: Map.Map Word64 SyscallTypeInfo
                     , spResultRegisters :: [Some (ArchReg arch)]
                     }
