module Reopt.X86 (
  osArchitectureInfo,
  osLinkName,
  osPersonality,
  X86OS(..),
  x86OSForABI,
) where

import Data.Map.Strict qualified as Map

import Data.Macaw.Architecture.Info (ArchitectureInfo (..))
import Data.ElfEdit qualified as Elf
import Data.Macaw.X86 qualified as X86
import Data.Macaw.X86.SyscallInfo (SyscallPersonality)
import Data.Macaw.X86 (X86_64)

data X86OS = Linux | FreeBSD

instance Show X86OS where
  show Linux = "Linux"
  show FreeBSD = "FreeBSD"

osPersonality :: X86OS -> SyscallPersonality
osPersonality Linux = X86.linux_syscallPersonality
osPersonality FreeBSD = X86.freeBSD_syscallPersonality

osArchitectureInfo :: X86OS -> ArchitectureInfo X86_64
osArchitectureInfo Linux = X86.x86_64_linux_info
osArchitectureInfo FreeBSD = X86.x86_64_freeBSD_info

-- | Return the name to pass the linker for this architecture.
osLinkName :: X86OS -> String
osLinkName Linux = "x86_64-unknown-linux-gnu"
osLinkName FreeBSD = "x86_64-unknown-freebsd-elf"

-- | Maps x86 ABIs to the x86 OS value we use for it.
x86ABIMap :: Map.Map Elf.ElfOSABI X86OS
x86ABIMap =
  Map.fromList
    [ (Elf.ELFOSABI_LINUX, Linux)
    , (Elf.ELFOSABI_SYSV, Linux)
    , (Elf.ELFOSABI_FREEBSD, FreeBSD)
    , (Elf.ELFOSABI_NETBSD, FreeBSD)
    ]

-- | Return x86 OS value for Elf file with given ABI
x86OSForABI :: Elf.ElfOSABI -> Maybe X86OS
x86OSForABI abi = Map.lookup abi x86ABIMap
