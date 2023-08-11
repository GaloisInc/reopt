{-# LANGUAGE CPP #-}

module Reopt.ELFArchInfo (
  getElfArchInfo,
  InitDiscM,
  ProcessPLTEntries,
  processX86PLTEntries,
  SomeArchitectureInfo (SomeArch),
  warnABIUntested,
) where

import Data.Map.Strict qualified as Map

import Data.ElfEdit qualified as Elf
import Data.Macaw.X86 qualified as X86
import Data.Maybe (isNothing)
import Text.Printf (printf)
import Numeric (showHex)
import Reopt.X86 (x86OSForABI)

import Data.Vector qualified as V
import Data.ByteString qualified as BS

import Data.Macaw.Architecture.Info (ArchitectureInfo (..))
import Data.Macaw.CFG qualified as Macaw
import Data.Macaw.Utils.IncComp ( incCompLog, IncCompM)
import Reopt.PLTParser as Reopt (
  PLTInfo (..),
  extractPLTEntries,
 )

#ifdef SUPPORT_ARM
import Data.Macaw.VEX.AArch32 (armArch32le)
import Data.Macaw.VEX.AArch64 (armArch64le)
import Data.VEX.FFI qualified
#endif

-- | Computation that logs string messages.
type InitDiscM = IncCompM String

type ProcessPLTEntries w =
  forall r.
  Elf.ElfHeaderInfo w ->
  V.Vector (Elf.Shdr BS.ByteString (Elf.ElfWordType w)) ->
  InitDiscM r (Maybe (PLTInfo w))

data SomeArchitectureInfo w where
  SomeArch ::
    Macaw.ArchConstraints arch =>
    !(ArchitectureInfo arch) ->
    !(ProcessPLTEntries (Macaw.ArchAddrWidth arch)) ->
    SomeArchitectureInfo (Macaw.ArchAddrWidth arch)

processX86PLTEntries ::
  Elf.ElfHeaderInfo 64 ->
  V.Vector (Elf.Shdr BS.ByteString (Elf.ElfWordType 64)) ->
  InitDiscM r (Maybe (PLTInfo 64))
processX86PLTEntries hdrInfo shdrs = do
  case extractPLTEntries hdrInfo shdrs of
    Left err -> do
      incCompLog err
      pure Nothing
    Right m ->
      pure m

-- | Warning message when encountering untested abi.
warnABIUntested :: Elf.ElfOSABI -> String
warnABIUntested abi = printf "Warning: Support for %s binaries is untested." (show abi)

getElfArchInfo ::
  Elf.ElfClass w ->
  Elf.ElfMachine ->
  Elf.ElfOSABI ->
  IO (Either String ([String], SomeArchitectureInfo w))
getElfArchInfo cl arch abi =
  case (cl, arch) of
    (Elf.ELFCLASS64, Elf.EM_X86_64) -> do
      -- Test if ABI is known.
      let warnings
            | isNothing (x86OSForABI abi) = [warnABIUntested abi]
            | otherwise = []
      pure $ Right (warnings, SomeArch X86.x86_64_linux_info processX86PLTEntries)
#ifdef SUPPORT_ARM
    (Elf.ELFCLASS32, Elf.EM_ARM) -> do
      let warnings
            | abi /= ELFOSABI_SYSV = [warnABIUntested abi]
            | otherwise = []
      Data.VEX.FFI.init Data.VEX.FFI.stdOptions
      pure $ Right $ (warnings, SomeArch armArch32le ignorePLTEntries)
    (Elf.ELFCLASS64, Elf.EM_AARCH64) -> do
      let warnings
            | abi /= ELFOSABI_SYSV = [warnABIUntested abi]
            | otherwise = []
      Data.VEX.FFI.init Data.VEX.FFI.stdOptions
      pure $ Right $ (warnings, SomeArch armArch64le ignorePLTEntries)
#endif
    _ -> do
      let archName = case Map.lookup arch Elf.elfMachineNameMap of
            Just nm -> nm
            Nothing -> "unknown-abi(" ++ showHex (Elf.fromElfMachine arch) ")"
      pure $
        Left $
          printf
            "Do not support %d-bit %s %s binaries."
            (Elf.elfClassBitWidth cl)
            archName
            (show abi)

#ifdef SUPPORT_ARM
ignorePLTEntries :: ProcessPLTEntries w
ignorePLTEntries _ _ _ _ _ _ _ = pure ()
#endif
