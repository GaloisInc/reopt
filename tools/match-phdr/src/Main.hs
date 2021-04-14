{-|
compare-dwarfdump generate dwarfdump-output on specific parts of binary,
and reports inconsistencies.

It currently only supports .eh_frame and .debug_frame sections.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State.Strict
import qualified Data.ByteString as BS
import qualified Data.ElfEdit as Elf
import           Data.Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import           GHC.Stack
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Error (isPermissionError)
import           Text.Printf

import           Reopt.Relinker.Binary

reportError :: HasCallStack => FilePath -> String -> IO a
reportError path msg = hPutStrLn stderr (path <> ": " <> msg) >> exitFailure

data MatchStats = MatchStats
  { -- | Number of binaries visited.
    successfulBinaries :: !Int
    -- | Total binaries traversed
  , totalBinaries :: !Int
    -- | Map from segment name and section name to file.
  , unknownSectionMap :: !(Map.Map BS.ByteString FilePath)
  , unexpectedShdrInfo :: !(Map.Map BS.ByteString FilePath)
  , failedBinaries :: [(FilePath, String)]
  }

addUnexpectedSection :: FilePath -> BS.ByteString -> MatchStats -> MatchStats
addUnexpectedSection path nm stats =
  stats { unknownSectionMap = Map.insertWith (\_ o -> o) nm path (unknownSectionMap stats) }

addUnexpectedShdrInfo :: FilePath -> BS.ByteString -> MatchStats -> MatchStats
addUnexpectedShdrInfo path nm stats =
  stats { unexpectedShdrInfo = Map.insertWith (\_ o -> o) nm path (unexpectedShdrInfo stats) }

emptyMatchStats :: MatchStats
emptyMatchStats =
  MatchStats
  { successfulBinaries = 0
  , totalBinaries = 0
  , unknownSectionMap = Map.empty
  , unexpectedShdrInfo = Map.empty
  , failedBinaries = []
  }

printMatchStats :: MatchStats -> IO ()
printMatchStats s = do
  putStrLn $ "Sucessfully Analyzed " ++ show (successfulBinaries s)
             ++ " of " ++ show (totalBinaries s) ++ " executables."
  let noErrors = null (failedBinaries s)
              && Map.null (unknownSectionMap s)
              && Map.null (unexpectedShdrInfo s)
  unless noErrors $ do
    forM_ (reverse (failedBinaries s)) $ \(path, msg) -> do
      hPutStrLn stderr $ path ++ ": " ++ msg
    unless (Map.null (unknownSectionMap s)) $ do
      hPutStrLn stderr "Unknown sections"
    forM_ (Map.toList (unknownSectionMap s)) $ \(secName, path) -> do
      hPutStrLn stderr $ printf "  %s -- %s" (show secName) path
    unless (Map.null (unexpectedShdrInfo s)) $ do
      hPutStrLn stderr "Unexpected shdr info"
    forM_ (Map.toList (unexpectedShdrInfo s)) $ \(secName, path) -> do
      hPutStrLn stderr $ printf "  %s -- %s" (show secName) path
    exitFailure

knownShdrs :: Set.Set BS.ByteString
knownShdrs = Set.fromList
  [ ""
  , ".bss"
  , ".comment"
  , ".ctors"
  , ".data"
  , ".data.rel.ro"
  , ".debug_abbrev"
  , ".debug_aranges"
  , ".debug_gdb_scripts"
  , ".debug_info"
  , ".debug_line"
  , ".debug_loc"
  , ".debug_ranges"
  , ".debug_str"
  , ".digest_md5"
  , ".dtors"
  , ".dynamic"
  , ".dynstr"
  , ".dynsym"
  , ".eh_frame"
  , ".eh_frame_hdr"
  , ".fini"
  , ".fini_array"
  , ".gcc_except_table"
  , ".gnu.hash"
  , ".gnu.version"
  , ".gnu.version_r"
  , ".gnu_debuglink"
  , ".go.buildinfo"
  , ".gopclntab"
  , ".gosymtab"
  , ".got"
  , ".got.plt"
  , ".hash"
  , ".init"
  , ".init_array"
  , ".interp"
  , ".itablink"
  , ".jcr"
  , ".noptrbss"
  , ".noptrdata"
  , ".note.ABI-tag"
  , ".note.gnu.build-id"
  , ".note.gnu.gold-version"
  , ".note.gnu.property"
  , ".note.go.buildid"
  , ".note.stapsdt"
  , ".plt"
  , ".plt.got"
  , ".plt.sec"
  , ".preinit_array"
  , ".probes"
  , ".r_debug"
  , ".rela"
  , ".rela.data.rel.ro"
  , ".rela.got"
  , ".rela.dyn"
  , ".rela.plt"
  , ".rodata"
  , ".sha256_sig"
  , ".shstrtab"
  , ".sig_key"
  , ".stapsdt.base"
  , ".strtab"
  , ".symtab"
  , ".tbss"
  , ".tdata"
  , ".text"
  , ".tm_clone_table"
  , ".typelink"
  , ".upd_info"
  , ".zdebug_abbrev"
  , ".zdebug_frame"
  , ".zdebug_info"
  , ".zdebug_line"
  , ".zdebug_loc"
  , ".zdebug_pubnames"
  , ".zdebug_pubtypes"
  , ".zdebug_ranges"

  , ",bss"
  , ".rela,bss"
  , "__libc_IO_vtables"
  , "__libc_atexit"
  , "__libc_freeres_fn" -- Qemu alpha static
  , "__libc_freeres_ptrs"
  , "__libc_subfreeres"
  ]

type CheckM = State (Bool, MatchStats)

runCheckM :: CheckM () -> MatchStats -> (Bool, MatchStats)
runCheckM act stats = execState act (False, stats)

markFailed :: (MatchStats -> MatchStats) -> CheckM ()
markFailed f = modify' $  \(_,s) -> let s' = f s in seq s' (True, s')

checkShdr :: FilePath
          -> V.Vector (Elf.Shdr BS.ByteString v)
          -> Elf.Shdr BS.ByteString v
          -> CheckM ()
checkShdr path shdrs shdr = do
  let nm = Elf.shdrName shdr
  when (Set.notMember nm knownShdrs) $ do
    markFailed $ addUnexpectedSection path nm
  case Elf.shdrType shdr of
    Elf.SHT_RELA -> do
      let info = Elf.shdrInfo shdr
      if toInteger info >= toInteger (V.length shdrs) then
        markFailed $ addUnexpectedShdrInfo path nm
       else do
        let tgt = shdrs V.! fromIntegral info
        if nm == ".rela.dyn" then
          when (Elf.shdrInfo shdr /= 0) $ do
            markFailed $ addUnexpectedShdrInfo path nm
         else if nm == ".rela.plt" then
          when (Elf.shdrName tgt `notElem` [".got", ".got.plt", ".plt"]) $ do
            markFailed $ addUnexpectedShdrInfo path nm
         else do
          when (".rela" <> Elf.shdrName tgt /= nm) $ do
            markFailed $ addUnexpectedShdrInfo path nm
    -- This section seems to use info to store the number of versions needed (VERNEEDNUM)
    Elf.SHT_GNU_verneed -> pure ()
    -- Symtab are use local symbol count.
    Elf.SHT_DYNSYM -> pure ()
    Elf.SHT_SYMTAB -> pure ()
    _ -> do
      when (Elf.shdrInfo shdr /= 0) $ do
        markFailed $ addUnexpectedShdrInfo path nm

-- | @compareElf dwarfDump path@ compares the output of the Haskell
-- ehframe parsing on output with the output of @dwarfDump --eh-frame
-- path@ and fails if they are different.
compareElf :: MatchStats -> FilePath -> BS.ByteString -> IO MatchStats
compareElf stats0 path bytes = do
  Elf.SomeElf elfHdr <-
    case Elf.decodeElfHeaderInfo bytes of
      Left (_o, m) -> reportError path m
      Right r -> pure r
  let hdr = Elf.header elfHdr
  shdrs <-
    case Elf.headerNamedShdrs elfHdr of
      Left _ -> reportError path "Could not parse section headers."
      Right r -> pure r
  let shouldExplore =  Elf.headerType hdr `elem` [ Elf.ET_REL, Elf.ET_EXEC ]
                    && Elf.headerMachine hdr `elem` [Elf.EM_X86_64]
                    && Elf.phdrCount elfHdr > 0
  if not shouldExplore then do
    pure stats0
   else do
    putStrLn $ "Checking " <> path
    let (seenError, stats) =
          flip runCheckM stats0 $ do
            V.mapM_ (checkShdr path shdrs) shdrs
    case inferBinaryLayout elfHdr shdrs of
      Left msg -> do
        pure $!
          stats { failedBinaries = (path,msg) : failedBinaries stats
                , totalBinaries = totalBinaries stats + 1
                }
      Right _l ->
        pure $!
          if seenError then
            stats { totalBinaries = totalBinaries stats + 1 }
           else
            stats { successfulBinaries = successfulBinaries stats + 1
                  , totalBinaries = totalBinaries stats + 1
                  }

-- | Runs the action on each file in a directory (recursively)
foreachFile :: a -> (a -> FilePath -> IO a) -> FilePath -> IO a
foreachFile v0 act path = do
  -- Ignore sym links
  isLink <- pathIsSymbolicLink path
  if isLink then
    pure v0
   else do
    dexist <- doesDirectoryExist path
    if dexist then do
      mentries <- try $ listDirectory path
      case mentries of
        Right entries -> do
          foldlM (\v f -> foreachFile v act (path </> f)) v0 entries
        Left e
          | isPermissionError e -> pure v0
          | otherwise -> throwError  e
     else do
      fexist <- doesFileExist path
      if fexist && not isLink then
        act v0 path
       else
        pure v0

-- | This reads an argument in the file.
withElfFilesInDir :: (a -> FilePath -> BS.ByteString -> IO a)
                  -> a
                  -> FilePath
                  -> IO a
withElfFilesInDir action v0 path = do
  fexist <- doesFileExist path

  if fexist then
    action v0 path =<< BS.readFile path
   else do
    dexist <- doesDirectoryExist path
    if dexist then do
      let m v fname = do
            mbytes <- try $ BS.readFile fname
            case mbytes of
              -- Ignore files we cannot read
              Left (_e :: IOException) -> do
                pure v
              Right bytes -> do
                if Elf.elfMagic `BS.isPrefixOf` bytes then
                  action v fname bytes
                 else
                  pure v
      mentries <- try $ listDirectory path
      case mentries of
        Right entries -> do
          foldlM (\v f -> foreachFile v m (path </> f)) v0 entries
        Left e
          | isPermissionError e -> pure v0
          | otherwise -> throwError  e
     else do
      reportError path "File not found"

main :: IO ()
main = do
  paths <- getArgs
  when (null paths) $ do
    hPutStrLn stderr $ "Please specify at least one file or directory for comparing."
    exitFailure
  errs <- foldlM (withElfFilesInDir compareElf) emptyMatchStats paths
  printMatchStats errs