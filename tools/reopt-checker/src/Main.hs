{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

-- |
-- reopt-checker runs various checks on a binary to ensure algorithms
-- correctly interpret the contents.
module Main (main) where

import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ElfEdit as Elf
import Data.Foldable (foldlM)
import qualified Data.Map as Map
import qualified Data.Vector as V
import Reopt.PLTParser
import Reopt.Relinker.Binary (inferBinaryLayout, infoIsShdrIndex)
import Reopt.Utils.Exit (reportErrorAndExit)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error (isPermissionError)
import Text.Printf

data MatchStats = MatchStats
  { -- | Number of binaries visited.
    successfulBinaries :: !Int,
    -- | Total binaries traversed
    totalBinaries :: !Int,
    -- Map for section headers with unexpected info values.
    unexpectedShdrInfo :: !(Map.Map BS.ByteString FilePath),
    -- Map for section headers with unexpected info values.
    unexpectedShdrLink :: !(Map.Map BS.ByteString FilePath),
    -- | Binaries missing an read-only and executable segment
    missingExecutableSegments :: ![FilePath],
    -- Binaries that had a fatal failure for some reason.
    failedBinaries :: [(FilePath, String)]
  }

incTotalBinaries :: MatchStats -> MatchStats
incTotalBinaries stats = stats {totalBinaries = totalBinaries stats + 1}

addFailedBinary :: FilePath -> String -> MatchStats -> MatchStats
addFailedBinary path msg stats = stats {failedBinaries = (path, msg) : failedBinaries stats}

addMissingExecutableSegment :: FilePath -> MatchStats -> MatchStats
addMissingExecutableSegment path stats =
  stats {missingExecutableSegments = path : missingExecutableSegments stats}

addUnexpectedShdrInfo :: FilePath -> BS.ByteString -> MatchStats -> MatchStats
addUnexpectedShdrInfo path nm stats =
  stats {unexpectedShdrInfo = Map.insertWith (\_ o -> o) nm path (unexpectedShdrInfo stats)}

addUnexpectedShdrLink :: FilePath -> BS.ByteString -> MatchStats -> MatchStats
addUnexpectedShdrLink path nm stats =
  stats {unexpectedShdrLink = Map.insertWith (\_ o -> o) nm path (unexpectedShdrLink stats)}

emptyMatchStats :: MatchStats
emptyMatchStats =
  MatchStats
    { successfulBinaries = 0,
      totalBinaries = 0,
      unexpectedShdrInfo = Map.empty,
      unexpectedShdrLink = Map.empty,
      missingExecutableSegments = [],
      failedBinaries = []
    }

printSecMap :: String -> Map.Map BS.ByteString FilePath -> IO ()
printSecMap nm m = do
  unless (Map.null m) $ do
    hPutStrLn stderr nm
  forM_ (Map.toList m) $ \(secName, path) -> do
    hPutStrLn stderr $ printf "  %s -- %s" (show secName) path

printErrorList :: String -> [FilePath] -> IO ()
printErrorList nm l = do
  unless (null l) $
    hPutStrLn stderr nm
  forM_ l $ \path -> do
    hPutStrLn stderr $ "  " ++ path

printMatchStats :: MatchStats -> IO ()
printMatchStats s = do
  putStrLn $
    printf
      "Sucessfully analyzed %s of %s executables."
      (show (successfulBinaries s))
      (show (totalBinaries s))
  let noErrors =
        null (failedBinaries s)
          && Map.null (unexpectedShdrInfo s)
          && Map.null (unexpectedShdrLink s)
          && null (missingExecutableSegments s)
  unless noErrors $ do
    forM_ (reverse (failedBinaries s)) $ \(path, msg) -> do
      hPutStrLn stderr $ path ++ ": " ++ msg
    printErrorList "Missing executable segment:" (missingExecutableSegments s)
    printSecMap "Unexpected shdr info:" (unexpectedShdrInfo s)
    printSecMap "Unexpected shdr link:" (unexpectedShdrLink s)
    exitFailure

type CheckM = ExceptT String (State (Bool, MatchStats))

runCheckM :: FilePath -> CheckM () -> MatchStats -> (Bool, MatchStats)
runCheckM path act stats =
  case runState (runExceptT act) (False, stats) of
    (Right (), s) -> s
    (Left msg, (_, stats2)) -> (True, addFailedBinary path msg stats2)

markFailed :: (MatchStats -> MatchStats) -> CheckM ()
markFailed f = modify' $ \(_, s) -> let s' = f s in seq s' (True, s')

-- | Validate assumptions about a section header
checkShdrInfo ::
  FilePath ->
  V.Vector (Elf.Shdr BS.ByteString v) ->
  Elf.Shdr BS.ByteString v ->
  CheckM ()
checkShdrInfo path shdrs shdr = do
  let nm = Elf.shdrName shdr
  case () of
    _
      | infoIsShdrIndex shdr -> do
        let info = Elf.shdrInfo shdr
        if toInteger info >= toInteger (V.length shdrs)
          then markFailed $ addUnexpectedShdrInfo path nm
          else do
            let tgt = shdrs V.! fromIntegral info
            if nm == ".rela.plt"
              then when (Elf.shdrName tgt `notElem` [".got", ".got.plt", ".plt"]) $ do
                markFailed $ addUnexpectedShdrInfo path nm
              else do
                when (".rela" <> Elf.shdrName tgt /= nm) $ do
                  markFailed $ addUnexpectedShdrInfo path nm

      -- This section seems to use info to store the number of versions needed (VERNEEDNUM)
      | Elf.shdrType shdr `elem` [Elf.SHT_GNU_verneed, Elf.SHT_GNU_verdef]
          || nm `elem` [".gnu.version_d", ".gnu.version_r"] ->
        pure ()
    -- Symtab are use local symbol count.
    _ | Elf.shdrType shdr `elem` [Elf.SHT_DYNSYM, Elf.SHT_SYMTAB]
          || Elf.shdrName shdr == ".dynsym" -> do
      pure ()
    _ -> do
      when (Elf.shdrInfo shdr /= 0) $ do
        markFailed $ addUnexpectedShdrInfo path nm

-- | Check section link matches expected
checkShdrLink ::
  FilePath ->
  Bool ->
  V.Vector (Elf.Shdr BS.ByteString v) ->
  Elf.Shdr BS.ByteString v ->
  CheckM ()
checkShdrLink path static shdrs shdr = do
  let nm = Elf.shdrName shdr
  let link = Elf.shdrLink shdr
  case () of
    _
      | Elf.shdrType shdr == Elf.SHT_RELA,
        not static -> do
        if toInteger link >= toInteger (V.length shdrs)
          then markFailed $ addUnexpectedShdrLink path nm
          else do
            let tgt = shdrs V.! fromIntegral link
            when (Elf.shdrType tgt /= Elf.SHT_DYNSYM) $ do
              markFailed $ addUnexpectedShdrLink path nm
      | not static,
        Elf.shdrType shdr `elem` [Elf.SHT_DYNAMIC, Elf.SHT_DYNSYM, Elf.SHT_SYMTAB, Elf.SHT_GNU_verneed] -> do
        if toInteger link >= toInteger (V.length shdrs)
          then markFailed $ addUnexpectedShdrLink path nm
          else do
            let tgt = shdrs V.! fromIntegral link
            when (Elf.shdrType tgt /= Elf.SHT_STRTAB) $ do
              markFailed $ addUnexpectedShdrLink path nm
    _ | not static,
        nm `elem` [".gnu.hash", ".gnu.version", ".hash", ".rela.dyn", ".rela.plt"] -> do
      if toInteger link >= toInteger (V.length shdrs)
        then markFailed $ addUnexpectedShdrLink path nm
        else do
          let tgt = shdrs V.! fromIntegral link
          when (Elf.shdrName tgt /= ".dynsym") $ do
            markFailed $ addUnexpectedShdrLink path nm
    _ | not static,
        nm `elem` [".dynamic", ".dynsym", ".gnu.version_d", ".gnu.version_r"] -> do
      if toInteger link >= toInteger (V.length shdrs)
        then markFailed $ addUnexpectedShdrLink path nm
        else do
          let tgt = shdrs V.! fromIntegral link
          when (Elf.shdrName tgt /= ".dynstr") $ do
            markFailed $ addUnexpectedShdrLink path nm
    _ -> do
      when (link /= 0) $ do
        markFailed $ addUnexpectedShdrLink path nm

-- Check properties of binaries.
checkBinary ::
  forall w.
  (w ~ 64) =>
  FilePath ->
  Elf.ElfHeaderInfo w ->
  V.Vector (Elf.Shdr BS.ByteString (Elf.ElfWordType w)) ->
  CheckM ()
checkBinary path elf shdrs = do
  let cl = Elf.headerClass (Elf.header elf)
  Elf.elfClassInstances cl $ do
    -- Check section headers
    let phdrs = Elf.headerPhdrs elf
    let isInterp = hasPhdrType Elf.PT_INTERP
    let isStatic = Elf.headerType (Elf.header elf) == Elf.ET_EXEC && not (any isInterp phdrs)
    forM_ shdrs $ \shdr -> do
      checkShdrInfo path shdrs shdr
      checkShdrLink path isStatic shdrs shdr
    -- Shdr map
    --let shdrMap = Map.fromListWith (++) [ (Elf.shdrName s, [s]) | s <- V.toList shdrs ]
    -- Check PLT sections
    _ <- either throwError pure $ extractPLTEntries elf shdrs

    let hasDynamic = any (hasPhdrType Elf.PT_DYNAMIC) phdrs

    case filter (hasPhdrType Elf.PT_INTERP) phdrs of
      [] -> do
        when (Elf.headerType (Elf.header elf) /= Elf.ET_DYN && hasDynamic) $ do
          throwError "Unexpected dynamic section."
      [interpPhdr] -> do
        let interpContents = phdrContents elf interpPhdr
        let klibcName :: BS.ByteString
            klibcName = "/lib/klibc-OvGFfwo2ORZBy9wDQTD7AsebxJc.so\0"
        case interpContents of
          "/lib/klibc-OvGFfwo2ORZBy9wDQTD7AsebxJc.so\0" -> do
            when hasDynamic $ do
              throwError "Unexpected dynamic section with klibc loader."
          _ -> do
            unless hasDynamic $ do
              throwError $
                printf
                  "Expected dynamic section\n%s\n%s\n%s"
                  (ppBuffer interpContents)
                  (ppBuffer klibcName)
                  (show (interpContents /= klibcName))
      _ : _ : _ -> do
        throwError "Multiple interpreter sections."

    --  case Map.findWithDefault [] ".fini" shdrMap of
    --    [] -> pure ()
    --    [s] -> checkFini elf s
    --    _ -> throwError "Multiple .fini sections"
    -- Check for single executable section
    let isExec p =
          Elf.phdrSegmentType p == Elf.PT_LOAD
            && Elf.phdrSegmentFlags p == (Elf.pf_r .|. Elf.pf_x)
    case filter isExec phdrs of
      [_] -> pure ()
      _ -> markFailed $ addMissingExecutableSegment path

hasSegmentType :: Elf.PhdrType -> Elf.Phdr w -> Bool
hasSegmentType tp p = Elf.phdrSegmentType p == tp

hasNullLoad :: (Eq (Elf.ElfWordType w), Num (Elf.ElfWordType w)) => Elf.ElfHeaderInfo w -> Bool
hasNullLoad elfHdr = any (\p -> hasSegmentType Elf.PT_LOAD p && (Elf.phdrFileSize p == 0)) (Elf.headerPhdrs elfHdr)

-- | @checkElf stats path bytes@ checks that the Elf file with path
-- @path@ and contnts @bytes@ satisfies all the assumptions we think we
-- can make on Elf files.
checkElf :: MatchStats -> FilePath -> BS.ByteString -> IO MatchStats
checkElf stats0 path bytes = do
  Elf.SomeElf elfHdr <-
    case Elf.decodeElfHeaderInfo bytes of
      Left (_o, m) -> reportErrorAndExit path m
      Right r -> pure r
  let hdr = Elf.header elfHdr
  shdrs <-
    case Elf.headerNamedShdrs elfHdr of
      Left _ -> reportErrorAndExit path "Could not parse section headers."
      Right r -> pure r
  let cl = Elf.headerClass hdr
  let shouldExplore =
        Elf.elfClassInstances cl $
          Elf.headerType hdr `elem` [Elf.ET_DYN, Elf.ET_EXEC]
            && Elf.headerMachine hdr == Elf.EM_X86_64
            && Elf.phdrCount elfHdr > 0
            && not (hasNullLoad elfHdr)
  case Elf.headerClass hdr of
    Elf.ELFCLASS64 | shouldExplore -> do
      putStrLn $ "Checking " <> path
      let (seenError, stats) = runCheckM path (checkBinary path elfHdr shdrs) stats0
      -- check that infer binary layout succeeeds
      case inferBinaryLayout elfHdr shdrs of
        Left msg -> do
          pure $ incTotalBinaries $ addFailedBinary path msg stats
        Right _l ->
          pure
            $! if seenError
              then incTotalBinaries stats
              else incTotalBinaries $ stats {successfulBinaries = successfulBinaries stats + 1}
    _ -> do
      putStrLn $ "Skipping " <> path
      pure stats0

-- | Runs the action on each file in a directory (recursively)
foreachFile :: a -> (a -> FilePath -> IO a) -> FilePath -> IO a
foreachFile v0 act path = do
  -- Ignore sym links
  isLink <- pathIsSymbolicLink path
  if isLink
    then pure v0
    else do
      dexist <- doesDirectoryExist path
      if dexist
        then do
          mentries <- try $ listDirectory path
          case mentries of
            Right entries -> do
              foldlM (\v f -> foreachFile v act (path </> f)) v0 entries
            Left e
              | isPermissionError e -> pure v0
              | otherwise -> throwError e
        else do
          fexist <- doesFileExist path
          if fexist && not isLink
            then act v0 path
            else pure v0

-- | This reads an argument in the file.
withElfFilesInDir ::
  (a -> FilePath -> BS.ByteString -> IO a) ->
  a ->
  FilePath ->
  IO a
withElfFilesInDir action v0 path = do
  fexist <- doesFileExist path

  if fexist
    then action v0 path =<< BS.readFile path
    else do
      dexist <- doesDirectoryExist path
      if dexist
        then do
          let m v fname = do
                mbytes <- try $ BS.readFile fname
                case mbytes of
                  -- Ignore files we cannot read
                  Left (_e :: IOException) -> do
                    pure v
                  Right bytes -> do
                    if Elf.elfMagic `BS.isPrefixOf` bytes
                      then action v fname bytes
                      else pure v
          mentries <- try $ listDirectory path
          case mentries of
            Right entries -> do
              foldlM (\v f -> foreachFile v m (path </> f)) v0 entries
            Left e
              | isPermissionError e -> pure v0
              | otherwise -> throwError e
        else do
          reportErrorAndExit path "File not found"

main :: IO ()
main = do
  paths <- getArgs
  when (null paths) $ do
    hPutStrLn stderr "Please specify at least one file or directory for comparing."
    exitFailure
  errs <- foldlM (withElfFilesInDir checkElf) emptyMatchStats paths
  printMatchStats errs
