{-|
Recursive exploration of Elf files in a directory.
-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reopt.Utils.Dir
  ( withElfFilesInDir
  , withElfExeFilesInDir
  , withSymLinksInDir
  ) where

import           Control.Exception ( IOException, try, throwIO )
import qualified Data.ByteString as BS
import qualified Data.ElfEdit as Elf
import           Data.Foldable ( foldlM )
import           Reopt.Utils.Exit (reportErrorAndExit)
import System.Directory
    ( doesDirectoryExist,
      doesFileExist,
      getPermissions,
      listDirectory,
      pathIsSymbolicLink,
      Permissions(executable) )
import           System.FilePath ( (</>) )
import           System.IO.Error ( isPermissionError )

-- | Runs the action on each file in a directory (recursively)
foreachFile :: a -> (a -> FilePath -> IO a) -> FilePath -> IO a
foreachFile v0 act path = do
  dexist <- doesDirectoryExist path
  if dexist then do
    mentries <- try $ listDirectory path
    case mentries of
      Right entries -> do
        foldlM (\v f -> foreachFile v act (path </> f)) v0 entries
      Left e
        | isPermissionError e -> pure v0
        | otherwise -> throwIO e
   else do
    fexist <- doesFileExist path
    if fexist then
      act v0 path
     else
      pure v0


-- | Visit a file, or recursively search for and visit
-- each such file in a directory, if it passes the predicate.
withFilesInDir ::
  -- | Predicate dictating whether a file should be considered.
  (FilePath -> IO Bool) ->
  -- | Action to perform on each visited file.
  (a -> FilePath -> IO a) ->
  -- | Initial accumulator value.
  a ->
  -- | Path to file or directory to visit.
  FilePath ->
  IO a
withFilesInDir visitCheck action v0 path = do
  fexist <- doesFileExist path
  if fexist then do
    shouldVisit <- visitCheck path
    if shouldVisit then action v0 path else pure v0
  else do
    dexist <- doesDirectoryExist path
    if dexist then do
      let visitFile v fname = do
            shouldVisit <- visitCheck fname
            if shouldVisit
            then action v fname
            else pure v
      mentries <- try $ listDirectory path
      case mentries of
        Right entries -> do
          foldlM (\v f -> foreachFile v visitFile (path </> f)) v0 entries
        Left e
          | isPermissionError e -> pure v0
          | otherwise -> throwIO e
    else do
      reportErrorAndExit path "File not found"


-- | Is this a file which begins with the ELF magic numbers, i.e.
-- [0x7f, 'E', 'L', 'F']. N.B., symbolic links are not followed.
isElfFile :: FilePath -> IO Bool
isElfFile path = do
  isLink <- pathIsSymbolicLink path
  if isLink then pure False
  else do
    mbytes <- try $ BS.readFile path
    case mbytes of
      -- Ignore files we cannot read
      Left (_e :: IOException) -> pure False
      Right bytes -> pure $ Elf.elfMagic `BS.isPrefixOf` bytes

-- | Visit an executable elf file, or recursively search for and visit
-- each such file in a directory.
withElfExeFilesInDir ::
  (a -> FilePath -> IO a) ->
  a ->
  FilePath ->
  IO a
withElfExeFilesInDir = withFilesInDir isElfExe
  where isElfExe :: FilePath -> IO Bool
        isElfExe path = do
          isAnExe <- executable <$> getPermissions path
          if not isAnExe then pure False
          else isElfFile path


-- | Visit an elf file (may or may not be executable),
-- or recursively search for and visit each such file
-- in a directory.
withElfFilesInDir ::
  (a -> FilePath -> IO a) ->
  a ->
  FilePath ->
  IO a
withElfFilesInDir = withFilesInDir isElfFile

-- | Visit a file if it is a symbolic link,
-- or recursively search for and visit each such file
-- in a directory.
withSymLinksInDir ::
  (a -> FilePath -> IO a) ->
  a ->
  FilePath ->
  IO a
withSymLinksInDir = withFilesInDir pathIsSymbolicLink
