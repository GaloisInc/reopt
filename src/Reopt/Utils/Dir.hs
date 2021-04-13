{-# LANGUAGE ScopedTypeVariables #-}
module Reopt.Utils.Dir
  ( withElfFilesInDir
  ) where

import           Control.Exception
import qualified Data.ByteString as BS
import qualified Data.ElfEdit as Elf
import           Data.Foldable ( foldlM )
import           System.Directory
import           System.Exit ( exitFailure )
import           System.FilePath ( (</>) )
import           System.IO ( stderr, hPutStrLn )
import           System.IO.Error ( isPermissionError )


reportError :: FilePath -> String -> IO a
reportError path msg = hPutStrLn stderr (path <> ": " <> msg) >> exitFailure

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
          | otherwise -> throwIO e
     else do
      fexist <- doesFileExist path
      if fexist && not isLink then
        act v0 path
       else
        pure v0

-- | This reads an argument in the file.
withElfFilesInDir :: (a -> FilePath -> IO a)
                  -> a
                  -> FilePath
                  -> IO a
withElfFilesInDir action v0 path = do
  fexist <- doesFileExist path
  if fexist then
    action v0 path
   else do
    dexist <- doesDirectoryExist path
    if dexist then do
      let visitFile v fname = do
            fPermissions <- getPermissions fname
            if executable fPermissions then do
              mbytes <- try $ BS.readFile fname
              case mbytes of
                -- Ignore files we cannot read
                Left (_e :: IOException) -> do
                  pure v
                Right bytes -> do
                  if Elf.elfMagic `BS.isPrefixOf` bytes then
                    action v fname
                  else
                    pure v
            else pure v
      mentries <- try $ listDirectory path
      case mentries of
        Right entries -> do
          foldlM (\v f -> foreachFile v visitFile (path </> f)) v0 entries
        Left e
          | isPermissionError e -> pure v0
          | otherwise -> throwIO e
     else do
      reportError path "File not found"