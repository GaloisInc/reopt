
module Common where

import           Control.Monad   (foldM)
import           Data.IORef      (IORef, modifyIORef', newIORef, readIORef)

import           Reopt.Utils.Dir (withElfExeFilesInDir)

findAllElfFilesInDirs ::
  [FilePath] ->
  IO [(Int, FilePath)]
findAllElfFilesInDirs paths = do
  counter <- newIORef 1
  files <- foldM (withElfExeFilesInDir (recordFile counter)) [] paths
  pure $ reverse files
  where recordFile :: IORef Int -> [(Int, FilePath)] -> FilePath -> IO [(Int, FilePath)]
        recordFile counter ps p = do
          index <- readIORef counter
          modifyIORef' counter (+ 1)
          pure $ (index, p):ps
