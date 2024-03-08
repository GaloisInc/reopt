module Common where

import Control.Monad (foldM)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)

import Reopt.Utils.Dir (withElfExeFilesInDir)
import Reopt.Events
import Reopt

findAllElfFilesInDirs ::
  [FilePath] ->
  IO [(Int, FilePath)]

findAllElfFilesInDirs paths = do
  counter <- newIORef 1
  files <- foldM (withElfExeFilesInDir (recordFile counter)) [] paths
  pure $ reverse files
 where
  recordFile :: IORef Int -> [(Int, FilePath)] -> FilePath -> IO [(Int, FilePath)]
  recordFile counter ps p = do
    index <- readIORef counter
    modifyIORef' counter (+ 1)
    pure $ (index, p) : ps

createLogger :: ReoptOptions -> IORef ReoptSummary -> IORef ReoptStats -> IO (ReoptLogEvent arch -> IO ())
createLogger reoptOpts summaryRef statsRef = do
  return $
       if roVerboseMode reoptOpts
          then joinLogEvents printLogEvent (recoverLogEvent summaryRef statsRef)
          else recoverLogEvent summaryRef statsRef
      
