module Reopt.Utils.Exit (
  handleEitherStringWithExit,
  handleEitherWithExit,
  handleExceptTStringWithExit,
  handleExceptTWithExit,
  runReoptInIO,
  reportErrorAndExit,
  checkedReadFile,
)
where

import Control.Exception (catch)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.ByteString qualified as BS
import Data.Macaw.Utils.IncComp (
  IncCompM,
  processIncCompLogs,
  runIncCompM,
 )
import System.Exit (exitFailure)
import System.IO (hPrint, hPutStrLn, stderr)
import System.IO.Error (ioeGetErrorType, isDoesNotExistError)

handleEitherWithExit :: Show e => Either e a -> IO a
handleEitherWithExit r = do
  case r of
    Left e -> do
      hPrint stderr e
      exitFailure
    Right b -> pure b

handleEitherStringWithExit :: Either String a -> IO a
handleEitherStringWithExit r = do
  case r of
    Left e -> do
      hPutStrLn stderr e
      exitFailure
    Right b -> pure b

handleExceptTStringWithExit :: ExceptT String IO a -> IO a
handleExceptTStringWithExit m =
  runExceptT m >>= handleEitherStringWithExit

handleExceptTWithExit :: Show e => ExceptT e IO a -> IO a
handleExceptTWithExit m =
  runExceptT m >>= handleEitherWithExit

-- | Run the reopt operations in IO.
runReoptInIO ::
  (l -> IO ()) ->
  IncCompM l (Either String a) a ->
  IO a
runReoptInIO logger m = do
  res <- processIncCompLogs logger $ runIncCompM (fmap Right m)
  handleEitherStringWithExit res

reportErrorAndExit :: FilePath -> String -> IO a
reportErrorAndExit path msg = hPutStrLn stderr (path <> ": " <> msg) >> exitFailure

-- | This reads a file as a strict bytestring while throwing
-- a string with  auseful error message.
checkedReadFile :: FilePath -> IO BS.ByteString
checkedReadFile path = do
  let h e
        | isDoesNotExistError e = do
            hPutStrLn stderr $ path ++ " does not exist."
            exitFailure
        | otherwise = do
            hPrint stderr e
            hPrint stderr (ioeGetErrorType e)
            exitFailure
  BS.readFile path `catch` h
