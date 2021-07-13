{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Exception
import Control.Monad
import Data.Version (versionBranch)
import qualified Data.Yaml as Yaml
import Paths_reopt (version)
import Reopt
import Reopt.Utils.Exit
import System.Console.CmdArgs.Explicit
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import System.IO.Error

reoptVersion :: String
reoptVersion = "Reopt relinker (reopt-relink) " ++ versionString ++ "."
  where
    [h, l, r] = versionBranch version
    versionString = show h ++ "." ++ show l ++ "." ++ show r

------------------------------------------------------------------------
-- Utilities

-- | We'll use stderr to log error messages
logger :: String -> IO ()
logger = hPutStrLn stderr

------------------------------------------------------------------------
-- Action

-- | Action to perform when running
data Action
  = -- | Print out help message
    ShowHelp
  | -- | Print out version
    ShowVersion
  | -- | Link an existing binary and new code together.
    Relink
  deriving (Show)

------------------------------------------------------------------------
-- Args

-- | Command line arguments.
data Args = Args
  { -- | Action to perform
    reoptAction :: !Action,
    -- | Path to output
    outputPath :: !(Maybe FilePath),
    -- | Path to input program to optimize/export
    binaryPath :: !(Maybe FilePath),
    -- | Path for new object to merge into program
    objectPath :: !(Maybe FilePath),
    -- | Path to file for manual redirections.
    infoPath :: !(Maybe FilePath)
  }

-- | Initial arguments if nothing is specified.
defaultArgs :: Args
defaultArgs =
  Args
    { reoptAction = Relink,
      outputPath = Nothing,
      binaryPath = Nothing,
      objectPath = Nothing,
      infoPath = Nothing
    }

-- | Flag to set the path to the binary to analyze.
pathArg :: String -> (Args -> Maybe FilePath -> Args) -> Arg Args
pathArg nm f =
  Arg
    { argValue = \p a -> do
        when (null p) $ do
          Left $ "Missing " ++ nm ++ " path."
        Right (f a (Just p)),
      argType = nm,
      argRequire = False
    }

-- | Arguments
arguments :: Mode Args
arguments =
  (modeEmpty defaultArgs)
    { modeNames = ["relink"],
      modeHelp =
        reoptVersion ++ "\n"
          ++ copyrightNotice
          ++ "\n\n"
          ++ "Usage: reopt-relink <output> <binary> <object> <relinkInfo>",
      modeArgs = (args, Nothing),
      modeGroupFlags = toGroup flags
    }
  where
    args =
      [ pathArg "OUTPUT" (\s f -> s {outputPath = f}),
        pathArg "BINARY" (\s f -> s {binaryPath = f}),
        pathArg "OBJECT" (\s f -> s {objectPath = f}),
        pathArg "INFO" (\s f -> s {infoPath = f})
      ]
    flags =
      [ flagHelpSimple (\s -> s {reoptAction = ShowHelp}),
        flagVersion (\s -> s {reoptAction = ShowVersion})
      ]

getCommandLineArgs :: IO Args
getCommandLineArgs = do
  argStrings <- getArgs
  case process arguments argStrings of
    Left msg -> do
      logger msg
      exitFailure
    Right v -> return v

getPath :: String -> Maybe FilePath -> IO FilePath
getPath nm Nothing = do
  hPutStrLn stderr $ "Provide " <> nm <> "."
  exitFailure
getPath _ (Just p) = pure p

-- | This is a mode for Reopt to just test that the relinker can successfully
-- combine two binaries.
performRelink :: Args -> IO ()
performRelink args = do
  outPath <- getPath "Output file" (outputPath args)
  binPath <- getPath "Binary file" (binaryPath args)
  objPath <- getPath "Object file" (objectPath args)
  thisInfoPath <- getPath "Relink info" (infoPath args)

  -- Get original binary
  bs <- checkedReadFile binPath
  origBinary <- handleEitherStringWithExit $ parseElfHeaderInfo64 binPath bs
  newObj <- checkedReadFile objPath
  mrelinkInfo <- Yaml.decodeFileEither thisInfoPath
  relinkInfo <-
    case mrelinkInfo of
      Left e -> do
        hPutStrLn stderr $ show e
        exitFailure
      Right r -> return r

  let (warnings, mergeRes) = mergeObject origBinary objPath newObj relinkInfo
  forM_ warnings $ \w -> do
    hPutStrLn stderr $ "Relinker warning: " ++ w
  case mergeRes of
    Left e -> do
      hPutStrLn stderr e
      exitFailure
    Right newBinary -> do
      writeExecutable outPath newBinary

-- | Main executable
main' :: IO ()
main' = do
  args <- getCommandLineArgs
  case reoptAction args of
    ShowHelp -> do
      print $ helpText [] HelpFormatAll arguments
    ShowVersion ->
      putStrLn (modeHelp arguments)
    Relink -> do
      performRelink args

main :: IO ()
main = main' `catch` h
  where
    h e
      | isUserError e = do
        hPutStrLn stderr "User error"
        hPutStrLn stderr $ ioeGetErrorString e
      | otherwise = do
        hPutStrLn stderr "Other error"
        hPutStrLn stderr $ show e
        hPutStrLn stderr $ show (ioeGetErrorType e)
