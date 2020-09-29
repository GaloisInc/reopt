{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.Version (versionBranch)
import qualified Data.Yaml as Yaml
import           System.Console.CmdArgs.Explicit
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO
import           System.IO.Error

import           Reopt

import           Paths_reopt (version)

reoptVersion :: String
reoptVersion = "Reopt binary reoptimizer (reopt) "  ++ versionString ++ "."
  where [h,l,r] = versionBranch version
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
   = ShowHelp        -- ^ Print out help message
   | Showversion     -- ^ Print out version
   | Relink          -- ^ Link an existing binary and new code together.
  deriving (Show)

------------------------------------------------------------------------
-- Args

-- | Command line arguments.
data Args
   = Args { _reoptAction  :: !Action
            -- ^ Action to perform
          , _programPath  :: !FilePath
            -- ^ Path to input program to optimize/export
          , _outputPath   :: !FilePath
            -- ^ Path to output
            --
            -- Only used when reoptAction is @Relink@ and @Reopt@.
          , _newobjPath   :: !FilePath
            -- ^ Path for new object to merge into program
            --
            -- Only used when reoptAction is @Relink@.
          , _redirPath    :: !FilePath
            -- ^ Path to file for manual redirections.
            --
            -- Only used when reoptAction is @Relink@.
          }

-- | Action to perform when running
reoptAction :: Lens' Args Action
reoptAction = lens _reoptAction (\s v -> s { _reoptAction = v })

-- | Path for main executable
programPath :: Lens' Args FilePath
programPath = lens _programPath (\s v -> s { _programPath = v })

-- | Path to write file to.
outputPath :: Lens' Args FilePath
outputPath = lens _outputPath (\s v -> s { _outputPath = v })

-- | Path to new object code for relinker
newobjPath :: Lens' Args FilePath
newobjPath = lens _newobjPath (\s v -> s { _newobjPath = v })

-- | Path to JSON file describing the redirections
redirPath :: Lens' Args FilePath
redirPath = lens _redirPath (\s v -> s { _redirPath = v })

-- | Initial arguments if nothing is specified.
defaultArgs :: Args
defaultArgs = Args { _reoptAction = Relink
                   , _programPath = ""
                   , _outputPath = "a.out"
                   , _newobjPath = ""
                   , _redirPath  = ""
                   }

------------------------------------------------------------------------
-- Other Flags

outputFlag :: Flag Args
outputFlag = flagReq [ "o", "output" ] upd "PATH" help
  where upd s old = Right $ old & outputPath .~ s
        help = "Path to write new binary."

-- | Object file to use for relinking only.
objectPathFlag :: Flag Args
objectPathFlag = flagReq [ "object" ] upd "PATH" help
  where upd s old = Right $ old & newobjPath .~ s
        help = "Path to new object code to link into existing binary."

-- | The patch file provides
patchFilePathFlag :: Flag Args
patchFilePathFlag = flagReq [ "patch-file" ] upd "PATH" help
  where upd s old = Right $ old & redirPath .~ s
        help = "Path to JSON file that specifies where to patch existing code."

-- | Arguments
arguments :: Mode Args
arguments = mode "reopt" defaultArgs help filenameArg flags
  where help = reoptVersion ++ "\n" ++ copyrightNotice
        flags = [ -- General purpose options
                  flagHelpSimple (reoptAction .~ ShowHelp)
                , flagVersion (reoptAction .~ ShowVersion)
                  -- Redirect output to file.
                , outputFlag
                  -- Options for explicit relinking options
                , objectPathFlag
                , patchFilePathFlag
                ]

  -- | Flag to set the path to the binary to analyze.
filenameArg :: Arg Args
filenameArg = Arg { argValue = setFilename
                  , argType = "FILE"
                  , argRequire = False
                  }
  where setFilename :: String -> Args -> Either String Args
        setFilename nm a = Right (a & programPath .~ nm)

getCommandLineArgs :: IO Args
getCommandLineArgs = do
  argStrings <- getArgs
  case process arguments argStrings of
    Left msg -> do
      logger msg
      exitFailure
    Right v -> return v

-- | This is a mode for Reopt to just test that the relinker can successfully
-- combine two binaries.
performRelink :: Args -> IO ()
performRelink args = do
  let objPath = args^.newobjPath
  when (objPath == "") $ do
    hPutStrLn stderr $ "Please provide a object file to merge."
    exitFailure

  -- Get original binary
  bs <- checkedReadFile (args^.programPath)
  origBinary <- parseElfHeaderInfo64 (args^.programPath) bs
  let outPath = args^.outputPath

  newObj <- checkedReadFile objPath
  redirs <-
    case args^.redirPath of
      "" -> return []
      redir_path -> do
        mredirs <- Yaml.decodeFileEither redir_path
        case mredirs of
          Left e -> do
            hPutStrLn stderr $ show e
            exitFailure
          Right r -> return r
  mergeAndWrite outPath origBinary objPath newObj redirs

-- | Main executable
main' :: IO ()
main' = do
  args <- getCommandLineArgs
  case args^.reoptAction of
    ShowHelp -> do
      print $ helpText [] HelpFormatAll arguments
    ShowVersion ->
      putStrLn (modeHelp arguments)
    Relink -> do
      performRelink args

main :: IO ()
main = main' `catch` h
  where h e
          | isUserError e = do
            hPutStrLn stderr "User error"
            hPutStrLn stderr $ ioeGetErrorString e
          | otherwise = do
            hPutStrLn stderr "Other error"
            hPutStrLn stderr $ show e
            hPutStrLn stderr $ show (ioeGetErrorType e)
