module Main (main) where

import Control.Lens hiding (Action)
import Control.Monad
import Data.Version
import System.Console.CmdArgs.Explicit
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Paths_reopt (version)

import Flexdis86
import Reopt.Loader
import Reopt.Memory


------------------------------------------------------------------------
-- Args

-- | Action to perform when running
data Action
   = DumpDisassembly -- ^ Print out disassembler output only.
   | ShowCFG         -- ^ Print out control-flow microcode.
   | ShowHelp        -- ^ Print out help message
   | ShowVersion     -- ^ Print out version

-- | Command line arguments.
data Args = Args { _reoptAction :: Action
                 , _programPath :: FilePath
                 }

-- | Action to perform when running.
reoptAction :: Simple Lens Args Action
reoptAction = lens _reoptAction (\s v -> s { _reoptAction = v })

-- | Action to perform when running.
programPath :: Simple Lens Args FilePath
programPath = lens _programPath (\s v -> s { _programPath = v })

-- | Initial arguments if nothing is specified.
defaultArgs :: Args
defaultArgs = Args { _reoptAction = ShowCFG
                   , _programPath = ""
                   }

------------------------------------------------------------------------
-- Argument processing

disassembleFlag :: Flag Args
disassembleFlag = flagNone [ "disassemble", "d" ] upd help
  where upd  = reoptAction .~ DumpDisassembly
        help = "Disassemble code segment of binary, and print it in an objdump style."

cfgFlag :: Flag Args
cfgFlag = flagNone [ "cfg", "c" ] upd help
  where upd  = reoptAction .~ ShowCFG
        help = "Print out recovered control flow graph of executable."

arguments :: Mode Args
arguments = mode "reopt" defaultArgs help filenameArg flags
  where help = reoptVersion ++ "\n" ++ copyrightNotice
        flags = [ disassembleFlag
                , cfgFlag
                , flagHelpSimple (reoptAction .~ ShowHelp)
                , flagVersion (reoptAction .~ ShowVersion)
                ]

reoptVersion :: String
reoptVersion = "Reopt binary reoptimizer (reopt) "
             ++ versionString ++ ", June 2014."
  where [h,l,r] = versionBranch version 
        versionString = show h ++ "." ++ show l ++ "." ++ show r

copyrightNotice :: String
copyrightNotice = "Copyright 2014 Galois, Inc. All rights reserved."

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
      putStrLn msg
      exitFailure
    Right v -> return v

------------------------------------------------------------------------
-- Execution

showUsage :: IO ()
showUsage = do
  putStrLn "For help on using reopt, run \"reopt --help\"."

dumpDisassembly :: FilePath -> IO ()
dumpDisassembly path = do
  when (null path) $ do
    putStrLn "Please specify a binary."
    showUsage
    exitFailure
  mm <- loadExecutable path
  case mm of
    Memory32 _m -> do
      putStrLn "32-bit executables are not yet supported."
      exitFailure
    Memory64 m -> do
     let segments = filter isExecutable $ memSegments m
     when (null segments) $ do
       putStrLn "Binary contains no executable segments."
     forM_ segments $ \s -> do
       let r = disassembleBuffer defaultX64Disassembler (memBase s) (memBytes s)
       putStrLn "TODO: Dump disassembly"
       undefined r

showCFG :: FilePath -> IO ()
showCFG path = do
  when (null path) $ do
    putStrLn "Please specify a binary."
    showUsage
    exitFailure
  putStrLn "TODO: Show CFG"

main :: IO ()
main = do
  args <- getCommandLineArgs
  case args^.reoptAction of
    DumpDisassembly -> do
      dumpDisassembly (args^.programPath)
    ShowCFG -> do
      showCFG (args^.programPath)
    ShowHelp ->
      print $ helpText [] HelpFormatDefault arguments
    ShowVersion ->
      putStrLn (modeHelp arguments)