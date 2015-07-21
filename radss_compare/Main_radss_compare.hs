{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.ByteString as B
import           Data.Elf
import           Data.Foldable (traverse_)
import           Data.Int
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           Data.Version
import           GHC.TypeLits
import           Numeric (showHex, readHex)
import           Reopt.Analysis.AbsState
import           System.Console.CmdArgs.Explicit
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF

import           Paths_reopt (version)
import           Data.Type.Equality as Equality

import           Flexdis86 (InstructionInstance(..))
import           Reopt
import           Reopt.CFG.CFGDiscovery
import           Reopt.CFG.Representation
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types
import           Reopt.Object.Loader
import           Reopt.Object.Memory
import           Reopt.Semantics.DeadRegisterElimination
import           Reopt.Semantics.Monad (Type(..))
import           Reopt.BasicBlock.Extract
import qualified Reopt.Concrete.Semantics as CS

------------------------------------------------------------------------
-- Args

-- | Action to perform when running
data Action
   = CompareBlocks   -- ^ Compare basic blocks in two programs
   | ShowSingleBlock -- ^ Print out concrete semantics of a basic block
   | ShowHelp        -- ^ Print out help message
   | ShowVersion     -- ^ Print out version

-- | Command line arguments.
data Args = Args { _reoptAction  :: !Action
                 , _programPaths :: [FilePath]
                 , _addr1        :: !Word64
                 , _addr2        :: !Word64
                 , _loadStyle    :: !LoadStyle
                 }

-- | How to load Elf file.
data LoadStyle
   = LoadBySection
     -- ^ Load loadable sections in Elf file.
   | LoadBySegment
     -- ^ Load segments in Elf file.

-- | Action to perform when running.
reoptAction :: Simple Lens Args Action
reoptAction = lens _reoptAction (\s v -> s { _reoptAction = v })

-- | Path to load
programPaths :: Simple Lens Args [FilePath]
programPaths = lens _programPaths (\s v -> s { _programPaths = v })

-- | Path to load
addr1 :: Simple Lens Args Word64
addr1 = lens _addr1 (\s v -> s { _addr1 = v })

-- | Path to load
addr2 :: Simple Lens Args Word64
addr2 = lens _addr2 (\s v -> s { _addr2 = v })

-- | Whether to load file by segment or sections.
loadStyle :: Simple Lens Args LoadStyle
loadStyle = lens _loadStyle (\s v -> s { _loadStyle = v })

-- | Initial arguments if nothing is specified.
defaultArgs :: Args
defaultArgs = Args { _reoptAction = ShowSingleBlock
                   , _programPaths = []
                   , _addr1 = 0
                   , _addr2 = 0
                   , _loadStyle = LoadBySection
                   }

------------------------------------------------------------------------
-- Argument processing

singleBlockFlag :: Flag Args
singleBlockFlag = flagNone [ "concrete-blocks", "b" ] upd help
  where upd  = reoptAction .~ ShowSingleBlock
        help = "Print out concrete semantics for basic blocks of executable."

compareBlocksFlag :: Flag Args
compareBlocksFlag = flagNone [ "compare-blocks", "c" ] upd help
  where upd  = reoptAction .~ CompareBlocks
        help = "Print out concrete semantics for basic blocks of executable."

segmentFlag :: Flag Args
segmentFlag = flagNone [ "load-segments" ] upd help
  where upd  = loadStyle .~ LoadBySegment
        help = "Load the Elf file using segment information."

sectionFlag :: Flag Args
sectionFlag = flagNone [ "load-sections" ] upd help
  where upd  = loadStyle .~ LoadBySection
        help = "Load the Elf file using section information (default)."

addr1Flag :: Flag Args
addr1Flag = flagReq [ "addr1" ] upd "Hex String" help
  where upd s old  = case readHex s of
          (addr, _) : _ -> Right $ (addr1 .~ addr) old
          _ -> Left "Could not parse addr1 - should be a bare hex value, like deadbeef"
        help = "Address to start from in the first executable."

addr2Flag :: Flag Args
addr2Flag = flagReq [ "addr2" ] upd "Hex String" help
  where upd s old  = case readHex s of
          (addr, _) : _ -> Right $ (addr2 .~ addr) old
          _ -> Left "Could not parse addr2 - should be a bare hex value, like deadbeef"
        help = "Address to start from in the first executable."

arguments :: Mode Args
arguments = mode "radss_compare" defaultArgs help filenameArg flags
  where help = reoptVersion ++ "\n" ++ copyrightNotice
        flags = [ segmentFlag
                , sectionFlag
                , singleBlockFlag
                , compareBlocksFlag
                , addr1Flag
                , addr2Flag
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
        setFilename nm a = Right (a & programPaths %~ (++ [nm]))

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

readElf64 :: FilePath -> IO (Elf Word64)
readElf64 path = do
  when (null path) $ do
    putStrLn "Please specify a binary."
    showUsage
    exitFailure
  bs <- B.readFile path
  case parseElf bs of
    Left (_,msg) -> do
      putStrLn $ "Error reading " ++ path ++ ":"
      putStrLn $ "  " ++ msg
      exitFailure
    Right (Elf32 _) -> do
      putStrLn "32-bit executables are not yet supported."
      exitFailure
    Right (Elf64 e) ->
      return e

readStaticElf :: FilePath -> IO (Elf Word64)
readStaticElf path = do
  e <- readElf64 path
  mi <- elfInterpreter e
  case mi of
    Nothing ->
      return ()
    Just{} ->
      fail "reopt does not yet support generating CFGs from dynamically linked executables."
  return e

showSingleBlock :: Memory Word64 -> Word64 -> IO ()
showSingleBlock mem start = 
  case runMemoryByteReader pf_x mem start $ extractBlock start of
    Left err -> putStrLn $ show err
    Right (Left err, _) -> putStrLn  $ "Could not disassemble instruction at 0x"
                                      ++ showHex err ""
    Right (Right (nexts, stmts), _) -> do
      putStrLn $ show nexts
      putStrLn $ show $ CS.ppStmts stmts

compareBlocks :: Memory Word64 -> Word64 -> Memory Word64 -> Word64 -> IO ()
compareBlocks mem1 start1 mem2 start2 = do
  (nexts1, stmts1) <- getBlock mem1 start1
  (nexts2, stmts2) <- getBlock mem2 start2
  putStrLn $ show nexts1
  putStrLn $ show $ CS.ppStmts stmts1
  putStrLn $ show nexts2
  putStrLn $ show $ CS.ppStmts stmts2
  where
  getBlock mem start = case runMemoryByteReader pf_x mem start $ extractBlock start of
    Left err -> fail $ show err
    Right (Left err, _) -> fail $ "Could not disassemble instruction at 0x" ++
                                    showHex err ""
    Right (Right (nexts, stmts), _) -> return (nexts, stmts)

mkElfMem :: (ElfWidth w, Functor m, Monad m) => LoadStyle -> Elf w -> m (Memory w)
mkElfMem LoadBySection e = memoryForElfSections e
mkElfMem LoadBySegment e = memoryForElfSegments e


main :: IO ()
main = do
  args <- getCommandLineArgs
  case args^.reoptAction of
    ShowSingleBlock -> do
      e <- case args^.programPaths of
        [] -> fail "A file name is required\n"
        _ : _ : _ -> fail "Exactly one file name for show single block"
        [path] -> readStaticElf path
      mem <- mkElfMem (args^.loadStyle) e
      showSingleBlock mem (elfEntry e)
    CompareBlocks -> do
      (e1, e2) <- case args^.programPaths of
        [] -> fail "Two file names are required\n"
        [_] -> fail "Two file names are required\n"
        [f1,f2] -> do
          e1' <- readStaticElf f1
          e2' <- readStaticElf f2
          return (e1', e2')
        _ : _ : _ : _ -> fail "Exactly two file names for comparison"
      mem1 <- mkElfMem (args^.loadStyle) e1
      mem2 <- mkElfMem (args^.loadStyle) e2
      let a1 = case args^.addr1 of 0 -> elfEntry e1
                                   a -> a
      let a2 = case args^.addr2 of 0 -> elfEntry e2
                                   a -> a
      compareBlocks mem1 a1 mem2 a2
    ShowHelp ->
      print $ helpText [] HelpFormatDefault arguments
    ShowVersion ->
      putStrLn (modeHelp arguments)
