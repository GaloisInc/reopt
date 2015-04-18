{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import           Control.Lens
import           Control.Monad
import qualified Data.ByteString as B
import           Data.Elf
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Version
import           GHC.TypeLits
import           Numeric (showHex)
import           System.Console.CmdArgs.Explicit
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Paths_reopt (version)
import Data.Type.Equality as Equality

import           Flexdis86
import           Reopt
import           Reopt.Memory
import           Reopt.Semantics.DeadRegisterElimination
import           Reopt.Semantics.CFGDiscovery
import           Reopt.Semantics.Representation
import qualified Reopt.Semantics.StateNames as N

------------------------------------------------------------------------
-- Args

-- | Action to perform when running
data Action
   = DumpDisassembly -- ^ Print out disassembler output only.
   | ShowCFG         -- ^ Print out control-flow microcode.
   | ShowGaps        -- ^ Print out gaps in discovered blocks
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

gapFlag :: Flag Args
gapFlag = flagNone [ "gap", "g" ] upd help
  where upd  = reoptAction .~ ShowGaps
        help = "Print out gaps in the recovered  control flow graph of executable."

arguments :: Mode Args
arguments = mode "reopt" defaultArgs help filenameArg flags
  where help = reoptVersion ++ "\n" ++ copyrightNotice
        flags = [ disassembleFlag
                , cfgFlag
                , gapFlag
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

dumpDisassembly :: FilePath -> IO ()
dumpDisassembly path = do
  e <- readElf64 path
  let sections = filter isCodeSection $ e^..elfSections
  when (null sections) $ do
    putStrLn "Binary contains no executable sections."
  mapM_ printSectionDisassembly sections
  -- print $ Set.size $ instructionNames sections
  --print $ Set.toList $ instructionNames sections

getCFG :: FilePath -> IO (Memory Word64, (CFG, Set CodeAddr))
getCFG path =  do
  e <- readElf64 path
  mi <- elfInterpreter e
  case mi of
    Nothing ->
      return ()
    Just{} ->
      fail "reopt does not yet support generating CFGs from dynamically linked executables."
  -- Build model of executable memory from elf.
  mem <- loadElf e
  -- Get list of code locations to explore starting from entry points (i.e., eltEntry)
  return $ (mem, cfgFromAddress mem (elfEntry e))

isInterestingCode :: Memory Word64 -> (CodeAddr, Maybe CodeAddr) -> Bool
isInterestingCode mem (start, Just end) = go start end
  where
    isNop ii = (iiOp ii == "nop")
               || (iiOp ii == "xchg" &&
                   case iiArgs ii of
                    [x, y] -> x == y
                    _      -> False)

    go b e | b < e = case readInstruction mem b of
                      Left _           -> False -- FIXME: ignore illegal sequences?
                      Right (ii, next) -> not (isNop ii) || go next e
           | otherwise = False

isInterestingCode _ _ = True -- Last bit

showGaps :: FilePath ->  IO ()
showGaps path = do (mem, (cfg, ends)) <- getCFG path
                   let blocks = [ addr | DecompiledBlock addr <- Map.keys (cfg ^. cfgBlocks) ]
                   let gaps = filter (isInterestingCode mem)
                              $ out_gap blocks (Set.elems ends)

                   mapM_ (print . pretty . ppOne) gaps
  where
    ppOne (start, m_end) = text ("[" ++ showHex start "..") <>
                           case m_end of
                            Nothing -> text "END)"
                            Just e  -> text (showHex e ")")

    in_gap start bs@(b:_) ess = (start, Just b) : out_gap bs (dropWhile (<= b) ess)
    in_gap start [] _ = [(start, Nothing)]

    out_gap (b:bs') ess@(e:es')
      | b < e          = out_gap bs' ess
      | b == e         = out_gap bs' es'
    out_gap bs (e:es') = in_gap e bs es'
    out_gap _ _        = []

showCFG :: FilePath -> IO ()
showCFG path = do
  (_, (g0, _)) <- getCFG path
  let g = eliminateDeadRegisters g0
--  forM_ (Map.elems (g^.cfgBlocks)) printSP
  print (pretty g)
--  print (Map.size (g^.cfgBlocks))

printSP :: Block -> IO ()
printSP b = do
  case blockTerm b of
    Branch _ _ _ -> return ()
    FetchAndExecute s -> do
      let rsp_val = s^.register N.rsp
      case rsp_val of
        _ | Initial v <- rsp_val
          , Just Refl <- testEquality v N.rsp ->
            return ()
        _ | Just (BVAdd _ (Initial r) BVValue{}) <- valueAsApp rsp_val
          , Just Refl <- testEquality r N.rsp -> do
            return ()
        _ | Just (BVAdd _ (Initial r) BVValue{}) <- valueAsApp rsp_val
          , Just Refl <- testEquality r N.rbp -> do
            return ()
          | otherwise -> do
            print $ "Block " ++ show (pretty (blockLabel b))
            print $ "RSP = " ++ show (pretty rsp_val)
      let rbp_val = s^.register N.rbp
      case rbp_val of
           -- This leaves the base pointer unchanged.  It is likely an
           -- internal block.
        _ | Initial v <- rbp_val
          , Just Refl <- testEquality v N.rbp ->
            return ()
           -- This assigns the base pointer the current stack.
           -- It is likely a function entry point
        _ | Just (BVAdd _ (Initial r) BVValue{}) <- valueAsApp rbp_val
          , Just Refl <- testEquality r N.rsp -> do
            return ()
           -- This block assigns the base pointer a value from the stack.
           -- It is likely a function exit.
        _ | AssignedValue (assignRhs -> Read (MemLoc addr _)) <- rbp_val
          , Initial v <- addr
          , Just Refl <- testEquality v N.rbp -> do
            return ()
           -- This block assigns the base pointer a value from the stack.
           -- It is likely a function exit.
        _ | AssignedValue (assignRhs -> Read (MemLoc addr _)) <- rbp_val
          , Just (BVAdd _ (Initial v) BVValue{}) <- valueAsApp addr
          , Just Refl <- testEquality v N.rsp -> do
            return ()

        _ | otherwise -> do
            print $ "Block " ++ show (pretty (blockLabel b))
            print $ "RBP = " ++ show (pretty rbp_val)

{-
printIP :: Block -> IO ()
printIP b =
    case blockTerm b of
      Branch _ _ _ -> return ()
      FetchAndExecute s ->
        case s^.cur of
          Initial a -> do
            print $ "Block " ++ show (pretty (blockLabel b))
            print $ "Next IP " ++ show a
          BVValue _ _ -> return ()
          AssignedValue a -> do
            print $ "Block " ++ show (pretty (blockLabel b))
            print $ "Next IP: " ++ show (pretty a)
-}

main :: IO ()
main = do
  args <- getCommandLineArgs
  case args^.reoptAction of
    DumpDisassembly -> do
      dumpDisassembly (args^.programPath)
    ShowCFG -> do
      showCFG (args^.programPath)
    ShowGaps -> showGaps (args^.programPath)
    ShowHelp ->
      print $ helpText [] HelpFormatDefault arguments
    ShowVersion ->
      putStrLn (modeHelp arguments)
