{-# LANGUAGE OverloadedStrings #-}

-- | This defines operations that require external tools installed on the
-- system.
module Reopt.ExternalTools (
  -- * Clang
  runClangPreprocessor,

  -- * Running llc
  LLCOptions (..),
  runLLC,

  -- * Running opt
  runOpt,

  -- * Running assembler
  runLlvmMc,

  -- * Running llvm-link
  runLLVMLink,

  -- * Running gas
  runGNUAssembler,

  -- * Slash
  runSlash,

  -- * Failure information
  Failure (..),
  ToolName (..),
  Control.Monad.Trans.Except.ExceptT (..),
) where

import Control.Exception (IOException, catch, throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT (..), throwE)
import Data.ByteString qualified as BS
import GHC.IO.Exception (
  ExitCode (ExitFailure, ExitSuccess),
  IOErrorType (ResourceVanished),
 )
import System.IO (Handle, hClose, hGetContents, hSetBinaryMode)
import System.IO.Error (
  ioeGetErrorType,
  isDoesNotExistError,
  isPermissionError,
 )
import System.Process qualified as P

data ToolName
  = Clang
  | Gas
  | Llc
  | LlvmLink
  | LlvmMc
  | Opt
  | -- | Slash
    Slash

instance Show ToolName where
  show Clang = "clang"
  show Gas = "gas"
  show Llc = "llc"
  show LlvmLink = "llvm-link"
  show LlvmMc = "llvm-mc"
  show Opt = "opt"
  show Slash = "slash"

-- |  Type of failure from running tools.
data Failure
  = ProgramNotFound !ToolName !String
  | PermissionError !ToolName !String
  | EarlyTerminationError !ToolName !String
  | -- | @ExitError tool out exit@ indicates program identified by @tool@ exited
    -- with a non-zero exit code @exit@.  @out@ denotes output from @stderr@.
    ExitError !ToolName !String !Int

instance Show Failure where
  show (ProgramNotFound _ path) = "Could not find " ++ path ++ "."
  show (PermissionError _ path) = "Do not have permissions to execute " ++ path ++ "."
  show (EarlyTerminationError _ msg)
    | null msg = "Unexpected end of output"
    | otherwise = "Unexpected end of output:\n  " ++ msg
  show (ExitError _ output exitCode)
    | null output = "Exited with error code " ++ show exitCode ++ "."
    | otherwise = output

-- | Try to create a process, and call the call back funtion with
-- the handles for 'stdin', 'stdout', and 'stderr' if it succeeds.
--
-- This will throw a failure and read from standard error is the process fails.
--
-- If the attempt fails, because the process does not exist, then throw the given
-- exception.
withCreateProcess ::
  -- | Name of tool for error purposes
  ToolName ->
  -- | Path of program to execute
  String ->
  -- | Arguments
  [String] ->
  ((Handle, Handle, Handle) -> ExceptT Failure IO a) ->
  ExceptT Failure IO a
withCreateProcess nm cmd args f = do
  let cp =
        (P.proc cmd args)
          { P.env = Nothing
          , P.std_in = P.CreatePipe
          , P.std_out = P.CreatePipe
          , P.std_err = P.CreatePipe
          }
  let h :: IOException -> IO (Either Failure a)
      h ioe
        | isDoesNotExistError ioe = return $! (Left $! ProgramNotFound nm cmd)
        | isPermissionError ioe = return $! (Left $! PermissionError nm cmd)
        | otherwise = throwIO ioe
  (Just inHandle, Just out, Just err, ph) <-
    ExceptT $
      (Right <$> P.createProcess cp) `catch` h
  f (inHandle, out, err) <* waitForEnd nm err ph

catchFailure :: IO a -> (IOError -> IO Failure) -> ExceptT Failure IO a
catchFailure m h = ExceptT $ fmap Right m `catch` (fmap Left . h)

-- | This is a helper function that runs an IO action and ensures that
-- some cleanup is performed if an exception is thrown.
writeAndClose ::
  ToolName ->
  -- | Input handle to write to.
  Handle ->
  -- | Error handle to close if things fail.
  Handle ->
  -- | Action to run
  IO () ->
  ExceptT Failure IO ()
writeAndClose nm inHandle errHandle action = do
  let h e
        | ioeGetErrorType e == ResourceVanished = do
            hClose inHandle
            EarlyTerminationError nm <$> hGetContents errHandle
        | otherwise = do
            hClose inHandle
            hClose errHandle
            throwIO (e :: IOError)
  action `catchFailure` h
  liftIO $ hClose inHandle

-- | Wait for a process to end and throw an error case if the return
-- value differs from the 'ExitSuccess'.
--
-- This function takes a handle to the process's stderr and closes it
-- or semicloses it before returning.
waitForEnd ::
  -- | Name of tool
  ToolName ->
  -- | Handle to read from for getting error
  Handle ->
  -- | Handle to process
  P.ProcessHandle ->
  ExceptT Failure IO ()
waitForEnd tool err_handle ph = do
  ec <- liftIO $ P.waitForProcess ph
  case ec of
    ExitSuccess ->
      liftIO $ hClose err_handle
    ExitFailure c -> do
      msg <- liftIO $ hGetContents err_handle
      throwE $! ExitError tool msg c

-- | The the file handle to binary and read its contents.
readContents :: Handle -> IO BS.ByteString
readContents h = do
  hSetBinaryMode h True
  BS.hGetContents h

------------------------------------------------------------------------
-- clang

-- | Run clang with just the preprocessor.
--
-- The assembler file should be text, and the output is the bytestring.
runClangPreprocessor ::
  -- | Path to clang
  FilePath ->
  -- | Path to header file to preprocessor.
  FilePath ->
  ExceptT Failure IO BS.ByteString
runClangPreprocessor cmd headerFile = do
  -- Run GNU asssembler
  let args =
        [ "-E"
        , "-nostdinc"
        , headerFile
        ]
  withCreateProcess Clang cmd args $ \(inHandle, outHandle, _errHandle) -> do
    liftIO $ do
      hClose inHandle
      readContents outHandle

------------------------------------------------------------------------
-- LLC

-- | Options to pass to 'llc'
data LLCOptions = LLCOptions
  { llcTriple :: !(Maybe String)
  -- ^ LLVM triple (otherwise default to .ll contents)
  , llcOptLevel :: !Int
  -- ^ Optimization level (must be between 0 and 3 inclusive)
  , llcFunctionSections :: !Bool
  -- ^ Set to true to use function sections.
  }

-- | Use 'llc' to create assembly from LLVM bitcode.
--
-- The contents of the input file may be either an assembly (.ll) file or a
-- bitcode (.bc) file.  This returns assembled output as a bytestring, or the reason
-- t conversion failed.
runLLC ::
  -- | Path to llc
  FilePath ->
  -- | Triple for LLC
  LLCOptions ->
  -- | Contents of input file.
  BS.ByteString ->
  ExceptT Failure IO BS.ByteString
runLLC llc_command opts input_file = do
  let llcTripleArgs =
        case llcTriple opts of
          Just arch -> ["-mtriple=" ++ arch]
          Nothing -> []
  let llcOptArgs
        | 0 <= lvl && lvl <= 3 = ["-O=" ++ show lvl]
        | otherwise = error "internal: runLLC given bad optimization level."
       where
        lvl = llcOptLevel opts
  let llcFunctionSectionsArgs
        | llcFunctionSections opts = ["--function-sections"]
        | otherwise = []
  let llc_args =
        ["-o", "-"]
          ++ llcTripleArgs
          ++ llcOptArgs
          ++ llcFunctionSectionsArgs
  withCreateProcess Llc llc_command llc_args $ \(in_handle, out_handle, err_handle) -> do
    writeAndClose Llc in_handle err_handle $ do
      hSetBinaryMode in_handle True
      BS.hPut in_handle input_file
    liftIO $ readContents out_handle

------------------------------------------------------------------------
-- llvm-mc

-- | Run llvm-mc to generate an object file from assembly.
--
-- The assembler file should be text, and the output is the bytestring.
runLlvmMc ::
  -- | Path to llvm-mc
  FilePath ->
  -- | Assembler file
  BS.ByteString ->
  -- | Triple for target architecture (e.g. "x86_64-unknown-linux-gnu")
  String ->
  ExceptT Failure IO BS.ByteString
runLlvmMc cmd asm triple = do
  -- Run GNU asssembler
  let args =
        [ "-assemble"
        , "-filetype=obj"
        , "-fatal-warnings"
        , "--triple=" ++ triple
        ]
  withCreateProcess LlvmMc cmd args $ \(in_handle, out_handle, err_handle) -> do
    -- Write to input handle and close it.
    writeAndClose LlvmMc in_handle err_handle $ do
      hSetBinaryMode in_handle True
      BS.hPut in_handle asm
    -- Get output
    liftIO $ readContents out_handle

------------------------------------------------------------------------
-- LLVM opt

-- | Use opt on a bitcode file and return bitcode file.
runOpt ::
  -- | Path to opt
  FilePath ->
  -- | Optimization level (must be between 0 and 3)
  Int ->
  -- | Callback for writing LLVM to opt
  (Handle -> IO ()) ->
  ExceptT Failure IO BS.ByteString
runOpt opt_command lvl writeInput = do
  when (lvl < 0 || lvl > 3) $ do
    error "Optimization level is out of range."
  let opt_args = ["-O" ++ show lvl]
  withCreateProcess Opt opt_command opt_args $ \(in_handle, out_handle, err_handle) -> do
    writeAndClose Opt in_handle err_handle $ do
      hSetBinaryMode in_handle True
      writeInput in_handle
    liftIO $ readContents out_handle

------------------------------------------------------------------------
-- llvm-link

-- | Use LLVM link to combine several ll or bc files into one.
runLLVMLink ::
  -- | Path to llvm-link
  FilePath ->
  [FilePath] ->
  ExceptT Failure IO BS.ByteString
runLLVMLink llvm_link_command paths = do
  let args = "-o=-" : paths
  withCreateProcess LlvmLink llvm_link_command args $ \(in_handle, out_handle, _err_handle) -> do
    liftIO $ hClose in_handle
    liftIO $ readContents out_handle

------------------------------------------------------------------------
-- GNU assembler

-- | Use GNU assembler to generate an object file from assembly.
--
-- The assembler file should be text, and the output is written to the given
-- path.
runGNUAssembler ::
  -- | Path to gas
  FilePath ->
  -- | Assembler file
  BS.ByteString ->
  -- | Path for object file.
  FilePath ->
  ExceptT Failure IO ()
runGNUAssembler gas_command asm obj_path = do
  -- Run GNU asssembler
  let args = ["-o", obj_path, "--fatal-warnings"]
  withCreateProcess Gas gas_command args $ \(in_handle, out_handle, err_handle) -> do
    -- Write to input handle and close it.
    writeAndClose Gas in_handle err_handle $ do
      hSetBinaryMode in_handle True
      BS.hPut in_handle asm
    -- Close output
    liftIO $ hClose out_handle

------------------------------------------------------------------------
-- slash

-- | Use LLVM link to combine several ll or bc files into one.
runSlash ::
  -- | Path to slash
  FilePath ->
  -- | Path to manifest file
  FilePath ->
  -- | Additional arguments to slash
  [String] ->
  ExceptT Failure IO ()
runSlash slashPath manifestPath args = do
  withCreateProcess Slash slashPath (manifestPath : args) $ \(in_handle, out_handle, _err_handle) -> do
    liftIO $ hClose in_handle
    liftIO $ hClose out_handle
