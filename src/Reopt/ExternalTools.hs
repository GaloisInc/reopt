{-|
This defines operations that require external tools installed on the system.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reopt.ExternalTools
  ( -- * Clang
    runClangPreprocessor
    -- * Running llc
  , LLCOptions(..)
  , runLLC
    -- * Running opt
  , run_opt
    -- * Running assembler
  , runLlvmMc
    -- * Running llvm-link
  , run_llvm_link
    -- * Running gas
  , run_gas
    -- * Slash
  , runSlash
    -- * Failure information
  , Failure(..)
  , ToolName(..)
  , Control.Monad.Trans.Except.ExceptT(..)
  ) where

import           Control.Exception
import           Control.Monad (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import qualified Data.ByteString as BS
import           GHC.IO.Exception
import           System.IO
import System.IO.Error
    ( ioeGetErrorType, isDoesNotExistError, isPermissionError )
import qualified System.Process  as P

data ToolName
   = Clang
   | Gas
   | Llc
   | LlvmLink
   | LlvmMc
   | Opt
   -- | Slash
   | Slash

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
     -- | @ExitError tool out exit@ indicates program identified by @tool@ exited
     -- with a non-zero exit code @exit@.  @out@ denotes output from @stderr@.
  | ExitError !ToolName !String !Int

instance Show Failure where
  show (ProgramNotFound _ path) = "Could not find " ++ path ++ "."
  show (PermissionError _ path) = "Do not have permissions to execute " ++ path ++ "."
  show (EarlyTerminationError _ msg)
    | null msg = "Unexpected end of output"
    | otherwise = "Unexpected end of output:\n  " ++ msg
  show (ExitError _ output exitCode)
    | null output = "Exited with error code " ++ show exitCode ++ "."
    | otherwise =  output

-- | Try to create a process, and call the call back funtion with
-- the handles for 'stdin', 'stdout', and 'stderr' if it succeeds.
--
-- This will throw a failure and read from standard error is the process fails.
--
-- If the attempt fails, because the process does not exist, then throw the given
-- exception.
withCreateProcess :: ToolName -- ^ Name of tool for error purposes
                  -> String -- ^ Path of program to execute
                  -> [String] -- ^ Arguments
                  -> ((Handle, Handle, Handle) -> ExceptT Failure IO a)
                  -> ExceptT Failure IO a
withCreateProcess nm cmd args f = do
  let cp = (P.proc cmd args)
                { P.env = Nothing
                , P.std_in  = P.CreatePipe
                , P.std_out = P.CreatePipe
                , P.std_err = P.CreatePipe
                }
  let h :: IOException -> IO (Either Failure a)
      h ioe
        | isDoesNotExistError ioe = return $! (Left $! ProgramNotFound nm cmd)
        | isPermissionError   ioe = return $! (Left $! PermissionError nm cmd)
        | otherwise = throwIO ioe
  (Just inHandle, Just out, Just err, ph) <- ExceptT $
     (Right <$> P.createProcess cp) `catch` h
  f (inHandle, out, err) <* waitForEnd nm err ph

catchFailure :: IO a -> (IOError -> IO Failure) -> ExceptT Failure IO a
catchFailure m h = ExceptT $ fmap Right m `catch` (fmap Left . h)

-- | This is a helper function that runs an IO action and ensures that
-- some cleanup is performed if an exception is thrown.
writeAndClose :: ToolName
              -> Handle -- ^ Input handle to write to.
              -> Handle -- ^ Error handle to close if things fail.
              -> IO () -- ^ Action to run
              -> ExceptT Failure IO ()
writeAndClose nm inHandle errHandle action = do
  let h e | ioeGetErrorType e == ResourceVanished = do
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
waitForEnd :: ToolName -- ^ Name of tool
           -> Handle -- ^ Handle to read from for getting error
           -> P.ProcessHandle -- ^ Handle to process
           -> ExceptT Failure IO ()
waitForEnd tool err_handle ph = do
  ec  <- liftIO $ P.waitForProcess ph
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
runClangPreprocessor :: FilePath
                     -- ^ Path to clang
                     -> FilePath
                     -- ^ Path to header file to preprocessor.
                     -> ExceptT Failure IO BS.ByteString
runClangPreprocessor cmd headerFile = do
  -- Run GNU asssembler
  let args= [ "-E"
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
data LLCOptions
  = LLCOptions { llcTriple :: !(Maybe String)
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
runLLC :: FilePath      -- ^ Path to llc
        -> LLCOptions    -- ^ Triple for LLC
        -> BS.ByteString -- ^ Contents of input file.
        -> ExceptT Failure IO BS.ByteString
runLLC llc_command opts input_file = do
  let llcTripleArgs =
        case llcTriple opts of
          Just arch -> [ "-mtriple=" ++ arch ]
          Nothing   -> []
  let llcOptArgs
          | 0 <= lvl && lvl <= 3 = [ "-O=" ++ show lvl ]
          | otherwise            = error "internal: runLLC given bad optimization level."
        where lvl = llcOptLevel opts
  let llcFunctionSectionsArgs
        | llcFunctionSections opts = ["--function-sections"]
        | otherwise = []
  let llc_args
        = [ "-o", "-" ]
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
runLlvmMc :: FilePath
          -- ^ Path to llvm-mc
          -> BS.ByteString
          -- ^ Assembler file
          -> String
          -- ^ Triple for target architecture (e.g. "x86_64-unknown-linux-gnu")
          -> ExceptT Failure IO BS.ByteString
runLlvmMc cmd asm triple = do
  -- Run GNU asssembler
  let args= [ "-assemble"
            , "-filetype=obj"
            , "-fatal-warnings"
            , "--triple=" ++ triple ]
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
run_opt :: FilePath -- ^ Path to opt
        -> Int -- ^ Optimization level (must be between 0 and 3)
        -> (Handle -> IO ())
           -- ^ Callback for writing LLVM to opt
        -> ExceptT Failure IO BS.ByteString
run_opt opt_command lvl writeInput = do
  when (lvl < 0 || lvl > 3) $ do
    error $ "Optimization level is out of range."
  let opt_args = [ "-O" ++ show lvl ]
  withCreateProcess Opt opt_command opt_args $ \(in_handle, out_handle, err_handle) -> do
    writeAndClose Opt in_handle err_handle $ do
      hSetBinaryMode in_handle True
      writeInput in_handle
    liftIO $ readContents out_handle

------------------------------------------------------------------------
-- llvm-link

-- | Use LLVM link to combine several ll or bc files into one.
run_llvm_link :: FilePath -- ^ Path to llvm-link
              -> [FilePath]
              -> ExceptT Failure IO BS.ByteString
run_llvm_link llvm_link_command paths = do
  let args = [ "-o=-" ] ++ paths
  withCreateProcess LlvmLink llvm_link_command args $ \(in_handle, out_handle, _err_handle) -> do
    liftIO $ hClose in_handle
    liftIO $ readContents out_handle

------------------------------------------------------------------------
-- GNU assembler

-- | Use GNU assembler to generate an object file from assembly.
--
-- The assembler file should be text, and the output is written to the given
-- path.
run_gas :: FilePath      -- ^ Path to gas
        -> BS.ByteString -- ^ Assembler file
        -> FilePath      -- ^ Path for object file.
        -> ExceptT Failure IO ()
run_gas gas_command asm obj_path = do
  -- Run GNU asssembler
  let args= [ "-o", obj_path, "--fatal-warnings" ]
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
  withCreateProcess Slash slashPath (manifestPath:args) $ \(in_handle, out_handle, _err_handle) -> do
    liftIO $ hClose in_handle
    liftIO $ hClose out_handle
