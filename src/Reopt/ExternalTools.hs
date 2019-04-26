{-|
This defines operations that require external tools installed on the system.
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
module Reopt.ExternalTools
  ( -- * Running llc
    LLCOptions(..)
  , defaultLLCOptions
  , run_llc
    -- * Running gas
  , run_gas
    -- * Running opt
  , run_opt
    -- * Running assembler
  , runLlvmMc
    -- * Running llvm-link
  , run_llvm_link
    -- * Failure information
  , Failure(..)
  , Control.Monad.Trans.Except.ExceptT(..)
  ) where

import           Control.Exception
import           Control.Monad (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import qualified Data.ByteString as BS
import           GHC.IO.Exception
import           System.IO
import           System.IO.Error
import qualified System.Process  as P

-- |  Type of failure from running tools.
data Failure
   = ProgramError !String
     -- ^ gas returned some error.
   | ProgramNotFound !String
   | PermissionError !String

instance Show Failure where
  show (ProgramError msg)     = msg
  show (ProgramNotFound path) = "Could not find " ++ path ++ "."
  show (PermissionError path) = "Do not have permissions to execute " ++ path ++ "."

-- | Try to create a process, and call the call back funtion with
-- the handles for 'stdin', 'stdout', and 'stderr' if it succeeds.
--
-- This will throw a failure and read from standard error is the process fails.
--
-- If the attempt fails, because the process does not exist, then throw the given
-- exception.
withCreateProcess :: String -- ^ Name of tool for error purposes
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
        | isDoesNotExistError ioe = return $! Left (ProgramNotFound cmd)
        | isPermissionError   ioe = return $! Left (PermissionError cmd)
        | otherwise = throwIO ioe
  (Just in_handle, Just out, Just err, ph) <- ExceptT $
     (Right <$> P.createProcess cp) `catch` h
  f (in_handle, out, err) <* waitForEnd nm err ph



catchFailure :: IO a -> (IOError -> IO Failure) -> ExceptT Failure IO a
catchFailure m h = ExceptT $ fmap Right m `catch` (fmap Left . h)


-- | This is a helper function that runs an IO action and ensures that
-- some cleanup is performed if an exception is thrown.
writeAndClose :: Handle -- ^ Input handle to write to.
              -> Handle -- ^ Error handle to close if things fail.
              -> IO () -- ^ Action to run
              -> ExceptT Failure IO ()
writeAndClose in_handle err_handle action = do
  let h e | ioeGetErrorType e == ResourceVanished = do
              hClose in_handle
              ProgramError <$> hGetContents err_handle
          | otherwise = do
              hClose in_handle
              hClose err_handle
              throwIO (e :: IOError)
  action `catchFailure` h
  liftIO $ hClose in_handle

-- | Wait for a process to end and throw an error case if the return value differs from
-- the 'ExitSuccess'.
--
-- This function takes a handle to the process's stderr and closes it or semicloses
-- it before returning.
waitForEnd :: String -- ^ Name of tool
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
      let msg' | null msg = tool ++ " exited with error code " ++ show c ++ "."
               | otherwise = tool ++ ": " ++ msg
      throwE $! ProgramError msg'

readContents :: Handle -> IO BS.ByteString
readContents out_handle = do
  hSetBinaryMode out_handle True
  BS.hGetContents out_handle

-- | Options to pass to 'llc'
data LLCOptions
  = LLCOptions { llc_triple :: !(Maybe String)
               , llc_opt_level :: !Int
               }

-- | Default LLC options
defaultLLCOptions :: LLCOptions
defaultLLCOptions = LLCOptions { llc_triple = Nothing
                               , llc_opt_level = 0
                               }

-- | Use 'llc' to create assembly from LLVM bitcode.
--
-- The contents of the input file may be either an assembly (.ll) file or a
-- bitcode (.bc) file.  This returns assembled output as a bytestring, or the reason
-- t conversion failed.
run_llc :: FilePath      -- ^ Path to llc
        -> LLCOptions    -- ^ Triple for LLC
        -> BS.ByteString -- ^ Contents of input file.
        -> ExceptT Failure IO BS.ByteString
run_llc llc_command opts input_file = do
  let llc_triple_args =
        case llc_triple opts of
          Just arch -> [ "-mtriple=" ++ arch ]
          Nothing   -> []
  let llc_opt_args
          | 0 <= lvl && lvl <= 3 = [ "-O=" ++ show lvl ]
          | otherwise            = error "run_llc given bad optimization level."
        where lvl = llc_opt_level opts
  let llc_args
        = [ "-o", "-" ]
        ++ llc_triple_args
        ++ llc_opt_args
  withCreateProcess "llc" llc_command llc_args $ \(in_handle, out_handle, err_handle) -> do
    writeAndClose in_handle err_handle $ do
      hSetBinaryMode in_handle True
      BS.hPut in_handle input_file
    liftIO $ readContents out_handle

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
  withCreateProcess "llvm-mc" cmd args $ \(in_handle, out_handle, err_handle) -> do
    -- Write to input handle and close it.
    writeAndClose in_handle err_handle $ do
      hSetBinaryMode in_handle True
      BS.hPut in_handle asm
    -- Get output
    liftIO (readContents out_handle)


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
  withCreateProcess "gas" gas_command args $ \(in_handle, out_handle, err_handle) -> do
    -- Write to input handle and close it.
    writeAndClose in_handle err_handle $ do
      hSetBinaryMode in_handle True
      BS.hPut in_handle asm
    -- Close output
    liftIO $ hClose out_handle

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
  withCreateProcess "opt" opt_command opt_args $ \(in_handle, out_handle, err_handle) -> do
    writeAndClose in_handle err_handle $ do
      hSetBinaryMode in_handle True
      writeInput in_handle
    liftIO $ readContents out_handle

-- | Use LLVM link to combine several ll or bc files into one.
run_llvm_link :: FilePath -- ^ Path to llvm-link
              -> [FilePath]
              -> ExceptT Failure IO BS.ByteString
run_llvm_link llvm_link_command paths = do
  let args = [ "-o=-" ] ++ paths
  withCreateProcess "llvm-link" llvm_link_command args $ \(in_handle, out_handle, _err_handle) -> do
    liftIO $ hClose in_handle
    liftIO $ readContents out_handle
