{-|
Module     : Reopt.ExternalTools
Copyright  : (c) Galois, Inc 2016
Maintainer : jhendrix@galois.com

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
    -- * Running llvm-link
  , run_llvm_link
    -- * Failure information
  , Failure(..)
  , Tool(..)
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
import           System.Process

data Tool = Gas | LLC | LLVM_LINK | OPT

instance Show Tool where
  show Gas = "GNU assembler (as)"
  show LLC = "llc"
  show LLVM_LINK = "llvm-link"
  show OPT = "opt"

-- |  Type of failure from running tools.
data Failure
   = ProgramError !Tool !String
     -- ^ gas returned some error.
   | ProgramNotFound !Tool !String
   | PermissionError !Tool !String

instance Show Failure where
  show (ProgramError _ msg)     = msg
  show (ProgramNotFound _ path) = "Could not find " ++ path ++ "."
  show (PermissionError _ path) = "Do not have permissions to execute " ++ path ++ "."

-- | Try to create a process, and return handles for 'stdin', 'stdout', 'stderr', and
-- the process itself if it succeeds.
--
-- If the attempt fails, because the process does not exist, then throw the given
-- exception.
tryCreateProcess :: Tool
                 -> String
                 -> [String]
                 -> ExceptT Failure IO (Handle, Handle, Handle, ProcessHandle)
tryCreateProcess tool cmd args = do
  let cp = (proc cmd args)
                { env = Nothing
                , std_in  = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                }
  let h :: IOException -> IO (Either Failure a)
      h ioe
        | isDoesNotExistError ioe = return $! Left (ProgramNotFound tool cmd)
        | isPermissionError   ioe = return $! Left (PermissionError tool cmd)
        | otherwise = throwIO ioe
  (Just in_handle, Just out, Just err, ph) <- ExceptT $
     (Right <$> createProcess cp) `catch` h
  return (in_handle, out, err, ph)


catchFailure :: IO a -> (IOError -> IO Failure) -> ExceptT Failure IO a
catchFailure m h = ExceptT $ fmap Right m `catch` (fmap Left . h)


-- | This is a helper function that runs an IO action and ensures that
-- some cleanup is performed if an exception is thrown.
writeAndClose :: Tool
                 -> Handle -- ^ Input handle to write to.
                 -> Handle -- ^ Error handle to close if things fail.
                 -> IO () -- ^ Action to run
                 -> ExceptT Failure IO ()
writeAndClose tool in_handle err_handle action = do
  let h e | ioeGetErrorType e == ResourceVanished = do
              hClose in_handle
              ProgramError tool <$> hGetContents err_handle
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
waitForEnd :: Tool -> Handle -> ProcessHandle -> ExceptT Failure IO ()
waitForEnd tool err_handle ph = do
  ec  <- liftIO $ waitForProcess ph
  case ec of
    ExitSuccess ->
      liftIO $ hClose err_handle
    ExitFailure c -> do
      msg <- liftIO $ hGetContents err_handle
      let msg' | null msg = show tool ++ " exited with error code " ++ show c ++ "."
               | otherwise = show tool ++ ": " ++ msg
      throwE $! ProgramError tool msg'

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
-- the conversion failed.
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
  (in_handle, out_handle, err_handle, ph) <-
    tryCreateProcess LLC llc_command llc_args

  writeAndClose LLC in_handle err_handle $ do
    hSetBinaryMode in_handle True
    BS.hPut in_handle input_file
  res <- liftIO $ readContents out_handle
  waitForEnd LLC err_handle ph
  return res

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
  (in_handle, out_handle, err_handle, ph) <- tryCreateProcess Gas gas_command args
  -- Write to input handle and close it.
  writeAndClose Gas in_handle err_handle $ do
    hSetBinaryMode in_handle True
    BS.hPut in_handle asm
  -- Close output
  liftIO $ hClose out_handle
  -- Wait for process to end.
  waitForEnd Gas err_handle ph

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
  (in_handle, out_handle, err_handle, ph) <-
    tryCreateProcess OPT opt_command opt_args
  writeAndClose OPT in_handle err_handle $ do
    hSetBinaryMode in_handle True
    writeInput in_handle
  res <- liftIO $ readContents out_handle
  waitForEnd OPT err_handle ph
  return res

-- | Use LLVM link to combine several ll or bc files into one.
run_llvm_link :: FilePath -- ^ Path to llvm-link
              -> [FilePath]
              -> ExceptT Failure IO BS.ByteString
run_llvm_link llvm_link_command paths = do
  let args = [ "-o=-" ] ++ paths
  (in_handle, out_handle, err_handle, ph) <- tryCreateProcess LLVM_LINK llvm_link_command args
  liftIO $ hClose in_handle
  res <- liftIO $ readContents out_handle
  waitForEnd LLVM_LINK err_handle ph
  return res
