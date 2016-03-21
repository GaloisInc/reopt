{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
module Assembler
  ( Failure(..)
  , llvm_to_object
  , llvm_link
  , Control.Monad.Trans.Except.ExceptT(..)
  ) where

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import qualified Data.ByteString as BS
import           GHC.IO.Exception
import           System.IO
import           System.IO.Error
import           System.Process

data Tool = Gas | LLC | LLVM_LINK

instance Show Tool where
  show Gas = "GNU assembler (as)"
  show LLC = "llc"
  show LLVM_LINK = "llvm-link"

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


-- | This writes a bytestring to the standard input of a program, and throws a 'Failure'
-- if this fails.
--
-- The input handle is closed after executing this even if an exception is thrown.
-- The standard error is closed or semiclosed if an exception is thrown, and open
-- otherwise.
tryWriteContents :: Tool -> Handle -> Handle -> BS.ByteString -> ExceptT Failure IO ()
tryWriteContents tool in_handle err_handle bs = do
  let h e | ioeGetErrorType e == ResourceVanished = do
              hClose in_handle
              ProgramError tool <$> hGetContents err_handle
          | otherwise = do
              hClose in_handle
              hClose err_handle
              throwIO (e :: IOError)
  liftIO $ hSetBinaryMode in_handle True
  BS.hPut in_handle bs `catchFailure` h
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

-- | Create assembly from LLVM bitcode.
--
-- The contents of the input file may be either an assembly (.ll) file or a
-- bitcode (.bc) file.  This returns assembled output as a bytestring, or the reason
-- the conversion failed.
llvmAssemble :: FilePath -- ^ Path to llc
             -> String
                -- ^ Tripple for LLC
             -> BS.ByteString
                -- ^ Contents of input file.
             -> ExceptT Failure IO BS.ByteString
llvmAssemble llc_command triple input_file = do
  let llc_args = [ "-o", "-", "-mtriple=" ++ triple ]
  (in_handle, out_handle, err_handle, ph) <-
    tryCreateProcess LLC llc_command llc_args
  tryWriteContents LLC in_handle err_handle input_file
  res <- liftIO $ readContents out_handle
  waitForEnd LLC err_handle ph
  return res

-- | Use GNU assembler to generate an object file from assembly.
assembleFile :: FilePath -- ^ Path to gas
             -> BS.ByteString -- ^ Assembler file
             -> FilePath      -- ^ Path for object file.
             -> ExceptT Failure IO ()
assembleFile gas_command asm obj_path = do
  -- Run GNU asssembler
  let args= [ "-o", obj_path, "--fatal-warnings" ]
  (in_handle, out_handle, err_handle, ph) <- tryCreateProcess Gas gas_command args
  -- Write to input handle and close it.
  tryWriteContents Gas in_handle err_handle asm
  -- Close output
  liftIO $ hClose out_handle
  -- Wait for process to end.
  waitForEnd Gas err_handle ph

-- | Use LLVM link to combine several BC files into one.
llvm_link :: FilePath -- ^ Path to llvm-link
          -> [FilePath]
          -> ExceptT Failure IO BS.ByteString
llvm_link llvm_link_command paths = do
  let args = [ "-o=-" ] ++ paths
  (in_handle, out_handle, err_handle, ph) <- tryCreateProcess LLVM_LINK llvm_link_command args
  liftIO $ hClose in_handle
  res <- liftIO $ readContents out_handle
  waitForEnd LLVM_LINK err_handle ph
  return res



-- | Generate an Elf file from LLVM using 'llc' and 'gas', and store it in the path.
--
-- The contents of the input file may be either an assembly (.ll) file or a
-- bitcode (.bc) file.
--
-- This returns a 'Failure object if either command fails.
llvm_to_object :: FilePath      -- ^ Path to llc
                  -> FilePath      -- ^ Path to gas
                  -> String        -- ^ "triple" string describing architecture for llc
                  -> BS.ByteString -- ^ Input llvm as bitcode or ll file.
                  -> FilePath      -- ^ Path to write object to
                  -> ExceptT Failure IO ()
llvm_to_object llc_path gas_command triple bc obj_path = do
  asm <- llvmAssemble llc_path triple bc
  assembleFile gas_command asm obj_path
