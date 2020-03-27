{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LLVMLoader
  ( getLLVMModule
  ) where

import           Control.Exception
import qualified Data.ByteString as BS
import           Data.LLVM.BitCode
import           GHC.IO.Exception (IOErrorType( ResourceVanished ))
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Error
import qualified System.Process as P
import           Text.LLVM hiding (comment, (:>), Value, LayoutSpec(..))

-- | Wait for a process to end and throw an error case if the return
-- value differs from the 'ExitSuccess'.
--
-- This function takes a handle to the process's stderr and closes it
-- or semicloses it before returning.
waitForEnd :: String -- ^ Name of tool
           -> Handle -- ^ Handle to read from for getting error
           -> P.ProcessHandle -- ^ Handle to process
           -> IO ()
waitForEnd tool errHandle ph = do
  ec  <- P.waitForProcess ph
  case ec of
    ExitSuccess ->
      hClose errHandle
    ExitFailure c -> do
      msg <- hGetContents errHandle
      let msg' | null msg = tool ++ " exited with error code " ++ show c ++ "."
               | otherwise = tool ++ ": " ++ msg
      hPutStrLn stderr msg'

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
                  -> ((Handle, Handle, Handle) -> IO a)
                  -> IO a
withCreateProcess nm cmd args f = do
  let cp = (P.proc cmd args)
                { P.env = Nothing
                , P.std_in  = P.CreatePipe
                , P.std_out = P.CreatePipe
                , P.std_err = P.CreatePipe
                }
  let h :: IOException -> IO a
      h ioe
        | isDoesNotExistError ioe = do
            hPutStrLn stderr "Could not find llvm-as"
            exitFailure
        | isPermissionError ioe = do
            hPutStrLn stderr "Do not have permission to execute llvm-as"
            exitFailure
        | otherwise = throwIO ioe
  (Just in_handle, Just out, Just err, ph) <-
     P.createProcess cp `catch` h
  f (in_handle, out, err) <* waitForEnd nm err ph

-- | This is a helper function that runs an IO action and ensures that
-- some cleanup is performed if an exception is thrown.
writeAndClose :: Handle -- ^ Input handle to write to.
              -> Handle -- ^ Error handle to close if things fail.
              -> IO () -- ^ Action to run
              -> IO ()
writeAndClose inHandle errHandle action = do
  let h :: IOError -> IO ()
      h e | ioeGetErrorType e == ResourceVanished = do
              hClose inHandle
              hPutStrLn stderr =<< hGetContents errHandle
              exitFailure
          | otherwise = do
              hClose inHandle
              hClose errHandle
              throwIO (e :: IOError)
  action `catch` h
  hClose inHandle

-- | Run llvm-as to generate an bitcode file from text.
runLlvmAs :: FilePath
          -- ^ Path to llvm-as
          -> BS.ByteString
          -- ^ .ll file file
          -> IO BS.ByteString
runLlvmAs cmd asm = do
  -- Run GNU asssembler
  let args= []
  withCreateProcess "llvm-as" cmd args $ \(inHandle, outHandle, errHandle) -> do
    -- Write to input handle and close it.
    writeAndClose inHandle errHandle $ do
      hSetBinaryMode inHandle True
      BS.hPut inHandle asm
    -- Get output
    hSetBinaryMode outHandle True
    BS.hGetContents outHandle

-- | Parse thte Get LLVM module
getLLVMBCModule :: BS.ByteString -> IO Module
getLLVMBCModule bs = do
  res <- parseBitCode bs
  case res of
    Left err -> do
      hPutStrLn stderr $ "Could not parse LLVM: " ++ show err
      exitFailure
    Right m ->
      pure m

-- | Parse the LLVM  module either in .bc or .ll format.
getLLVMModule :: FilePath -> IO Module
getLLVMModule path = do
  mbs <- try $ BS.readFile path
  case mbs of
    Left (err :: IOError) -> do
      if isDoesNotExistError err then
        hPutStrLn stderr $ "Could not find LLVM module: " ++  path
       else do
        hPutStrLn stderr $ "Error reading: " ++  path
        hPutStrLn stderr $ show err
      exitFailure
    Right bs
      | BS.take 4 bs == "BC\xc0\xde" -> do
          getLLVMBCModule bs
      | takeExtension path == ".ll" -> do
          bcBS <- runLlvmAs "llvm-as" bs
          getLLVMBCModule bcBS
      | otherwise -> do
          hPutStrLn stderr $ "Could not determine type of LLVM file: " ++ path ++ "\n"
                          ++ show (BS.take 4 bs)
          exitFailure
