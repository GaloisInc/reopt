{-# OPTIONS_GHC -Wall #-}
module Assembler ( generateObjectFromBitcode ) where

import qualified Data.ByteString as BS
import           System.Exit
import           System.IO.Temp
import           System.Process

-- | Create assembly from LLVM bitcode.
--
-- Returns assembled output as a bytestring.
llvmAssemble :: FilePath -- ^ Path to llc
             -> BS.ByteString -- ^ bytecode file
             -> IO BS.ByteString
llvmAssemble llc_command bc_file = do
  let cp = (proc llc_command [ "-o", "-" ])
                { env = Nothing
                , std_in  = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                }
  (Just in_handle, Just out_handle, Just _err, ph) <- createProcess cp
  BS.hPut in_handle bc_file
  res <- BS.hGetContents out_handle
  ec <- waitForProcess ph
  case ec of
    ExitSuccess ->
      return $ res
    ExitFailure c ->
      fail $ llc_command ++ " failed with code " ++ show c

-- | Use gas to generate an object file from assembly.
assembleFile :: FilePath -- ^ Path to gas
             -> BS.ByteString -- ^ Assembler file
             -> FilePath      -- ^ Path for object file.
             -> IO ()
assembleFile gas_command asm obj_path = do
  let cp = (proc gas_command [ "-o", obj_path, "--fatal-warnings" ])
                { env = Nothing
                , std_in  = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                }
  (Just in_handle, Just _out, Just _err, ph) <- createProcess cp
  BS.hPut in_handle asm
  ec <- waitForProcess ph
  case ec of
    ExitSuccess ->
      return ()
    ExitFailure c ->
      fail $ gas_command ++ " failed with code " ++ show c

-- | Create an object file from bitcode.
writeObjectFileFromBitcode :: FilePath -- ^ Path to llc
                           -> FilePath -- ^ Path to gas
                           -> BS.ByteString -- ^ LLVM bitcode
                           -> FilePath -- ^ Path for object file.
                           -> IO ()
writeObjectFileFromBitcode llc_command gas_command bc obj_path = do
  asm <- llvmAssemble llc_command bc
  assembleFile gas_command asm obj_path

-- | Generate a bytecode file from an object.
generateObjectFromBitcode :: FilePath -- ^ Path to llc
                          -> FilePath -- ^ Path to gas
                          -> String   -- ^ Template for temporary file
                          -> BS.ByteString -- ^ LLVM bitcode
                          -> IO BS.ByteString
generateObjectFromBitcode llc_command gas_command obj_template bc = do
  withSystemTempFile obj_template $ \obj_path _ -> do
    writeObjectFileFromBitcode llc_command gas_command bc obj_path
    BS.readFile obj_path

{-

main :: IO ()
main = do
  let llc_command = "llc-3.5"
  let gas_command = "as-new"
  let bc_path  = "tree.bc"
  let temp_template = "tree_temp.o"
  let obj_path = "tree.o"

  putStrLn $ "Writing object to " ++ obj_path
  bc <- BS.readFile bc_path
  obj <- generateObjectFromBitcode llc_command gas_command temp_template bc
  BS.writeFile obj_path obj
-}
