{-|
This defines operations that require external tools installed on the system.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -Werror #-}
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
    -- * Running slash
  , run_slash
    -- * Failure information
  , Failure(..)
  , Control.Monad.Trans.Except.ExceptT(..)
  ) where

import           Control.Exception
import           Control.Monad (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Aeson (encodeFile)
import qualified Data.Aeson as Aeson (decodeFileStrict)
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.List (intercalate)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           GHC.IO.Exception
import           System.Directory ( getCurrentDirectory )
import           System.IO
import System.IO.Error
    ( ioeGetErrorType, isDoesNotExistError, isPermissionError )
import           System.Exit (exitFailure)
import           System.FilePath ((</>), (<.>))
import qualified System.Process  as P
import           Reopt.Utils.Builder (builderWriteFile)

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
writeAndClose inHandle errHandle action = do
  let h e | ioeGetErrorType e == ResourceVanished = do
              hClose inHandle
              ProgramError <$> hGetContents errHandle
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
  withCreateProcess "clang" cmd args $ \(inHandle, outHandle, _errHandle) -> do
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
  withCreateProcess "llc" llc_command llc_args $ \(in_handle, out_handle, err_handle) -> do
    writeAndClose in_handle err_handle $ do
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
  withCreateProcess "llvm-mc" cmd args $ \(in_handle, out_handle, err_handle) -> do
    -- Write to input handle and close it.
    writeAndClose in_handle err_handle $ do
      hSetBinaryMode in_handle True
      BS.hPut in_handle asm
    -- Get output
    liftIO $ readContents out_handle

------------------------------------------------------------------------
-- OCCAM integration

-- | User configuration for SRI's OCCAM tool when used by Reopt.
--
-- Essentially a subset and combination of the information contained
-- in an OCCAM manifest file and the command line options
-- to OCCAM's `slash` tool.
data ReoptOccamConfig =
  ReoptOccamConfig
  { occamSlashOptions :: [Text]
  -- ^ Command line options for OCCAM's `slash` tool (not including
  -- the manifest file).
  , occamLDFlags :: [Text]
  -- ^ User specified `ldflags` entry -- we explicitly capture this because we add to it
  -- if it already exists.
  , occamBitcodeFileName :: String
  -- ^ Name of the bitcode file to emit and pass to OCCAM.
  , occamOtherOptions :: HashMap Text Aeson.Value
  -- ^ Manifest entries provided by the user that are passed on to OCCAM/slash.
  }


instance Aeson.FromJSON ReoptOccamConfig where
  parseJSON (Aeson.Object userData) =
    case HM.lookup "main" userData of
      Just (Aeson.String bcFileName) -> do
        slashOpts <- getJSONStringListFromObjField (pure []) "slash_options" userData
        ldFlags <- getJSONStringListFromObjField (pure []) "ldflags" userData
        pure $ ReoptOccamConfig
               { occamSlashOptions = slashOpts
               , occamLDFlags = ldFlags
               , occamBitcodeFileName = T.unpack bcFileName
               , occamOtherOptions = HM.delete "slash_options"
                                     $ HM.delete "ldflags"
                                     $ HM.delete "main" userData
               }
      Just other -> fail $ "Expected \"main\" field of JSON object to contain a string but found " ++ (show other)
      Nothing -> fail $ "Expected a JSON object with a \"main\" entry"
  parseJSON js = fail $ "Expected a JSON object describing the OCCAM config but got " ++ show js

getJSONStringListFromObjField :: Aeson.Parser [Text] -> Text -> HashMap Text Aeson.Value -> Aeson.Parser [Text]
getJSONStringListFromObjField dflt fieldName obj = do
  case HM.lookup fieldName obj of
    Nothing -> dflt
    Just (Aeson.Array elems) -> do
      let getStr (Aeson.String txt) = pure txt
          getStr other = fail $ "Expected a string but got "++ (show other)
      mapM getStr $ V.toList elems
    Just other -> fail $ "Expected a JSON array of strings in object field `"++(T.unpack fieldName)++"` but got "++(show other)

-- | Generate an OCCAM manifest from a Reopt config for OCCAM.
toOccamManifest ::
  ReoptOccamConfig
  -> String -- ^ Name of the original program
  -> String -- ^ Name (not path) of file to emit after optimization
  -> Aeson.Value
toOccamManifest cfg progName bcOutFile =
  Aeson.Object
  $ HM.insert "main" (Aeson.String $ T.pack $ occamBitcodeFileName cfg)
  $ HM.insert "binary" (Aeson.toJSON bcOutFile)
  $ HM.insert "name" (Aeson.String $ T.pack progName)
  $ HM.insert "ldflags" (Aeson.toJSON $ (["-c", "-emit-llvm"] ++ (occamLDFlags cfg)))
  $ occamOtherOptions cfg


-- | Use OCCAM's slash on a bitcode file to optimize and recompile it.
run_slash ::
  FilePath -- ^ Path to slash script on host machine
  -> FilePath -- ^ Configuration file for OCCAM
  -> String -- ^ Name of the original program
  -> Builder.Builder -- ^ Bitcode to optimize and compile with OCCAM's slash
  -> ExceptT Failure IO BS.ByteString
run_slash slash_command cfgPath progName bc = do
  cfg <- do res <- liftIO $ Aeson.decodeFileStrict cfgPath
            case res of
              Nothing -> error $ "Failed to parse JSON in OCCAM config file: "++cfgPath
              Just c -> pure c
  let onBitcodeErr :: IOException -> IO ()
      onBitcodeErr e = do
        hPutStrLn stderr "Error writing bitcode file:"
        hPutStrLn stderr $ "  " <> show e
        exitFailure
  curDir <- liftIO $ getCurrentDirectory
  liftIO $ builderWriteFile (occamBitcodeFileName cfg) bc `catch` onBitcodeErr
  let bcOutFile = (occamBitcodeFileName cfg) <.> "occam"
  let manifest = toOccamManifest cfg progName bcOutFile
  let manifestPath = cfgPath <.> "occam"
  liftIO $ encodeFile manifestPath manifest
  let slashArgs = [manifestPath] ++ (map T.unpack $ occamSlashOptions cfg)
  liftIO $ hPutStrLn stderr $ "running slash "++(intercalate " " slashArgs)
  liftIO $ P.callProcess slash_command slashArgs
  liftIO $ withFile (curDir </> bcOutFile) ReadMode readContents


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
  withCreateProcess "opt" opt_command opt_args $ \(in_handle, out_handle, err_handle) -> do
    writeAndClose in_handle err_handle $ do
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
  withCreateProcess "llvm-link" llvm_link_command args $ \(in_handle, out_handle, _err_handle) -> do
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
  withCreateProcess "gas" gas_command args $ \(in_handle, out_handle, err_handle) -> do
    -- Write to input handle and close it.
    writeAndClose in_handle err_handle $ do
      hSetBinaryMode in_handle True
      BS.hPut in_handle asm
    -- Close output
    liftIO $ hClose out_handle

