module Reopt.Utils.Builder (builderWriteFile) where

import qualified Data.ByteString.Builder as Builder
import System.IO ( withFile, IOMode(WriteMode) )

builderWriteFile :: FilePath -> Builder.Builder -> IO ()
builderWriteFile path bld =
  withFile path WriteMode $ \h -> do
    Builder.hPutBuilder h bld
