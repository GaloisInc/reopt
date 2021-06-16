{-# LANGUAGE OverloadedStrings #-}

-- |
-- Utilities for invoking Occam.
module Reopt.Occam
  ( -- * Running Occam
    ReoptOccamConfig (..),
    toOccamManifest,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

------------------------------------------------------------------------
-- OCCAM integration

-- | User configuration for SRI's OCCAM tool when used by Reopt.
--
-- Essentially a subset and combination of the information contained
-- in an OCCAM manifest file and the command line options
-- to OCCAM's `slash` tool.
data ReoptOccamConfig = ReoptOccamConfig
  { -- | Command line options for OCCAM's `slash` tool (not including
    -- the manifest file).
    occamSlashOptions :: [Text],
    -- | User specified `ldflags` entry -- we explicitly capture this because we add to it
    -- if it already exists.
    occamLDFlags :: [Text]
  }

instance Aeson.FromJSON ReoptOccamConfig where
  parseJSON (Aeson.Object userData) = do
    slashOpts <- getJSONStringListFromObjField (pure []) "slash_options" userData
    ldFlags <- getJSONStringListFromObjField (pure []) "ldflags" userData
    pure $
      ReoptOccamConfig
        { occamSlashOptions = slashOpts,
          occamLDFlags = ldFlags
        }
  parseJSON js = fail $ "Expected a JSON object describing the OCCAM config but got " ++ show js

getJSONStringListFromObjField :: Aeson.Parser [Text] -> Text -> HashMap Text Aeson.Value -> Aeson.Parser [Text]
getJSONStringListFromObjField dflt fieldName obj = do
  case HM.lookup fieldName obj of
    Nothing -> dflt
    Just (Aeson.Array elems) -> do
      let getStr (Aeson.String txt) = pure txt
          getStr other = fail $ "Expected a string but got " ++ (show other)
      mapM getStr $ V.toList elems
    Just other -> fail $ "Expected a JSON array of strings in object field `" ++ (T.unpack fieldName) ++ "` but got " ++ (show other)

-- | Generate an OCCAM manifest from a Reopt config for OCCAM.
toOccamManifest ::
  -- | Path to LLVM input
  FilePath ->
  -- | Name of the original program
  String ->
  -- | Name (not path) of file to emit after optimization
  String ->
  -- | LD Flags to provide to
  [Text] ->
  Aeson.Value
toOccamManifest inputPath progName bcOutFile ldFlags =
  Aeson.Object $
    HM.insert "main" (Aeson.String $ T.pack inputPath) $
      HM.insert "binary" (Aeson.toJSON bcOutFile) $
        HM.insert "name" (Aeson.String $ T.pack progName) $
          HM.insert "ldflags" (Aeson.toJSON $ ["-c", "-emit-llvm"] ++ ldFlags) $
            HM.empty