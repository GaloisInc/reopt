{-|
Operations for exporting relevant parts of events to JSON.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

module Reopt.Events.Export
  ( exportEvent,
    exportFatalError
  ) where

import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Reopt.Events
import System.IO

-- | Exported function address
type ExportFunId = Word64

data ExportEvent
  -- | @CFGError f b sz idx msg@ indicates an error in a function at the given
  -- function identifier @f@, block identifier @b@, block size @sz@, instruction
  -- index @idx@ and message @msg@.
  = CFGError !ExportFunId !Word64 !Int !Int !Text
  deriving (Generic)

instance Aeson.ToJSON ExportEvent where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

emitEvent :: Handle -> ExportEvent -> IO ()
emitEvent h e = do
  BSL.hPutStr h (Aeson.encode e)
  hPutChar h '\n'
  hFlush h

exportEvent ::
  Handle ->
  ReoptLogEvent arch ->
  IO ()
exportEvent h evt =
  case evt of
    ReoptGlobalStepStarted{} -> pure ()
    ReoptGlobalStepFinished{} -> pure ()
    ReoptGlobalStepWarning{} -> pure ()
    ReoptFunStepStarted{} -> pure ()
    ReoptFunStepFinished{} -> pure ()
    ReoptFunStepFailed fs fId fsErr ->
      case fs of
        Discovery -> do
          forM_ fsErr $ \e -> do
            let f = funIdAddr fId
            let b = discErrorBlockAddr e
            let insn = discErrorBlockInsnIndex e
            let sz = discErrorBlockSize e
            let msg = discErrorMessage e
            emitEvent h $ CFGError f b sz insn msg
        InvariantInference -> do
          pure ()
        AnnotationGeneration -> do
          pure ()
        Recovery -> do
          let f = funIdAddr fId
          let b = segoffWord64 (recoverErrorBlock fsErr)
          let insn = recoverErrorInsnIndex fsErr
          let sz = recoverErrorBlockSize fsErr
          let msg = recoverErrorMessage fsErr
          emitEvent h $ CFGError f b sz insn msg
    ReoptFunStepLog{} -> pure ()
    ReoptFunStepAllFinished{} -> pure ()

exportFatalError ::
  Handle ->
  ReoptFatalError ->
  IO ()
exportFatalError _h _e = do
  pure ()