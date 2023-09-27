-- | Operations for exporting relevant parts of events to JSON.
module Reopt.Events.Export (
  exportEvent,
  exportFatalError,
) where

import Control.Monad (forM_)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import Reopt.Events (
  DiscoveryError (
    discErrorBlockAddr,
    discErrorBlockInsnIndex,
    discErrorBlockSize,
    discErrorMessage
  ),
  FunId (funIdAddr),
  RecoverError (
    recoverErrorBlock,
    recoverErrorBlockSize,
    recoverErrorInsnIndex,
    recoverErrorMessage
  ),
  ReoptFatalError,
  ReoptFunStep (
    AnnotationGeneration,
    Discovery,
    InvariantInference,
    Recovery
  ),
  ReoptLogEvent (..),
  segoffWord64,
 )
import System.IO (Handle, hFlush, hPutChar)

-- | Exported function address
type ExportFunId = Word64

data ExportEvent
  = -- | @CFGError f b sz idx msg@ indicates an error in a function at the given
    -- function identifier @f@, block identifier @b@, block size @sz@, instruction
    -- index @idx@ and message @msg@.
    CFGError !ExportFunId !Word64 !Int !Int !Text
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
    ReoptGlobalStepInfo{} -> pure ()
    ReoptGlobalStepWarning{} -> pure ()
    ReoptFunStepStarted{} -> pure ()
    ReoptFunStepFinished{} -> pure ()
    ReoptFunStepInfo{} -> pure ()
    ReoptFunStepFailed fs fId fsErr ->
      case fs of
        Discovery -> do
          forM_ fsErr $ \e -> do
            let f = funIdAddr fId
            let b = discErrorBlockAddr e
            let insn = discErrorBlockInsnIndex e
            let sz = discErrorBlockSize e
            let msg = discErrorMessage e
            emitEvent h $
              CFGError f b sz insn $
                PP.renderStrict $
                  PP.layoutPretty PP.defaultLayoutOptions msg
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
