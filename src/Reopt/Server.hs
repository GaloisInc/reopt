{-# LANGUAGE OverloadedStrings #-}

module Reopt.Server (runServer, Symbol (..)) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Monad.Cont (
  MonadIO (..),
  MonadTrans (lift),
  when,
 )
import Control.Monad.Reader (
  MonadReader (ask),
  ReaderT (runReaderT),
 )
import Control.Monad.State (
  MonadState,
  StateT (StateT),
  evalStateT,
  execStateT,
  gets,
  modify,
 )
import Data.Aeson qualified as A
import Data.Aeson.Encoding qualified as A
import Data.Aeson.KeyMap qualified as A
import Data.Attoparsec.ByteString qualified as AT
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.UTF8 qualified as UTF8
import Data.ElfEdit qualified as Elf
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Scientific qualified as Sci
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding.Base64 qualified as Base64
import Data.Vector qualified as V
import Data.Word (Word64)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, stdin, stdout)
import Text.Printf (printf)

import Data.Macaw.Utils.IncComp (
  ContT (..),
  IncCompM,
  processIncCompLogs,
  runIncCompM,
 )

import Data.Functor (($>))
import Reopt
import Reopt.TypeInference.FunTypeMaps (symAddrMapContents)

-------------------------------------------------------------------------------
-- Encode

class Encode a where
  encode :: a -> A.Encoding

instance Encode String where
  encode = A.string

instance Encode Word64 where
  encode = A.word64

encodeList :: Encode a => [a] -> A.Encoding
encodeList = A.list encode

instance Encode a => Encode (V.Vector a) where
  encode = encodeList . V.toList

(.=) :: A.Key -> A.Encoding -> A.Series
nm .= val = A.pair nm val
infix 7 .=

sanitizeString :: BS.ByteString -> A.Encoding
sanitizeString = A.string . UTF8.toString

-------------------------------------------------------------------------------
-- Decoding

type Parser = Either String

decodeObject :: A.Value -> Parser A.Object
decodeObject (A.Object x) = pure x
decodeObject _ = Left "Expected string."

decodeField :: A.Object -> A.Key -> Parser A.Value
decodeField o nm = do
  case A.lookup nm o of
    Nothing -> Left $ "Missing " <> show nm <> " field."
    Just x -> pure x

decodeStringField :: A.Object -> A.Key -> Parser Text
decodeStringField o nm = do
  v <- decodeField o nm
  case v of
    A.String x -> pure x
    _ -> Left $ "Field " <> show nm <> " is not a string."

decodeNumberField :: A.Object -> A.Key -> Parser Sci.Scientific
decodeNumberField o nm = do
  v <- decodeField o nm
  case v of
    A.Number x -> pure x
    _ -> Left $ "Field " <> show nm <> " is not a string."

decodeWord64Field :: A.Object -> A.Key -> Parser Word64
decodeWord64Field o nm = do
  s <- decodeNumberField o nm
  case Sci.toBoundedInteger s of
    Nothing -> Left $ "Expected " <> show nm <> " to be a valid Word64."
    Just x -> pure x

decodeBase64Field :: A.Object -> A.Key -> Parser BS.ByteString
decodeBase64Field o nm = do
  t <- decodeStringField o nm
  -- NOTE: This may be incorrect, I was only following the types to remove a
  -- dependency on base-encoding.
  case Base64.decodeBase64 t of
    Left _ -> Left "Base64 decoding failed."
    Right r -> Right $ encodeUtf8 r

-----------------------------------------------------------------------
-- Server types

-- | Identifier to indicate a particular binary.
type JobId = Word64

data Symbol = Symbol
  { symName :: !BS.ByteString
  , symRegion :: !Int
  -- ^ Region index:
  --  0 for absolute
  --  1+ for relative to some offset
  --    base offset for dynamically linked executables, shared libraries, object files)
  --  Numbers greater than 1 only appear in object files with function sections.
  , symAddr :: !Word64
  }

mkSymbol :: BS.ByteString -> MemSegmentOff w -> Symbol
mkSymbol nm o =
  let a = segoffAddr o
   in Symbol
        { symName = nm
        , symRegion = addrBase a
        , symAddr = memWordValue (addrOffset a)
        }

instance Encode Symbol where
  encode s = A.pairs ("name" .= sanitizeString (symName s) <> "addr" .= encode (symAddr s))

data ServerOutput
  = Ready --
  | OpenStarted !JobId !(V.Vector Symbol) ![String]
  | -- | Indicates request to decode failed with the given message
    DecodingFailed !JobId !Text
  | -- | Indicate no more messages about job will be received
    DecodingStopped !JobId
  | -- | Fatal error
    FatalError !String

encodeServerOutput :: Text -> A.Series -> A.Encoding
encodeServerOutput t s = A.pairs ("tag" .= A.text t <> s)

instance Encode ServerOutput where
  encode s =
    case s of
      Ready -> encodeServerOutput "ready" mempty
      OpenStarted jobId symbols warnings ->
        encodeServerOutput "start" $
          "job"
            .= encode jobId
            <> "symbol"
            .= encode symbols
            <> "warnings"
            .= encodeList warnings
      DecodingFailed jobId msg ->
        encodeServerOutput "failed" $ "job" .= encode jobId <> "message" .= A.text msg
      DecodingStopped jobId ->
        encodeServerOutput "stop" $ "job" .= encode jobId
      FatalError msg ->
        encodeServerOutput "fatal" $ "message" .= A.string msg

-------------------------------------------------------------------------------
-- Server

newtype ServerState = ServerState
  { ssJobs :: Map JobId ()
  }

initialServerState :: ServerState
initialServerState =
  ServerState
    { ssJobs = Map.empty
    }

newtype ServerM a = ServerM (StateT ServerState (ContT (Maybe ServerState) IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState ServerState)

output :: ServerOutput -> ServerM ()
output s = do
  liftIO $ BSL.hPut stdout $ A.encodingToLazyByteString (encode s) <> "\n"

abort :: ServerM a
abort = ServerM $ StateT $ \_ -> ContT $ \_ -> pure Nothing

fatalError :: String -> ServerM a
fatalError msg = do
  output $ FatalError msg
  liftIO exitFailure

newtype Server = Server
  { serverStateVar :: MVar ServerState
  }

withServer :: Server -> ServerM () -> IO ()
withServer s (ServerM m) =
  modifyMVar_ (serverStateVar s) $ \ss -> do
    mr <- runContT (execStateT m ss) (pure . Just)
    pure $! fromMaybe ss mr

-----------------------------------------------------------------------
-- Open

-- | Open monad with warnings in reverse order.
type OpenM = ReaderT JobId (StateT [String] ServerM)

runOpen :: JobId -> OpenM a -> ServerM a
runOpen jobId m = evalStateT (runReaderT m jobId) []

getOpenJobId :: OpenM JobId
getOpenJobId = ask

openServerM :: ServerM a -> OpenM a
openServerM = lift . lift

openFailed :: String -> OpenM a
openFailed msg = do
  jobId <- getOpenJobId
  openServerM $ do
    output $ DecodingFailed jobId (Text.pack msg)
    abort

openWarning :: String -> OpenM ()
openWarning msg = modify (msg :)

getWarnings :: OpenM [String]
getWarnings = gets reverse

decodeHeader :: BS.ByteString -> OpenM (Elf.ElfHeaderInfo 64)
decodeHeader bytes = do
  case Elf.decodeElfHeaderInfo bytes of
    Right (Elf.SomeElf hdrInfo) -> do
      case Elf.headerClass (Elf.header hdrInfo) of
        Elf.ELFCLASS64 -> do
          pure hdrInfo
        Elf.ELFCLASS32 -> do
          openFailed "Elf file must be 64-bit."
    Left (_, msg) -> do
      openFailed msg

openInit :: IncCompM String (Either String a) a -> OpenM a
openInit comp = do
  mr <- processIncCompLogs openWarning (runIncCompM (fmap Right comp))
  case mr of
    Left msg -> openFailed msg
    Right r -> pure r

-- | Interpret the given bytestring as a Elf64 file.
doOpen :: BS.ByteString -> OpenM ()
doOpen bytes = do
  jobId <- getOpenJobId
  -- Check job unique
  do
    m <- openServerM $ gets ssJobs
    when (Map.member jobId m) $ openServerM $ fatalError "Job already created"

  -- Decode header
  hdrInfo <- decodeHeader bytes
  let hdr = Elf.header hdrInfo
  -- Check machine
  when (Elf.headerMachine hdr /= Elf.EM_X86_64) $ do
    openFailed $ printf "Do not support %s binaries." (show (Elf.headerMachine hdr))
  -- Check ABI
  os <-
    case x86OSForABI (Elf.headerOSABI hdr) of
      Just os -> pure os
      Nothing -> openWarning (warnABIUntested (Elf.headerOSABI hdr)) $> Linux

  openServerM $ modify $ \s -> s{ssJobs = Map.insert jobId () (ssJobs s)}

  let loadOpts = defaultLoadOptions
  let reoptOpts = defaultReoptOptions
  let ainfo = osArchitectureInfo os
  let pltFn = processX86PLTEntries

  initState <- openInit $ doInit loadOpts hdrInfo ainfo pltFn reoptOpts
  let symAddrMap = initDiscSymAddrMap initState
  let symbols = uncurry mkSymbol <$> V.fromList (symAddrMapContents symAddrMap)
  w <- getWarnings
  openServerM $ output (OpenStarted jobId symbols w)

openBinary :: JobId -> BS.ByteString -> ServerM ()
openBinary jobId bytes = runOpen jobId (doOpen bytes)

-----------------------------------------------------------------------
-- ServerInput

data ServerInput
  = -- | Request to start decoding the given file.
    OpenBinary !JobId !BS.ByteString
  | -- | Stop the given binary
    CloseBinary !JobId
  | -- | Shutdown the server
    Shutdown

decode :: A.Value -> Either String ServerInput
decode v = do
  o <- decodeObject v
  tag <- decodeStringField o "tag"
  case tag of
    "open" -> do
      jobId <- decodeWord64Field o "job"
      dta <- decodeBase64Field o "data"
      pure $! OpenBinary jobId dta
    "close" -> do
      jobId <- decodeWord64Field o "job"
      pure $ CloseBinary jobId
    "shutdown" -> do
      pure Shutdown
    _ -> do
      Left $ "Unexpected tag: " <> show tag

processInput :: ServerInput -> ServerM ()
processInput (OpenBinary jobId bytes) = do
  openBinary jobId bytes
processInput (CloseBinary _job) = do
  pure ()
processInput Shutdown = do
  pure ()

-------------------------------------------------------------------------------
-- Server running

initialServer :: IO Server
initialServer = do
  serverVar <- newMVar $! initialServerState
  pure $! Server{serverStateVar = serverVar}

doRun :: Server -> IO ()
doRun s = do
  l <- BS.hGetLine stdin
  case AT.parseOnly A.json' l >>= decode of
    Left msg -> do
      hPutStrLn stderr $ "Parse error: " ++ msg
      exitFailure
    Right input -> do
      withServer s $ processInput input
      case input of
        Shutdown -> pure ()
        _ -> doRun s

-- | This command is called when reopt is called to run in server mode.
--
-- All output is written to stdout and input is read from stdin.
runServer :: IO ()
runServer = do
  server <- initialServer
  withServer server $ output Ready
  doRun server
