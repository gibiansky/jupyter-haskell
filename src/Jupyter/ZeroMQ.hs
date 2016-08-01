{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Jupyter.ZeroMQ (
    withKernelSockets,
    KernelSockets(..),
    withClientSockets,
    ClientSockets(..),
    sendMessage,
    receiveMessage,
    KernelProfile(..),
    Port,
    IP,
    Transport(..),
    readProfile,
    messagingError,
    MessagingException(..),
    mkRequestHeader,
    mkReplyHeader,
    ) where

import           Data.Monoid ((<>))
import           Data.ByteString (ByteString)
import           Control.Monad (void, unless)
import           Text.Read (readMaybe)
import           Data.Char (isNumber)
import qualified Data.Map as Map
import           Control.Exception (throwIO, Exception)
import           Control.Monad.Catch (catch)
import           Data.Proxy (Proxy(..))

import           Data.Aeson (FromJSON(..), Value(..), (.:), ToJSON(..), encode, decode, (.=), object,
                             eitherDecodeStrict')
import           Data.Aeson.Types (parseEither)
import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as T
import           Data.Digest.Pure.SHA as SHA

import           System.ZMQ4.Monadic (Socket, ZMQ, runZMQ, socket, Rep(..), Router(..), Pub(..),
                                      Dealer(..), Req(..), Sub(..), Flag(..), send, receive, Receiver,
                                      Sender, lastEndpoint, bind, connect, subscribe, ZMQError, setIdentity, restrict)

import           Jupyter.Messages.Metadata (MessageHeader(..), IsMessage(..), Username(..))
import qualified Jupyter.UUID as UUID


parseMessage :: forall v. IsMessage v
             => [ByteString]
             -> ByteString
             -> ByteString
             -> ByteString
             -> ByteString
             -> Either String (MessageHeader, v)
parseMessage identifiers headerData parentHeaderData metadata content = do
  header <- parseHeader identifiers headerData parentHeaderData metadata
  case parseMessageContent (messageType header) of
    Nothing -> Left $ "Unrecognize message type: " ++ show (messageType header)
    Just parser ->  do
      value <-  eitherDecodeStrict' content
      case value of
        Object obj -> (header,) <$> parseEither parser obj
        _        -> Left $ "Expected object when parsing message, but got: " ++ show value

-- | Attempt to parse a message header.
parseHeader :: [ByteString]
            -> ByteString
            -> ByteString
            -> ByteString
            -> Either String MessageHeader
parseHeader identifiers headerData parentHeaderData metadata = do
  header <- eitherDecodeStrict' headerData

  let messageIdentifiers = identifiers
  messageParent <- if parentHeaderData == "{}"
                     then return Nothing
                     else Just <$> parseHeader identifiers parentHeaderData "{}" metadata
  messageType <- parseEither (.: "msg_type") header
  messageUsername <- parseEither (.: "username") header
  messageId <- parseEither (.: "msg_id") header
  messageSession <- parseEither (.: "session") header
  messageMetadata <- eitherDecodeStrict' metadata

  return MessageHeader { .. }

data ClientSockets z =
       ClientSockets
         { clientHeartbeatSocket :: Socket z Req
         , clientControlSocket :: Socket z Dealer
         , clientShellSocket :: Socket z Dealer
         , clientStdinSocket :: Socket z Dealer
         , clientIopubSocket :: Socket z Sub
         }

-- | The collection of <http://zeromq.org/ ZeroMQ> sockets needed to communicate with Jupyter on the
-- <https://jupyter- Jupyter messaging wire protocol>.
--
-- Roles of different sockets are described
-- <https://jupyter-client.readthedocs.io/en/latest/messaging.html#introduction here>.
data KernelSockets z =
       KernelSockets
         { kernelHeartbeatSocket :: Socket z Rep  -- ^ The /heartbeat/ socket, which echoes anything sent to
                                            -- it immediately and is used by frontends solely to
                                            -- confirm that the kernel is still alive.
         , kernelControlSocket :: Socket z Router -- ^ The /control/ socket, optionally used to send
                                            -- 'ClientRequest' messages that deserve immediate
                                            -- response (shutdown requests, etc), rather than
                                            -- long-running requests (execution).
         , kernelShellSocket :: Socket z Router   -- ^ The /shell/ socket, used to send the majority of
                                            -- 'ClientRequest' messages (introspection, completion,
                                            -- execution, etc) and their responses.
         , kernelStdinSocket :: Socket z Router   -- ^ The /stdin/ socket, used for communication from a
                                            -- kernel to a single frontend; currently only used for
                                            -- retrieving standard input from the user, hence the
                                            -- socket name.
         , kernelIopubSocket :: Socket z Pub      -- ^ The /iopub/ socket, used for publishing 'KernelOutput's
                                            -- to all frontends.
         }

-- | A TCP port, encoded as an integer.
type Port = Int

-- | An IP address, encoded as a string.
type IP = String

-- | The transport mechanism used to communicate with the Jupyter frontend.
data Transport = TCP -- ^ Default transport mechanism via TCP.
  deriving (Eq, Ord, Show, Read)

-- | Decode a transport mechanism from a JSON string.
instance FromJSON Transport where
  parseJSON (String "tcp") = return TCP
  parseJSON _ = fail "Could not parse transport, expecting string \"tcp\""

-- | Encode a transport mechanism as a JSON string.
instance ToJSON Transport where
  toJSON TCP = "tcp"

-- | Exception to throw when the messaging protocol is not being observed.
--
-- See 'messagingError'.
data MessagingException = MessagingException String 
  deriving (Eq, Ord, Show)

instance Exception MessagingException

-- | Throw a 'MessagingException' with a descriptive error message.
--
-- Should be used when the messaging protocol is not being properly observed or in other
-- unrecoverable situations.
messagingError :: MonadIO m 
               => String -- ^ Module name in which error happened.
               -> String -- ^ Error message.
               -> m a
messagingError moduleName msg =
  liftIO $ throwIO $ MessagingException $ concat [moduleName, ": ", msg]

-- | A kernel profile, specifying how the kernel communicates.
--
-- The kernel profile is usually obtained by a kernel by parsing the connection file passed to it as
-- an argument as indicated by the kernelspec.
--
-- The @profileTransport@, @profileIp@ and five @profile\*Port@ fields specify five ports which the
-- kernel should bind to using ZeroMQ. New ports are chosen at random for each kernel started.
--
-- @profileSignatureKey@ is used to cryptographically sign messages, so that other users on the
-- system can’t send code to run in this kernel. See the
-- <http://jupyter-client.readthedocs.io/en/latest/messaging.html#wire-protocol wire protocol
-- documentation> for the details of how this signature is calculated.
--
-- More info on the fields of the connection file and the 'KernelProfile' is available in the
-- <http://jupyter-client.readthedocs.io/en/latest/kernels.html#connection-files respective Jupyter
-- documentation>.
data KernelProfile =
       KernelProfile
         { profileIp :: IP                     -- ^ The IP on which to listen.
         , profileTransport :: Transport       -- ^ The transport mechanism.
         , profileStdinPort :: Port            -- ^ The stdin channel port.
         , profileControlPort :: Port          -- ^ The control channel port.
         , profileHeartbeatPort :: Port        -- ^ The heartbeat channel port.
         , profileShellPort :: Port            -- ^ The shell command port.
         , profileIopubPort :: Port            -- ^ The IOPub port.
         , profileSignatureKey :: ByteString   -- ^ The HMAC encryption key.
         }
  deriving (Eq, Ord, Show, Read)

-- | Decode a 'KernelProfile' from a JSON object.
--
-- This object is passed to kernels in the connection file.
instance FromJSON KernelProfile where
  parseJSON (Object o) = do
    -- Check that the signature scheme is as expected.
    signatureScheme <- o .: "signature_scheme"
    unless (signatureScheme == "hmac-sha256") $
      fail $ "Unsupported signature scheme: " ++ signatureScheme

    profileIp <- o .: "ip"
    profileTransport <- o .: "transport"
    profileStdinPort <- o .: "stdin_port"
    profileControlPort <- o .: "control_port"
    profileHeartbeatPort <- o .: "hb_port"
    profileShellPort <- o .: "shell_port"
    profileIopubPort <- o .: "iopub_port"
    profileSignatureKey <- T.encodeUtf8 <$> o .: "key"
    return KernelProfile { .. }

  parseJSON _ = fail "Expecting object for parsing KernelProfile"

instance ToJSON KernelProfile where
  toJSON KernelProfile { .. } =
    object
      [ "ip" .= profileIp
      , "transport" .= profileTransport
      , "stdin_port" .= profileStdinPort
      , "control_port" .= profileControlPort
      , "hb_port" .= profileHeartbeatPort
      , "shell_port" .= profileShellPort
      , "iopub_port" .= profileIopubPort
      , "key" .= T.decodeUtf8 profileSignatureKey
      , "signature_scheme" .= ("hmac-sha256" :: String)
      ]

-- | Read a 'KernelProfile' from a file. This file (the connection file) should contain a
-- JSON-encoded object with all necessary fields, as described in the
-- <https://jupyter-client.readthedocs.io/en/latest/kernels.html#connection-files connection files>
-- section of the Jupyter documentation.
--
-- If the file contents cannot be parsed, 'Nothing' is returned.
readProfile :: FilePath -> IO (Maybe KernelProfile)
readProfile path = decode <$> LBS.readFile path

-- | Create and bind all ZeroMQ sockets used for serving a Jupyter kernel. Store info about the
-- created sockets in a 'KernelProfile', and then run a 'ZMQ' action, providing the used
-- 'KernelProfile' and the sockets themselves in a 'JupyterSockets' record.
withKernelSockets :: Maybe KernelProfile -- ^ Optionally, specify how the ZeroMQ sockets should be
                                          -- opened, including the ports on which they should be
                                          -- opened. If 'Nothing' is provided, ports are chosen
                                          -- automatically, and a 'KernelProfile' is generated with
                                          -- the chosen ports.
                   -> (forall z. KernelProfile -> KernelSockets z -> ZMQ z a) -- ^ Callback to
                                                                               -- invoke with the
                                                                               -- socket info and
                                                                               -- ZeroMQ sockets.
                   -> IO a
withKernelSockets mProfile callback = runZMQ $ do
  kernelHeartbeatSocket <- socket Rep
  kernelControlSocket <- socket Router
  kernelShellSocket <- socket Router
  kernelStdinSocket <- socket Router
  kernelIopubSocket <- socket Pub

  heartbeatPort <- bindSocket mProfile profileHeartbeatPort kernelHeartbeatSocket
  controlPort   <- bindSocket mProfile profileControlPort   kernelControlSocket
  shellPort     <- bindSocket mProfile profileShellPort     kernelShellSocket
  stdinPort     <- bindSocket mProfile profileStdinPort     kernelStdinSocket
  iopubPort     <- bindSocket mProfile profileIopubPort     kernelIopubSocket

  let profile = KernelProfile
        { profileTransport = maybe TCP profileTransport mProfile
        , profileIp = maybe "127.0.0.1" profileIp mProfile
        , profileHeartbeatPort = heartbeatPort
        , profileControlPort = controlPort
        , profileShellPort = shellPort
        , profileStdinPort = stdinPort
        , profileIopubPort = iopubPort
        , profileSignatureKey = maybe "" profileSignatureKey mProfile
        }

  callback profile KernelSockets { .. }

-- | Create and bind all ZeroMQ sockets used for using a Jupyter kernel from a client. Store info about the
-- created sockets in a 'KernelProfile', and then run a 'ZMQ' action, providing the used
-- 'KernelProfile' and the sockets themselves in a 'JupyterSockets' record.
withClientSockets :: Maybe KernelProfile -- ^ Optionally, specify how the ZeroMQ sockets should be
                                          -- opened, including the ports on which they should be
                                          -- opened. If 'Nothing' is provided, ports are chosen
                                          -- automatically, and a 'KernelProfile' is generated with
                                          -- the chosen ports.
                   -> (forall z. KernelProfile -> ClientSockets z -> ZMQ z a) -- ^ Callback to
                                                                               -- invoke with the
                                                                               -- socket info and
                                                                               -- ZeroMQ sockets.
                   -> IO a
withClientSockets mProfile callback = runZMQ $ do
  clientHeartbeatSocket <- socket Req
  clientControlSocket   <- socket Dealer
  clientShellSocket     <- socket Dealer
  clientStdinSocket     <- socket Dealer
  clientIopubSocket     <- socket Sub

  -- Set the identity of all dealer sockets to the same thing. This is really important only for the
  -- stdin socket – it must have the same identity as the shell socket (see the Note in the stdin
  -- section of the messaging protocol.) If we don't set the identity ourselves, then ZeroMQ will set
  -- its own null-byte-prefixed identity, and the identities will be different, so the client won't be
  -- able to receive the stdin messages from the kernel.
  setIdentity (restrict "TEST") clientShellSocket
  setIdentity (restrict "TEST") clientStdinSocket
  setIdentity (restrict "TEST") clientControlSocket

  heartbeatPort <- connectSocket mProfile 10730 profileHeartbeatPort clientHeartbeatSocket
  controlPort   <- connectSocket mProfile 11840 profileControlPort   clientControlSocket
  shellPort     <- connectSocket mProfile 12950 profileShellPort     clientShellSocket
  stdinPort     <- connectSocket mProfile 13160 profileStdinPort     clientStdinSocket
  iopubPort     <- connectSocket mProfile 14270 profileIopubPort     clientIopubSocket

  -- Subscribe to all topics on the iopub socket!
  -- If we don't do this, then no messages get received on it. 
  subscribe clientIopubSocket ""

  let profile = KernelProfile
        { profileTransport = maybe TCP profileTransport mProfile
        , profileIp = maybe "127.0.0.1" profileIp mProfile
        , profileHeartbeatPort = heartbeatPort
        , profileControlPort = controlPort
        , profileShellPort = shellPort
        , profileStdinPort = stdinPort
        , profileIopubPort = iopubPort
        , profileSignatureKey = maybe "" profileSignatureKey mProfile
        }

  callback profile ClientSockets { .. }

extractAddress :: Maybe KernelProfile -> (KernelProfile -> Int) -> String
extractAddress mProfile accessor = "tcp://127.0.0.1:" ++ maybe "*" (show . accessor) mProfile

connectSocket :: forall z t. Maybe KernelProfile -> Port -> (KernelProfile -> Int) -> Socket z t -> ZMQ z Int
connectSocket mProfile startPort accessor sock = do
  case mProfile of
    Just _  -> connect sock (extractAddress mProfile accessor)
    Nothing -> findRandomPort 100 startPort

  endpoint sock

  where
    findRandomPort 0 _ = fail "fatal error (Jupyter.ZeroMQ): Could not find port to connect to."
    findRandomPort triesLeft tryPort =
      let handler :: ZMQError -> ZMQ z ()
          handler = const $ findRandomPort (triesLeft - 1) (tryPort + 1)
      in connect sock ("tcp://127.0.0.1:" ++ show tryPort) `catch` handler


bindSocket :: Maybe KernelProfile -> (KernelProfile -> Int) -> Socket z t -> ZMQ z Int
bindSocket mProfile accessor sock = do
  bind sock (extractAddress mProfile accessor)
  endpoint sock

endpoint :: Socket z t -> ZMQ z Int
endpoint sock = do
  endpointString <- lastEndpoint sock
  case parsePort endpointString of
    Nothing   -> fail "fatal error (Jupyter.ZeroMQ): could not parse port as integer."
    Just port -> return port

parsePort :: String -> Maybe Int
parsePort s = readMaybe num
  where
    num = reverse (takeWhile isNumber (reverse s))

-- | Read a client message from a ZeroMQ socket.
receiveMessage :: (IsMessage v, Receiver a) => Socket z a -> ZMQ z (Either String (MessageHeader, v))
receiveMessage sock = do
  -- Read all identifiers until the identifier/message delimiter.
  idents <- readUntil sock "<IDS|MSG>"

  -- Ignore the signature for now.
  void $ receive sock

  headerData <- receive sock
  parentHeader <- receive sock
  metadata <- receive sock
  content <- receive sock
  liftIO $ CBS.putStrLn content
  return $ parseMessage idents headerData parentHeader metadata content 

-- | Read data from the socket until we hit an ending string. Return all data as a list, which does
-- not include the ending string.
readUntil :: Receiver a => Socket z a -> ByteString -> ZMQ z [ByteString]
readUntil sock terminator = do
  line <- receive sock
  if line /= terminator
    then do
      remaining <- readUntil sock terminator
      return $ line : remaining
    else return []

encodeHeader :: MessageHeader -> ByteString
encodeHeader MessageHeader { .. } =
  encodeStrict $ object
                   [ "msg_id" .= messageId
                   , "session" .= messageSession
                   , "username" .= messageUsername
                   , "version" .= ("5.0" :: String)
                   , "msg_type" .= messageType
                   ]

mkRequestHeader :: IsMessage v => UUID.UUID -> Username -> v -> IO MessageHeader
mkRequestHeader session username content = do
  uuid <- UUID.random
  return
    MessageHeader
      { messageIdentifiers = []
      , messageParent = Nothing
      , messageMetadata = mempty
      , messageId = uuid
      , messageSession = session
      , messageUsername = username
      , messageType = getMessageType content
      }

mkReplyHeader :: IsMessage v => MessageHeader -> v -> IO MessageHeader
mkReplyHeader parentHeader content = do
  uuid <- UUID.random
  return
    MessageHeader
      { messageIdentifiers = messageIdentifiers parentHeader
      , messageParent = Just parentHeader
      , messageMetadata = mempty
      , messageId = uuid
      , messageSession = messageSession parentHeader
      , messageUsername = messageUsername parentHeader
      , messageType = getMessageType content
      }


-- | Send a Jupyter message on a socket, encoding it as described in the <http://jupyter-client.readthedocs.io/en/latest/messaging.html#wire-protocol wire protocol documentation>.
sendMessage :: (IsMessage v, Sender a)
            => ByteString -- ^ HMAC key used to sign the message.
            -> Socket z a -- ^ Socket on which to send the message.
            -> MessageHeader -- ^ Header for the message.
            -> v -- ^ Data type representing the message to be send.
            -> ZMQ z ()
sendMessage hmacKey sock header content = do
  let parentHeaderStr = maybe "{}" encodeHeader $ messageParent header
      idents = messageIdentifiers header
      metadata = "{}"
      headerStr = encodeHeader header
      contentStr = encodeStrict content

      -- Signature for the message using HMAC SHA-256.
      signature = hmac $ headerStr <> parentHeaderStr <> metadata <> contentStr

  -- Send all pieces of the message.
  mapM_ sendPiece idents
  sendPiece "<IDS|MSG>"
  sendPiece signature
  sendPiece headerStr
  sendPiece parentHeaderStr
  sendPiece metadata

  -- Conclude transmission with content.
  sendLast contentStr

  where
    sendPiece = send sock [SendMore]
    sendLast = send sock []

    -- Compute the HMAC SHA-256 signature of a bytestring message.
    hmac :: ByteString -> ByteString
    hmac = CBS.pack . SHA.showDigest . SHA.hmacSha256 (LBS.fromStrict hmacKey) . LBS.fromStrict

-- Encode to a strict bytestring.
encodeStrict :: ToJSON a => a -> ByteString
encodeStrict = LBS.toStrict . encode
