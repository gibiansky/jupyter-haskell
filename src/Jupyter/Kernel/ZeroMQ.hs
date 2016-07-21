{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Jupyter.Kernel.ZeroMQ (
    withJupyterSockets,
    JupyterSockets(..),
    sendMessage,
    receiveMessage,
    KernelProfile(..),
    Port,
    IP,
    Transport(..),
    readProfile,
    ) where

import           Data.Monoid ((<>))
import           Data.ByteString (ByteString)
import           Control.Monad (void, unless)
import           Text.Read (readMaybe)
import           Data.Char (isNumber)
import qualified Data.Map as Map

import           Data.Aeson (FromJSON(..), Value(..), (.:), ToJSON(..), encode, decode, (.=), object)
import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as T
import           Data.Digest.Pure.SHA as SHA

import           System.ZMQ4.Monadic (Socket, ZMQ, runZMQ, socket, Rep(..), Router(..), Pub(..),
                                      Flag(..), send, receive, Receiver, Sender, lastEndpoint, bind)

import           Jupyter.Kernel.Parser (parseClientMessage)
import           Jupyter.Messages (Client, Message, Comm, ClientRequest)
import           Jupyter.Messages.Metadata (MessageHeader(..), Username, IsMessage(..))
import qualified Jupyter.UUID as UUID

-- | The collection of <http://zeromq.org/ ZeroMQ> sockets needed to communicate with Jupyter on the
-- <https://jupyter- Jupyter messaging wire protocol>.
--
-- Roles of different sockets are described
-- <https://jupyter-client.readthedocs.io/en/latest/messaging.html#introduction here>.
data JupyterSockets z =
       JupyterSockets
         { heartbeatSocket :: Socket z Rep  -- ^ The /heartbeat/ socket, which echoes anything sent to
                                            -- it immediately and is used by frontends solely to
                                            -- confirm that the kernel is still alive.
         , controlSocket :: Socket z Router -- ^ The /control/ socket, optionally used to send
                                            -- 'ClientRequest' messages that deserve immediate
                                            -- response (shutdown requests, etc), rather than
                                            -- long-running requests (execution).
         , shellSocket :: Socket z Router   -- ^ The /shell/ socket, used to send the majority of
                                            -- 'ClientRequest' messages (introspection, completion,
                                            -- execution, etc) and their responses.
         , stdinSocket :: Socket z Router   -- ^ The /stdin/ socket, used for communication from a
                                            -- kernel to a single frontend; currently only used for
                                            -- retrieving standard input from the user, hence the
                                            -- socket name.
         , iopubSocket :: Socket z Pub      -- ^ The /iopub/ socket, used for publishing 'KernelOutput's
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
withJupyterSockets :: Maybe KernelProfile -- ^ Optionally, specify how the ZeroMQ sockets should be
                                          -- opened, including the ports on which they should be
                                          -- opened. If 'Nothing' is provided, ports are chosen
                                          -- automatically, and a 'KernelProfile' is generated with
                                          -- the chosen ports.
                   -> (forall z. KernelProfile -> JupyterSockets z -> ZMQ z a) -- ^ Callback to
                                                                               -- invoke with the
                                                                               -- socket info and
                                                                               -- ZeroMQ sockets.
                   -> IO a
withJupyterSockets mProfile callback = runZMQ $ do
  heartbeatSocket <- socket Rep
  controlSocket <- socket Router
  shellSocket <- socket Router
  stdinSocket <- socket Router
  iopubSocket <- socket Pub

  heartbeatPort <- bindSocket mProfile profileHeartbeatPort heartbeatSocket
  controlPort <- bindSocket mProfile profileControlPort controlSocket
  shellPort <- bindSocket mProfile profileShellPort shellSocket
  stdinPort <- bindSocket mProfile profileStdinPort stdinSocket
  iopubPort <- bindSocket mProfile profileIopubPort iopubSocket

  let profile = KernelProfile
        { profileTransport = maybe TCP profileTransport mProfile
        , profileIp = maybe "127.0.0.1" profileIp mProfile
        , profileHeartbeatPort = heartbeatPort
        , profileControlPort = controlPort
        , profileShellPort = shellPort
        , profileStdinPort = stdinPort
        , profileIopubPort = iopubPort
        , profileSignatureKey = ""
        }

  callback profile JupyterSockets { .. }

extractAddress :: Maybe KernelProfile -> (KernelProfile -> Int) -> String
extractAddress mProfile accessor = "tcp://127.0.0.1:" ++ maybe "*" (show . accessor) mProfile

bindSocket :: Maybe KernelProfile -> (KernelProfile -> Int) -> Socket z t -> ZMQ z Int
bindSocket mProfile accessor sock = do
  bind sock (extractAddress mProfile accessor)
  endpointString <- lastEndpoint sock
  case parsePort endpointString of
    Nothing -> fail "fatal error (Jupyter.Kernel.ZeroMQ): could not parse port as integer."
    Just port -> return port

parsePort :: String -> Maybe Int
parsePort s = readMaybe num
  where
    num = reverse (takeWhile isNumber (reverse s))

-- | Read a client message from a ZeroMQ socket.
receiveMessage :: Receiver a => Socket z a -> ZMQ z (Either String (Message Client))
receiveMessage sock = do
  -- Read all identifiers until the identifier/message delimiter.
  idents <- readUntil sock "<IDS|MSG>"

  -- Ignore the signature for now.
  void $ receive sock

  headerData <- receive sock
  parentHeader <- receive sock
  metadata <- receive sock
  content <- receive sock

  return $ parseClientMessage idents headerData parentHeader metadata content

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
      , messageMetadata = Map.fromList []
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
      , messageMetadata = Map.fromList []
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
sendMessage hmacKey sock parentHeader content = do
  header <- liftIO $ mkReplyHeader parentHeader content
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
