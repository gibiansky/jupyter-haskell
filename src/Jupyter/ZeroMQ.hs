{-|
Module      : Jupyter.ZeroMQ
Description : Low-level communication primitives for Jupyter's ZeroMQ channels.
Copyright   : (c) Andrew Gibiansky, 2016
License     : MIT
Maintainer  : andrew.gibiansky@gmail.com
Stability   : stable
Portability : POSIX

This module provides a low-level interface to the Jupyter ZeroMQ sockets, message encoding, and
message decoding. The primary interface consists of 'withKernelSockets' and 'withClientSockets', which
create the sets of sockets needed to serve a kernel or run a client, and 'sendMessage' and 'receiveMessage',
which, as the names may imply, send and receive messages (encoding and decoding them along the way) on the
sockets.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Jupyter.ZeroMQ (
    -- * Opening ZeroMQ Sockets
    KernelSockets(..),
    withKernelSockets,
    ClientSockets(..),
    withClientSockets,

    -- * Kernel Profiles
    KernelProfile(..),
    Port,
    IP,
    Transport(..),
    readProfile,
    writeProfile,

    -- * Sending and Receiving messages
    sendMessage,
    receiveMessage,
    mkRequestHeader,
    mkReplyHeader,

    -- * Miscellaneous utilities
    threadKilledHandler,
    messagingError,
    MessagingException(..),
    ) where

-- Imports from 'base'
import           Control.Exception (throwIO, Exception, AsyncException(ThreadKilled))
import           Control.Monad (void, unless)
import           Data.Char (isNumber)
import           Data.Monoid ((<>))
import           Text.Read (readMaybe)

-- Imports from 'bytestring'
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Lazy as LBS

-- Imports from 'exceptions'
import           Control.Monad.Catch (catch)

-- Imports from 'aeson'
import           Data.Aeson (FromJSON(..), Value(..), (.:), ToJSON(..), encode, decode, (.=), object,
                             eitherDecodeStrict')
import           Data.Aeson.Types (parseEither)

-- Imports from 'mtl'
import           Control.Monad.IO.Class (MonadIO(..))

-- Imports from 'text'
import qualified Data.Text.Encoding as T

-- Imports from 'SHA'
import           Data.Digest.Pure.SHA as SHA

-- Imports from 'zeromq4-haskell'
import           System.ZMQ4.Monadic (Socket, ZMQ, runZMQ, socket, Rep(..), Router(..), Pub(..),
                                      Dealer(..), Req(..), Sub(..), Flag(..), send, receive, Receiver,
                                      Sender, lastEndpoint, bind, unbind, connect, subscribe,
                                      ZMQError, setIdentity, restrict, monitor, EventType(..))

-- Imports from 'jupyter'
import           Jupyter.Messages.Internal (MessageHeader(..), IsMessage(..), Username(..))
import qualified Jupyter.UUID as UUID


-- | Given all the bytes read from the wire, parse the Jupyter message into one of the @jupyter@
-- package's many data types representing different types of Jupyter messages.
--
-- Return the 'MessageHeader' along with the message itself, assuming parsing succeeds, or returns
-- an error message if it doesn't.
parseMessage :: IsMessage v
             => [ByteString] -- ^ List of ZeroMQ identifiers for this message.
             -> ByteString   -- ^ The encoded JSON message header.
             -> ByteString   -- ^ The encoded JSON parent message header (or @{}@ for no parent).
             -> ByteString   -- ^ Any metadata associated with the message (as JSON).
             -> ByteString   -- ^ The contents of the message itself, encoded as JSON.
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
parseHeader :: [ByteString]  -- ^ List of ZeroMQ identifiers for this message.
            -> ByteString    -- ^ The encoded JSON message header.
            -> ByteString    -- ^ The encoded JSON parent message header (or @{}@ for no parent).
            -> ByteString    -- ^ Any metadata associated with the message (as JSON).
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

-- | The collection of <http://zeromq.org/ ZeroMQ> sockets needed to communicate with Jupyter
-- kernels on the <https://jupyter- Jupyter messaging wire protocol>. These sockets are to be used
-- by a client communicating with a kernel.
--
-- Roles of different sockets are described
-- <https://jupyter-client.readthedocs.io/en/latest/messaging.html#introduction here>.
data ClientSockets z =
       ClientSockets
         { clientHeartbeatSocket :: Socket z Req
         -- ^ The /heartbeat/ socket, which, for functioning kernels, will echo anything sent to it immediately.
         -- Clients can use this socket to check if the kernel is still alive.
         , clientControlSocket :: Socket z Dealer
         -- ^ The /control/ socket, optionally used to send 'ClientRequest' messages that deserve immediate
         -- response (shutdown requests, etc), rather than long-running requests (execution).
         , clientShellSocket :: Socket z Dealer
         -- ^ The /shell/ socket, used to send the majority of 'ClientRequest' messages (introspection,
         -- completion, execution, etc) and their responses.
         , clientStdinSocket :: Socket z Dealer
         -- ^ The /stdin/ socket, used for communication from a kernel to a single frontend; currently only
         -- used for retrieving standard input from the user, hence the socket name.
         , clientIopubSocket :: Socket z Sub
         -- ^ The /iopub/ socket, used for receiving 'KernelOutput's from the kernel.
         , clientWaitForConnections :: IO ()
         -- ^ A function which waits for one connection on each of the sockets, using socket monitoring.
         }

-- | The collection of <http://zeromq.org/ ZeroMQ> sockets needed to communicate with Jupyter
-- clients on the <https://jupyter- Jupyter messaging wire protocol>. These sockets are to be used
-- by a kernel communicating with a client.
--
-- Roles of different sockets are described
-- <https://jupyter-client.readthedocs.io/en/latest/messaging.html#introduction here>.
data KernelSockets z =
       KernelSockets
         { kernelHeartbeatSocket :: Socket z Rep
         -- ^ The /heartbeat/ socket, which echoes anything sent to it immediately and is used by frontends
         -- solely to confirm that the kernel is still alive.
         , kernelControlSocket :: Socket z Router
         -- ^ The /control/ socket, optionally used to send 'ClientRequest' messages that deserve immediate
         -- response (shutdown requests, etc), rather than long-running requests (execution).
         , kernelShellSocket :: Socket z Router
         -- ^ The /shell/ socket, used to send the majority of 'ClientRequest' messages (introspection,
         -- completion, execution, etc) and their responses.
         , kernelStdinSocket :: Socket z Router
         -- ^ The /stdin/ socket, used for communication from a kernel to a single frontend; currently only
         -- used for retrieving standard input from the user, hence the socket name.
         , kernelIopubSocket :: Socket z Pub
         -- ^ The /iopub/ socket, used for publishing 'KernelOutput's to all frontends.
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

-- | Convert a 'Transport' to a 'String' representing the protocol, to be used as the first part of an address.
--
-- >>> transportToProtocolString TCP == "tcp"
transportToProtocolString :: Transport -> String
transportToProtocolString TCP = "tcp"

-- | Exception to throw when the messaging protocol is not being observed.
--
-- See 'messagingError'.
data MessagingException = MessagingException String 
  deriving (Eq, Ord, Show)

-- | An 'Exception' instance allows 'MessagingException' to be thrown as an exception.
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

-- | Instance to decode a 'KernelProfile' from connection file contents.
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

-- | Write a 'KernelProfile' to a JSON file, which can be passed as the connection file to a
-- starting kernel.
writeProfile :: KernelProfile -> FilePath -> IO ()
writeProfile profile path = LBS.writeFile path (encode profile) 

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
  identity <- CBS.pack . UUID.uuidToString <$> liftIO UUID.random
  setIdentity (restrict identity) clientShellSocket
  setIdentity (restrict identity) clientStdinSocket
  setIdentity (restrict identity) clientControlSocket

  -- Set up socket monitoring. When you monitor a socket, you specify the event to listen for. Then,
  -- you can call the function return from 'monitor' to block until an event is received. This lets us
  -- easily wait for the kernel to connect, by waiting for one accepted connection event per socket.
  -- Once we receive that, we can turn off monitoring. (Passing True listens for an event; False turns
  -- off monitoring.)
  -- 
  -- You can't use 'mapM' because the sockets have different types, e.g. Socket z Req vs Socket z Dealer.
  monitors <- sequence
                [ monitor [ConnectedEvent] clientHeartbeatSocket
                , monitor [ConnectedEvent] clientControlSocket
                , monitor [ConnectedEvent] clientShellSocket
                , monitor [ConnectedEvent] clientStdinSocket
                , monitor [ConnectedEvent] clientIopubSocket
                ]
  let clientWaitForConnections = do
        mapM_ ($ True) monitors
        mapM_ ($ False) monitors

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

-- | Compute the address to bind a socket to, given the 'KernelProfile', using the provided tranport
-- mechanism, IP, and port. If no 'KernelProfile' is provided (and 'Nothing' is passed), then return
-- the default address to bind to.
--
-- This default address has no explicit port, but rather uses @*@, as in @tcp://127.0.0.1:*@, and so
-- cannot be used with ZeroMQ 'connect' (only with 'bind').
extractAddress :: Maybe KernelProfile    -- ^ Optional kernel profile to get address info from
               -> (KernelProfile -> Port) -- ^ Given a kernel profile, get the port to use,e.g.
                                          -- 'profileIopubPort'
               -> String                 -- ^ An address string, e.g. @tcp://127.0.0.1:8283@
extractAddress mProfile accessor =
  concat
    [ maybe "tcp" (transportToProtocolString . profileTransport) mProfile
    , "://"
    , maybe "127.0.0.1" profileIp mProfile
    , ":"
    , maybe "*" (show . accessor) mProfile
    ]

-- | Connect the provided socket to a port.
--
-- The port to connect to is determined as follows:
--
-- 1. If a 'KernelProfile' is provided, use the given @'KernelProfile' -> 'Port'@ accessor to compute
-- the port that the socket should bind to, and use it (along with the transport mechanism and IP)
-- to generate an address to connect to. If connecting to this address fails, an exception is
-- raised.
--
-- 2. If no 'KernelProfile' is provided, attempt to bind to the provided default 'Port'.
--
-- 3. If binding to the default 'Port' fails, increment the port by one, and try again. Repeat this
-- until either it succeeds, or until a fixed number of tries has been attempted.
--
-- Returns the port to which the socket was connected.
connectSocket :: forall z t. Maybe KernelProfile -- ^ Optional 'KernelProfile'
              -> Port -- ^ Default port to try, if no 'KernelProfile' provided
              -> (KernelProfile -> Port) -- ^ Accessor function to get desired port from profile
              -> Socket z t -- ^ Socket to connect
              -> ZMQ z Port -- ^ Returns port to which socket connected
connectSocket mProfile startPort accessor sock = do
  case mProfile of
    Just _  -> connect sock (extractAddress mProfile accessor)
    Nothing -> findOpenPort 100 startPort

  endpoint sock

  where
    -- Try binding to a port. If it fails, try the next one (up to a fixed limit).
    -- Any ZMQ error is treated as a retriable failure, regardless of the error code or message.
    findOpenPort :: Int -> Int -> ZMQ z ()
    findOpenPort 0 _ = fail "fatal error (Jupyter.ZeroMQ): Could not find port to connect to."
    findOpenPort triesLeft tryPort =
      let handler :: ZMQError -> ZMQ z ()
          handler = const $ findOpenPort (triesLeft - 1) (tryPort + 1)
          address = "tcp://127.0.0.1:" ++ show (tryPort :: Int)
      in flip catch handler $ do
        -- `connect` allows you to connect multiple sockets to the same port. We don't want that! So, in
        -- order to find out if we have a kernel already running on the port we're about to connect to, we
        -- `bind` the socket. If the bind fails, that means the port is used; if it doesn't fail, the port
        -- is open, so we unbind and then connect to it. This is pretty hacky and not thread-safe, but
        -- should not cause any issues in practice.
        bind sock address
        unbind sock address
        connect sock address


-- | Bind a socket to a port.
--
-- If a 'KernelProfile' is provided, then the @'KernelProfile' -> 'Port'@ accessor is used
-- to determine which port to connect to. Otherwise, some available port is chosen. The port
-- that was bound to is returned.
bindSocket :: Maybe KernelProfile     -- ^ Optional kernel profile with port info
           -> (KernelProfile -> Port) -- ^ Accessor for 'Port' inside the profile
           -> Socket z t -- ^ Socket to 'bind'
           -> ZMQ z Port -- ^ Return port socket was bound to
bindSocket mProfile accessor sock = do
  bind sock (extractAddress mProfile accessor)
  endpoint sock

-- | Get the port that the socket was last bound to.
endpoint :: Socket z t -> ZMQ z Port
endpoint sock = do
  endpointString <- lastEndpoint sock
  case parsePort endpointString of
    Nothing   -> fail "fatal error (Jupyter.ZeroMQ): could not parse port as integer."
    Just port -> return port

-- | Try to parse the 'Port' from an address string along the lines of @"tcp://127.0.0.1:8829"@.
--
-- >>> parsePort "tcp://127.0.0.1:8829" == 8829
parsePort :: String -> Maybe Int
parsePort s = readMaybe num
  where
    num = reverse (takeWhile isNumber (reverse s))

-- | Read a client message from a ZeroMQ socket, as well as the message header that came with it.
-- Block until all data for the message has been received.
--
-- If receiving all the data succeeds but parsing fails, return a 'String' error message.
--
-- This message is polymorphic in its return type @v@, and so may be used to parse /any/ message
-- type.
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
  return $ parseMessage idents headerData parentHeader metadata content

-- | Read data from the socket until we hit an ending string. Return all data as a list, which does
-- not include the ending string.
readUntil :: Receiver a
          => Socket z a -- ^ Socket to read from
          -> ByteString -- ^ Delimiter chunk
          -> ZMQ z [ByteString] -- ^ Messages until (but not including) delimiter chunk
readUntil sock terminator = do
  line <- receive sock
  if line /= terminator
    then do
      remaining <- readUntil sock terminator
      return $ line : remaining
    else return []

-- | Create a new 'MessageHeader', which is suitable to be used for a request from a client to a
-- kernel.
--
-- The main difference between 'mkRequestHeader' and 'mkReplyHeader' is that a reply header has a
-- parent header, while a request header is not triggered by another message, and so has no parent
-- header. However, since there is no parent header to inherit information from, the session UUID
-- and username must be set explicitly.
mkRequestHeader :: IsMessage v
                => UUID.UUID -- ^ Session UUID for this client session
                -> Username  -- ^ Username to use in the header
                -> v -- ^ Message for which to make header (necessary to get 'MessageType')
                -> IO MessageHeader -- ^ New 'MessageHeader', with fresh randomly generated id
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

-- | Create a new 'MessageHeader' for a message which is a reply to a previous message.
--
-- Unlike 'mkRequestHeader', 'mkReplyHeader' requires a parent header, and so is used for replies,
-- rather than for initiating a communication.
mkReplyHeader :: IsMessage v
              => MessageHeader -- ^ Header of message being replied to
              -> v -- ^ Reply message for which to generate header (necessary to get 'MessageType')
              -> IO MessageHeader -- ^ New 'MessageHeader', with fresh randomly generated id
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


-- | Send a Jupyter message on a socket, encoding it as described in the
-- <http://jupyter-client.readthedocs.io/en/latest/messaging.html#wire-protocol wire protocol documentation>.
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
    -- Send one piece of a multipart message (with the 'SendMore' flag).
    sendPiece = send sock [SendMore]

    -- Send the last piece of a multipart message.
    sendLast = send sock []

    -- Compute the HMAC SHA-256 signature of a bytestring message.
    hmac :: ByteString -> ByteString
    hmac = CBS.pack . SHA.showDigest . SHA.hmacSha256 (LBS.fromStrict hmacKey) . LBS.fromStrict

    -- Encode a 'MessageHeader' as a JSON ByteString.
    encodeHeader :: MessageHeader -> ByteString
    encodeHeader MessageHeader { .. } =
      encodeStrict $ object
                       [ "msg_id" .= messageId
                       , "session" .= messageSession
                       , "username" .= messageUsername
                       , "version" .= ("5.0" :: String)
                       , "msg_type" .= messageType
                       ]
    

-- | Encode JSON to a strict bytestring.
encodeStrict :: ToJSON a => a -> ByteString
encodeStrict = LBS.toStrict . encode

-- | Handle an 'AsyncException': if the exception is 'ThreadKilled', then do nothing,
-- otherwise, rethrow the exception.
--
-- This helper utility exists to gracefully shutdown infinite loops in which we listen on
-- ZeroMQ sockets, and exists to stop 'ThreadKilled' exceptions from propagating back to
-- the main thread (which, presumably, is the thread that killed the thread in question).
--
-- This is a utility provided for use with listener threads.
threadKilledHandler :: AsyncException -> IO ()
threadKilledHandler ThreadKilled = return ()
threadKilledHandler ex = throwIO ex
