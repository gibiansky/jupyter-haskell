{-|
Module      : Main
Description : Client interface for Jupyter kernels.
Copyright   : (c) Andrew Gibiansky, 2016
License     : MIT
Maintainer  : andrew.gibiansky@gmail.com
Stability   : stable
Portability : POSIX

This module serves as the primary interface to Jupyter clients, as provided by the @jupyter@ library.

Communication with clients is done in the 'Client' monad, which is a thin wrapper over 'IO' which 
maintains a small bit of required state to identify a running kernel and the sockets on which to 
communicate with it. The initial state and connection information is supplied when you use 'runClient',
which requires connection information and the 'Client' action to run.

The 'runClient' function also requires a set of 'ClientHandlers', which are callbacks that get called
when the kernel sends any sort of message to the client ('KernelRequest's, 'KernelOutput's, and 'Comm's).

These functions can be used quite succinctly to communicate with external clients. For example, the
following code connects to an installed Python kernel (the @ipykernel@ package must be installed):

@
import Jupyter.Client

main :: IO ()
main = 
  'runClient' Nothing Nothing handlers $ \profile -> do
    -- The `profile` provided is a generated 'KernelProfile'
    -- that the client will connect to. Start an IPython kernel
    -- that listens on that profile.
    liftIO $ do
      'writeProfile' profile "profile.json"
      'System.Process.spawnProcess' "python" ["-m", "ipykernel", "-f", "profile.json"]

    -- Find out info about the kernel by sending it a kernel info request.
    connection <- 'connectKernel'
    reply <- 'sendClientRequest' connection 'KernelInfoRequest'
    liftIO $ print reply
@

More information about the client and kernel interfaces can be found on the @jupyter@ <https://github.com/gibiansky/jupyter-haskell README>.
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Jupyter.Client (
    -- * Communicating with Clients
    Client,
    runClient,
    connectKernel,
    sendClientRequest,
    sendClientComm,
    ClientHandlers(..),
    defaultClientCommHandler,
    KernelConnection,

    -- * Writing Connection Files
    writeProfile,

    -- * Locating kernels
    Kernelspec(..),
    findKernel,
    findKernels,
    ) where

-- Imports from 'base'
import           Control.Exception (bracket, catch)
import           Control.Monad (forever)
import           Data.Maybe (fromMaybe)

-- Imports from 'bytestring'
import           Data.ByteString (ByteString)

-- Imports from 'async'
import           Control.Concurrent.Async (async, link, link2, cancel, Async)

-- Imports from 'mtl'
import           Control.Monad.Reader (MonadReader, ReaderT, runReaderT, ask)

-- Imports from 'transformers'
import           Control.Monad.IO.Class (MonadIO(..))

-- Imports from 'exceptions'
import           Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)

-- Imports from 'monad-control'
import           Control.Monad.Trans.Control (liftBaseWith)

-- Imports from 'zeromq4-haskell'
import           System.ZMQ4.Monadic (ZMQ)

-- Imports from 'jupyter'
import           Jupyter.Install (findKernel, findKernels, Kernelspec(..))
import           Jupyter.Messages (Comm, KernelRequest, ClientReply, KernelOutput, ClientRequest,
                                   KernelReply)
import           Jupyter.Messages.Internal (Username)
import           Jupyter.ZeroMQ (ClientSockets(..), withClientSockets, sendMessage, receiveMessage,
                                 messagingError, mkRequestHeader, KernelProfile(..), mkReplyHeader,
                                 threadKilledHandler, writeProfile)
import qualified Jupyter.UUID as UUID

-- | All the state required to maintain a connection to the client.
data ClientState = forall z.
       ClientState
         { clientSockets :: ClientSockets z  -- ^ A set of sockets used to communicate with the kernel.
         , clientSessionUsername :: Username -- ^ A username to use in message headers.
         , clientSessionUuid :: UUID.UUID    -- ^ A session UUID to use in message headers.
         , clientSignatureKey :: ByteString  -- ^ An HMAC signature key to hash message signature with.
         , clientLiftZMQ :: forall a m. MonadIO m => ZMQ z a -> m a
           -- ^ A helper function to convert from ZMQ actions to any IO monad. 
         }

-- | A client action, representing a computation in which communication happens with a Jupyter
-- client.
--
-- Use 'sendClientRequest' and 'sendClientComm' to construct 'Client' values, the 'Monad' interface to
-- manipulate them, and 'runClient' to supply all needed connection info and run the action.
newtype Client a = Client { unClient :: ReaderT ClientState IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader ClientState, MonadThrow, MonadCatch, MonadMask)

-- | A connection to a kernel from a client. This 
data KernelConnection = KernelConnection
  deriving (Eq, Ord)

-- | A set of callbacks for the client. These callbacks get called when the client receives any
-- message from the kernel.
--
-- One callback exists per message type that the clients can receive. Each callbacks can also send
-- 'Comm' messages to kernel, and receive a function of type @'Comm' -> IO ()@ that sends a single
-- 'Comm' message to the kernel.
data ClientHandlers =
       ClientHandlers
         { kernelRequestHandler :: (Comm -> IO ()) -> KernelRequest -> IO ClientReply
           -- ^ A callback for handling 'KernelRequest's. A 'KernelRequest' is sent from a 
           -- kernel to just one client, and that client must generate a 'ClientReply' with 
           -- the corresponding constructor. 
           --
           -- The handler is passed a function @'Comm' -> IO ()@ which may be used to send 'Comm' messages
           -- back to the client that sent the message.
         , commHandler :: (Comm -> IO ()) -> Comm -> IO ()
           -- ^ A callback for handling 'Comm' messages from the kernel. 'Comm' messages may be handled in
           -- any way, and 'defaultClientCommHandler' may be used as a 'Comm' handler that simply does nothing.
           --
           -- The handler is passed a function @'Comm' -> IO ()@ which may be used to send 'Comm' messages
           -- back to the client that sent the message.
         , kernelOutputHandler :: (Comm -> IO ()) -> KernelOutput -> IO ()
           -- ^ A callback for handling 'KernelOutput's. 'KernelOutput' messages are the primary
           -- way for a kernel to publish outputs, and are sent to all connected frontends.
           --
           -- The handler is passed a function @'Comm' -> IO ()@ which may be used to send 'Comm' messages
           -- back to the client that sent the message.
         }

-- | Run a 'Client' action in 'IO'.
--
-- This function sets up ZeroMQ sockets on which it can connect to a kernel; if no 'KernelProfile'
-- is provided, it generates a fresh 'KernelProfile' which contains information about the ports and
-- transport protocols which it expects the kernel to connect with. It guarantees that the ports it
-- chooses are open – that is, that no kernel is currently connected to those ports.
--
-- The generated 'KernelProfile' is passed to the user-provided @'KernelProfile' -> 'Client' a@
-- callback, which may use functions such as 'sentClientRequest' to communicate with the kernel. If
-- the kernel sends messages to the client, they are handled with the callbacks provided in the
-- 'ClientHandlers' record.
runClient :: Maybe KernelProfile -- ^ Optionally, a 'KernelProfile' to connect to. If no
                                 -- 'KernelProfile' is provided, one is generated on the fly.
                                 -- However, if a 'KernelProfile' /is/ provided, and connecting to
                                 -- the specified ports fails, an exception is thrown.
          -> Maybe Username -- ^ Optionally, a username to use when sending messages to the client.
                            -- If no username is provided, a default one is used.
          -> ClientHandlers -- ^ A record containing handlers for messages the kernel sends to the
                            -- client.
          -> (KernelProfile -> Client a) -- ^ Provided with the 'KernelProfile' that was being used
                                         -- (either a freshly generated one or the one passed in by
                                         -- the user), generate a 'Client' action. This action is
                                         -- then run, handling all the ZeroMQ communication in the
                                         -- background.
          -> IO a
runClient mProfile mUser clientHandlers client =
  withClientSockets mProfile $ \profile sockets ->
    liftBaseWith $ \runInBase -> do
      let sessionUsername = fromMaybe "default-username" mUser
      sessionUuid <- UUID.random

      -- Don't let the listenrs start immediately. If so, we can get into an ugly, ugly
      -- intermediate state, where the listeners are running but their Asyncs are not linked
      -- to each other *or* to the main thread. That means that sometimes, with low probability,
      -- the Asyncs can get an exception thrown to them *before* they are linked, and so the
      -- thread will die without killing the other thread or the main thread. This can then
      -- lead to deadlocks if you expect the threads to be running. 
      --
      -- We avoid this but ensuring that the async threads cannot get exceptions until they
      -- are linked, using an MVar for this lock.
      let clientState = ClientState
            { clientSockets = sockets
            , clientSessionUsername = sessionUsername
            , clientSessionUuid = sessionUuid
            , clientSignatureKey = profileSignatureKey profile
            , clientLiftZMQ = liftIO . runInBase
            }

          setupListeners :: IO (Async (), Async ())
          setupListeners = do
            async1 <- listenStdin clientState clientHandlers
            async2 <- listenIopub clientState clientHandlers


            -- Ensure that if any exceptions are thrown on the handler threads,
            -- those exceptions are re-raised on the main thread.
            link async1
            link2 async1 async2

            return (async1, async2)

      -- Ensure that if any exceptions are thrown on the main thread, the asyncs
      -- are cancelled with a ThreadKilled exception, and that if no exceptions
      -- are thrown, then the threads are terminated as appropriate.
      bracket setupListeners
              (\(async1, async2) -> cancel async1 >> cancel async2)
              (const $ runReaderT (unClient $ client profile) clientState)
            

-- | Wait for a kernel to connect to this client, and return a 'KernelConnection' once the kernel
-- has connected.
--
-- This 'KernelConnection' must be passed to 'sendClientRequest' and 'sendClientComm' to communicate
-- with the connected kernel.
connectKernel :: Client KernelConnection
connectKernel = do
  ClientState {..} <- ask
  liftIO $ clientWaitForConnections clientSockets
  return KernelConnection

-- | Send a 'ClientRequest' to the kernel. Wait for the kernel to reply with a 'KernelReply',
-- blocking until it does so.
sendClientRequest :: KernelConnection -- ^ A kernel connection, produced by 'connectKernel'.
                  -> ClientRequest -- ^ The request to send to the connected kernel.
                  -> Client KernelReply
sendClientRequest KernelConnection req = do
  ClientState { .. } <- ask
  header <- liftIO $ mkRequestHeader clientSessionUuid clientSessionUsername req
  clientLiftZMQ $ sendMessage clientSignatureKey (clientShellSocket clientSockets) header req
  received <- clientLiftZMQ $ receiveMessage (clientShellSocket clientSockets)

  case received of
    Left err ->
      -- There's no way to recover from this, so just die.
      messagingError "Jupyter.Client" $
        "Unexpected failure parsing KernelReply message: " ++ err
    Right (_, message) -> return message

-- | Send a 'Comm' message to the kernel. The kernel is not obligated to respond in any way, so do
-- not block, but return immediately upon sending the message.
sendClientComm :: KernelConnection -- ^ A kernel connection, produced by 'connectKernel'.
               -> Comm -- ^ The 'Comm' message to send.
               -> Client ()
sendClientComm KernelConnection comm = do
  ClientState { .. } <- ask
  header <- liftIO $ mkRequestHeader clientSessionUuid clientSessionUsername  comm
  clientLiftZMQ $ sendMessage clientSignatureKey (clientShellSocket clientSockets) header comm

-- | A default client 'Comm' handlers, which, upon receiving a 'Comm' message, does nothing.
--
-- For use with the 'ClientHandlers' 'commHandler' field.
defaultClientCommHandler :: (Comm -> IO ()) -> Comm -> IO ()
defaultClientCommHandler _ _ = return ()

-- | Spawn a new thread that forever listens on the /iopub/ socket, parsing the messages
-- as they come in and calling the appropriate callback from the 'ClientHandlers' record.
-- 
-- If the thread receives a 'ThreadKilled' exception, it will die silently, without letting
-- the exception propagate.
listenIopub :: ClientState -> ClientHandlers -> IO (Async ())
listenIopub ClientState { .. } handlers = async $ catch (forever respondIopub) threadKilledHandler
  where
    respondIopub = do
      received <- clientLiftZMQ $ receiveMessage (clientIopubSocket clientSockets)
      case received of
        Left err ->
          -- There's no way to recover from this, so just die.
          messagingError "Jupyter.Client" $
            "Unexpected failure parsing Comm or KernelOutput message: " ++ err
        Right (header, message) -> do
          let sendReplyComm comm = do
                commHeader <- mkReplyHeader header comm
                clientLiftZMQ $ sendMessage
                                  clientSignatureKey
                                  (clientShellSocket clientSockets)
                                  commHeader
                                  comm

          case message of
            Left comm    -> commHandler handlers sendReplyComm comm
            Right output -> kernelOutputHandler handlers sendReplyComm output

-- | Spawn a new thread that forever listens on the /stdin/ socket, parsing the messages
-- as they come in and calling the appropriate callback from the 'ClientHandlers' record.
-- 
-- If the thread receives a 'ThreadKilled' exception, it will die silently, without letting
-- the exception propagate.
listenStdin :: ClientState -> ClientHandlers -> IO (Async ())
listenStdin ClientState{..} handlers = async $ catch (forever respondStdin) threadKilledHandler
  where
    respondStdin = do
      received <- clientLiftZMQ $ receiveMessage (clientStdinSocket clientSockets)
      case received of
        Left err ->
          -- There's no way to recover from this, so just die.
          messagingError "Jupyter.Client" $
            "Unexpected failure parsing KernelRequest message: " ++ err
        Right (header, message) -> do
          let sendReplyComm comm = do
                commHeader <- mkReplyHeader header comm
                clientLiftZMQ $ sendMessage clientSignatureKey (clientShellSocket clientSockets) commHeader comm
          reply <- kernelRequestHandler handlers sendReplyComm message
          replyHeader <- mkReplyHeader header reply
          clientLiftZMQ $ sendMessage clientSignatureKey (clientStdinSocket clientSockets) replyHeader reply
