{-|
Module      : Jupyter.Kernel
Description : Functions for creating and serving a Jupyter kernel.
Copyright   : (c) Andrew Gibiansky, 2016
License     : MIT
Maintainer  : andrew.gibiansky@gmail.com
Stability   : stable
Portability : POSIX

The 'Jupyter.Kernel' module provides an API for quickly and easily creating Jupyter kernels.

Jupyter kernels are programs which communicate using the
<https://jupyter-client.readthedocs.io/en/latest/messaging.html Jupyter messaging spec>; most kernels
are language backends that allow using a particular programming language with Jupyter frontends such as
the <http://jupyter.org/ notebook> or <http://jupyter.org/qtconsole/stable/ QtConsole>.

To run a kernel, call the 'serve' function, which provides a type-safe implementation of the Jupyter
messaging spec.

More information about the client and kernel interfaces can be found on the @jupyter@
<https://github.com/gibiansky/jupyter-haskell README>, and several example kernels may be found in 
the <https://github.com/gibiansky/jupyter-haskell/tree/master/examples examples> directory.
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module Jupyter.Kernel (
  -- * Serving kernels
  serve,
  serveDynamic,
  KernelCallbacks(..),

  -- * Defining a kernel
  simpleKernelInfo,
  ClientRequestHandler,
  defaultClientRequestHandler,
  CommHandler,
  defaultCommHandler,

  -- * Reading Kernel Profiles
  KernelProfile(..),
  readProfile,
  ) where

-- Imports from 'base'
import           Control.Exception (bracket, catch, finally)
import           Control.Monad (forever)
import           System.Exit (exitSuccess)
import           System.IO (hPutStrLn, stderr)

-- Imports from 'bytestring'
import           Data.ByteString (ByteString)

-- Imports from 'text'
import           Data.Text (Text)

-- Imports from 'mtl'
import           Control.Monad.IO.Class (MonadIO(..))

-- Imports from 'monad-control'
import           Control.Monad.Trans.Control (liftBaseWith)

-- Imports from 'async'
import           Control.Concurrent.Async (link2, waitAny, cancel)

-- Imports from 'zeromq4-haskell'
import           System.ZMQ4.Monadic (ZMQ, Socket, Rep, Router, Pub, async, receive, send)

-- Imports from 'jupyter'
import           Jupyter.Messages (KernelOutput, Comm, ClientRequest(..), KernelReply(..),
                                   pattern ExecuteOk, pattern InspectOk, pattern CompleteOk,
                                   CursorRange(..), CodeComplete(..), CodeOffset(..), ConnectInfo(..),
                                   KernelInfo(..), LanguageInfo(..), KernelOutput(..),
                                   KernelStatus(..), KernelRequest(..), ClientReply(..))
import           Jupyter.ZeroMQ (withKernelSockets, KernelSockets(..), sendMessage, receiveMessage,
                                 KernelProfile(..), readProfile, messagingError, mkReplyHeader,
                                 threadKilledHandler, readProfile)

-- | Create the simplest possible 'KernelInfo'.
--
-- Defaults version numbers to \"0.0\", mimetype to \"text/plain\", empty banner, and a \".txt\"
-- file extension.
--
-- Mostly intended for use in tutorials and demonstrations; if publishing production kernels, make
-- sure to use the full 'KernelInfo' constructor.
--
-- >>> let kernelInfo = simpleKernelInfo "python3"
simpleKernelInfo :: Text -- ^ Kernel name, used for 'kernelImplementation' and 'languageName'.
                 -> KernelInfo
simpleKernelInfo kernelName =
  KernelInfo
    { kernelProtocolVersion = "5.0"
    , kernelBanner = ""
    , kernelImplementation = kernelName
    , kernelImplementationVersion = "0.0"
    , kernelHelpLinks = []
    , kernelLanguageInfo = LanguageInfo
      { languageName = kernelName
      , languageVersion = "0.0"
      , languageMimetype = "text/plain"
      , languageFileExtension = ".txt"
      , languagePygmentsLexer = Nothing
      , languageCodeMirrorMode = Nothing
      , languageNbconvertExporter = Nothing
      }
    }


-- | The 'KernelCallbacks' data type contains callbacks that the kernel may use to communicate with
-- the client. Specifically, it can send 'KernelOutput' and 'Comm' messages using 'sendKernelOutput'
-- and 'sendComm', respectively, which are often sent to frontends in response to 'ExecuteRequest'
-- messsages.
--
-- In addition, 'sentKernelRequest' can be used to send a 'KernelRequest' to the client, and
-- synchronously wait and receive a 'ClientReply'.
data KernelCallbacks =
       KernelCallbacks
         { sendKernelOutput :: KernelOutput -> IO () -- ^ Publish an output to all connected
                                                     -- frontends. This is the primary mechanism by
                                                     -- which a kernel shows output to the user.
         , sendComm :: Comm -> IO () -- ^ Publish a 'Comm' message to the frontends. This allows for
                                     -- entirely freeform back-and-forth communication between
                                     -- frontends and kernels, avoiding the structure of the Jupyter
                                     -- messaging protocol. This can be used for implementing custom
                                     -- features such as support for the Jupyter notebook widgets.
         , sendKernelRequest :: KernelRequest -> IO ClientReply -- ^ Send a 'KernelRequest' to the
                                                                -- client that send the first message
                                                                -- and wait for it to reply with a
                                                                -- 'ClientReply'.
         }

-- | When calling 'serve', the caller must provide a 'CommHandler'.
--
-- The handler is used when the kernel receives a 'Comm' message from a frontend; the 'Comm' message
-- is passed to the handler, along with a set of callbacks the handler may use to send messages to
-- the client.
--
-- Since 'Comm's are used for free-form communication outside the messaging spec, kernels should
-- ignore 'Comm' messages they do not expect.
--
-- The 'defaultCommHandler' handler is provided for use with kernels that wish to ignore all 'Comm'
-- messages.
type CommHandler = KernelCallbacks -> Comm -> IO ()

-- | When calling 'serve', the caller must provide a 'ClientRequestHandler'.
--
-- The handler is used when the kernel receives a 'ClientRequest' message from a frontend; the
-- 'ClientRequest' message is passed to the handler, along with a set of callbacks the handler may
-- use to send messages to the client.
--
-- The handler must return a 'KernelReply' to be sent in response to this request. 'ClientRequest'
-- and 'KernelReply' constructors come in pairs, and the output reply constructor /must/ match the
-- input request constructor.
--
-- Note: When the request is a 'ExecuteRequest' with the 'executeSilent' option set to @True@, the
-- 'KernelReply' will not be sent.
type ClientRequestHandler = KernelCallbacks -> ClientRequest -> IO KernelReply

-- | Handler which ignores all 'Comm' messages sent to the kernel (and does nothing).
defaultCommHandler :: CommHandler
defaultCommHandler _ _ = return ()

-- | Handler which responds to all 'ClientRequest' messages with a default, empty reply.
defaultClientRequestHandler :: KernelProfile -- ^ The profile this kernel is running on. Used to
                                             -- respond to 'ConnectRequest's.
                            -> KernelInfo    -- ^ Information about this kernel. Used to respond to
                                             -- 'KernelInfoRequest's.
                            -> ClientRequestHandler
defaultClientRequestHandler KernelProfile { .. } kernelInfo callbacks req =
  case req of
    ExecuteRequest code _ -> do
      sendKernelOutput callbacks $ ExecuteInputOutput 0 code
      return $ ExecuteReply 0 ExecuteOk
    InspectRequest{} -> return $ InspectReply $ InspectOk Nothing
    HistoryRequest{} -> return $ HistoryReply []
    CompleteRequest _ (CodeOffset offset) ->
      return $ CompleteReply $ CompleteOk [] (CursorRange offset offset) mempty
    IsCompleteRequest{} -> return $ IsCompleteReply CodeUnknown
    CommInfoRequest{} -> return $ CommInfoReply mempty
    ShutdownRequest restart -> return $ ShutdownReply restart
    KernelInfoRequest{} -> return $ KernelInfoReply kernelInfo
    ConnectRequest{} -> return $ ConnectReply
                                   ConnectInfo
                                     { connectShellPort = profileShellPort
                                     , connectIopubPort = profileIopubPort
                                     , connectHeartbeatPort = profileHeartbeatPort
                                     , connectStdinPort = profileStdinPort
                                     , connectControlPort = profileControlPort
                                     }

-- | Indefinitely serve a kernel on the provided ports. If the ports are not open, fails with an
-- exception.
--
-- This starts several threads which listen and write to ZeroMQ sockets on the ports indicated in
-- the 'KernelProfile'. If an exception is raised and any of the threads die, the exception is
-- re-raised on the main thread.
--
-- Using this function generally requires a bit of setup. The most common pattern for use is as follows:
--
-- 1. In your kernel @Main.hs@, parse command line arguments. Some combination of arguments should include
-- a path to a connection file; this file can be read and parsed into a 'KernelProfile' with 'readProfile'.
-- (Often, the same executable will have a different mode that installs the kernel
-- using 'Jupyter.Install.installKernelspec').
-- 2. Set up any state your kernel may need, storing it in an 'Control.Concurrent.MVar' or 'Data.IORef.IORef'.
-- 3. Define your 'CommHandler' and 'ClientRequestHandler' handlers, which read from the state and reply
-- with any necessary messages. (These handlers /may/ be called concurrently from different threads!)
-- 4. Provide the kernel profile and handlers to the 'serve' function, which blocks indefinitely.
--
-- Example kernels may be found in the
-- <https://github.com/gibiansky/jupyter-haskell/tree/master/examples examples> directory.
serve :: KernelProfile         -- ^ The kernel profile specifies how to listen for client messages (ports,
                               -- transport mechanism, message signing, etc).
      -> CommHandler           -- ^ The 'Comm' handler is called when 'Comm' messages are received from a
                               -- frontend.
      -> ClientRequestHandler  -- ^The request handler is called when 'ClientRequest' messages are
                               -- received from a frontend.
      -> IO ()
serve profile = serveInternal (Just profile) (const $ return ())

-- | Indefinitely serve a kernel on some ports. Ports are allocated dynamically and so, unlike
-- 'serve', 'serveDynamic' may be used when you do not know which ports are open or closed.
--
-- The ports allocated by 'serveDynamic' are passed to the provided callback in the 'KernelProfile'
-- so that clients may connect to the served kernel.
--
-- After the callback is run, several threads are started which listen and write to ZeroMQ sockets
-- on the allocated ports. If an exception is raised and any of the threads die, the exception is
-- re-raised on the main thread. Otherwise, this listens on the kernels indefinitely after running
-- the callback.
--
-- This function serves as a form of inverting control over the allocated ports: usually, clients
-- will choose what ports to listen on, and provide the kernel with the ports with a connection file
-- path in the kernel command-line arguments. With this function, you can instead first start the
-- kernel, and then connect a client to the ports that the kernel chooses to bind to.
serveDynamic :: (KernelProfile -> IO ()) -- ^ This function is called with the dynamically-generated
                                         -- kernel profile that the kernel will serve on, so that
                                         -- clients may be notified of which ports to use to connect
                                         -- to this kernel. The callback is called after sockets are
                                         -- bound but before the kernel begins listening for
                                         -- messages, so if the callback fails with an exception the
                                         -- kernel threads are never started.
             -> CommHandler           -- ^ The 'Comm' handler is called when 'Comm' messages are received from
                                      -- a frontend.
             -> ClientRequestHandler  -- ^The request handler is called when 'ClientRequest' messages
                                      -- are received from a frontend.
             -> IO ()
serveDynamic = serveInternal Nothing


-- | Serve a kernel.
--
-- If a 'KernelProfile' is provided, then open sockets bound to the specified ports; otherwise,
-- dynamically allocate ports and bind sockets to them. In both cases, the final 'KernelProfile'
-- used is passed to the provided callback, so that clients can be informed about how to connect to
-- this kernel.
--
-- Users of the library should use 'serve' or 'serveDynamic' instead.
--
-- After the callback is run, several threads are started which listen and write to ZeroMQ sockets
-- on the allocated ports. If an exception is raised and any of the threads die, the exception is
-- re-raised on the main thread. Otherwise, this listens on the kernels indefinitely after running
-- the callback.
serveInternal :: Maybe KernelProfile
              -> (KernelProfile -> IO ())
              -> CommHandler
              -> ClientRequestHandler
              -> IO ()
serveInternal mProfile profileHandler commHandler requestHandler =
  withKernelSockets mProfile $ \profile KernelSockets { .. } -> do
    -- If anything is going to be done with the profile information, do it now, after sockets have been
    -- bound but before we start listening on them infinitely.
    liftIO $ profileHandler profile

    let key = profileSignatureKey profile
        -- Repeat an action forever. If the thread is killed with a ThreadKilled exception,
        -- do not propagate the exception, but instead just let the thread die. This ensures that
        -- when all the Async's are linked together, the ThreadKilled does not get propagated to the
        -- main thread, which presumably is the thread that killed this action.
        loop action = 
            async $ liftBaseWith $ \runInBase ->
              catch (runInBase $ forever action) threadKilledHandler
        handlers = (commHandler, requestHandler)

    -- Start all listening loops in separate threads.
    let setupListeners = do
          async1 <- loop $ echoHeartbeat kernelHeartbeatSocket
          async2 <- loop $ serveRouter kernelControlSocket key kernelIopubSocket kernelStdinSocket handlers
          async3 <- loop $ serveRouter kernelShellSocket key kernelIopubSocket kernelStdinSocket handlers

          -- Make sure that a fatal exception on any thread kills all threads.
          liftIO $ link2 async1 async2
          liftIO $ link2 async2 async3
          liftIO $ link2 async3 async1

          return [async1, async2, async3]

    -- Wait indefinitely; if any of the threads encounter a fatal exception, the fatal exception is
    -- re-raised on the main thread. If the main thread dies, then the asyncs are killed via 'cancel'.
    liftBaseWith $ \runInBase ->
      bracket (runInBase setupListeners)
              (mapM_ cancel)
              (fmap snd . waitAny)

-- | Heartbeat once.
--
-- To heartbeat, listen for a message on the socket, and when you receive one, immediately write it
-- back to the same socket.
echoHeartbeat :: Socket z Rep -> ZMQ z ()
echoHeartbeat heartbeatSocket =
  receive heartbeatSocket >>= send heartbeatSocket []

-- | Receive and respond to a single message on the /shell/ or /control/ sockets.
serveRouter :: Socket z Router  -- ^ The /shell/ or /control/ socket to listen on and write to
            -> ByteString       -- ^ The signature key to sign messages with
            -> Socket z Pub     -- ^ The /iopub/ socket to publish outputs to
            -> Socket z Router  -- ^ The /stdin/ socket to use to get input from the client
            -> (CommHandler, ClientRequestHandler) -- ^ The handlers to use to respond to messages
            -> ZMQ z ()
serveRouter sock key iopub stdin handlers =
  -- We use 'liftBaseWith' and the resulting 'RunInBase' from the 'MonadBaseControl' class in order to
  -- hide from the kernel implementer the fact that all of this is running in the ZMQ monad. This ends
  -- up being very straightforward, because the ZMQ monad is a very thin layer over IO.
  liftBaseWith $ \runInBase -> do
    received <- runInBase $ receiveMessage sock
    case received of
      Left err -> liftIO $ hPutStrLn stderr $ "Error receiving message: " ++ err
      Right (header, message) ->
        -- After receiving a message, create the publisher callbacks which use that message as the "parent"
        -- for any responses they generate. This means that when outputs are generated in response to a
        -- message, they automatically inherit that message as a parent.
        let publishers = KernelCallbacks
              { sendComm = runInBase . sendReplyMessage key iopub header
              , sendKernelOutput = runInBase . sendReplyMessage key iopub header
              , sendKernelRequest = runInBase . stdinCommunicate header
              }
            sendReply = runInBase . sendReplyMessage key sock header
            sendReplyMessage k s parentHeader msg = do
              replyHeader <- liftIO $ mkReplyHeader parentHeader msg 
              sendMessage k s replyHeader msg
        in handleRequest sendReply publishers handlers message
  where
    stdinCommunicate parentHeader req = do
      header <- liftIO $ mkReplyHeader parentHeader req
      sendMessage key stdin header req
      received <- receiveMessage stdin
      case received of
        Left err ->
          -- There's no way to recover from this, so just die.
          messagingError "Jupyter.Kernel" $ "Unexpected failure parsing ClientReply message: " ++ err
        Right (_, message) -> return message

-- | Handle a request using the appropriate handler.
--
-- A request may either be a 'ClientRequest' or a 'Comm', which correspond to the
-- 'ClientRequestHandler' and the 'CommHandler' respectively. In the case of a 'ClientRequest', the
-- 'KernelReply' is also sent back to the frontend.
handleRequest :: (KernelReply -> IO ()) -- ^ Callback to send reply messages to the frontend
              -> KernelCallbacks -- ^ Callbacks for publishing outputs to frontends
              -> (CommHandler, ClientRequestHandler) -- ^ Handlers for messages from frontends
              -> Either ClientRequest Comm -- ^ The received message content
              -> IO ()
handleRequest sendReply callbacks (commHandler, requestHandler) message =
  case message of
    Left clientRequest -> do
      output $ KernelStatusOutput KernelBusy
      finally (requestHandler callbacks clientRequest >>= sendReply) $
        case clientRequest of
          ShutdownRequest restart -> do
            output $ ShutdownNotificationOutput restart
            output $ KernelStatusOutput KernelIdle
            exitSuccess
          _ -> output $ KernelStatusOutput KernelIdle
 
    Right comm -> commHandler callbacks comm
  where
    output = sendKernelOutput callbacks
