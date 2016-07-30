{-# LANGUAGE OverloadedStrings #-}
module Jupyter.Test.Client (clientTests) where

import           Control.Concurrent (newEmptyMVar, MVar, forkIO, putMVar, takeMVar, readMVar,
                                     modifyMVar_)
import           Control.Concurrent.Async (link, async)
import           System.Environment (setEnv)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (forM_)
import           Control.Monad.Catch (finally)
import           Control.Exception (SomeException)
import qualified Data.Map as Map

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase, testCaseSteps, (@=?), assertFailure, assertBool)

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Aeson (encode)
import           System.Process (spawnProcess, terminateProcess)

import           System.ZMQ4.Monadic (socket, Req(..), Dealer(..), send, receive, bind, connect, ZMQ,
                                      Socket, SocketType, runZMQ)

import           Jupyter.ZeroMQ
import           Jupyter.Kernel
import           Jupyter.Client
import           Jupyter.Messages
import           Jupyter.Messages.Metadata
import qualified Jupyter.UUID as UUID

import           Utils (inTempDir, connectedSocket)

clientTests :: TestTree
clientTests = testGroup "Client Tests" [testClient]

data MessageExchange =
       MessageExchange
         { exchangeRequest :: ClientRequest
         , exchangeReply :: KernelReply
         , exchangeKernelRequests :: [(KernelRequest, ClientReply)]
         , exchangeComms :: [Comm]
         , exchangeKernelOutputs :: [KernelOutput]
         }
  deriving (Eq, Show)

-- Test that messages can be sent and received on the heartbeat socket.
testClient :: TestTree
testClient = testCaseSteps "Simple Client" $ \step -> do
  currentExchangeVar <- newEmptyMVar
  kernelOutputsVar <- newEmptyMVar
  commsVar <- newEmptyMVar
  kernelRequestsVar <- newEmptyMVar
  clientRepliesVar <- newEmptyMVar
  let clientHandlers = ClientHandlers (kernelRequestHandler' clientRepliesVar kernelRequestsVar) (commHandler' commsVar) (kernelOutputHandler' kernelOutputsVar)

  inTempDir $ \tmpDir -> runClient Nothing Nothing clientHandlers $ \profile -> do
    proc <- liftIO $ do
      step "Starting kernel..."
      writeProfile profile "profile.json"

      -- set JPY_PARENT_PID to shut up the kernel about its Ctrl-C behaviour
      setEnv "JPY_PARENT_PID" "-1"

      spawnProcess "python" ["-m", "ipykernel", "-f", "profile.json"]

    liftIO $ step "Checking messages..."
    flip finally (liftIO $ terminateProcess proc) $
      forM_ (exchanges profile) $ \messageExchange -> do
        liftIO $ do
          putMVar currentExchangeVar messageExchange
          putMVar kernelOutputsVar []
          putMVar commsVar []
          putMVar kernelRequestsVar []

        reply <- sendClientRequest $ exchangeRequest messageExchange

        liftIO $ do
          exchangeReply messageExchange @=? reply

          receivedOutputs <- takeMVar kernelOutputsVar
          exchangeKernelOutputs messageExchange @=? reverse receivedOutputs

          receivedComms <- takeMVar commsVar
          exchangeComms messageExchange @=? reverse receivedComms

          receivedKernelRequests <- takeMVar kernelRequestsVar
          map fst (exchangeKernelRequests messageExchange) @=? reverse receivedKernelRequests

    
  where
    exchanges profile =
      [ MessageExchange
        { exchangeRequest = ConnectRequest
        , exchangeReply = ConnectReply
                            ConnectInfo
                              { connectShellPort = profileShellPort profile
                              , connectIopubPort = profileIopubPort profile
                              , connectStdinPort = profileStdinPort profile
                              , connectHeartbeatPort = profileHeartbeatPort profile
                              }
        , exchangeKernelRequests = []
        , exchangeComms = []
        , exchangeKernelOutputs = []
        }
      ]


kernelRequestHandler' :: MVar [(KernelRequest, ClientReply)] -> MVar [KernelRequest] -> (Comm -> IO ()) -> KernelRequest -> IO ClientReply
kernelRequestHandler' repliesVar var _ req = do
  modifyMVar_ var $ return . (req :)
  replies <- readMVar repliesVar
  case lookup req replies of
    Just reply -> return reply
    Nothing -> fail "Could not find appropriate client reply"


commHandler' :: MVar [Comm] -> (Comm -> IO ()) -> Comm -> IO ()
commHandler' var _ comm = modifyMVar_ var $ return . (comm :)

kernelOutputHandler' :: MVar [KernelOutput] -> (Comm -> IO ()) -> KernelOutput -> IO ()
kernelOutputHandler' var _ out = modifyMVar_ var $ return . (out :)

-- test what happens if exception is thrown in handler
-- test what happens if port is taken for client
