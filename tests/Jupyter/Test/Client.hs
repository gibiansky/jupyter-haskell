{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Jupyter.Test.Client (clientTests) where

import           Control.Concurrent (newMVar, swapMVar, newEmptyMVar, MVar, forkIO, putMVar, takeMVar, tryTakeMVar, readMVar,
                                     modifyMVar_, threadDelay, tryReadMVar)
import           Control.Concurrent.Async (link, async)
import           System.Environment (setEnv)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (forM_, unless, void)
import           Control.Monad.Catch (finally)
import           Control.Exception (SomeException)
import qualified Data.Map as Map
import Data.Monoid ((<>))

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
         { exchangeName :: String
         , exchangeRequest :: ClientRequest
         , exchangeReply :: KernelReply
         , exchangeKernelRequests :: [(KernelRequest, ClientReply)]
         , exchangeComms :: [Comm]
         , exchangeKernelOutputs :: [KernelOutput]
         }
  deriving (Eq, Show)

-- Test that messages can be sent and received on the heartbeat socket.
testClient :: TestTree
testClient = testCaseSteps "Simple Client" $ \step -> do
  kernelOutputsVar <- newMVar []
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

    -- Wait for the kernel to initialize. We know that the kernel is done initializing when it sends its
    -- first response; however, sometimes we also get a "starting" status. Since later on we check for
    -- equality of kernel outputs, we want to get rid of this timing inconsistencey immediately by just 
    -- doing on preparation message.
    liftIO $ step "Waiting for kernel to start..."
    sendClientRequest ConnectRequest
    liftIO $ waitForKernelOutput kernelOutputsVar $ elem $ KernelStatusOutput KernelIdle
    liftIO $ takeMVar kernelOutputsVar

    liftIO $ step "Checking messages exchanges..."
    flip finally (liftIO $ terminateProcess proc) $
      forM_ (mkMessageExchanges profile) $ \exchange@MessageExchange{..} -> do
        liftIO $ do
          step $ "\t..." ++ exchangeName
          putMVar kernelOutputsVar []
          putMVar commsVar []
          putMVar kernelRequestsVar []

          void $ tryTakeMVar clientRepliesVar
          putMVar clientRepliesVar exchangeKernelRequests

        reply <- sendClientRequest exchangeRequest

        liftIO $ do
          waitForKernelOutput kernelOutputsVar (elem $ KernelStatusOutput KernelIdle)
          exchangeReply @=? reply

          receivedOutputs <- takeMVar kernelOutputsVar
          exchangeKernelOutputs @=? reverse receivedOutputs

          receivedComms <- takeMVar commsVar
          exchangeComms @=? reverse receivedComms

          receivedKernelRequests <- takeMVar kernelRequestsVar
          map fst exchangeKernelRequests  @=? reverse receivedKernelRequests

waitForKernelOutput :: MVar [KernelOutput] -> ([KernelOutput] -> Bool) -> IO ()
waitForKernelOutput var cond = do
  outputs <- readMVar var
  unless (cond outputs) $ do
    threadDelay 100000
    waitForKernelOutput var cond
    
mkMessageExchanges :: KernelProfile -> [MessageExchange]
mkMessageExchanges profile =
  [ MessageExchange
    { exchangeName = "connect_request"
    , exchangeRequest = ConnectRequest
    , exchangeReply = ConnectReply
                        ConnectInfo
                          { connectShellPort = profileShellPort profile
                          , connectIopubPort = profileIopubPort profile
                          , connectStdinPort = profileStdinPort profile
                          , connectHeartbeatPort = profileHeartbeatPort profile
                          }
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [kernelBusy, kernelIdle]
    }
  , MessageExchange
    { exchangeName = "execute_request (stream output)"
    , exchangeRequest = ExecuteRequest "import sys\nprint(sys.version.split()[0])"
                          defaultExecuteOptions
    , exchangeReply = ExecuteReply 1 ExecuteOk
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [ kernelBusy
                              , ExecuteInputOutput "import sys\nprint(sys.version.split()[0])" 1
                              , StreamOutput StreamStdout "3.5.0\n"
                              , kernelIdle
                              ]
    }
  , MessageExchange
    { exchangeName = "execute_request (expr)"
    , exchangeRequest = ExecuteRequest "3 + 3" defaultExecuteOptions
    , exchangeReply = ExecuteReply 2 ExecuteOk
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [ kernelBusy
                              , ExecuteInputOutput "3 + 3" 2
                              , ExecuteResultOutput 2 $ displayPlain "6"
                              , kernelIdle
                              ]
    }
  , MessageExchange
    { exchangeName = "execute_request (none)"
    , exchangeRequest = ExecuteRequest "" defaultExecuteOptions
    , exchangeReply = ExecuteReply 2 ExecuteOk
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [kernelBusy, ExecuteInputOutput "" 3, kernelIdle]
    }
  , MessageExchange
    { exchangeName = "execute_request (display)"
    , exchangeRequest = ExecuteRequest "from IPython.display import *\ndisplay(HTML('<b>Hi</b>'))"
                          defaultExecuteOptions
    , exchangeReply = ExecuteReply 3 ExecuteOk
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [ kernelBusy
                              , ExecuteInputOutput
                                  "from IPython.display import *\ndisplay(HTML('<b>Hi</b>'))"
                                  3
                              , DisplayDataOutput $ displayPlain
                                                      "<IPython.core.display.HTML object>" <> displayHtml
                                                                                                "<b>Hi</b>"
                              , kernelIdle
                              ]
    }
  , MessageExchange
    { exchangeName = "execute_request (input)"
    , exchangeRequest = ExecuteRequest "print(input('Hello'))"
                          defaultExecuteOptions { executeAllowStdin = True }
    , exchangeReply = ExecuteReply 4 ExecuteOk
    , exchangeKernelRequests = [ (InputRequest
                                    InputOptions { inputPassword = False, inputPrompt = "Hello" }, InputReply
                                                                                                     "stdin")
                               ]
    , exchangeComms = []
    , exchangeKernelOutputs = [ kernelBusy
                              , ExecuteInputOutput "print(input('Hello'))" 4
                              , StreamOutput StreamStdout "stdin\n"
                              , kernelIdle
                              ]
    }
  , MessageExchange
    { exchangeName = "execute_request (password)"
    , exchangeRequest = ExecuteRequest "import getpass\nprint(getpass.getpass('Hello'))"
                          defaultExecuteOptions { executeAllowStdin = True }
    , exchangeReply = ExecuteReply 5 ExecuteOk
    , exchangeKernelRequests = [ (InputRequest
                                    InputOptions { inputPassword = True, inputPrompt = "Hello" }, InputReply
                                                                                                    "stdin")
                               ]
    , exchangeComms = []
    , exchangeKernelOutputs = [ kernelBusy
                              , ExecuteInputOutput "import getpass\nprint(getpass.getpass('Hello'))" 5
                              , StreamOutput StreamStdout "stdin\n"
                              , kernelIdle
                              ]
    }
  ]
  where
    kernelIdle = KernelStatusOutput KernelIdle
    kernelBusy = KernelStatusOutput KernelBusy


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
kernelOutputHandler' var _ out =
  modifyMVar_ var $ return . (out :)

-- test what happens if exception is thrown in handler
-- test what happens if port is taken for client
