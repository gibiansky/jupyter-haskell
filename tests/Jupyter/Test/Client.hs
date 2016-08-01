{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Jupyter.Test.Client (clientTests) where

import           Control.Concurrent (newMVar, swapMVar, newEmptyMVar, MVar, forkIO, putMVar, takeMVar,
                                     tryTakeMVar, readMVar, modifyMVar_, threadDelay, tryReadMVar)
import           Control.Concurrent.Async (link, async)
import           System.Environment (setEnv)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (forM_, unless, void)
import           Control.Monad.Catch (finally)
import           Control.Exception (SomeException)
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Data.Maybe (listToMaybe)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase, testCaseSteps, (@=?), assertFailure, assertBool)

import           Data.ByteString (ByteString)
import qualified Data.Text as T
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
  liftIO $ print "setup the vars"

  inTempDir $ \tmpDir -> (>>) (liftIO $ print "in tmp dir") $ runClient Nothing Nothing clientHandlers $ \profile -> do
    liftIO $ print "running a client"
    proc <- liftIO $ do
      print "proc a proc"
      step "Starting kernel..."
      writeProfile profile "profile.json"

      -- set JPY_PARENT_PID to shut up the kernel about its Ctrl-C behaviour
      setEnv "JPY_PARENT_PID" "-1"

      spawnProcess "python" ["-m", "ipykernel", "-f", "profile.json"]
    liftIO $ print "proc a proc 2"

    -- Wait for the kernel to initialize. We know that the kernel is done initializing when it sends its
    -- first response; however, sometimes we also get a "starting" status. Since later on we check for
    -- equality of kernel outputs, we want to get rid of this timing inconsistencey immediately by just 
    -- doing on preparation message.
    liftIO $ step "Waiting for kernel to start..."
    sendClientRequest ConnectRequest
    liftIO $ do
      waitForKernelIdle kernelOutputsVar
      swapMVar kernelOutputsVar []

    -- Acquire the current session number. Without this, we can't accurately test the history replies,
    -- since they contain the session numbers. To acquire the session number, send an execute request followed
    -- by a history request.
    execReply <- sendClientRequest $ ExecuteRequest "3 + 3" defaultExecuteOptions
    execCount <- case execReply of
      ExecuteReply count _ -> return count
      _ -> fail "Expected ExecuteReply for ExecuteRequest"
    liftIO $ do
      waitForKernelIdle kernelOutputsVar
      swapMVar kernelOutputsVar []

    histReply <- sendClientRequest $ HistoryRequest $ HistoryOptions False True $ HistoryTail 1
    sessionNum <- case histReply of
      HistoryReply items -> return $ maybe 1 historyItemSession (listToMaybe items)
      _ -> fail "Expected HistoryReply for HistoryRequest"
    liftIO $ waitForKernelIdle kernelOutputsVar
    liftIO $ takeMVar kernelOutputsVar

    liftIO $ step "Checking messages exchanges..."
    flip finally (liftIO $ terminateProcess proc) $
      forM_ (mkMessageExchanges sessionNum execCount profile) $ \exchange@MessageExchange{..} -> do
        liftIO $ do
          step $ "\t..." ++ exchangeName
          putMVar kernelOutputsVar []
          putMVar commsVar []
          putMVar kernelRequestsVar []

          void $ tryTakeMVar clientRepliesVar
          putMVar clientRepliesVar exchangeKernelRequests

        reply <- sendClientRequest exchangeRequest

        liftIO $ do
          waitForKernelIdle kernelOutputsVar
          exchangeReply @=? reply

          receivedOutputs <- takeMVar kernelOutputsVar
          exchangeKernelOutputs @=? reverse receivedOutputs

          receivedComms <- takeMVar commsVar
          exchangeComms @=? reverse receivedComms

          receivedKernelRequests <- takeMVar kernelRequestsVar
          map fst exchangeKernelRequests  @=? reverse receivedKernelRequests

waitForKernelIdle :: MVar [KernelOutput] -> IO ()
waitForKernelIdle var = do
  outputs <- readMVar var
  unless (KernelStatusOutput KernelIdle `elem` outputs) $ do
    threadDelay 100000
    waitForKernelIdle var
    
mkMessageExchanges :: Int -> ExecutionCount -> KernelProfile -> [MessageExchange]
mkMessageExchanges sessionNum execCount profile =
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
    , exchangeReply = ExecuteReply (execCount + 1) ExecuteOk
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [ kernelBusy
                              , ExecuteInputOutput "import sys\nprint(sys.version.split()[0])" (execCount + 1)
                              , StreamOutput StreamStdout "3.5.0\n"
                              , kernelIdle
                              ]
    }
  , MessageExchange
    { exchangeName = "execute_request (expr)"
    , exchangeRequest = ExecuteRequest "3 + 3" defaultExecuteOptions
    , exchangeReply = ExecuteReply (execCount + 2) ExecuteOk
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [ kernelBusy
                              , ExecuteInputOutput "3 + 3" (execCount + 2)
                              , ExecuteResultOutput (execCount + 2) $ displayPlain "6"
                              , kernelIdle
                              ]
    }
  , MessageExchange
    { exchangeName = "execute_request (none)"
    , exchangeRequest = ExecuteRequest "" defaultExecuteOptions
    , exchangeReply = ExecuteReply (execCount + 2) ExecuteOk
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [kernelBusy, ExecuteInputOutput "" (execCount + 3), kernelIdle]
    }
  , MessageExchange
    { exchangeName = "execute_request (display)"
    , exchangeRequest = ExecuteRequest "from IPython.display import *\ndisplay(HTML('<b>Hi</b>'))"
                          defaultExecuteOptions
    , exchangeReply = ExecuteReply (execCount + 3) ExecuteOk
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [ kernelBusy
                              , ExecuteInputOutput
                                  "from IPython.display import *\ndisplay(HTML('<b>Hi</b>'))"
                                  (execCount + 3)
                              , DisplayDataOutput $ displayPlain
                                                      "<IPython.core.display.HTML object>" <> displayHtml
                                                                                                "<b>Hi</b>"
                              , kernelIdle
                              ]
    }
  , MessageExchange
    { exchangeName = "execute_request (input)"
    , exchangeRequest = ExecuteRequest "x = input('Hello')\nprint(x)\nx"
                          defaultExecuteOptions { executeAllowStdin = True }
    , exchangeReply = ExecuteReply (execCount + 4) ExecuteOk
    , exchangeKernelRequests = [ (InputRequest
                                    InputOptions { inputPassword = False, inputPrompt = "Hello" }, InputReply
                                                                                                     "stdin")
                               ]
    , exchangeComms = []
    , exchangeKernelOutputs = [ kernelBusy
                              , ExecuteInputOutput "x = input('Hello')\nprint(x)\nx" (execCount + 4)
                              , StreamOutput StreamStdout "stdin\n"
                              , ExecuteResultOutput (execCount + 4) $ displayPlain "'stdin'"
                              , kernelIdle
                              ]
    }
  , MessageExchange
    { exchangeName = "execute_request (password)"
    , exchangeRequest = ExecuteRequest "import getpass\nprint(getpass.getpass('Hello'))"
                          defaultExecuteOptions { executeAllowStdin = True }
    , exchangeReply = ExecuteReply (execCount + 5) ExecuteOk
    , exchangeKernelRequests = [ (InputRequest
                                    InputOptions { inputPassword = True, inputPrompt = "Hello" }, InputReply
                                                                                                    "stdin")
                               ]
    , exchangeComms = []
    , exchangeKernelOutputs = [ kernelBusy
                              , ExecuteInputOutput "import getpass\nprint(getpass.getpass('Hello'))"
                                  (execCount + 5)
                              , StreamOutput StreamStdout "stdin\n"
                              , kernelIdle
                              ]
    }
  , MessageExchange
    { exchangeName = "is_complete_request (complete)"
    , exchangeRequest = IsCompleteRequest "import getpass"
    , exchangeReply = IsCompleteReply CodeComplete
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [kernelBusy, kernelIdle]
    }
  , MessageExchange
    { exchangeName = "is_complete_request (incomplete)"
    , exchangeRequest = IsCompleteRequest "for x in [1, 2, 3]:\n"
    , exchangeReply = IsCompleteReply (CodeIncomplete "    ")
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [kernelBusy, kernelIdle]
    }
  , MessageExchange
    { exchangeName = "is_complete_request (invalid)"
    , exchangeRequest = IsCompleteRequest "x ="
    , exchangeReply = IsCompleteReply CodeInvalid
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [kernelBusy, kernelIdle]
    }
  , MessageExchange
    { exchangeName = "inspect_request (empty)"
    , exchangeRequest = InspectRequest "3" 1 DetailLow
    , exchangeReply = InspectReply (InspectOk Nothing)
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [kernelBusy, kernelIdle]
    }
  , MessageExchange
    { exchangeName = "inspect_request (low)"
    , exchangeRequest = InspectRequest "print" 5 DetailLow
    , exchangeReply = InspectReply $ InspectOk $ Just $
      displayPlain $ T.unlines
                       [ "\ESC[0;31mDocstring:\ESC[0m"
                       , "print(value, ..., sep=' ', end='\\n', file=sys.stdout, flush=False)"
                       , ""
                       , "Prints the values to a stream, or to sys.stdout by default."
                       , "Optional keyword arguments:"
                       , "file:  a file-like object (stream); defaults to the current sys.stdout."
                       , "sep:   string inserted between values, default a space."
                       , "end:   string appended after the last value, default a newline."
                       , "flush: whether to forcibly flush the stream."
                       , "\ESC[0;31mType:\ESC[0m      builtin_function_or_method"
                       ]
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [kernelBusy, kernelIdle]
    }
  , MessageExchange
    { exchangeName = "inspect_request (high)"
    , exchangeRequest = InspectRequest "print" 5 DetailHigh
    , exchangeReply = InspectReply $ InspectOk $ Just $
      displayPlain "\ESC[0;31mType:\ESC[0m builtin_function_or_method\n"
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [kernelBusy, kernelIdle]
    }
  , MessageExchange
    { exchangeName = "inspect_request (missing)"
    , exchangeRequest = InspectRequest "p" 1 DetailHigh
    , exchangeReply = InspectReply $ InspectOk Nothing
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [kernelBusy, kernelIdle]
    }
  , MessageExchange
    { exchangeName = "complete_request"
    , exchangeRequest = CompleteRequest "prinx" 4
    , exchangeReply = CompleteReply $ CompleteOk ["print"] (CursorRange 0 4) mempty
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [kernelBusy, kernelIdle]
    }
  , MessageExchange
    { exchangeName = "complete_request (missing)"
    , exchangeRequest = CompleteRequest "prinx" 5
    , exchangeReply = CompleteReply $ CompleteOk [] (CursorRange 0 5) mempty
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [kernelBusy, kernelIdle]
    }
  , MessageExchange
    { exchangeName = "history_request (tail)"
    , exchangeRequest = HistoryRequest $ HistoryOptions False True $ HistoryTail 3
    , exchangeReply = HistoryReply $
      [ HistoryItem sessionNum 4 "from IPython.display import *\ndisplay(HTML('<b>Hi</b>'))" Nothing
      , HistoryItem sessionNum 5 "x = input('Hello')\nprint(x)\nx" Nothing
      , HistoryItem sessionNum 6 "import getpass\nprint(getpass.getpass('Hello'))" Nothing
      ]
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [kernelBusy, kernelIdle]
    }
  , MessageExchange
    { exchangeName = "history_request (tail output)"
    , exchangeRequest = HistoryRequest $ HistoryOptions True True $ HistoryTail 3
    , exchangeReply = HistoryReply $
      [ HistoryItem sessionNum 4 "from IPython.display import *\ndisplay(HTML('<b>Hi</b>'))" Nothing
      , HistoryItem sessionNum 5 "x = input('Hello')\nprint(x)\nx" Nothing
      , HistoryItem sessionNum 6 "import getpass\nprint(getpass.getpass('Hello'))" Nothing
      ]
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [kernelBusy, kernelIdle]
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
