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
import qualified Data.HashMap.Strict as HashMap
import           Data.Monoid ((<>))
import           Data.Maybe (listToMaybe)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase, testCaseSteps, (@=?), assertFailure, assertBool)

import           Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import           Data.Aeson (encode, ToJSON(..), object, (.=))
import           Data.Aeson.Types (Value(..))
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
    liftIO $ print "about to send connect request"
    sendClientRequest ConnectRequest
    liftIO $ print "waiting on connect request"
    liftIO $ do
      waitForKernelIdle kernelOutputsVar
      print "waiting on kernel idle"
      swapMVar kernelOutputsVar []
    liftIO $ print "waiting on kernel idle 2"

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

          receivedComms <- takeMVar commsVar
          exchangeComms @=? reverse receivedComms

          receivedKernelRequests <- takeMVar kernelRequestsVar
          map fst exchangeKernelRequests  @=? reverse receivedKernelRequests

          receivedOutputs <- takeMVar kernelOutputsVar
          exchangeKernelOutputs @=? reverse receivedOutputs

waitForKernelIdle :: MVar [KernelOutput] -> IO ()
waitForKernelIdle var = do
  outputs <- readMVar var
  unless (KernelStatusOutput KernelIdle `elem` outputs) $ do
    threadDelay 100000
    print outputs
    print "waiting for kernel idle..."
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
                              , ExecuteInputOutput "import sys\nprint(sys.version.split()[0])"
                                  (execCount + 1)
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
    { exchangeName = "execute_request (clear)"
    , exchangeRequest = ExecuteRequest "from IPython.display import clear_output\nclear_output()"
                          defaultExecuteOptions
    , exchangeReply = ExecuteReply (execCount + 3) ExecuteOk
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [ kernelBusy
                              , ExecuteInputOutput
                                  "from IPython.display import clear_output\nclear_output()"
                                  (execCount + 3)
                              , ClearOutput ClearImmediately
                              , kernelIdle
                              ]
    }
  , MessageExchange
    { exchangeName = "execute_request (comms)"
    , exchangeRequest = ExecuteRequest "import ipywidgets as widgets\nwidgets.FloatSlider()"
                          defaultExecuteOptions
    , exchangeReply = ExecuteReply (execCount + 4) ExecuteOk
    , exchangeKernelRequests = []
    , exchangeComms = [ CommOpen
                          fakeUUID
                          (object
                             [ "align_items" .= str ""
                             , "_view_module" .= str "jupyter-js-widgets"
                             , "height" .= str ""
                             , "bottom" .= str ""
                             , "display" .= str ""
                             , "overflow_y" .= str ""
                             , "min_height" .= str ""
                             , "_view_name" .= str "LayoutView"
                             , "justify_content" .= str ""
                             , "left" .= str ""
                             , "min_width" .= str ""
                             , "overflow_x" .= str ""
                             , "width" .= str ""
                             , "margin" .= str ""
                             , "visibility" .= str ""
                             , "msg_throttle" .= toJSON (3 :: Int)
                             , "overflow" .= str ""
                             , "border" .= str ""
                             , "max_height" .= str ""
                             , "flex" .= str ""
                             , "flex_flow" .= str ""
                             , "max_width" .= str ""
                             , "_model_module" .= str "jupyter-js-widgets"
                             , "right" .= str ""
                             , "_model_name" .= str "LayoutModel"
                             , "top" .= str ""
                             , "align_content" .= str ""
                             , "align_self" .= str ""
                             , "padding" .= str ""
                             ])
                          "jupyter.widget"
                          Nothing
                      , CommOpen
                          fakeUUID
                          (object
                             [ "max" .= toJSON (100 :: Int)
                             , "readout" .= True
                             , "background_color" .= (Nothing :: Maybe ())
                             , "slider_color" .= (Nothing :: Maybe ())
                             , "_view_module" .= str "jupyter-js-widgets"
                             , "font_family" .= str ""
                             , "_view_name" .= str "FloatSliderView"
                             , "color" .= (Nothing :: Maybe ())
                             , "disabled" .= False
                             , "value" .= toJSON (0 :: Int)
                             , "visible" .= True
                             , "msg_throttle" .= toJSON (3 :: Int)
                             , "font_weight" .= str ""
                             , "step" .= toJSON (0.1 :: Float)
                             , "min" .= toJSON (0 :: Int)
                             , "_model_module" .= str "jupyter-js-widgets"
                             , "readout_format" .= str ".2f"
                             , "_model_name" .= str "FloatSliderModel"
                             , "_range" .= False
                             , "continuous_update" .= True
                             , "font_style" .= str ""
                             , "orientation" .= str "horizontal"
                             , "_dom_classes" .= ([] :: [()])
                             , "description" .= str ""
                             , "font_size" .= str ""
                             ])
                          "jupyter.widget"
                          Nothing
                      , CommMessage fakeUUID (object ["method" .= str "display"])
                      ]
    , exchangeKernelOutputs = [ kernelBusy
                              , ExecuteInputOutput
                                  "import ipywidgets as widgets\nwidgets.FloatSlider()"
                                  (execCount + 4)
                              , StreamOutput StreamStderr $
                                T.unwords
                                  [ "Widget Javascript not detected. "
                                  , "It may not be installed properly."
                                  , "Did you enable the widgetsnbextension?"
                                  , "If not, then run"
                                  , "\"jupyter nbextension enable --py --sys-prefix widgetsnbextension\"\n"
                                  ]
                              , kernelIdle
                              ]
    }
  , MessageExchange
    { exchangeName = "execute_request (display)"
    , exchangeRequest = ExecuteRequest "from IPython.display import *\ndisplay(HTML('<b>Hi</b>'))"
                          defaultExecuteOptions
    , exchangeReply = ExecuteReply (execCount + 5) ExecuteOk
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [ kernelBusy
                              , ExecuteInputOutput
                                  "from IPython.display import *\ndisplay(HTML('<b>Hi</b>'))"
                                  (execCount + 5)
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
    , exchangeReply = ExecuteReply (execCount + 6) ExecuteOk
    , exchangeKernelRequests = [ (InputRequest
                                    InputOptions { inputPassword = False, inputPrompt = "Hello" }, InputReply
                                                                                                     "stdin")
                               ]
    , exchangeComms = []
    , exchangeKernelOutputs = [ kernelBusy
                              , ExecuteInputOutput "x = input('Hello')\nprint(x)\nx" (execCount + 6)
                              , StreamOutput StreamStdout "stdin\n"
                              , ExecuteResultOutput (execCount + 6) $ displayPlain "'stdin'"
                              , kernelIdle
                              ]
    }
  , MessageExchange
    { exchangeName = "execute_request (password)"
    , exchangeRequest = ExecuteRequest "import getpass\nprint(getpass.getpass('Hello'))"
                          defaultExecuteOptions { executeAllowStdin = True }
    , exchangeReply = ExecuteReply (execCount + 7) ExecuteOk
    , exchangeKernelRequests = [ (InputRequest
                                    InputOptions { inputPassword = True, inputPrompt = "Hello" }, InputReply
                                                                                                    "stdin")
                               ]
    , exchangeComms = []
    , exchangeKernelOutputs = [ kernelBusy
                              , ExecuteInputOutput "import getpass\nprint(getpass.getpass('Hello'))"
                                  (execCount + 7)
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
      [ HistoryItem sessionNum 6 "from IPython.display import *\ndisplay(HTML('<b>Hi</b>'))" Nothing
      , HistoryItem sessionNum 7 "x = input('Hello')\nprint(x)\nx" Nothing
      , HistoryItem sessionNum 8 "import getpass\nprint(getpass.getpass('Hello'))" Nothing
      ]
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [kernelBusy, kernelIdle]
    }
  , MessageExchange
    { exchangeName = "history_request (tail output)"
    , exchangeRequest = HistoryRequest $ HistoryOptions True True $ HistoryTail 3
    , exchangeReply = HistoryReply $
      [ HistoryItem sessionNum 6 "from IPython.display import *\ndisplay(HTML('<b>Hi</b>'))" Nothing
      , HistoryItem sessionNum 7 "x = input('Hello')\nprint(x)\nx" Nothing
      , HistoryItem sessionNum 8 "import getpass\nprint(getpass.getpass('Hello'))" Nothing
      ]
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [kernelBusy, kernelIdle]
    }
  , MessageExchange
    { exchangeName = "history_request (range)"
    , exchangeRequest = HistoryRequest $ HistoryOptions False True $
      HistoryRange $ HistoryRangeOptions sessionNum 6 8
    , exchangeReply = HistoryReply $
      [ HistoryItem 0 6 "from IPython.display import *\ndisplay(HTML('<b>Hi</b>'))" Nothing
      , HistoryItem 0 7 "x = input('Hello')\nprint(x)\nx" Nothing
      ]
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [kernelBusy, kernelIdle]
    }
  , MessageExchange
    { exchangeName = "history_request (range output)"
    , exchangeRequest = HistoryRequest $ HistoryOptions True True $ HistoryRange $ HistoryRangeOptions
                                                                                     sessionNum
                                                                                     6
                                                                                     8
    , exchangeReply = HistoryReply $
      [ HistoryItem 0 6 "from IPython.display import *\ndisplay(HTML('<b>Hi</b>'))" Nothing
      , HistoryItem 0 7 "x = input('Hello')\nprint(x)\nx" (Just "'stdin'")
      ]
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [kernelBusy, kernelIdle]
    }
  , MessageExchange
    { exchangeName = "history_request (search)"
    , exchangeRequest = HistoryRequest $ HistoryOptions True True $ HistorySearch $ HistorySearchOptions
                                                                                      1
                                                                                      "x = input('Hello')\nprint(?)\nx"
                                                                                      False
    , exchangeReply = HistoryReply $
      [HistoryItem sessionNum 7 "x = input('Hello')\nprint(x)\nx" Nothing]
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [kernelBusy, kernelIdle]
    }
  , MessageExchange
    { exchangeName = "history_request (search output)"
    , exchangeRequest = HistoryRequest $ HistoryOptions True True $ HistorySearch $ HistorySearchOptions
                                                                                      1
                                                                                      "x = input('Hello')\nprint(?)\nx"
                                                                                      False
    , exchangeReply = HistoryReply $
      [HistoryItem sessionNum 7 "x = input('Hello')\nprint(x)\nx" Nothing]
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [kernelBusy, kernelIdle]
    }
  , MessageExchange
    { exchangeName = "kernel_info_request"
    , exchangeRequest = KernelInfoRequest
    , exchangeReply = KernelInfoReply $
      KernelInfo
        { kernelProtocolVersion = "5.0"
        , kernelBanner = T.unlines
                           [ "Python 3.5.0 (default, Oct  3 2015, 11:20:34) "
                           , "Type \"copyright\", \"credits\" or \"license\" for more information."
                           , ""
                           , "IPython 5.0.0 -- An enhanced Interactive Python."
                           , "?         -> Introduction and overview of IPython's features."
                           , "%quickref -> Quick reference."
                           , "help      -> Python's own help system."
                           , "object?   -> Details about 'object', use 'object??' for extra details."
                           ]
        , kernelImplementation = "ipython"
        , kernelImplementationVersion = "5.0.0"
        , kernelLanguageInfo = LanguageInfo
          { languageName = "python"
          , languageVersion = "3.5.0"
          , languageMimetype = "text/x-python"
          , languageFileExtension = ".py"
          , languagePygmentsLexer = Just "ipython3"
          , languageCodeMirrorMode = Just $
            OptionsMode "ipython" [("version", toJSON (3 :: Int))]
          , languageNbconvertExporter = Just "python"
          }
        , kernelHelpLinks = [ HelpLink
                              { helpLinkText = "Python"
                              , helpLinkURL = "http://docs.python.org/3.5"
                              }
                            , HelpLink
                              { helpLinkText = "IPython"
                              , helpLinkURL = "http://ipython.org/documentation.html"
                              }
                            , HelpLink
                              { helpLinkText = "NumPy"
                              , helpLinkURL = "http://docs.scipy.org/doc/numpy/reference/"
                              }
                            , HelpLink
                              { helpLinkText = "SciPy"
                              , helpLinkURL = "http://docs.scipy.org/doc/scipy/reference/"
                              }
                            , HelpLink
                              { helpLinkText = "Matplotlib"
                              , helpLinkURL = "http://matplotlib.org/contents.html"
                              }
                            , HelpLink
                              { helpLinkText = "SymPy"
                              , helpLinkURL = "http://docs.sympy.org/latest/index.html"
                              }
                            , HelpLink
                              { helpLinkText = "pandas"
                              , helpLinkURL = "http://pandas.pydata.org/pandas-docs/stable/"
                              }
                            ]
        }
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [kernelBusy, kernelIdle]
    }
  , MessageExchange
    { exchangeName = "shutdown (restart)"
    , exchangeRequest = ShutdownRequest Restart
    , exchangeReply = ShutdownReply Restart
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [kernelBusy, ShutdownNotificationOutput Restart, kernelIdle]
    }
  ]
  where
    kernelIdle = KernelStatusOutput KernelIdle
    kernelBusy = KernelStatusOutput KernelBusy

    str :: String -> String
    str = id

fakeUUID :: UUID.UUID
fakeUUID = UUID.uuidFromString "fake"


kernelRequestHandler' :: MVar [(KernelRequest, ClientReply)] -> MVar [KernelRequest] -> (Comm -> IO ()) -> KernelRequest -> IO ClientReply
kernelRequestHandler' repliesVar var _ req = do
  modifyMVar_ var $ return . (req :)
  replies <- readMVar repliesVar
  case lookup req replies of
    Just reply -> return reply
    Nothing -> fail "Could not find appropriate client reply"


commHandler' :: MVar [Comm] -> (Comm -> IO ()) -> Comm -> IO ()
commHandler' var _ comm = modifyMVar_ var $ return . (comm' :)
  where
    comm' =
      case comm of
        CommOpen _ val b c -> CommOpen fakeUUID (dropJSONKey "layout" val) b c
        CommClose _ a -> CommClose fakeUUID a
        CommMessage _ a -> CommMessage fakeUUID a

    dropJSONKey key val =
      case val of
        Object o -> Object (HashMap.delete key o)
        other -> other

kernelOutputHandler' :: MVar [KernelOutput] -> (Comm -> IO ()) -> KernelOutput -> IO ()
kernelOutputHandler' var _ out =
  modifyMVar_ var $ return . (out :)

-- test what happens if exception is thrown in handler
-- test what happens if port is taken for client
