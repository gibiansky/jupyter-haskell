{-# LANGUAGE OverloadedStrings #-}
module Jupyter.Test.Client (clientTests) where

-- Imports from 'base'
import           Control.Exception (throwIO, bracket)
import           Control.Monad (forM_, void)
import           Data.Monoid ((<>))

-- Imports from 'transformers'
import           Control.Monad.IO.Class (liftIO)

-- Imports from 'tasty'
import           Test.Tasty (TestTree, testGroup)

-- Imports from 'tasty-hunit'
import           Test.Tasty.HUnit (testCase, (@=?), assertBool)

-- Imports from 'text'
import           Data.Text (Text)
import qualified Data.Text as T

-- Imports from 'aeson'
import           Data.Aeson (ToJSON(..), object, (.=))

-- Imports from 'process'
import           System.Process (terminateProcess, ProcessHandle)

-- Imports from 'jupyter'
import           Jupyter.Client
import           Jupyter.Install
import           Jupyter.Kernel
import           Jupyter.Messages

import           Jupyter.Test.MessageExchange
import           Jupyter.Test.Utils (inTempDir, shouldThrow, HandlerException(..))

clientTests :: TestTree
clientTests = testGroup "Client Tests"
                [ testBasic
                , testStdin
                , testCalculator
                , testClientPortsTaken
                , testClient
                , testHandlerExceptions
                , testFindingKernelspecs
                ]

testFindingKernelspecs :: TestTree
testFindingKernelspecs = testCase "Finding Kernelspecs" $ do
  kernels <- findKernels
  let kernelNames = map kernelspecDisplayName kernels
  assertBool "Basic kernelspec not found" $ "Basic" `elem` kernelNames
  assertBool "Calculator kernelspec not found" $ "Calculator" `elem` kernelNames
  assertBool "Python 3 kernelspec not found" $ "Python 3" `elem` kernelNames
  assertBool "Stdin kernelspec not found" $ "Stdin" `elem` kernelNames

  let expectedKernels = [("basic", "Basic"), ("stdin", "Stdin"), ("calculator", "Calculator")]
  forM_ expectedKernels $ \(name, displayName) -> do
    Just kernel <- findKernel name
    kernelspecLanguage kernel @=? name
    kernelspecDisplayName kernel @=? displayName
    assertBool "Connection file command doesn't include connection file" $
      "abcxyz" `elem` kernelspecCommand kernel "" "abcxyz"

testBasic :: TestTree
testBasic = 
  testMessageExchange "Basic Kernel" (commandFromKernelspec "basic") "" $
    \_ _ profile ->
      [ MessageExchange
        { exchangeName = "execute_request"
        , exchangeRequest = ExecuteRequest "some input" defaultExecuteOptions
        , exchangeReply = ExecuteReply 0 ExecuteOk
        , exchangeKernelRequests = []
        , exchangeComms = []
        , exchangeKernelOutputs = [kernelBusy, ExecuteInputOutput 0 "some input", kernelIdle]
        }
      , MessageExchange
        { exchangeName = "inspect_request"
        , exchangeRequest = InspectRequest "3" 1 DetailLow
        , exchangeReply = InspectReply (InspectOk Nothing)
        , exchangeKernelRequests = []
        , exchangeComms = []
        , exchangeKernelOutputs = [kernelBusy, kernelIdle]
        }
      , MessageExchange
        { exchangeName = "complete_request"
        , exchangeRequest = CompleteRequest "prinx" 5
        , exchangeReply = CompleteReply $ CompleteOk [] (CursorRange 5 5) mempty
        , exchangeKernelRequests = []
        , exchangeComms = []
        , exchangeKernelOutputs = [kernelBusy, kernelIdle]
        }
      ] ++ defaultMessageExchange "Basic" profile

defaultMessageExchange :: Text -> KernelProfile -> [MessageExchange]
defaultMessageExchange name profile =
  [ MessageExchange
    { exchangeName = "connect_request"
    , exchangeRequest = ConnectRequest
    , exchangeReply = ConnectReply
                        ConnectInfo
                          { connectShellPort = profileShellPort profile
                          , connectIopubPort = profileIopubPort profile
                          , connectStdinPort = profileStdinPort profile
                          , connectHeartbeatPort = profileHeartbeatPort profile
                          , connectControlPort = profileControlPort profile
                          }
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [kernelBusy, kernelIdle]
    }
  , MessageExchange
    { exchangeName = "kernel_info_request"
    , exchangeRequest = KernelInfoRequest
    , exchangeReply = KernelInfoReply $ simpleKernelInfo name
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [kernelBusy, kernelIdle]
    }
  , MessageExchange
    { exchangeName = "history_request"
    , exchangeRequest = HistoryRequest $ HistoryOptions False True $ HistoryTail 3
    , exchangeReply = HistoryReply []
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

testStdin :: TestTree
testStdin =
  testMessageExchange "Stdin Kernel" (commandFromKernelspec "stdin") "skip" $
    \_ _ profile ->
      [ MessageExchange
        { exchangeName = "execute_request (input)"
        , exchangeRequest = ExecuteRequest "prompt"
                              defaultExecuteOptions { executeAllowStdin = True }
        , exchangeReply = ExecuteReply 1 ExecuteOk
        , exchangeKernelRequests = [ (InputRequest
                                        InputOptions
                                          { inputPassword = False
                                          , inputPrompt = "prompt"
                                          }, InputReply "stdin")
                                   ]
        , exchangeComms = []
        , exchangeKernelOutputs = [ kernelBusy
                                  , ExecuteInputOutput 1 "prompt"
                                  , DisplayDataOutput $ displayPlain "stdin"
                                  , kernelIdle
                                  ]
        }
      , MessageExchange
        { exchangeName = "execute_request (skip input)"
        , exchangeRequest = ExecuteRequest "skip"
                              defaultExecuteOptions { executeAllowStdin = True }
        , exchangeReply = ExecuteReply 1 ExecuteOk
        , exchangeKernelRequests = []
        , exchangeComms = []
        , exchangeKernelOutputs = [ kernelBusy
                                  , ExecuteInputOutput 1 "skip"
                                  , kernelIdle
                                  ]
        }
      , MessageExchange
        { exchangeName = "execute_request (input password)"
        , exchangeRequest = ExecuteRequest "password"
                              defaultExecuteOptions { executeAllowStdin = True }
        , exchangeReply = ExecuteReply 1 ExecuteOk
        , exchangeKernelRequests = [ (InputRequest
                                        InputOptions
                                          { inputPassword = True
                                          , inputPrompt = "password"
                                          }, InputReply "stdin two")
                                   ]
        , exchangeComms = []
        , exchangeKernelOutputs = [ kernelBusy
                                  , ExecuteInputOutput 1 "password"
                                  , DisplayDataOutput $ displayPlain "stdin two"
                                  , kernelIdle
                                  ]
        }
      ] ++ defaultMessageExchange "Stdin" profile

testCalculator :: TestTree
testCalculator =
  testMessageExchange "Calculator Kernel" (commandFromKernelspec "calculator") "Lit 5" $
    \_ execCount profile ->
      [ MessageExchange
        { exchangeName = "connect_request"
        , exchangeRequest = ConnectRequest
        , exchangeReply = ConnectReply
                            ConnectInfo
                              { connectShellPort = profileShellPort profile
                              , connectIopubPort = profileIopubPort profile
                              , connectStdinPort = profileStdinPort profile
                              , connectHeartbeatPort = profileHeartbeatPort profile
                              , connectControlPort = profileControlPort profile
                              }
        , exchangeKernelRequests = []
        , exchangeComms = []
        , exchangeKernelOutputs = [kernelBusy, kernelIdle]
        }
      , MessageExchange
        { exchangeName = "execute_request (compute)"
        , exchangeRequest = ExecuteRequest
                              "Compute [('x', 3)] (Add (Divide (Lit 100) (Lit 5)) (Multiply (Lit 10) (Var 'x')))"
                              defaultExecuteOptions
        , exchangeReply = ExecuteReply (execCount + 1) ExecuteOk
        , exchangeKernelRequests = []
        , exchangeComms = []
        , exchangeKernelOutputs = [ kernelBusy
                                  , ExecuteInputOutput
                                      (execCount + 1)
                                      "Compute [('x', 3)] (Add (Divide (Lit 100) (Lit 5)) (Multiply (Lit 10) (Var 'x')))"
                                  , DisplayDataOutput $ displayPlain "50"
                                  , kernelIdle
                                  ]
        }
      , MessageExchange
        { exchangeName = "execute_request (compute)"
        , exchangeRequest = ExecuteRequest
                              "Print (Add (Divide (Lit 100) (Lit 5)) (Multiply (Lit 10) (Var 'x')))"
                              defaultExecuteOptions
        , exchangeReply = ExecuteReply (execCount + 2) ExecuteOk
        , exchangeKernelRequests = []
        , exchangeComms = []
        , exchangeKernelOutputs = [ kernelBusy
                                  , ExecuteInputOutput
                                      (execCount + 2)
                                      "Print (Add (Divide (Lit 100) (Lit 5)) (Multiply (Lit 10) (Var 'x')))"
                                  , DisplayDataOutput $
                                    displayPlain "((100 / 5) + (10 * x))" <> displayLatex
                                                                               "(\\frac{100}{5} + (10 \\cdot x))"
                                  , kernelIdle
                                  ]
        }
      , MessageExchange
        { exchangeName = "complete_request"
        , exchangeRequest = CompleteRequest "Computx" 6
        , exchangeReply = CompleteReply $ CompleteOk ["Compute"] (CursorRange 0 6) mempty
        , exchangeKernelRequests = []
        , exchangeComms = []
        , exchangeKernelOutputs = [kernelBusy, kernelIdle]
        }
      , MessageExchange
        { exchangeName = "inspect_request (low)"
        , exchangeRequest = InspectRequest "Printblahblah" 5 DetailLow
        , exchangeReply = InspectReply $ InspectOk $ Just $
          displayPlain "Print: Print an expression as text or LaTeX."
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
            , kernelBanner = "Welcome to the Haskell Calculator Test Kernel!"
            , kernelImplementation = "Calculator-Kernel"
            , kernelImplementationVersion = "1.0"
            , kernelLanguageInfo = LanguageInfo
              { languageName = "calculator"
              , languageVersion = "1.0"
              , languageMimetype = "text/plain"
              , languageFileExtension = ".txt"
              , languagePygmentsLexer = Nothing
              , languageCodeMirrorMode = Nothing
              , languageNbconvertExporter = Nothing
              }
            , kernelHelpLinks = [ HelpLink
                                  { helpLinkText = "jupyter package doc"
                                  , helpLinkURL = "http://github.com/gibiansky/jupyter-haskell"
                                  }
                                ]
            }
        , exchangeKernelRequests = []
        , exchangeComms = []
        , exchangeKernelOutputs = [kernelBusy, kernelIdle]
        }
      , MessageExchange
        { exchangeName = "history_request"
        , exchangeRequest = HistoryRequest $ HistoryOptions False True $ HistoryTail 3
        , exchangeReply = HistoryReply []
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

commandFromKernelspec :: Text -> IO (FilePath -> [String])
commandFromKernelspec name = do
  kernel <- findKernel name
  case kernel of
    Nothing   -> fail $ "Could not find kernelspec " ++ T.unpack name
    Just spec -> return $ kernelspecCommand spec ""
  
startIPythonKernel :: KernelProfile -> IO ProcessHandle
startIPythonKernel = startKernel $ \profileFile -> ["python", "-m", "ipykernel", "-f", profileFile]


testHandlerExceptions :: TestTree
testHandlerExceptions = testCase "Client Handler Exceptions" $ do
  let exception = const $ const $ throwIO HandlerException
      returnStdin = const . const . return $ InputReply "<>"
      handlerKernelRequestException = ClientHandlers exception defaultClientCommHandler defaultKernelOutputHandler
      handlerCommException = ClientHandlers returnStdin exception defaultKernelOutputHandler
      handlerKernelOutputException = ClientHandlers returnStdin defaultClientCommHandler exception

  -- ConnectRequest results in status updates, so erroring on the kernel output
  -- should raise an exception in the main thread.
  raisesHandlerException $ runIPython handlerKernelOutputException $ \_ _ ->
    sendClientRequest ConnectRequest

  -- ConnectRequest does not sent any stdin messages, so clients that error
  -- when handling stdin messages should not crash here.
  void $ runIPython handlerKernelRequestException $ \_ _ ->
    sendClientRequest ConnectRequest

  -- This particular ExecuteRequest should reply with comm messages, and
  -- so a comm handler that raises an exception should cause the main thread to crash.
  raisesHandlerException $ runIPython handlerCommException $ \_ _ ->
    sendClientRequest $
      ExecuteRequest "import ipywidgets as widgets\nwidgets.FloatSlider()" defaultExecuteOptions

  -- This particular ExecuteRequest should reply with kernel requests for stdin, and
  -- so a kernel request handler that raises an exception should cause the main thread to crash.
  raisesHandlerException $ runIPython handlerKernelRequestException $ \_ _ ->
    sendClientRequest $
      ExecuteRequest "print(input())" defaultExecuteOptions { executeAllowStdin = True }

  where
    runIPython = runKernelAndClient startIPythonKernel
    raisesHandlerException io = io `shouldThrow` [HandlerException]

defaultClientCommHandler :: (Comm -> IO ()) -> Comm -> IO ()
defaultClientCommHandler _ _ = return ()

defaultKernelOutputHandler :: (Comm -> IO ()) -> KernelOutput -> IO ()
defaultKernelOutputHandler _ _ = return ()

testClientPortsTaken :: TestTree
testClientPortsTaken = testCase "Client Ports Taken"  $
  inTempDir $ \_ ->
    runClient Nothing Nothing emptyHandler $ \profile1 -> liftIO $
      bracket (startIPythonKernel profile1) terminateProcess $ const $
        runClient Nothing Nothing emptyHandler $ \profile2 -> liftIO $
          bracket (startIPythonKernel profile2) terminateProcess $ const $
            runClient Nothing Nothing emptyHandler $ \profile3 -> liftIO $ do
              1 + profileShellPort profile1     @=? profileShellPort profile2
              1 + profileHeartbeatPort profile1 @=? profileHeartbeatPort profile2
              1 + profileControlPort profile1   @=? profileControlPort profile2
              1 + profileStdinPort profile1     @=? profileStdinPort profile2
              1 + profileIopubPort profile1     @=? profileIopubPort profile2
              1 + profileShellPort profile2     @=? profileShellPort profile3
              1 + profileHeartbeatPort profile2 @=? profileHeartbeatPort profile3
              1 + profileControlPort profile2   @=? profileControlPort profile3
              1 + profileStdinPort profile2     @=? profileStdinPort profile3
              1 + profileIopubPort profile2     @=? profileIopubPort profile3
    where
      emptyHandler =
          ClientHandlers (const . const . return $ InputReply "")
                         (const . const $ return ())
                         (const . const $ return ())


-- Test that messages can be sent and received on the heartbeat socket.
testClient :: TestTree
testClient = testMessageExchange
               "Communicate with IPython Kernel"
               (return $ \prof -> ["python", "-m", "ipykernel", "-f", prof])
               "3 + 3" $ \sessionNum execCount profile ->
  [ MessageExchange
    { exchangeName = "connect_request"
    , exchangeRequest = ConnectRequest
    , exchangeReply = ConnectReply
                        ConnectInfo
                          { connectShellPort = profileShellPort profile
                          , connectIopubPort = profileIopubPort profile
                          , connectStdinPort = profileStdinPort profile
                          , connectHeartbeatPort = profileHeartbeatPort profile
                          , connectControlPort = profileControlPort profile
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
                              , ExecuteInputOutput (execCount + 1)
                                  "import sys\nprint(sys.version.split()[0])"
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
                              , ExecuteInputOutput (execCount + 2) "3 + 3"
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
    , exchangeKernelOutputs = [kernelBusy, ExecuteInputOutput (execCount + 3) "", kernelIdle]
    }
  , MessageExchange
    { exchangeName = "execute_request (clear)"
    , exchangeRequest = ExecuteRequest "from IPython.display import clear_output\nclear_output()"
                          defaultExecuteOptions
    , exchangeReply = ExecuteReply (execCount + 3) ExecuteOk
    , exchangeKernelRequests = []
    , exchangeComms = []
    , exchangeKernelOutputs = [ kernelBusy
                              , ExecuteInputOutput (execCount + 3)
                                  "from IPython.display import clear_output\nclear_output()"
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
                              , ExecuteInputOutput (execCount + 4)
                                  "import ipywidgets as widgets\nwidgets.FloatSlider()"
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
                              , ExecuteInputOutput (execCount + 5)
                                  "from IPython.display import *\ndisplay(HTML('<b>Hi</b>'))"
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
                              , ExecuteInputOutput (execCount + 6) "x = input('Hello')\nprint(x)\nx"
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
                              , ExecuteInputOutput (execCount + 7)
                                  "import getpass\nprint(getpass.getpass('Hello'))"
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

kernelIdle, kernelBusy :: KernelOutput
kernelIdle = KernelStatusOutput KernelIdle
kernelBusy = KernelStatusOutput KernelBusy

str :: String -> String
str = id
