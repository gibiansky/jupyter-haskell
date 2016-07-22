{-# LANGUAGE OverloadedStrings #-}
module Jupyter.Test.Kernel (kernelTests) where

import           Control.Concurrent (newEmptyMVar, MVar, forkIO, putMVar, takeMVar, readMVar)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (forM_)
import qualified Data.Map as Map

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase, testCaseSteps, (@=?), assertFailure, assertBool)

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Aeson (encode)

import           System.ZMQ4.Monadic (socket, Req(..), Dealer(..), send, receive, bind, connect, ZMQ,
                                      Socket, SocketType, runZMQ)

import           Jupyter.Kernel.ZeroMQ
import           Jupyter.Kernel
import           Jupyter.Messages
import           Jupyter.Messages.Metadata
import qualified Jupyter.UUID as UUID

import           Utils (inTempDir, connectedSocket)

kernelTests :: TestTree
kernelTests = testGroup "Kernel Tests" [testKernel]

-- Test that messages can be sent and received on the heartbeat socket.
testKernel :: TestTree
testKernel = testCaseSteps "Simple Kernel" $ \step -> do
  -- Start serving the kernel and obtain the port info so we can connect to it.
  step "Starting kernel..."
  profileVar <- newEmptyMVar
  clientMessageVar <- newEmptyMVar
  forkIO $ serveWithDynamicPorts (putMVar profileVar) defaultCommHandler (reqHandler profileVar clientMessageVar)
  profile <- readMVar profileVar

  runZMQ $ do
    liftIO $ step "Connecting to kernel..."
    heartbeatClientSocket <- connectedSocket profile profileHeartbeatPort Req
    shellClientSocket <- connectedSocket profile profileShellPort Dealer

    liftIO $ step "Checking heartbeat..."
    let message = "heartbeat"
    send heartbeatClientSocket [] message
    response <- receive heartbeatClientSocket
    liftIO $ message @=? response

    liftIO $ step "Checking client request encoding / decoding..."
    forM_ clientMessages $ \msg -> do
      header <- liftIO $ mkFreshTestHeader msg
      sendMessage "" shellClientSocket header msg
      received <- liftIO $ takeMVar clientMessageVar
      liftIO $ msg @=? received
  where
    kernelInfo :: KernelInfo
    kernelInfo = KernelInfo
      { kernelProtocolVersion = [1, 2, 3]
      , kernelBanner = "Banner"
      , kernelImplementation = "kernel"
      , kernelImplementationVersion = "1.0.0"
      , kernelHelpLinks = []
      , kernelLanguageInfo = LanguageInfo
        { languageName = "Test"
        , languageVersion = "1.0.0"
        , languageMimetype = "text/plain"
        , languageFileExtension = ".txt"
        , languagePygmentsLexer = Nothing
        , languageCodeMirrorMode = Nothing
        , languageNbconvertExporter = Nothing
        }
      }

    reqHandler :: MVar KernelProfile -> MVar ClientRequest -> ClientRequestHandler
    reqHandler profileVar clientMessageVar cb req = do
      putMVar clientMessageVar req
      profile <- readMVar profileVar
      defaultClientRequestHandler profile kernelInfo cb req

    clientMessages = [ ExecuteRequest (CodeBlock "1 + 1")
                         ExecuteOptions
                           { executeSilent = False
                           , executeStoreHistory = True
                           , executeAllowStdin = False
                           , executeStopOnError = True
                           }
                     , InspectRequest (CodeBlock "print 'X'") 5 DetailLow
                     , InspectRequest (CodeBlock "print 'X'") 5 DetailHigh
                     , HistoryRequest
                         HistoryOptions
                           { historyShowOutput = True
                           , historyRaw = True
                           , historyAccessType = HistoryTail 3
                           }
                     , HistoryRequest
                         HistoryOptions
                           { historyShowOutput = True
                           , historyRaw = True
                           , historyAccessType = HistoryRange
                                                   HistoryRangeOptions
                                                     { historyRangeSession = -1
                                                     , historyRangeStart = 10
                                                     , historyRangeStop = 100
                                                     }
                           }
                     , HistoryRequest
                         HistoryOptions
                           { historyShowOutput = True
                           , historyRaw = True
                           , historyAccessType = HistorySearch
                                                   HistorySearchOptions
                                                     { historySearchCells = 10
                                                     , historySearchPattern = "putStr*"
                                                     , historySearchUnique = True
                                                     }
                           }
                     , CompleteRequest (CodeBlock "putStrL + 3") 7
                     , IsCompleteRequest (CodeBlock "let x = 3 in")
                     , ConnectRequest
                     , CommInfoRequest Nothing
                     , CommInfoRequest (Just (TargetName "comm_target"))
                     , KernelInfoRequest
                     , ShutdownRequest Restart
                     , ShutdownRequest NoRestart
                     ]

    mkFreshTestHeader :: IsMessage v => v -> IO MessageHeader
    mkFreshTestHeader content = do
      uuid <- UUID.random
      sess <- UUID.random
      return
        MessageHeader
          { messageIdentifiers = ["ABC", "DEF"]
          , messageParent = Nothing
          , messageMetadata = Map.fromList []
          , messageId = uuid
          , messageSession = sess
          , messageUsername = "test-user"
          , messageType = getMessageType content
          }
