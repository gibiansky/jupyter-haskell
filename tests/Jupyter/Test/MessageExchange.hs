{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Jupyter.Test.MessageExchange where

import           Control.Concurrent (newMVar, swapMVar, newEmptyMVar, MVar, forkIO, putMVar, takeMVar,
                                     tryTakeMVar, readMVar, modifyMVar_, threadDelay, tryReadMVar)
import           Control.Concurrent.Async (link, async)
import           System.Environment (setEnv)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (forM_, unless, void)
import           Control.Monad.Catch (finally)
import           Control.Exception (SomeException, Exception, throwIO, bracket)
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
import           System.Process (spawnProcess, terminateProcess, ProcessHandle, getProcessExitCode)


import           System.ZMQ4.Monadic (socket, Req(..), Dealer(..), send, receive, bind, connect, ZMQ,
                                      Socket, SocketType, runZMQ)

import           Jupyter.ZeroMQ
import           Jupyter.Kernel
import           Jupyter.Client
import           Jupyter.Messages
import           Jupyter.Install
import           Jupyter.Messages.Metadata
import qualified Jupyter.UUID as UUID

import           Utils (inTempDir, connectedSocket, shouldThrow, HandlerException(..))

import           System.Timeout (timeout)



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

startKernel :: (FilePath -> [String]) -> KernelProfile -> IO ProcessHandle
startKernel mkCmd profile = do
  uuid <- UUID.uuidToString <$> UUID.random
  let filename = "profile-" ++ uuid ++ ".json"
  writeProfile profile filename

  -- set JPY_PARENT_PID to shut up the kernel about its Ctrl-C behaviour
  setEnv "JPY_PARENT_PID" "-1"

  -- Start the kernel, and then give it a bit of time to start. If we don't give it some time to
  -- start, then it is possible for it to miss our first message. In that case, this test suite just
  -- spins forever...
  case mkCmd filename of
    [] -> fail "Jupyter.Test.Client.startKernel: Expected command with at the executable name"
    cmd:args -> do
      proc <- spawnProcess cmd args
      threadDelay $ 500 * 1000
      return proc

testMessageExchange :: String
                    -> IO (FilePath -> [String])
                    -> CodeBlock
                    -> (Int -> ExecutionCount -> KernelProfile -> [MessageExchange])
                    -> TestTree
testMessageExchange name mkKernelCommand validCode mkMessageExchanges = testCaseSteps name $ \step -> do
  kernelOutputsVar <- newMVar []
  commsVar <- newEmptyMVar
  kernelRequestsVar <- newEmptyMVar
  clientRepliesVar <- newMVar []
  let clientHandlers = ClientHandlers (exchangeKernelRequestHandler clientRepliesVar kernelRequestsVar)
                                      (exchangeCommHandler commsVar)
                                      (exchangeKernelOutputHandler kernelOutputsVar)

  mk <- mkKernelCommand
  runKernel (startKernel mk) clientHandlers $ \profile proc -> do
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
    execReply <- sendClientRequest $ ExecuteRequest validCode defaultExecuteOptions
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

        case exchangeReply of
          ShutdownReply{} -> do
            threadDelay $ 500 * 1000
            exitCodeM <- getProcessExitCode proc
            case exitCodeM of
              Nothing -> assertFailure "Kernel did not shut down after shutdown request"
              _ -> return ()
          _ -> return ()

waitForKernelIdle :: MVar [KernelOutput] -> IO ()
waitForKernelIdle var = do
  res <- timeout 1000000 wait
  case res of
    Just _  -> return ()
    Nothing -> fail "Timed out in waitForKernelIdle: deadlock?"

  where
    wait = do
      outputs <- readMVar var
      unless (KernelStatusOutput KernelIdle `elem` outputs) $ do
        threadDelay 100000
        waitForKernelIdle var

fakeUUID :: UUID.UUID
fakeUUID = UUID.uuidFromString "fake"

exchangeKernelRequestHandler :: MVar [(KernelRequest, ClientReply)]
                             -> MVar [KernelRequest]
                             -> (Comm -> IO ())
                             -> KernelRequest
                             -> IO ClientReply
exchangeKernelRequestHandler repliesVar var _ req = do
  modifyMVar_ var $ return . (req :)
  replies <- readMVar repliesVar
  case lookup req replies of
    Just reply -> return reply
    Nothing    -> fail "Could not find appropriate client reply"


exchangeCommHandler :: MVar [Comm] -> (Comm -> IO ()) -> Comm -> IO ()
exchangeCommHandler var _ comm = modifyMVar_ var $ return . (comm' :)
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

exchangeKernelOutputHandler :: MVar [KernelOutput] -> (Comm -> IO ()) -> KernelOutput -> IO ()
exchangeKernelOutputHandler var _ out =
  modifyMVar_ var $ return . (out :)

runKernel :: (KernelProfile -> IO ProcessHandle) -> ClientHandlers -> (KernelProfile -> ProcessHandle -> Client a) -> IO a
runKernel start handlers action =
  inTempDir $ \tmpDir ->
    runClient Nothing Nothing handlers $ \profile -> do
      proc <- liftIO $ start profile
      finally (action profile proc) $ liftIO $ terminateProcess proc
