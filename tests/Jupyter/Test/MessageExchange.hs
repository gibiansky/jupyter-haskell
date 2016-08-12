{-|
Module      : Jupyter.Test.MessageExchange
Description : Testing infrastructure for back-and-forth communication between clients and kernels
Copyright   : (c) Andrew Gibiansky, 2016
License     : MIT
Maintainer  : andrew.gibiansky@gmail.com
Stability   : stable
Portability : POSIX

This module serves as one mechanism of testing @jupyter@'s Client functionality, as well as kernels 
included with this project.

This module allows defining a list of 'MessageExchange' values, which indicate a complete message exchange
that should happen between a client and a kernel. The 'testMessageExchange' function then takes the list of
exchanges, runs them using the 'Client' interface, and verifies that all replies (and generated kernel
requests and outputs) match the expected ones.
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Jupyter.Test.MessageExchange (
    MessageExchange(..),
    startKernel,
    fakeUUID,
    testMessageExchange,
    runKernelAndClient,
    ) where

-- Imports from 'base'
import           Control.Concurrent (newMVar, swapMVar, newEmptyMVar, MVar, putMVar, takeMVar,
                                     readMVar, modifyMVar_, threadDelay, tryTakeMVar)
import           Control.Monad (forM_, unless, void)
import           Data.Maybe (listToMaybe)
import           System.Environment (setEnv)
import           System.Timeout (timeout)

-- Imports from 'transformers'
import           Control.Monad.IO.Class (liftIO)

-- Imports from 'exceptions'
import           Control.Monad.Catch (finally)

-- Imports from 'unordered-containers'
import qualified Data.HashMap.Strict as HashMap

-- Imports from 'tasty'
import           Test.Tasty (TestTree)

-- Imports from 'tasty-hunit'
import           Test.Tasty.HUnit (testCaseSteps, (@=?), assertFailure)

-- Imports from 'aeson'
import           Data.Aeson.Types (Value(..))

-- Imports from 'process'
import           System.Process (spawnProcess, terminateProcess, ProcessHandle, getProcessExitCode)

-- Imports from 'jupyter'
import           Jupyter.Client
import           Jupyter.Kernel
import           Jupyter.Messages
import qualified Jupyter.UUID as UUID

import           Jupyter.Test.Utils (inTempDir)

-- | A data type representing an exchange of messages between a client and a kernel.
--
-- Given a list of 'MessageExchange's, this module can run the communication and check that all
-- requests, outputs, and replies match what was specified.
--
-- This currently does not support sending 'Comm' messages as part of the exchange, but does support
-- receiving them.
data MessageExchange =
       MessageExchange
         { exchangeName :: String
         -- ^ Name for the message exchange (to show in errors and test outputs)
         , exchangeRequest :: ClientRequest
         -- ^ The 'ClientRequest' that initiates the exchange
         , exchangeReply :: KernelReply
         -- ^ The 'KernelReply' that is returned
         , exchangeKernelRequests :: [(KernelRequest, ClientReply)]
         -- ^ The 'KernelRequest's that are sent to the client, with a reply for each one
         , exchangeComms :: [Comm]
         -- ^ The 'Comm' messages that are sent to the client
         , exchangeKernelOutputs :: [KernelOutput]
         -- ^ The 'KernelOutput' messages that are sent to the client
         }
  deriving (Eq, Show)

-- | Start an external-process kernel.
--
-- First, write the provided profile to a randomly named JSON file. Then, pass the path to this
-- connection file to the provided function, which should return the command that should be used to
-- start the kernel. Return the 'ProcessHandle' for the kernel process.
--
-- For example, given a profile @kernelProfile@, the IPython kernel would be started as
--
-- >     startKernel (\connFile -> ["python", "-m", "ipykernel", "-f", connFile]) kernelProfile
--
-- It is recommended that this be run in a temporary directory that is deleted afterward, so that
-- this does not litter the user's path with JSON connection files.
startKernel :: (FilePath -> [String]) -- ^ Function to generate kernel command given JSON connection
                                      -- file
            -> KernelProfile -- ^ Kernel profile to write to the connection file
            -> IO ProcessHandle -- ^ Handle to the spawned kernel process
startKernel mkCmd profile = do
  -- Write a randomly-named profile JSON file. These are randomly named to avoid the possibility of
  -- accidentally passing an old connection file to a newly spawned kernel.
  uuid <- UUID.uuidToString <$> UUID.random
  let filename = "profile-" ++ uuid ++ ".json"
  writeProfile profile filename

  -- Set JPY_PARENT_PID to shut up the kernel about its Ctrl-C behaviour (this is mostly for the
  -- IPython kernel).
  setEnv "JPY_PARENT_PID" "-1"

  -- Start the kernel, and then give it a bit of time to start. If we don't give it some time to
  -- start, then it is possible for it to miss our first message. In that case, this test suite just
  -- spins forever...
  case mkCmd filename of
    [] -> fail "Jupyter.Test.Client.startKernel: Expected command with at the executable name"
    cmd:args -> do
      proc <- spawnProcess cmd args
      threadDelay $ 1000 * 1000
      return proc

-- | Run a kernel and a client connected to that kernel (in a temporary directory).
--
-- This function starts a client, uses the generated kernel profile to start an external kernel
-- process, connects to that kernel and communicates with it as indicated by a 'Client' action, and
-- then ensures that the external process is shutdown before exiting.
runKernelAndClient :: (KernelProfile -> IO ProcessHandle) -- ^ Function to start external kernel
                   -> ClientHandlers -- ^ Client handlers for messages received from the kernel
                   -> (KernelProfile -> ProcessHandle -> Client a) -- ^ Client action to run
                   -> IO a
runKernelAndClient start handlers action =
  inTempDir $ \_ -> runClient Nothing Nothing handlers $ \profile -> do
    proc <- liftIO $ start profile
    finally (action profile proc) $ liftIO $ terminateProcess proc

-- | Use the 'MessageExchange' data type to generate a test case for a test suite.
--
-- The generated test suite starts a kernel, acquires some basic information about it, sets up a
-- client that talks to the kernel, and plays through the 'MessageExchange's provided by the user.
-- All interactions between the client and the kernel are recorded and verified against the
-- 'MessageExchange's.
testMessageExchange :: String -- ^ Name for the test case
                    -> IO (FilePath -> [String])
                    -- ^ IO action that generates a function, which, if given the path to the
                    -- connection file, returns the full command to run. This is allowed to be
                    -- dynamically generated so that it can use 'findKernel' to locate external
                    -- kernels Jupyter knows about.
                    -> CodeBlock
                    -- ^ A valid block of code accepted by the kernel, used to establish
                    -- the execution counter and session number.
                    -> (Int -> ExecutionCount -> KernelProfile -> [MessageExchange])
                    -- ^ A function to generate the all desired message exchanges, given the
                    -- session number in progress (for history requests), the previous execution
                    -- request, and the profile that was connected to.
                    -> TestTree
testMessageExchange name mkKernelCommand validCode mkMessageExchanges = testCaseSteps name $ \step -> do
  -- All communication from kernel to client is recorded in MVars. Allocate those MVars
  -- and create the handlers that write to those MVars.
  kernelOutputsVar <- newMVar []
  commsVar <- newEmptyMVar
  kernelRequestsVar <- newEmptyMVar
  clientRepliesVar <- newMVar []
  let clientHandlers = ClientHandlers (exchangeKernelRequestHandler clientRepliesVar kernelRequestsVar)
                                      (exchangeCommHandler commsVar)
                                      (exchangeKernelOutputHandler kernelOutputsVar)

  mk <- mkKernelCommand
  runKernelAndClient (startKernel mk) clientHandlers $ \profile proc -> do
    -- Wait for the kernel to initialize. We know that the kernel is done initializing when it sends its
    -- first response; however, sometimes we also get a "starting" status. Since later on we check for
    -- equality of kernel outputs, we want to get rid of this timing inconsistencey immediately by just 
    -- doing on preparation message.
    liftIO $ step "Waiting for kernel to start..."
    void $ sendClientRequest ConnectRequest
    liftIO $ do
      waitForKernelIdle kernelOutputsVar
      void $ swapMVar kernelOutputsVar []

    -- Acquire the current session number. Without this, we can't accurately test the history replies,
    -- since they contain the session numbers. To acquire the session number, send an execute request followed
    -- by a history request.
    execReply <- sendClientRequest $ ExecuteRequest validCode defaultExecuteOptions
    execCount <- case execReply of
      ExecuteReply count _ -> return count
      _ -> fail "Expected ExecuteReply for ExecuteRequest"
    liftIO $ do
      waitForKernelIdle kernelOutputsVar
      void $ swapMVar kernelOutputsVar []

    histReply <- sendClientRequest $ HistoryRequest $ HistoryOptions False True $ HistoryTail 1
    sessionNum <- case histReply of
      HistoryReply items -> return $ maybe 1 historyItemSession (listToMaybe items)
      _ -> fail "Expected HistoryReply for HistoryRequest"
    liftIO $ waitForKernelIdle kernelOutputsVar
    void $ liftIO $ takeMVar kernelOutputsVar

    liftIO $ step "Checking messages exchanges..."
    forM_ (mkMessageExchanges sessionNum execCount profile) $ \MessageExchange{..} -> do
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

        -- In the case of message exchanges that end in a shutdown message, the kernel is
        -- responsible for shutting itself down after it sends the shutdown reply. Test that
        -- this happens, and fail if the kernel process hasn't terminated after some time.
        case exchangeReply of
          ShutdownReply{} -> do
            threadDelay $ 500 * 1000
            exitCodeM <- getProcessExitCode proc
            case exitCodeM of
              Nothing -> assertFailure "Kernel did not shut down after shutdown request"
              _ -> return ()
          _ -> return ()

-- | Wait for the kernel to send a 'KernelIdle' status update.
--
-- This function polls the given 'MVar', waiting for the contained list to have a 'KernelIdle'
-- status update. If this doesn't happen within a fixed but long timeout (1 second or so), an
-- exception is raised, as it is likely indicative of a deadlock.
waitForKernelIdle :: MVar [KernelOutput] -> IO ()
waitForKernelIdle var = do
  res <- timeout 1000000 wait
  case res of
    Just _  -> return ()
    Nothing -> fail "Timed out in waitForKernelIdle: deadlock?"

  where
    -- Poll the MVar until it has the KernelIdle in it.
    wait = do
      outputs <- readMVar var
      unless (KernelStatusOutput KernelIdle `elem` outputs) $ do
        threadDelay 100000
        waitForKernelIdle var

-- | A fake UUID that replaces all UUIDs in 'Comm' messages.
--
-- This is necessary because kernels generate UUIDs randomly, so we cannot use equality to test
-- them, unless we replace all UUIDs with fake ones. This is the fake UUID that UUIDs from the
-- kernels get replaced with.
fakeUUID :: UUID.UUID
fakeUUID = UUID.uuidFromString "fake"

-- | A handler for the 'kernelRequestHandler' field of 'ClientHandlers'.
--
-- This handler listens for kernel requests, and, upon a kernel request, stores it in a list in an
-- MVar, and then looks up a response to this kernel request and replies with it.
--
-- This allows for scripted interactions between clients and kernels to include /stdin/ channel requests.
exchangeKernelRequestHandler :: MVar [(KernelRequest, ClientReply)]
                             -- ^ Variable holding request / response pairs; response is looked up here.
                             -> MVar [KernelRequest]
                             -- ^ Variable to store the received kernel request in.
                             -> (Comm -> IO ())
                             -- ^ (unused) callback to send 'Comm' messages to the kernel
                             -> KernelRequest
                             -- ^ Received kernel request
                             -> IO ClientReply
exchangeKernelRequestHandler repliesVar var _ req = do
  modifyMVar_ var $ return . (req :)
  replies <- readMVar repliesVar
  case lookup req replies of
    Just reply -> return reply
    Nothing    -> fail "Could not find appropriate client reply"


-- | A handler for the 'commHandler' field of 'ClientHandlers'.
--
-- This handler listens for comm messages and stores them into a mutable variable. All 'Comm'
-- messages have a UUID, which is replaced by 'fakeUUID'. In addition, any known fields that are
-- non-deterministic are dropped from the data. (For instance, 'layout' is dropped from IPython
-- widget messages.)
--
-- This function is not meant to be incredibly reusable.
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

-- | A handler for the 'kernelOutputHandler' field of 'ClientHandlers'.
--
-- This handler listens for 'KernelOutput' messages and stores them into a mutable variable.
exchangeKernelOutputHandler :: MVar [KernelOutput] -> (Comm -> IO ()) -> KernelOutput -> IO ()
exchangeKernelOutputHandler var _ out =
  modifyMVar_ var $ return . (out :)
