{-# LANGUAGE OverloadedStrings #-}
module Jupyter.Test.ZeroMQ (zmqTests) where

-- Imports from 'transformers'
import           Control.Monad.IO.Class (liftIO)

-- Imports from 'tasty'
import           Test.Tasty (TestTree, testGroup)

-- Imports from 'tasty-hunit'
import           Test.Tasty.HUnit (testCase, (@=?))

-- Imports from 'zeromq4-haskell'
import           System.ZMQ4.Monadic (Req(..), send, receive)

-- Imports from 'jupyter'
import           Jupyter.ZeroMQ

import           Jupyter.Test.Utils (inTempDir, connectedSocket)

zmqTests :: TestTree
zmqTests = testGroup "ZeroMQ Tests" [testHeartbeatSocket, testReadProfile]

-- Test that messages can be sent and received on the heartbeat socket.
testHeartbeatSocket :: TestTree
testHeartbeatSocket = testCase "Heartbeat Socket" $
  withKernelSockets Nothing $ \profile socks -> do
    heartbeatClientSocket <- connectedSocket profile profileHeartbeatPort Req

    let message = "heartbeat"
    send heartbeatClientSocket [] message
    received <- receive (kernelHeartbeatSocket socks)
    liftIO $ message @=? received

-- Test that kernel profile encoding and decoding works as expected.
testReadProfile :: TestTree
testReadProfile = testCase "Reading profile file" $
  inTempDir $ \_ -> do
    let filename = "profile.json"
    writeProfile testProfile filename
    profile <- readProfile filename
    Just testProfile @=? profile
  where
    testProfile = 
       KernelProfile
         { profileIp = "127.0.0.1"
         , profileTransport = TCP
         , profileStdinPort = 3982
         , profileControlPort = 3983
         , profileHeartbeatPort = 3984
         , profileShellPort = 3945
         , profileIopubPort = 3942
         , profileSignatureKey = ""
         }
