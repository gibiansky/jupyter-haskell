{-# LANGUAGE ScopedTypeVariables #-}
module Utils where

import           Data.Monoid (mempty)
import           Data.List (isInfixOf)
import           Control.Monad (forM_, (=<<))
import           System.Environment (setEnv)
import           Control.Exception (catch, Exception)
import           Data.Proxy (Proxy(..))
import           System.Directory (setPermissions, getPermissions, Permissions(..), canonicalizePath,
                                   getDirectoryContents, createDirectoryIfMissing, removeFile,
                                   doesFileExist)

import           Test.Tasty.HUnit (testCase, (@=?), assertFailure, assertBool)

import           System.IO.Extra (withTempDir)
import           System.Directory.Extra (withCurrentDirectory)

import           System.ZMQ4.Monadic (socket, Req(..), send, receive, bind, connect, ZMQ, Socket,
                                      SocketType)

import           Jupyter.Kernel.ZeroMQ (KernelProfile)

-- Create a temporary directory and execute an action with that temporary directory as the working
-- directory. This is not threadsafe, since working directories are global values.
inTempDir :: (FilePath -> IO a) -> IO a
inTempDir action = withTempDir $ \tmp -> withCurrentDirectory tmp (action tmp)

-- Check that an IO action throws an exception of the expected type.
shouldThrow :: forall a proxy e. Exception e => IO a -> proxy e -> IO ()
shouldThrow action _ =
  catch (action >> assertFailure "Did not throw expected exception")
        handler
  where
    handler :: e -> IO ()
    handler _ = return ()

connectedSocket :: SocketType s => KernelProfile -> (KernelProfile -> Int) -> s -> ZMQ z (Socket z s)
connectedSocket profile accessor socketType = do
  sock <- socket socketType
  connect sock $ "tcp://127.0.0.1:" ++ show (accessor profile)
  return sock
