{-# LANGUAGE ScopedTypeVariables #-}
module Jupyter.Test.Utils where

-- Imports from 'base'
import           Control.Exception (catch, Exception)

-- Imports from 'tasty-hunit
import           Test.Tasty.HUnit (assertFailure)

-- Imports from 'extra'
import           System.IO.Extra (withTempDir)
import           System.Directory.Extra (withCurrentDirectory)

-- Imports from 'zeromq4-haskell'
import           System.ZMQ4.Monadic (socket, connect, ZMQ, Socket, SocketType)

-- Imports from 'jupyter'
import           Jupyter.ZeroMQ (KernelProfile, Port)

-- | Create a temporary directory and execute an action with that temporary directory as the working
-- directory. This is not threadsafe, since working directories are global values.
inTempDir :: (FilePath -> IO a) -> IO a
inTempDir action = withTempDir $ \tmp -> withCurrentDirectory tmp (action tmp)

-- | Check that an IO action throws an exception of the expected type.
shouldThrow :: forall a proxy e. Exception e => IO a -> proxy e -> IO ()
shouldThrow action _ =
  catch (action >> assertFailure "Did not throw expected exception") handler
  where
    handler :: e -> IO ()
    handler _ = return ()

-- | Create and connect a socket to a port, obtained by applying an accessor to a 'KernelProfile'.
connectedSocket :: SocketType s
                => KernelProfile -- ^ Profile to get port from
                -> (KernelProfile -> Port) -- ^ Accessor to get port from profile
                -> s -- ^ Socket type to create, e.g. 'Rep'
                -> ZMQ z (Socket z s) -- ^ Returns connected ZeroMQ socket
connectedSocket profile accessor socketType = do
  sock <- socket socketType
  connect sock $ "tcp://127.0.0.1:" ++ show (accessor profile)
  return sock

-- | An exception type to be thrown during tests.
data HandlerException = HandlerException | HandlerExceptionWithMessage String
  deriving (Eq, Ord, Show)

-- | Make 'HandlerException' an instance of 'Exception' so it can be thrown
instance Exception HandlerException
