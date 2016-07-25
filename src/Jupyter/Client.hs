{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Jupyter.Client () where

import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import qualified Data.Text as T

import           Control.Monad.Trans (lift)
import           Control.Monad.Reader (ReaderT, runReaderT, ask)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Control (liftBaseWith)

import           Jupyter.ZeroMQ (ClientSockets(..), withClientSockets, sendMessage, receiveMessage,
                                 messagingError, mkRequestHeader, KernelProfile(..))
import           Jupyter.Messages (Comm, KernelRequest, ClientReply, KernelOutput, ClientRequest,
                                   KernelReply, Message(..), messageHeader)
import           Jupyter.Messages.Metadata (Username, MessageHeader(..), MessageType(..))
import qualified Jupyter.UUID as UUID

import           System.ZMQ4.Monadic (ZMQ)

data ClientState = forall z.
       ClientState
         { clientSockets :: ClientSockets z
         , clientSessionUsername :: Username
         , clientSessionUuid :: UUID.UUID
         , clientSignatureKey :: ByteString
         , clientLiftZMQ :: forall a. ZMQ z a -> Client a
         }

newtype Client a = Client { unClient :: ReaderT ClientState IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

data ClientHandlers =
       ClientHandlers
         { kernelRequestHandler :: (Comm -> IO ()) -> KernelRequest -> IO ClientReply
         , commHandler :: (Comm -> IO ()) -> Comm -> IO ()
         , kernelOutputHandler :: (Comm -> IO ()) -> KernelOutput -> IO ()
         }

runClient :: Maybe KernelProfile -> Maybe Username -> ClientHandlers -> Client a -> IO a
runClient mProfile mUser clientHandlers (Client client) =
  withClientSockets mProfile $ \profile sockets ->
    liftBaseWith $ \runInBase -> do
      let sessionUsername = fromMaybe "default-username" mUser
      sessionUuid <- UUID.random
      let clientState = ClientState
            { clientSockets = sockets
            , clientSessionUsername = sessionUsername
            , clientSessionUuid = sessionUuid
            , clientSignatureKey = profileSignatureKey profile
            , clientLiftZMQ = liftIO . runInBase
            }

      runReaderT client clientState

sendClientRequest :: ClientRequest -> Client KernelReply
sendClientRequest req = do
  ClientState { .. } <- ask
  header <- liftIO $ mkRequestHeader clientSessionUuid clientSessionUsername req
  clientLiftZMQ $ sendMessage clientSignatureKey (clientShellSocket clientSockets) header req
  message <- clientLiftZMQ $ receiveMessage (clientShellSocket clientSockets)

  case message of
    Left err ->
      -- There's no way to recover from this, so just die.
      messagingError "Jupyter.Client" $
        "Unexpected failure parsing KernelReply message: " ++ err
    Right message ->
      case message of
        KernelReply _ reply -> return reply
        _ ->
          let msgType = T.unpack $ messageTypeText $ messageType $ messageHeader message
              err = "Unexpected message type on shell socket: " ++ msgType
          in messagingError "Jupyter.Client" err

sendClientComm :: Comm -> Client ()
sendClientComm comm = do
  ClientState { .. } <- ask
  header <- liftIO $ mkRequestHeader clientSessionUuid clientSessionUsername  comm
  clientLiftZMQ $ sendMessage clientSignatureKey (clientShellSocket clientSockets) header comm
