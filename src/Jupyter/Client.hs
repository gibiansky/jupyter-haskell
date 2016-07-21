{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Jupyter.Client () where

data ClientState  = ClientState {
      clientCommHandler :: CommHandler,
      clientSockets :: ClientSockets
      clientSessionUsername :: Username,
      clientSessionUuid :: UUID,
      clientSignatureKey :: ByteString
   }

newtype Client a = Client (ReaderT ClientState IO a)
  deriving (Functor, Applicative, Monad)

type KernelRequestHandler = (Comm -> IO ()) -> KernelRequest -> IO ClientReply

type KernelOutputHandler = KernelOutput

runClient :: KernelProfile -> Maybe Username -> CommHandler -> Client a -> IO a
runClient profile mUser commHandler (Client client) = runZMQ $ do
  let sessionUsername = fromMaybe "default-username" mUser
  sessionUuid <- UUID.random
  sockets <- connectToKernel profile
  let clientState = ClientState
        { clientCommHandler = commHandler
        , clientSockets = sockets
        , clientSessionUsername = sessionUsername
        , clientSessionUuid = sessionUuid
        , clientSignatureKey = profileSignatureKey profile
        }

  runReaderT clientState client

sendRequest :: KernelRequestHandler -> KernelOutputHandler -> ClientRequest -> (KernelReply -> Client ()) -> Client () 
sendRequest kernelRequestHandler kernelOutputHandler req = do
  ClientState{..} <- ask
  header <- liftIO $ mkRequestHeader clientSessionUsername clientSessionUuid req
  sendMessage clientSignatureKey socket header req

sendComm :: Comm -> Client ()
sendComm comm
