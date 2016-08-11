{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           System.Process (spawnProcess)

import           Jupyter.Client (runClient, sendClientRequest, ClientHandlers(..),
                                 defaultClientCommHandler, findKernel, writeProfile, Kernelspec(..))
import           Jupyter.Messages (ClientRequest(KernelInfoRequest), KernelReply(KernelInfoReply),
                                   KernelRequest(InputRequest), ClientReply(InputReply))

main :: IO ()
main = do
  -- Find the kernelspec for the python 3 kernel
  Just kernelspec <- findKernel "python3"

  -- Start the client connection
  runClient Nothing Nothing handlers $ \profile -> do
    -- Write the profile connection file to a JSON file
    liftIO $ writeProfile profile "profile.json"

    -- Launch the kernel process, giving it the path to the JSON file
    let command = kernelspecCommand kernelspec "" "profile.json"
    _ <- liftIO $ spawnProcess (head command) (tail command)

    -- Send a kernel info request and get the reply
    KernelInfoReply info <- sendClientRequest KernelInfoRequest
    liftIO $ print info

handlers :: ClientHandlers
handlers = ClientHandlers {
    -- Do nothing on comm messages
    commHandler = defaultClientCommHandler,

    -- Return a fake stdin string if asked for stdin
    kernelRequestHandler = \_ req ->
        case req of
        InputRequest{} -> return $ InputReply "Fake Stdin",

    -- Do nothing on kernel outputs
    kernelOutputHandler = \_ _ -> return ()
  }
