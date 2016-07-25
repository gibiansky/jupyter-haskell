{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
module Main(main) where

import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO (stderr)
import qualified Data.Text.IO as T
import Data.Text (Text)

import           Jupyter.Install (installKernel, simpleKernelspec, InstallUser(..), InstallResult(..),
                                  Kernelspec)
import           Jupyter.Kernel (readProfile, simpleKernelInfo, serve, defaultCommHandler,
                                 defaultClientRequestHandler, KernelCallbacks(..), KernelProfile,
                                 ClientRequestHandler)
import           Jupyter.Messages (KernelOutput(..), KernelReply(..), displayPlain, ClientRequest(..),
                                   pattern ExecuteOk, CodeBlock(..), KernelRequest(..),
                                   ClientReply(..), InputOptions(..))

-- | In `main`, support two commands:
--
--    - `kernel-stdin install`: Register this kernel with Jupyter. 
--    - `kernel-stdin kernel $FILE`: Serve a kernel given ports in connection file $FILE.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["install"] -> runInstall
    ["kernel", profilePath] -> runKernel profilePath
    _ -> putStrLn $ "Invalid arguments: " ++ show args

-- | Register this kernel with Jupyter.
runInstall :: IO ()
runInstall =
  installKernel InstallLocal stdinKernelspec >>= handleInstallResult
  where
    -- A basic kernelspec with limited info for testing stdin.
    stdinKernelspec :: Kernelspec
    stdinKernelspec =
      simpleKernelspec "Stdin" "stdin" $ \exe connect -> [exe, "kernel", connect]

    -- Print an error message and exit with non-zero exit code if the install failed. 
    handleInstallResult :: InstallResult -> IO ()
    handleInstallResult installResult =
      case installResult of
        InstallSuccessful -> return ()
        InstallFailed reason -> do
          T.hPutStrLn stderr reason
          exitFailure

-- | Run the kernel on ports determined by parsing the connection file provided.
runKernel :: FilePath -> IO ()
runKernel profilePath = do
  Just profile <- readProfile profilePath
  serve profile defaultCommHandler $ clientRequestHandler profile

-- | Client request handler which acts in all ways as the default, except for execute requests,
-- it reads data from stdin and writes it back to the client as a display data message.
clientRequestHandler :: KernelProfile -> ClientRequestHandler
clientRequestHandler profile callbacks req =
  case req of
    ExecuteRequest (CodeBlock code) _ -> do
      echoStdin code callbacks
      return $ ExecuteReply 1 ExecuteOk
    _ -> defaultClientRequestHandler profile (simpleKernelInfo "Stdin") callbacks req

-- | Read some text from the client stdin using the 'KernelCallbacks', then publish that text back
-- to the client as a 'DisplayDataOutput'.
--
-- If the execute promppt is "password", then the input is done in password mode.
echoStdin :: Text -> KernelCallbacks -> IO ()
echoStdin code callbacks = do
  clientReply <- sendKernelRequest callbacks $
                   InputRequest
                     InputOptions { inputPrompt = code, inputPassword = code == "password" }
  case clientReply of
    InputReply stdinText ->
      sendKernelOutput callbacks $ DisplayDataOutput $ displayPlain stdinText
