{-# Language OverloadedStrings #-}
module Main(main) where

import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO (stderr)
import qualified Data.Text.IO as T

import           Jupyter.Install (installKernel, simpleKernelspec, InstallUser(..), InstallResult(..),
                                  Kernelspec)
import           Jupyter.Kernel (readProfile, simpleKernelInfo, serve, defaultCommHandler,
                                 defaultClientRequestHandler)

-- | In `main`, support two commands:
--
--    - `kernel-basic install`: Register this kernel with Jupyter. 
--    - `kernel-basic kernel $FILE`: Serve a kernel given ports in connection file $FILE.
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
  installKernel InstallLocal basicKernelspec >>= handleInstallResult
  where
    -- A basic kernelspec with limited info.
    basicKernelspec :: Kernelspec
    basicKernelspec =
      simpleKernelspec "Basic" "basic" $ \exe connect -> [exe, "kernel", connect]

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
  serve profile defaultCommHandler $
    defaultClientRequestHandler profile $ simpleKernelInfo "Basic"
