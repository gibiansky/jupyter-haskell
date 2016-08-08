{-|
Module      : Main
Description : Main module for a basic bare-minimum Jupyter kernel created using the @jupyter@ library.
Copyright   : (c) Andrew Gibiansky, 2016
License     : MIT
Maintainer  : andrew.gibiansky@gmail.com
Stability   : stable
Portability : POSIX

This module is the Main module for @kernel-basic@, a bare-minimum Jupyter kernel created using the
@jupyter@ library. It is intended to demo the bare minimum amount of code required to create a
Jupyter kernel.
-}
{-# Language OverloadedStrings #-}
module Main(main) where

-- Imports from 'base'
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO (stderr)

-- Imports from 'text'
import qualified Data.Text.IO as T

-- Imports from 'jupyter'
import           Jupyter.Install (installKernel, simpleKernelspec, InstallUser(..), InstallResult(..),
                                  Kernelspec)
import           Jupyter.Kernel (readProfile, simpleKernelInfo, serve, defaultCommHandler,
                                 defaultClientRequestHandler)

-- | In @main@, support two commands:
--
--    - @kernel-basic install@: Register this kernel with Jupyter.
--    - @kernel-basic kernel $FILE@: Serve a kernel given ports in connection file $FILE.
--
-- These are the two main functions that a kernel must support, and the installed kernelspec must
-- indicate that the client should invoke the @kernel@ command in order to launch the kernel.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["install"] -> runInstall
    ["kernel", profilePath] -> runKernel profilePath
    _ -> putStrLn $ "Invalid arguments: " ++ show args

-- | Register this kernel with Jupyter.
--
-- This eventually calls @jupyter kernelspec install@ to register the kernel, but the command
-- invocation and directory setup is handled by 'installKernel'.
runInstall :: IO ()
runInstall =
  installKernel InstallLocal basicKernelspec >>= handleInstallResult
  where
    -- A basic kernelspec with limited info. The key part of this definition is the function which
    -- defines how to invoke the kernel; in this case, the kernel is invoked by calling this same
    -- executable with the command-line argument "kernel", followed by a path to a connection file.
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
