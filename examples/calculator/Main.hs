{-|
Module      : Main
Description : Main module for a stdin-using Jupyter kernel created using the @jupyter@ library.
Copyright   : (c) Andrew Gibiansky, 2016
License     : MIT
Maintainer  : andrew.gibiansky@gmail.com
Stability   : stable
Portability : POSIX

This module is the Main module for @kernel-calculator@, a Jupyter kernel which implements a simple
calculator language using the @jupyter@ library. It is intended to demonstrate a full yet basic kernel,
one which implements execution, inspection, completion, as well as the basic informational requests.
-}

{-# Language OverloadedStrings #-}
module Main(main) where

-- Imports from 'base'
import           Control.Concurrent (newMVar)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO (stderr)

-- Imports from 'text'
import qualified Data.Text.IO as T

-- Imports from 'jupyter'
import           Jupyter.Install (installKernel, simpleKernelspec, InstallUser(..), InstallResult(..),
                                  Kernelspec)
import           Jupyter.Kernel (readProfile, serve, defaultCommHandler)

-- Imports from 'kernel-calculator'
import           Calculator.Handler (requestHandler)

-- | In `main`, support two commands:
--
--    - `kernel-calculator install`: Register this kernel with Jupyter. 
--    - `kernel-calculator kernel $FILE`: Serve a kernel given ports in connection file $FILE.
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
  installKernel InstallLocal calculatorKernelspec >>= handleInstallResult
  where
    -- A basic kernelspec with limited info.
    calculatorKernelspec :: Kernelspec
    calculatorKernelspec = 
      simpleKernelspec "Calculator" "calculator" $ \exe connect -> [exe, "kernel", connect]

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

  -- Keep track of the current execution using an MVar. In general, kernel state (when it exists)
  -- often needs to be kept in some sort of temporary mutable state.
  print profile
  execCountVar <- newMVar 1
  serve profile defaultCommHandler $ requestHandler profile execCountVar
