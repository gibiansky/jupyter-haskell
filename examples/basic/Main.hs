{-# Language OverloadedStrings #-}
module Main(main) where

import           System.Environment (getArgs)

import           Jupyter.Install (installKernel, simpleKernelspec, InstallUser(..), InstallResult(..))
import           Jupyter.Kernel (readProfile, simpleKernelInfo, serve, defaultCommHandler,
                                 defaultClientRequestHandler, KernelProfile, ClientRequestHandler)
import           Jupyter.Messages (KernelInfo(..), LanguageInfo(..))

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["install"] ->
      print =<<
      installKernel InstallGlobal (simpleKernelspec "Basic" "basic" $
                                     \exe connect -> [exe, "kernel", connect])
    ["kernel", profilePath] -> do
      Just profile <- readProfile profilePath
      print profile
      serve profile defaultCommHandler $
        defaultClientRequestHandler profile $ simpleKernelInfo "Basic"
    _ -> putStrLn "Invalid arguments."

-- todo:
-- in docs, make sure there's an easy link both directions (client request constructor) <--> (kernel reply constructor)
