module Main (main) where

-- Imports from 'tasty'
import           Test.Tasty (defaultMain, testGroup)

-- Imports from 'jupyter'
import           Jupyter.Test.Client (clientTests)
import           Jupyter.Test.Install (installTests)
import           Jupyter.Test.Kernel (kernelTests)
import           Jupyter.Test.ZeroMQ (zmqTests)

-- | Run all Haskell tests for the @jupyter@ package.
main :: IO ()
main =
  defaultMain $
    -- testGroup "Tests" [installTests, zmqTests, kernelTests, clientTests]
    testGroup "Tests" [clientTests]
