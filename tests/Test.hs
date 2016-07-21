module Main (main) where

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Jupyter.Test.Install (installTests)
import Jupyter.Test.ZeroMQ (zmqTests)
import Jupyter.Test.Kernel (kernelTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [installTests, zmqTests, kernelTests]
