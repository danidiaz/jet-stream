module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Data.IORef

import Jet

tests :: TestTree
tests =
  testGroup
    "All"
    [
    ]

main :: IO ()
main = defaultMain tests
