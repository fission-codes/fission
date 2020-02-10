module Main (main) where

import Test.Tasty

-- ⚛

import Fission.Prelude

-- Specs

import qualified Test.Random


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests"
  [ Test.Random.tests
  ]
