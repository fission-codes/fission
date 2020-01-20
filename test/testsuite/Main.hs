
module Main (main) where

import Test.Tasty

-- ⚛

import Fission.Prelude

-- Specs

import qualified Test.Random

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = do
  randomTests <- Test.Random.tests

  return <| testGroup "Tests"
    [ randomTests
    ]
