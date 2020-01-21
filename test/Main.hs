module Main (main) where

import           Test.Fission.Prelude
import qualified Test.Fission.Random
import qualified Test.Fission.Web.Auth.Class as Web.Auth.Class

main :: IO ()
main = defaultMain . testGroup "Fission Tests" =<< tests

tests :: IO [TestTree]
tests = sequence
  [ Web.Auth.Class.tests
  , Test.Fission.Random.tests
  ]
