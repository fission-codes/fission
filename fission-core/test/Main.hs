module Main (main) where

import           Test.Fission.Prelude
import qualified Test.Fission.Random

import qualified Test.Fission.User.DID as DID

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests =
  testGroup "Fission Specs" <$> sequence
    [ DID.tests
    , Test.Fission.Random.tests
    ]
