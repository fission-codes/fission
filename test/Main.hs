module Main (main) where

import           Test.Fission.Prelude
import qualified Test.Fission.Random
import qualified Test.Fission.Web.Auth.Class as Web.Auth.Class

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests =
  [ Web.Auth.Class.tests
  , Test.Fission.Random.tests
  ] |> sequence
    |> fmap (testGroup "Fission Specs")
