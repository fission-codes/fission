module Main (main) where

import           Test.Fission.Prelude
import qualified Test.Fission.Web.Client as Web.Client

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests =
  testGroup "Fission CLI Specs" <$> sequence
    [ Web.Client.tests
    ]
