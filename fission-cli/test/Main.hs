module Main (main) where

import           Test.Fission.CLI.Prelude
import qualified Test.Fission.CLI.Web.Client as CLI.Web.Client

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests =
  testGroup "Fission CLI Specs" <$> sequence
    [ CLI.Web.Client.tests
    ]
