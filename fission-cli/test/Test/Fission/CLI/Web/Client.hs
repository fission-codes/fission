module Test.Fission.CLI.Web.Client (tests) where

import           Test.Fission.CLI.Prelude
import qualified Test.Fission.CLI.Web.Client.Error as Error

tests :: IO TestTree
tests =
  testGroup "Fission.Web.Client" <$> sequence
    [ Error.tests ]
