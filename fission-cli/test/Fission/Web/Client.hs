module Test.Fission.Web.Client (tests) where

import           Test.Fission.Prelude
import qualified Test.Fission.Web.Client.Error as Error

tests :: IO TestTree
tests =
  testGroup "Fission.Web.Client" <$> sequence
    [ Error.tests ]
