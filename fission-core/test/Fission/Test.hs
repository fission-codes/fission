module Fission.Test (spec) where

import           Fission.Test.Prelude

import qualified Fission.Test.DNS      as DNS
import qualified Fission.Test.Random   as Random
import qualified Fission.Test.URL      as URL
import qualified Fission.Test.User.DID as DID

spec :: Spec
spec =
  parallel do
    describe "Fission" do
      DID.spec
      DNS.spec
      Random.spec
      URL.spec
