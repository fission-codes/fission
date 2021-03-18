module Fission.Test.Web.Server (spec) where

import           Fission.Test.Web.Server.Prelude

import qualified Fission.Test.Web.Server.Auth    as Web.Auth
import qualified Fission.Test.Web.Server.Error   as Error
import qualified Fission.Test.Web.Server.Ping    as Web.Ping

spec :: Spec
spec =
  describe "Fission.Web.Server" $ parallel do
    Web.Auth.spec
    Web.Ping.spec
    Error.spec
