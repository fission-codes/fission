module Fission.Test.Web.Server.Root (spec) where

import           Servant

import           Fission.Internal.Mock
import qualified Fission.Web.API.Types           as API
-- import           Fission.Web.Server.Mock.Config

import           Fission.Test.Web.Server.Prelude

spec :: Spec
spec =
  describe "GET /" do
    with rootServer do
      it "is always successful" do
        get "/" `shouldRespondWith` 200

      it "has an empty body'" do
        get "/" `shouldRespondWith` 200
          { matchBody =
              MatchBody \_ body ->
                if body == "" then Nothing else Just "not empty"
          }

rootServer :: IO Application
rootServer = return . serve (Proxy @API.Root) $ runMockIO () rootHandler

rootHandler :: Mock '[] () NoContent -- i.e. this type enforces that it produces no effects
rootHandler = return NoContent
