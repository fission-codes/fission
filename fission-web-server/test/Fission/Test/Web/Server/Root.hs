module Fission.Test.Web.Server.Root (spec) where

import           Servant
import           Servant.API.Generic

import qualified Fission.Web.API.Types           as API

import           Fission.Test.Web.Server.Prelude

spec :: Spec
spec =
  describe "GET /" do
    with rootServer do
      it "is always be successful" do
        get "/" `shouldRespondWith` 200

      it "contains the text 'pong'" do
        get "/" `shouldRespondWith` 200
          { matchBody = MatchBody $ bodyMatches Null }

rootServer :: IO Application
rootServer = return . serve (Proxy @API.Root) $ runMockIO defaultConfig rootHandler

rootHandler :: Mock '[] NoContent -- i.e. this type enforces that it produces no effects
rootHandler = return NoContent
