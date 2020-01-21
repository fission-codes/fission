module Test.Fission.Web.Ping (tests) where

import           Servant

import           Test.Tasty.Hspec
import           Test.Fission.Prelude

import           Fission.Web.Ping
import           Fission.Web.Handler

api :: Proxy API
api = Proxy @API

server :: MonadIO m => m Pong
server = hoistServer api fromHandler (pure pong)

tests :: IO TestTree
tests =
  testSpec "Fission.Web.Ping" do
    describe "GET /" do
      it "follows best practices" do
        withServantServer api (pure server) \baseURL ->
          serverSatisfies api baseURL defaultArgs do
            unauthorizedContainsWWWAuthenticate
              <%> not500
              <%> mempty

      with (pure <| serve api server) do
        it "is always be successful" do
          get "/" `shouldRespondWith` 200

        it "contains the text 'pong'" do
          get "/" `shouldRespondWith` 200
            { matchBody = MatchBody <| bodyMatches <| String "pong" }
