module Fission.Test.Web.Server.Ping (spec) where

import           Servant

import           Fission.Pong.Types
import           Fission.Web.API.Ping.Types
import           Fission.Web.Server.Handler.Ping as Web.Ping

import           Fission.Test.Web.Server.Prelude

spec :: Spec
spec =
  describe "Fission.Web.Ping" do
    describe "GET /ping" do
      with pingServer do
        it "is always be successful" do
          get "/ping" `shouldRespondWith` 200

        it "contains the text 'pong'" do
          get "/ping" `shouldRespondWith` 200
            { matchBody = MatchBody . bodyMatches $ String "pong" }

pingServer :: IO Application
pingServer = return $ serve (Proxy @Ping) $ runMockIO defaultConfig pingHandler

pingHandler :: Mock '[] Pong -- i.e. this type enforces that it produces no effects
pingHandler = Web.Ping.handler
