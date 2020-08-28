module Test.Fission.Web.Ping (tests) where

import           Servant

import           Test.Fission.Prelude
import           Fission.Web.Ping as Web.Ping

tests :: IO TestTree
tests =
  testSpec "Fission.Web.Ping" do
    describe "GET /ping" do
      with pingServer do
        it "is always be successful" do
          get "/" `shouldRespondWith` 200

        it "contains the text 'pong'" do
          get "/" `shouldRespondWith` 200
            { matchBody = MatchBody . bodyMatches $ String "pong" }

pingServer :: IO Application
pingServer =
  pingHandler
    |> runMockIO defaultConfig
    |> serve (Proxy @Web.Ping.API)
    |> return

pingHandler :: Mock '[] Pong -- i.e. this type enforces that it produces no effects
pingHandler = pure Web.Ping.pong
