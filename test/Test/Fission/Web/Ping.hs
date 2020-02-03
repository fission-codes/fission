module Test.Fission.Web.Ping (tests) where

import qualified Network.IPFS.Types as IPFS
import           Servant

import           Test.Fission.Prelude

import qualified Fission.Web        as Web
import qualified Fission.Web.Auth   as Auth
import qualified Fission.Web.Routes as Web.Routes
import           Fission.Web.Ping   as Web.Ping

import           Fission.Internal.Orphanage.CID ()
import           Fission.Internal.Orphanage.Serilaized ()

import qualified Fission.Platform.Heroku.Auth.Types as Heroku

api :: Proxy Web.Ping.API
api = Proxy @Web.Ping.API

pingServer :: Application
pingServer = serve api <| pure Web.Ping.pong

tests :: IO TestTree
tests =
  testSpec "Fission.Web.Ping" do
    describe "GET /ping" do
      with (pure pingServer) do
        it "is always be successful" do
          get "/" `shouldRespondWith` 200

        it "contains the text 'pong'" do
          get "/" `shouldRespondWith` 200
            { matchBody = MatchBody <| bodyMatches <| String "pong" }

-- type Effs =
--   '[ CheckTime
--    , RunAWS
--    , RunDB
--    , SetDNSLink
--    , UpdateRoute53
--    , RunLocalIPFS
--    , RunRemoteIPFS
--    , LogMsg
--    , DestroyHerokuAddOn
--    , DestroyUser
--    , DestroyUserCID
--    , RetrieveUser
--    , RetrieveUserCID
--    , ModifyUser
--    , CreateUser
--    , CreateUserCID
--    , CreateHerokuAddOn
--    ]

-- mockRunner :: Mock '[] a -> IO a
-- mockRunner = runMockIO defaultConfig

-- mockServer :: Server Web.Routes.API
-- mockServer = Auth.authWithContext api (toHandler mockRunner) Web.bizServer

-- completeServer authChecks =
--   Web.bizServer
--     |> Auth.authWithContext api (toHandler mockRunner)
--     |> serveWithContext     api authChecks
