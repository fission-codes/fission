module Test.Fission.Web.Ping (tests) where

import           Servant

import           Test.Tasty.Hspec
import           Test.Fission.Prelude

import           Fission.Web.Ping
import           Fission.Web.Handler

import qualified Fission.Web.Routes as Web.Routes
import qualified Fission.Web as Web
import qualified Fission.Web.Auth as Auth

import Fission.Internal.Orphanage.CID ()
import Fission.Internal.Orphanage.Serilaized ()

import           Control.Monad.Writer
import           Database.Esqueleto (Entity (..))

import qualified Network.IPFS.Types as IPFS
import           Servant

import           Fission.Prelude
import           Fission.IPFS.Linked.Class
import           Fission.Web.Auth.Class
import           Fission.Models

import qualified Fission.Platform.Heroku.Auth.Types as Heroku

api :: Proxy Web.Routes.API
api = Proxy @Web.Routes.API

type Effs =
  '[ CheckTime
   , GetLinkedPeers
   , GetVerifier
   , RunAWS
   , RunDB
   , SetDNSLink
   , UpdateRoute53
   , RunLocalIPFS
   , RunRemoteIPFS
   , LogMsg
   ]

mockRunner :: Mock Effs a -> IO a
mockRunner = runMockIO defaultConfig

myServer :: MonadIO m => ServerT Web.Routes.API m
myServer serverCtx = Auth.authWithContext api (toHandler mockRunner) Web.bizServer

completeServer authChecks =
  Web.bizServer
    |> Auth.authWithContext api (toHandler mockRunner)
    |> serveWithContext     api authChecks

tests :: IO TestTree
tests = do
  authChecks <- mockRunner Auth.mkAuth

  testSpec "Fission.Web.Ping" do
    describe "GET /" do
      it "follows best practices" do
        withServantServerAndContext api authChecks (pure myServer) \baseURL ->
          serverSatisfies api baseURL defaultArgs do
            unauthorizedContainsWWWAuthenticate
              <%> not500
              <%> mempty

      with (pure <| completeServer authChecks) do
        it "is always be successful" do
          get "/" `shouldRespondWith` 200

        it "contains the text 'pong'" do
          get "/" `shouldRespondWith` 200
            { matchBody = MatchBody <| bodyMatches <| String "pong" }
