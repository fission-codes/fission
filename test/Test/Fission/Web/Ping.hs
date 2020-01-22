module Test.Fission.Web.Ping (tests) where

import           Servant

import           Test.Tasty.Hspec
import           Test.Fission.Prelude

import           Fission.Web.Ping
import           Fission.Web.Handler

import qualified Fission.Web.Routes as Web.Routes
import qualified Fission.Web as Web
import qualified Fission.Web.Auth as Auth

import           Test.Fission.Fixture.Entity as Fixture
import           Test.Fission.Fixture.User   as Fixture

import Fission.Internal.Orphanage.CID ()
import Fission.Internal.Orphanage.Serilaized ()

import           Control.Monad.Writer
import           Data.Generics.Product
import           Database.Esqueleto (Entity (..))

import qualified Network.IPFS.Types as IPFS
import           Servant

import           Test.Fission.Mock.Effect

import           Fission.Prelude
import           Fission.IPFS.Linked.Class
import           Fission.Web.Auth.Class
import           Fission.Models

import qualified Fission.Platform.Heroku.Auth.Types as Heroku

data TestContext = TestContext
  { linkedPeers     :: NonEmpty IPFS.Peer
  -- , authCheckText   :: BasicAuthCheck Text
  , authCheckUser   :: BasicAuthCheck (Entity User)
  , authCheckHeroku :: BasicAuthCheck Heroku.Auth
  , now             :: Maybe UTCTime
  } deriving Generic

ctx = TestContext
  { linkedPeers     = pure <| IPFS.Peer "ipv4/somePeer"
  , authCheckUser   = BasicAuthCheck \_ -> pure <| Authorized <| Fixture.entity Fixture.testUser
  , authCheckHeroku = BasicAuthCheck \_ -> pure <| Authorized <| Heroku.Auth "FAKE HEROKU"
  , now = Nothing
  }

api :: Proxy Web.Routes.API
api = Proxy @Web.Routes.API

type Effs =
  '[ SetDNSLink
   , CheckTime
   , RunDB
   , GetLinkedPeers
   , GetVerifier
   ]

mockRunner :: Mock Effs  TestContext a -> IO a
mockRunner = runMockIO ctx

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
