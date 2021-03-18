module Fission.Test.Web.Server.Auth (spec) where

import           Network.Wai
import           Servant
import           Servant.Server.Experimental.Auth

import           Test.Tasty.Hspec

import qualified Fission.Internal.Fixture.Key.Ed25519                   as Ed25519

import           Fission.Web.Server.Fixture.Entity                      as Fixture
import           Fission.Web.Server.Fixture.User                        as Fixture

import           Fission.User.DID.Types
import qualified Fission.Web.API.Heroku.Auth.Types                      as Heroku
import           Fission.Web.Server.Auth
import           Fission.Web.Server.Authorization.Types

import qualified Fission.Test.Web.Server.Auth.Token                     as Token
import qualified Fission.Test.Web.Server.Auth.Token.Bearer              as Bearer
import qualified Fission.Test.Web.Server.Auth.Token.JWT                 as JWT
import           Fission.Test.Web.Server.Prelude                        as Mock

import qualified Fission.Test.Web.Server.Auth.Token.UCAN.Resource       as Resource
import qualified Fission.Test.Web.Server.Auth.Token.UCAN.Resource.Scope as Scope

import qualified Fission.Test.Web.Server.Auth.Signature.Ed25519         as Ed

spec :: Spec
spec =
  describe "Fission.Web.Auth" $ parallel do
    Token.spec
    Bearer.spec
    JWT.spec
    Ed.spec
    Resource.spec
    Scope.spec

    describe "mkAuth" do
      (didResult, userResult, herokuResult) <- runIO setup

      describe "value" $ parallel do
        context "user auth" do
          it "uses the encapsulated function" do
            fmap about userResult `shouldBe` Right (Fixture.entity Fixture.user)

        context "DID auth" do
          it "uses the encapsulated function" do
            didResult `shouldBe` Right (DID Key Ed25519.pk)

        context "heroku auth" do
          it "uses the encapsulated function" do
            herokuResult `shouldBe` Authorized (Heroku.Auth "FAKE HEROKU")

setup :: IO (Either ServerError DID, Either ServerError Authorization, BasicAuthResult Heroku.Auth)
setup = do
  Mock.Session
    { effectLog = _effectLog :: [OpenUnion '[]]
    , result = AuthHandler    didVerifier
            :. AuthHandler    userVerifier
            :. BasicAuthCheck herokuVerifier
            :. EmptyContext
    } <- runMock @'[] defaultConfig mkAuth

  did    <- runHandler $ didVerifier  defaultRequest
  user'  <- runHandler $ userVerifier defaultRequest
  heroku <- herokuVerifier $ BasicAuthData "username" "password"

  return (did, user', heroku)
