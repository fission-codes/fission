module Test.Fission.Web.Auth (tests) where

import           Network.Wai
import           Servant
import           Servant.Server.Experimental.Auth

import           Test.Tasty.Hspec

import qualified Fission.Internal.Fixture.Key.Ed25519            as Ed25519

import           Fission.Web.Server.Fixture.Entity               as Fixture
import           Fission.Web.Server.Fixture.User                 as Fixture

import           Fission.User.DID.Types
import qualified Fission.Web.API.Heroku.Auth.Types               as Heroku
import           Fission.Web.Server.Auth
import           Fission.Web.Server.Authorization.Types

import           Test.Fission.Prelude                            as Mock
import qualified Test.Fission.Web.Auth.Token                     as Token
import qualified Test.Fission.Web.Auth.Token.Bearer              as Bearer
import qualified Test.Fission.Web.Auth.Token.JWT                 as JWT

import qualified Test.Fission.Web.Auth.Token.UCAN.Resource       as Resource
import qualified Test.Fission.Web.Auth.Token.UCAN.Resource.Scope as Scope

import qualified Test.Fission.Web.Auth.Signature.Ed25519         as Ed

tests :: IO TestTree
tests = do

  -----------------------
  -- EFFECTFUL SESSION --
  -----------------------

  Mock.Session
    { effectLog = _effectLog :: [OpenUnion '[]]
    , result = AuthHandler    didVerifier
            :. AuthHandler    userVerifier
            :. BasicAuthCheck herokuVerifier
            :. EmptyContext
    } <- runMock defaultConfig mkAuth

  didResult    <- runHandler $ didVerifier  defaultRequest
  userResult   <- runHandler $ userVerifier defaultRequest
  herokuResult <- herokuVerifier $ BasicAuthData "username" "password"

  -----------
  -- SPECS --
  -----------

  testSpec "Fission.Web.Auth" $ parallel do
    Token.tests
    Bearer.tests
    JWT.tests
    Ed.tests
    Resource.tests
    Scope.tests

    describe "mkAuth" do
      describe "value" do
        context "user auth" do
          it "uses the encapsulated function" do
            fmap about userResult `shouldBe` Right (Fixture.entity Fixture.user)

        context "DID auth" do
          it "uses the encapsulated function" do
            didResult `shouldBe` Right (DID Key Ed25519.pk)

        context "heroku auth" do
          it "uses the encapsulated function" do
            herokuResult `shouldBe` Authorized (Heroku.Auth "FAKE HEROKU")
