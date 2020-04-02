module Test.Fission.Web.Auth (tests) where

import           Network.Wai
import           Servant
import           Servant.Server.Experimental.Auth

import           Test.Tasty.Hspec
import           Test.Fission.Prelude as Mock

import           Fission.Key as Key
import           Fission.User.DID.Types

import           Fission.Internal.Fixture.Entity as Fixture
import           Fission.Internal.Fixture.User   as Fixture

import           Fission.Web.Auth
import qualified Fission.Platform.Heroku.Auth.Types as Heroku
import           Fission.User.DID.Types

tests :: IO TestTree
tests = do

  ------------------------
  -- EFFECTFIUL SESSION --
  ------------------------

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
    describe "mkAuth" do
      describe "value" do
        context "user auth" do
          it "uses the encapsulated function" do
            userResult `shouldBe` Right (Fixture.entity Fixture.user)

        context "DID auth" do
          it "uses the encapsulated function" do
            didResult `shouldBe` Right $
              DID (Key.PublicKey "thisismydid") RSA2048 Key

        context "heroku auth" do
          it "uses the encapsulated function" do
            herokuResult `shouldBe` Authorized (Heroku.Auth "FAKE HEROKU")
