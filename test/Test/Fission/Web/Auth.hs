module Test.Fission.Web.Auth (tests) where

import           Servant

import           Test.Tasty.Hspec
import           Test.Fission.Prelude as Mock

import           Fission.Internal.Fixture.Entity as Fixture
import           Fission.Internal.Fixture.User   as Fixture

import           Fission.Web.Auth
import qualified Fission.Platform.Heroku.Auth.Types as Heroku

tests :: IO TestTree
tests = do

  ------------------------
  -- EFFECTFIUL SESSION --
  ------------------------

  Mock.Session
    { effectLog = _effectLog :: [OpenUnion '[]]
    , result = BasicAuthCheck userVerifier
            :. BasicAuthCheck herokuVerifier
            :. EmptyContext
    } <- runMock defaultConfig mkAuth

  userResult   <- userVerifier   <| BasicAuthData "username" "password"
  herokuResult <- herokuVerifier <| BasicAuthData "username" "password"

  -----------
  -- SPECS --
  -----------

  testSpec "Fission.Web.Auth" <| parallel do
    describe "mkAuth" do
      describe "value" do
        context "user auth" do
          it "uses the encapsulated function" do
            userResult `shouldBe` Authorized (Fixture.entity Fixture.user)

        context "heroku auth" do
          it "uses the encapsulated function" do
            herokuResult `shouldBe` Authorized (Heroku.Auth "FAKE HEROKU")
