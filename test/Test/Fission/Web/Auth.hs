module Test.Fission.Web.Auth (tests) where

import           Database.Esqueleto
import           Servant

import           Test.Tasty.Hspec

import           Test.Fission.Prelude
import           Fission.Models
import           Fission.Web.Auth

import           Test.Fission.Fixture.Entity as Fixture
import           Test.Fission.Fixture.User   as Fixture

import qualified Fission.Platform.Heroku.Auth.Types as Heroku

data Context = Context
  { authCheckHeroku :: BasicAuthCheck Heroku.Auth
  , authCheckUser   :: BasicAuthCheck (Entity User)
  } deriving Generic

userAuth   = Authorized <| Fixture.entity Fixture.testUser
herokuAuth = Authorized <| Heroku.Auth "FAKE HEROKU"
authData   = BasicAuthData "username" "password"

ctx = Context
  { authCheckHeroku = BasicAuthCheck \_ -> pure herokuAuth
  , authCheckUser   = BasicAuthCheck \_ -> pure userAuth
  }

tests :: IO TestTree
tests = do

  ------------------------
  -- EFFECTFIUL SESSION --
  ------------------------

  MockSession
    { effectLog = effectLog :: [OpenUnion '[GetVerifier]]
    , result = BasicAuthCheck userVerifier
            :. BasicAuthCheck herokuVerifier
            :. EmptyContext
    } <- runMock ctx mkAuth

  userResult   <- userVerifier   authData
  herokuResult <- herokuVerifier authData

  -- SPECS --

  testSpec "Fission.Web.Auth" <| parallel do
    describe "mkAuth" do
      describe "effects" do
        it "fires exactly two effects" do
          effectLog
            |> length
            |> shouldBe 2

      describe "value" do
        context "user auth" do
          it "uses the encapsulated function" do
            userResult `shouldBe` userAuth

        context "heroku auth" do
          it "uses the encapsulated function" do
            herokuResult `shouldBe` herokuAuth
