module Test.Fission.Web.Auth.Class (tests) where

import           Servant

import           Test.Tasty.Hspec

import           Test.Fission.Prelude
import           Fission.Web.Auth.Class

newtype AuthMock = AuthMock { authCheck :: BasicAuthCheck Text }
  deriving (Generic)

tests :: IO TestTree
tests = do
  -- SETUP --

  let
    dummyAuth = Authorized "YUP"
    authData  = BasicAuthData "username" "password"
    ctx       = AuthMock { authCheck = BasicAuthCheck \_ -> pure dummyAuth }

  MockSession
    { result    = BasicAuthCheck authCheck
    , effectLog = effectLog :: [OpenUnion '[GetVerifier]]
    } <- runMock ctx verify

  authResult <- liftIO <| authCheck authData

  -- SPECS --

  testSpec "MonadAuth" <| parallel do
    describe "verify" do
      describe "effects" do
        it "fires exactly one effect" do
          effectLog
            |> length
            |> shouldBe 1

        it "looked up the auth verifier" do
          effectLog `shouldHaveRun` GetVerifier

      describe "value" do
        it "uses the encapsulated function" do
          authResult `shouldBe` dummyAuth
