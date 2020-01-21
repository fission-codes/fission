module Test.Fission.Web.Auth.Class (tests) where

import           Servant

import           Test.Tasty.Hspec

import           Test.Fission.Prelude
import           Fission.Web.Auth.Class

newtype Context = Context { authCheckText :: BasicAuthCheck Text }
  deriving Generic

tests :: IO TestTree
tests = do

  --------------
  -- IO SETUP --
  --------------

  let
    dummyAuth = Authorized ("YUP" :: Text)
    authData  = BasicAuthData "username" "password"
    ctx       = Context { authCheckText = BasicAuthCheck \_ -> pure dummyAuth }

  MockSession
    { result    = BasicAuthCheck authCheck
    , effectLog = effectLog :: [OpenUnion '[GetVerifier]]
    } <- runMock ctx verify

  authResult <- authCheck authData

  -----------
  -- SPECS --
  -----------

  testSpec "Fission.Web.Auth.Class" <| parallel do
    describe "MonadAuth" do
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
