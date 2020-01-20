module Test.Random (tests) where

import qualified RIO.ByteString as BS
import           Test.Tasty
import           Test.Tasty.Hspec

import qualified Fission.Random as Random
import Servant
import Test.Types
import           Fission.Web.Auth.Class

import Test.QuickCheck

import Fission.Prelude

import Test.QuickCheck.Instances ()

newtype AuthMock = AuthMock { authCheck :: BasicAuthCheck Text }
  deriving (Generic)

tests :: IO TestTree
tests = do
  let
    ctx = AuthMock { authCheck = BasicAuthCheck \_ -> pure dummyAuth }
    authData  = BasicAuthData "username" "password"
    dummyAuth = Authorized "YUP"

  MockSession
    { result  = BasicAuthCheck authCheck
    , effects = (effects :: [OpenUnion '[GetVerifier]] )
    } <- runMock ctx verify

  authResult <- liftIO <| authCheck authData

  testSpec "spec" do
    describe "Random" do
      describe "bsRandomLength" do
        it "is the given length" <| property \n -> do
          alphaNum <- Random.bsRandomLength n

          alphaNum
            |> BS.length
            |> shouldBe (fromIntegral n)

    describe "can mock auth" do
      describe "effects" do
        it "fires exectly one effect" do
          effects
            |> length
            |> shouldBe 1

        it "looked up the auth verifier" do
          effects `shouldContain` [openUnionLift GetVerifier]

      describe "value" do
        it "uses the encapsulated function" do
          authResult `shouldBe` dummyAuth
