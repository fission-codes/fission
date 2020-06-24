module Test.Fission.Web.Client.Error (tests) where

import           Test.Fission.Prelude as Mock


import Servant.Client
import Fission.Web.Client.Error
import Network.HTTP.Types.Status
import Fission.Web.Ping as Ping
import Data.List as L


import           Fission.Internal.Fixture            as Fixture

isLogMsg :: OpenUnion '[LogMsg, APICall] -> Bool
isLogMsg effect = 
  case effect of
    This _ -> True
    _ -> False

isAPICall :: OpenUnion '[LogMsg, APICall] -> Bool
isAPICall = not . isLogMsg 

tests :: IO TestTree
tests = do

  -----------------------
  -- EFFECTFUL SESSION --
  -----------------------

  Mock.Session 
    { effectLog = failEffectLog :: [OpenUnion '[LogMsg, APICall]]
    , result = failResult :: (Either ClientError a) 
    } <- runMock defaultConfig do
          retryOnErr [status502, status504] 100 <| client (Proxy @Ping.API)

  -----------
  -- SPECS --
  -----------

  testSpec "Fission.Web.Client.Error" $ parallel do
    describe "retryOnErr" do
      it "fails if only gets failures" do
        failResult `shouldBe` Left (Fixture.failure502)

      it "retries 100 times" do
        L.length (L.filter isAPICall failEffectLog) `shouldBe` 101 -- (100 retries == 101 tries)

      it "logs for every failure" do
        L.length (L.filter isLogMsg failEffectLog) `shouldBe` 100




