module Fission.Test.Web.Server.Error (spec) where

import           Fission.Error

import           Fission.Test.Web.Server.Prelude as Mock

spec :: Spec
spec =
  describe "Fission.Error" do
    (successSession, failureSession) <- runIO setup

    let
      Mock.Session
        { effectLog = successEffectLog :: [OpenUnion '[LogMsg]]
        , result    = successResult    :: Natural
        } = successSession

      Mock.Session
        { effectLog = failEffectLog :: [OpenUnion '[LogMsg]]
        , result    = failResult    :: Natural
        } = failureSession

    describe "retryOnErr" do
      describe "handles a valid result" do
        it "only runs once" do
          length successEffectLog `shouldBe` 1

        it "should return the valid result" do
          successResult `shouldBe` 5

      describe "retries on an invalid result" do
        it "retries 100 times" do
          length failEffectLog `shouldBe` 100

        it "should return the invalid result after retrying" do
          failResult `shouldBe` 6

equal5 :: MonadLogger m => Natural -> m Bool
equal5 num = do
  logWarn @Text "log"
  return $ num == 5

setup :: IO (Session '[LogMsg] Natural, Session '[LogMsg] Natural)
setup = do
  good <- runMock defaultConfig $ retryOnErr equal5 100 (pure 5)
  bad  <- runMock defaultConfig $ retryOnErr equal5 100 (pure 6)
  return (good, bad)
