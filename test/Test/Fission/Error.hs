module Test.Fission.Error (tests) where

import Test.Fission.Prelude as Mock
import Fission.Error


tests :: IO TestTree
tests = do

  Mock.Session 
    { effectLog = successEffectLog :: [OpenUnion '[LogMsg]]
    , result = successResult :: Natural
    } <- runMock defaultConfig do
        retryOnErr equal5 100 $ pure 5

  Mock.Session 
    { effectLog = failEffectLog :: [OpenUnion '[LogMsg]]
    , result = failResult :: Natural
    } <- runMock defaultConfig do
        retryOnErr equal5 100 $ pure 6


  testSpec "Fission.Error" $ parallel do
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


equal5 :: 
  MonadLogger m 
  => Natural 
  -> m Bool
equal5 num = do
  logWarn @Text "log"
  return $ num == 5

