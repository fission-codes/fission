module Test.Fission.Web.Client.Error (tests) where

import           Test.Fission.Prelude as Mock


import           Fission.Web.Client.Error
import           Network.HTTP.Types.Status
import qualified Fission.Internal.Fixture as Fixture

tests :: IO TestTree
tests = do

  -----------------------
  -- EFFECTFUL SESSION --
  -----------------------

  Mock.Session 
    { effectLog = _effectLog :: [OpenUnion '[LogMsg]]
    , result = validResult 
    } <- runMock defaultConfig do
        checkStatus [status502, status504] (Right (0 :: Integer))

  Mock.Session 
    { effectLog = _effectLog :: [OpenUnion '[LogMsg]]
    , result = errResult 
    } <- runMock defaultConfig do
        checkStatus [status502, status504] (Left Fixture.failure502)

  Mock.Session 
    { effectLog = _effectLog :: [OpenUnion '[LogMsg]]
    , result = allowedErrResult 
    } <- runMock defaultConfig do
        checkStatus [status504] (Left Fixture.failure502)

  -----------
  -- SPECS --
  -----------

  testSpec "Fission.Web.Client.Error" $ parallel do
    describe "retryOnErr" do
      it "is true when a valid result is returned" do
        validResult `shouldBe` True

      it "is false when a disallowed error status code is returned" do
        errResult `shouldBe` False

      it "is true when an allowed error status code is returned" do
        allowedErrResult `shouldBe` True
