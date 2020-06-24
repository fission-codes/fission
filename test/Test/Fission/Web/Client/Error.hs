module Test.Fission.Web.Client.Error (tests) where

import           Test.Fission.Prelude as Mock


import Servant.Client
import Fission.Web.Client.Error
import Network.HTTP.Types.Status
import Fission.Web.Ping as Ping

import           Fission.Internal.Fixture            as Fixture



tests :: IO TestTree
tests = do

  -----------------------
  -- EFFECTFUL SESSION --
  -----------------------

  Mock.Session 
    { effectLog = _effectLog :: [OpenUnion '[LogMsg, APICall]]
    , result = result :: (Either ClientError a) 
    } <- runMock defaultConfig do
          retryOnErr [status502, status504] 100 <| client (Proxy @Ping.API)

  -----------
  -- SPECS --
  -----------

  testSpec "Fission.Web.Client.Error" $ parallel do
    describe "retryOnErr" do
      it "should work" do
        result `shouldBe` Left (Fixture.failure502)
