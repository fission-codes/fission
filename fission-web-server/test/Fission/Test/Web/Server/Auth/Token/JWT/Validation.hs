module Fission.Test.Web.Server.Auth.Token.JWT.Validation (spec) where

import qualified Data.Aeson                             as JSON
import           Network.HTTP.Client.TLS                as HTTP

import qualified Fission.Internal.Fixture.Bearer        as Fixture
import qualified Fission.Internal.Fixture.Bearer.Nested as Nested.Fixture

import           Fission.Web.Auth.Token.JWT.Types       as JWT
import qualified Fission.Web.Auth.Token.JWT.Validation  as JWT

import           Fission.Test.Web.Server.Prelude
import           Fission.User.DID.Types

import           Fission.Web.Auth.Token.JWT.Resolver    as Proof

spec :: Spec
spec =
  describe "JWT Validation" do
    return ()
    -- context "RSA 2048" do
      -- FIXME when we have a functioning real world case
      -- context "real world bearer token" do
      --   it "is valid" do
      --     JWT.pureChecks Fixture.rawContent Fixture.jwtRSA2048
      --       `shouldBe` Right Fixture.jwtRSA2048

      -- context "real world nested bearer token -- end to end" do
      --   it "is valid" do
      --     JWT.check Nested.Fixture.rawContent Nested.Fixture.jwtRSA2048
      --       `shouldBe` Nested.Fixture.InTimeBounds (pure $ Right Nested.Fixture.jwtRSA2048)

--     describe "ION" do
--       it "is valid" do   --    $ runIO do
--         mgr    <- HTTP.newTlsManager
--         result <- runIonic $ JWT.check mgr serverDID ionContent ionUCAN
--         result `shouldBe` Right ionUCAN

ionContent :: JWT.RawContent
ionContent = JWT.RawContent "eyJhbGciOiJFZERTQSIsInR5cCI6IkpXVCIsInVhdiI6IjEuMC4wIn0.eyJhdWQiOiJkaWQ6a2V5OnpTdEVacHpTTXRUdDlrMnZzemd2Q3dGNGZMUVFTeUExNVc1QVE0ejNBUjZCeDRlRko1Y3JKRmJ1R3hLbWJtYTQiLCJleHAiOjE2MzI0MDg5MTMsImZjdCI6W10sImlzcyI6ImRpZDppb246RWlBWFl0WU9zRlBYSzlyRXc5eGJMand3UG42VmotVWlvRnZSUlgxM01CSU5lUSIsIm5iZiI6MTYzMjMyMjQ1MywicHJmIjpudWxsLCJwdGMiOiJBUFBFTkQiLCJyc2MiOiIqIn0"

ionRaw :: ByteString
ionRaw = "eyJhbGciOiJFZERTQSIsInR5cCI6IkpXVCIsInVhdiI6IjEuMC4wIn0.eyJhdWQiOiJkaWQ6a2V5OnpTdEVacHpTTXRUdDlrMnZzemd2Q3dGNGZMUVFTeUExNVc1QVE0ejNBUjZCeDRlRko1Y3JKRmJ1R3hLbWJtYTQiLCJleHAiOjE2MzI0MDg5MTMsImZjdCI6W10sImlzcyI6ImRpZDppb246RWlBWFl0WU9zRlBYSzlyRXc5eGJMand3UG42VmotVWlvRnZSUlgxM01CSU5lUSIsIm5iZiI6MTYzMjMyMjQ1MywicHJmIjpudWxsLCJwdGMiOiJBUFBFTkQiLCJyc2MiOiIqIn0.oJ1S6j2nwSR3w6PC_pJnQ4FJjifq8SZKJx4kTcMkEGhghSkZdto5qNLpXQZ5UXGw8NPgdWw0AVszQVvxcdikCA"

ionUCAN :: JWT
Just ionUCAN = JSON.decodeStrict ("\""<> ionRaw <> "\"")

newtype IONIC a = IONIC { runIonic :: IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTime)

instance Proof.Resolver IONIC where
  resolve _ = return . Left $ InvalidJWT "Should not hit this code path"

serverDID :: DID
Just serverDID = JSON.decode "\"did:key:z6MkgYGF3thn8k1Fv4p4dWXKtsXCnLH7q9yw4QgNPULDmDKB\""
