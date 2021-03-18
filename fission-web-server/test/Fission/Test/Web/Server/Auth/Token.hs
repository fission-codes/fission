module Fission.Test.Web.Server.Auth.Token (spec) where

import qualified Data.Aeson                         as JSON

import qualified Network.Wai                        as Wai
import           Network.Wai.Internal

-- import qualified RIO.ByteString                     as Strict

-- import           Fission.Internal.Fixture.Bearer

import qualified Fission.Web.Auth.Token.Basic.Types as Basic
import           Fission.Web.Auth.Token.Types
-- import qualified Fission.Web.Auth.Token.Types    as Token

import           Fission.Web.Server.Auth.Error
import qualified Fission.Web.Server.Auth.Token      as Token

import           Fission.Test.Web.Server.Prelude

spec :: Spec
spec =
  describe "Auth.Token" $ parallel do
    describe "serialization" $ parallel do
      itsProp' "serialized is isomorphic to ADT" \(token :: Token) ->
        JSON.eitherDecode (JSON.encode token) `shouldBe` Right token

      context "no token" do
        it "reports the lack of a token" do
          Token.get Wai.defaultRequest `shouldBe` Left NoToken

      context "unknown auth type" do
        let authed = Wai.defaultRequest {requestHeaders = [("authorization", "12345")]}

        it "returns error message" do
          Token.get authed `shouldBe` Left (CannotParse "12345 is not a valid auth header")

      describe "Basic token" do
        let authed = Wai.defaultRequest {requestHeaders = [("authorization", "Basic 12345")]}

        it "parses the token" do
          Token.get authed `shouldBe` Right (Token.Basic $ Basic.Token "12345")

    -- describe "Bearer token" do
    --   let jsonJWT = encodeUtf8 jsonRSA2048

    --   context "titlecase 'Bearer'" do
    --     let authed = Wai.defaultRequest {requestHeaders = [("authorization", jsonJWT)]}

    --     it "parses the token" do
    --       Token.get authed `shouldBe` Right (Bearer tokenRSA2048)

    --   context "lowerecase 'bearer'" do
    --     let
    --       jsonJWTLowercase = "b" <> Strict.drop 1 jsonJWT
    --       authed = Wai.defaultRequest {requestHeaders = [("authorization", jsonJWTLowercase)]}

    --     it "parses the token" do
    --       Token.get authed `shouldBe` Right (Bearer tokenRSA2048)
