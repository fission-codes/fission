module Test.Fission.Web.Auth.Token (tests) where

import qualified Data.Aeson                         as JSON

import qualified Network.Wai                        as Wai
import           Network.Wai.Internal

import qualified RIO.ByteString                     as Strict

import           Fission.Web.Auth.Error

import qualified Fission.Web.Auth.Token             as Token
import qualified Fission.Web.Auth.Token.Basic.Types as Basic
import           Fission.Web.Auth.Token.Types

import           Fission.Internal.Fixture.Bearer

import           Test.Fission.Prelude

tests :: SpecWith ()
tests =
  describe "Auth.Token" do
  describe "serialization" do
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
        Token.get authed `shouldBe` Right (Basic $ Basic.Token "12345")

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
