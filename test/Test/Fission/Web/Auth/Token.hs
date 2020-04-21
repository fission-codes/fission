module Test.Fission.Web.Auth.Token (tests) where

import qualified Data.Aeson as JSON

import qualified Network.Wai as Wai
import           Network.Wai.Internal

import qualified RIO.ByteString as Strict

import qualified Fission.Web.Auth.Token              as Token
import           Fission.Web.Auth.Token.Types
import qualified Fission.Web.Auth.Token.Basic.Types  as Basic

import           Fission.Internal.Fixture.Bearer

import           Test.Fission.Prelude

tests :: SpecWith ()
tests =
  describe "Auth.Token" do
    describe "serialization" do
      itsProp' "serialize+deserialize is the identity function" \(token :: Token) ->
        JSON.decode (JSON.encode token) `shouldBe` Just token

    context "no token" do
      it "is Nothing" do
        Token.get Wai.defaultRequest `shouldBe` Nothing

    context "unknown auth type" do
      let authed = Wai.defaultRequest {requestHeaders = [("authorization", "12345")]}
     
      it "is Nothing" do
        Token.get authed `shouldBe` Nothing

    describe "Basic token" do
      let authed = Wai.defaultRequest {requestHeaders = [("authorization", "Basic 12345")]}

      it "parses the token" do
        Token.get authed `shouldBe` Just (Basic $ Basic.Token "12345")

    describe "Bearer token" do
      let jsonJWT = encodeUtf8 jsonRSA2048

      context "titlecase 'Bearer'" do
        let authed = Wai.defaultRequest {requestHeaders = [("authorization", jsonJWT)]}

        it "parses the token" do
          Token.get authed `shouldBe` Just (Bearer tokenRSA2048)

      context "lowerecase 'bearer'" do
        let
          jsonJWTLowercase = "b" <> Strict.drop 1 jsonJWT
          authed = Wai.defaultRequest {requestHeaders = [("authorization", jsonJWTLowercase)]}

        it "parses the token" do
          Token.get authed `shouldBe` Just (Bearer tokenRSA2048)
