module Fission.Test.Web.Auth.Token.JWT (spec) where

import qualified Data.Aeson                                       as JSON
import qualified Data.ByteString.Lazy.Char8                       as Lazy.Char8
import qualified RIO.ByteString.Lazy                              as Lazy
import           Servant.API

import qualified Fission.Internal.UTF8                            as UTF8
import           Fission.Web.Auth.Token.Ucan.Fact.Types
import           Fission.Web.Auth.Token.Ucan.Resource.Scope.Types
import           Fission.Web.Auth.Token.Ucan.Resource.Types
import           Web.Ucan.Types

import           Fission.Test.Prelude
import qualified Fission.Test.Web.Auth.Token.JWT.Validation       as Validation

import qualified Fission.Test.Web.Auth.Token.JWT.Proof            as Proof

spec :: Spec
spec =
  describe "Fission.Web.Auth.Token.JWT" do
    Proof.spec
    Validation.spec

    describe "Header serialization" do
      itsProp' "text serialization is unquoted JSON" \(ucan :: Ucan Fact (Scope Resource)) ->
        ucan
          |> toUrlPiece
          |> UTF8.wrapIn "\""
          |> encodeUtf8
          |> Lazy.fromStrict
          |> shouldBe (JSON.encode ucan)

    describe "JSON serialization" do
      itsProp' "serialized is isomorphic to ADT" \(ucan :: Ucan Fact (Scope Resource)) ->
        JSON.eitherDecode (JSON.encode ucan) `shouldBe` Right ucan

      describe "format" do
        itsProp' "contains exactly two '.'s" \(ucan :: Ucan Fact (Scope Resource)) ->
          ucan
            |> JSON.encode
            |> Lazy.count (fromIntegral $ ord '.')
            |> shouldBe 2

        itsProp' "contains only valid base64 URL characters" \(ucan :: Ucan Fact (Scope Resource)) ->
          let
            encoded = JSON.encode ucan
          in
            encoded
              |> Lazy.take (Lazy.length encoded - 2)
              |> Lazy.drop 2
              |> Lazy.filter (not . isValidChar)
              |> shouldBe mempty

isValidChar :: Word8 -> Bool
isValidChar w8 = Lazy.elem w8 (" " <> validEncodedJWTChars)

validEncodedJWTChars :: Lazy.ByteString
validEncodedJWTChars = Lazy.Char8.pack (base64URLChars <> ['.']) -- dot is used as a separator in JWTs
  where
    base64URLChars :: [Char]
    base64URLChars =
         ['a'..'z']
      <> ['A'..'Z']
      <> ['0'..'9']
      <> ['_', '-']
