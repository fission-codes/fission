module Test.Fission.Web.Auth.Token.JWT (tests) where

import qualified Data.Aeson                                 as JSON
import qualified Data.ByteString.Lazy.Char8                 as Lazy.Char8
import qualified RIO.ByteString.Lazy                        as Lazy

import           Fission.Web.Auth.Token.JWT

import           Test.Fission.Prelude
import qualified Test.Fission.Web.Auth.Token.JWT.Validation as Validation

import qualified Test.Fission.Web.Auth.Token.JWT.Proof      as Proof

tests :: SpecWith ()
tests =
  describe "Fission.Web.Auth.Token.JWT" do
    Proof.tests
    Validation.tests

    describe "serialization" do
      itsProp' "serialized is isomorphic to ADT" \(jwt :: JWT) ->
        JSON.eitherDecode (JSON.encode jwt) `shouldBe` Right jwt

      describe "format" do
        itsProp' "contains exactly two '.'s" \(jwt :: JWT) ->
          jwt
            |> JSON.encode
            |> Lazy.count (fromIntegral $ ord '.')
            |> shouldBe 2

        itsProp' "contains only valid base64 URL characters" \(jwt :: JWT) ->
          let
            encoded = JSON.encode jwt
          in
            encoded
              |> Lazy.take (Lazy.length encoded - 2)
              |> Lazy.drop 2
              |> Lazy.filter (not . isValidChar)
              |> shouldBe mempty

isValidChar :: Word8 -> Bool
isValidChar w8 = Lazy.elem w8 validB64URLChars

validB64URLChars :: Lazy.ByteString
validB64URLChars = Lazy.Char8.pack chars
  where
    chars :: [Char]
    chars = ['a'..'z']
         <> ['A'..'Z']
         <> ['0'..'9']
         <> ['_', '-', '.']
