{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Fission.Web.Auth.Token.Bearer (tests) where

import qualified Data.Aeson                          as JSON
import qualified Data.ByteString.Lazy.Char8          as Lazy.Char8
import qualified RIO.ByteString.Lazy                 as Lazy

import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer
import           Fission.Web.Auth.Token.JWT

import qualified Fission.Internal.Fixture.Bearer     as Bearer
import qualified Fission.Internal.UTF8               as UTF8

import           Test.Fission.Prelude

tests :: SpecWith ()
tests =
  describe "Bearer Token" do
    -- FIXME waiting on new real world example
    -- describe "real world fixture" do
      -- it "deserializes" do
      --   ("\"" <> Bearer.jsonRSA2048 <> "\"")
      --     |> encodeUtf8
      --     |> Lazy.fromStrict
      --     |> JSON.decode
      --     |> shouldBe (Just Bearer.tokenRSA2048)

    describe "serialization" do
      itsProp' "serialized is isomorphic to ADT" \(bearer :: Bearer.Token) ->
        JSON.eitherDecode (JSON.encode bearer) `shouldBe` Right bearer

      describe "outgoing" do
        itsProp' "has no internal quotes" \(bearer :: Bearer.Token) ->
          bearer
            |> JSON.encode
            |> Lazy.count (fromIntegral $ ord '"')
            |> shouldBe 2

        itsProp' "starts with 'Bearer'" \(bearer :: Bearer.Token) ->
          Lazy.isPrefixOf "\"Bearer " (JSON.encode bearer) `shouldBe` True

        itsProp' "contains only valid base64 URL characters" \(bearer :: Bearer.Token) ->
          let
            encoded :: Text
            String encoded = JSON.toJSON bearer
          in
            encoded
              |> encodeUtf8
              |> Lazy.fromStrict
              |> Lazy.filter (not . isValidChar)
              |> shouldBe mempty

      describe "incoming" do
        describe "Postel's Law" do
          itsProp' "lowercase 'bearer'" \(jwt :: JWT) -> do
            let
              jwt' :: Lazy.ByteString
              jwt' = UTF8.stripQuotesLazyBS (JSON.encode jwt)

              encoded :: Lazy.ByteString
              encoded = "\"bearer " <> jwt' <> "\""

              Right Bearer.Token {jwt = result} = eitherDecode encoded

            result `shouldBe` jwt

isValidChar :: Word8 -> Bool
isValidChar w8 = Lazy.elem w8 (" " <> validB64URLChars)

validB64URLChars :: Lazy.ByteString
validB64URLChars = Lazy.Char8.pack chars
  where
    chars :: [Char]
    chars = ['a'..'z']
         <> ['A'..'Z']
         <> ['0'..'9']
         <> ['_', '-', '.']
