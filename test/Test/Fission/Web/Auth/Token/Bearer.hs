{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Fission.Web.Auth.Token.Bearer (tests) where

import qualified Data.Aeson                 as JSON
import qualified Data.ByteString.Lazy.Char8 as Lazy.Char8
import qualified RIO.ByteString.Lazy        as Lazy

import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer
import           Fission.Web.Auth.Token.JWT

import qualified Fission.Internal.UTF8           as UTF8
import qualified Fission.Internal.Fixture.Bearer as Bearer

import           Test.Fission.Prelude

tests :: SpecWith ()
tests =
  describe "Bearer Token" do
    describe "real world fixture" do
      it "deserializes" do
        ("\"" <> Bearer.jsonRSA2048 <> "\"")
          |> encodeUtf8
          |> Lazy.fromStrict
          |> JSON.decode
          |> shouldBe (Just Bearer.tokenRSA2048)

      -- it "can decode Steven's real world example" do
      --   let ex = "\"Bearer eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsInVhdiI6IjAuMS4wIn0.eyJhdWQiOiJkaWQ6a2V5OnpTdEVacHpTTXRUdDlrMnZzemd2Q3dGNGZMUVFTeUExNVc1QVE0ejNBUjZCeDRlRko1Y3JKRmJ1R3hLbWJtYTQiLCJleHAiOjE1ODgyNjU0MjAsImlzcyI6ImRpZDprZXk6ejEzVjNTb2cyWWFVS2hkR0NtZ3g5VVp1VzFvMVNoRkpZYzZEdkdZZTdOVHQ2ODlOb0wyY0V3ckIzTHA0cWMyczVuUUJScmJSUkVwR0dyYzhZaFJncHRKNjZFRWZRdHhnWlR2ZDRuajY2dmk4aWVqWXIxS0xoaXM3YnRyVlRpQXd4MzR5VWt0eUFMQU1kYzlEVFFMOXhRam5aMVFQWm1jVjJwdVFZejMya21uSEdFV0Z3Tk5lTGhmS2Z6elpxWXdCdTdDc3drQ3pycHpkWXhkOTVKVHd2RGJFNjFMTUFwanE2dTlQNlBUdlhOOUpGaVphUU1hZ0hwa2VCd3JZdjhQMUpZcFBmc1k4NHVyMmtYQzRhcTRaSGt3NXpEb1BwTDNVV240cTZUcHpGMjQ4R01LVExBejJtRE5NZWJYTjNUdW5QV3dudTlDaG92VlQ1TFN4cHVyU21WQW55OE5EaldQSktRbnJickRyMUUzNVlUdERIUjVrdWduNm90RHVtUGZpVm9XMWltdlhMTTJEam1tdnhDYUVVdXYiLCJuYmYiOjE1ODgyNjUzMzAsInBjeSI6IkFQUEVORCIsInByZiI6bnVsbCwic2NwIjoiLyJ9.S87ewBXBPb_P5BksTLu1lX-aCA_K4MqdMl9i8EuIpfbI7tuf9XK42NI0G-9Z7R3qV6zlQexQxKPlny-QE-Zz0W7fhvvameaNK0Eh-f1HRwJgGxHTMc3xmO9sFeOaCQjxe4Xon6V66en6XCM0ZQZm2J7fwVb9vYL2JKkWpOs1FuaREBTIddtP-gChHQOjLzCzkC8ti2hPhdF133R4WnHY7zKzMt4US2ufp_QOai5idga1YqIB2M1SkUUy_azqB_rUTZ9riMkcKiSJlnVj6gs1Zm4IK-3Xfq4DjtqOF8P4sckt-2-u_ZIICMBdQppjT30lXdeuDlJcU5JxL9tzhIwWMw\""
      --   JSON.eitherDecode ex `shouldBe` Right Bearer.tokenRSA2048

    describe "serialization" do
      itsProp' "serialize+deserialize is the identity function" \(bearer :: Bearer.Token) ->
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
