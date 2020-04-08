module Test.Fission.Web.Auth.Bearer (tests) where

import qualified System.IO.Unsafe as Unsafe

import qualified Data.Aeson                 as JSON
import qualified Data.ByteString.Lazy.Char8 as Lazy.Char8
import qualified RIO.ByteString.Lazy        as Lazy

import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer
import           Fission.Web.Auth.JWT

import           Test.Fission.Prelude

tests :: SpecWith ()
tests =
  describe "Bearer Token" do
    describe "serialization" do
      itsProp' "serialize+deserialize is the identity function" \(bearer :: Bearer.Token) ->
        JSON.eitherDecode (JSON.encode bearer) `shouldBe` Right bearer

      describe "serialized" do
        itsProp' "has no quotes" \(bearer :: Bearer.Token) ->
          bearer
            |> JSON.encode
            |> Lazy.count (fromIntegral $ ord '"')
            |> shouldBe 0

        -- itsProp' "contains only valid base64 URL characters" \(jwt :: JWT) ->
        --   let
        --     encoded = JSON.encode jwt
        --   in
        --     encoded
        --       |> Lazy.take (Lazy.length encoded - 2)
        --       |> Lazy.drop 2
        --       |> Lazy.filter (not . isValidChar)
        --       |> shouldBe mempty
