module Test.Fission.Web.Auth.JWT (tests) where

import qualified System.IO.Unsafe as Unsafe

import qualified Data.Aeson                 as JSON
import qualified Data.ByteString.Lazy.Char8 as Lazy.Char8

import qualified RIO.ByteString.Lazy as Lazy
import           RIO.Char (ord)

import           Fission.Web.Auth.JWT

import           Test.Fission.Prelude

tests :: SpecWith ()
tests =
  describe "Fission.Web.Auth.JWT" do
    it "foo"
      let
        jwt :: JWT = Unsafe.unsafePerformIO (generate arbitrary)
      in
        JSON.eitherDecode (JSON.encode jwt) `shouldBe` Right jwt -- Just jwt { claims = clms }

    it "bar"
      let
        jwt :: JWT = Unsafe.unsafePerformIO (generate arbitrary)
        encoded =
          JSON.encode jwt

        jwt' =
          encoded
            |> Lazy.take (Lazy.length encoded - 2)
            |> Lazy.drop 2
      in
        Lazy.filter (not . isValidChar) jwt' `shouldBe` ""

    describe "serialization" do
      itsProp' "serialize+deserialize is the identity function" \(jwt :: JWT) ->
        JSON.decode' (JSON.encode jwt) `shouldBe` Just jwt

      describe "format" do
        itsProp "contains exactly two '.'s"  100 \(jwt :: JWT) ->
          Lazy.count (fromIntegral $ ord '.') (JSON.encode jwt) `shouldBe` 2
         
        itsProp' "contains only valid base64 URL characters" \(jwt :: JWT) ->
          let
            encoded =
              JSON.encode jwt
             
            jwt' =
              encoded
                |> Lazy.take (Lazy.length encoded - 2)
                |> Lazy.drop 2
          in
            Lazy.filter (not . isValidChar) jwt' `shouldBe` ""
 
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
