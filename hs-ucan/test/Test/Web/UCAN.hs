module Test.Web.UCAN (spec) where

import qualified Data.Aeson                        as JSON
import qualified Data.ByteString.Lazy.Char8        as Lazy.Char8
import qualified RIO.ByteString.Lazy               as Lazy
import qualified RIO.Char                          as Char
import           Web.UCAN.Types

import           Test.Web.UCAN.Prelude

import qualified Test.Web.UCAN.Attenuation         as Attenuation
import qualified Test.Web.UCAN.DelegationSemantics as DelegationSemantics
import qualified Test.Web.UCAN.Example             as Ex


spec :: Spec
spec =
  describe "UCAN" do
    Attenuation.spec

    describe "serialization" do
      itsProp' "serialized is isomorphic to ADT" \(ucan :: UCAN () Ex.Resource Ex.Ability) ->
        JSON.eitherDecode (JSON.encode ucan) `shouldBe` Right ucan

      describe "format" do
        itsProp' "contains exactly two '.'s" \(ucan :: UCAN () Ex.Resource Ex.Ability) ->
          ucan
            & JSON.encode
            & Lazy.count (fromIntegral $ Char.ord '.')
            & shouldBe 2

        itsProp' "contains only valid base64 URL characters" \(ucan :: UCAN () Ex.Resource Ex.Ability) ->
          let
            encoded = JSON.encode ucan
          in
            encoded
              & Lazy.take (Lazy.length encoded - 2)
              & Lazy.drop 2
              & Lazy.filter (not . isValidChar)
              & shouldBe mempty

    describe "DelegationSemantics" do
      describe "Resource" do
        DelegationSemantics.itHasPartialOrderProperties @Ex.Resource

      describe "Potency" do
        DelegationSemantics.itHasPartialOrderProperties @Ex.Ability

      describe "Maybe _" do
        DelegationSemantics.itHasPartialOrderProperties @(Maybe Ex.Ability)

      describe "(Resource, Ability)" do
        DelegationSemantics.itHasPartialOrderProperties @(Ex.Resource, Ex.Ability)



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
