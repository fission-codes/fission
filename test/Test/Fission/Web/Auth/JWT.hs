module Test.Fission.Web.Auth.JWT where

import qualified Data.Aeson                 as JSON
import qualified Data.ByteString.Lazy.Char8 as Lazy.Char8
import qualified RIO.ByteString.Lazy        as Lazy

import           Fission.Web.Auth.JWT

import           Test.Fission.Prelude

tests :: IO TestTree
tests =
  testSpec "Fission.Web.Auth.JWT" do
    describe "serialization" do
      describe "format" do
        itsProp' "contains only valid JWT caharacters" \(jwt :: JWT) ->
          Lazy.filter (not . containedIn) (JSON.encode jwt) == ""
        -- [a-zA-Z0-9-_.]+

        -- itsProp' "contains exactly two '.'s" \

containedIn w8 = Lazy.elem w8 validJWTChars

validJWTChars :: Lazy.ByteString
validJWTChars = Lazy.Char8.pack chars
  where
    chars :: [Char]
    chars =
           ['a'..'z']
        <> ['A'..'Z']
        <> ['0'..'9']
        <> ['_', '-', '.']
