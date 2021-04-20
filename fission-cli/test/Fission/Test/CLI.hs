module Fission.Test.CLI (spec) where

import qualified RIO.ByteString           as Strict
import qualified RIO.ByteString.Lazy      as Lazy
import qualified RIO.Text                 as Text

import           Fission.Internal.UTF8

import           Fission.Test.CLI.Prelude

spec :: Spec
spec =
  describe "Fission.Internal.UTF8" do
    describe "toBase58Text" do
      it "converts regular text" $
        toBase58Text "hello world" `shouldBe` Encoded @'Base58_BTC "StV1DL6CwTryKyV"

      it "converts short hexadecimal" $
        toBase58Text (Strict.pack ([0x0ed, 0x01] :: [Word8])) `shouldBe` Encoded @'Base58_BTC "K36"
