module Fission.Test.CLI.PubSub.Secure.Session.Handshake (spec) where

import           Fission.Test.Prelude

spec :: Spec ()
spec =
  describe "Fission.CLI.PubSub.Secure.Session.Handshake" do
    describe "JSON serialization" do
      it "converts regular text" $
        toBase58Text "hello world" `shouldBe` Encoded @'Base58_BTC "StV1DL6CwTryKyV"

      it "converts short hexadecimal" $
        toBase58Text (Strict.pack ([0x0ed, 0x01] :: [Word8])) `shouldBe` Encoded @'Base58_BTC "K36"
