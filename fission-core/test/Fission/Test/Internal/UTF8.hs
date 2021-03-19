module Fission.Test.Internal.UTF8 (spec) where

import qualified RIO.ByteString        as Strict
import qualified RIO.ByteString.Lazy   as Lazy
import qualified RIO.Text              as Text

import           Fission.Internal.UTF8

import           Fission.Test.Prelude

spec :: Spec
spec =
  describe "Fission.Internal.UTF8" do
    describe "toBase58Text" do
      it "converts regular text" $
        toBase58Text "hello world" `shouldBe` "StV1DL6CwTryKyV"

      it "converts short hexadecimal" $
        toBase58Text (Strict.pack ([0x0ed, 0x01] :: [Word8])) `shouldBe` "K36"

      it "converts longer hexadecimal" do
        [0xed, 0x01, 0x01, 0x23, 0x45, 0x67]
          |> Strict.pack
          |> toBase58Text
          |> shouldBe "332DkaEge"

      describe "must concatenation through base2" do
        it "fails with a naive concatenation" $
          toBase58Text "hello " <> toBase58Text "world"
            `shouldNotBe` toBase58Text "hello world"

      describe "stripNewline" do
        it "removes the newline from a string with one" $
          stripNewline ";)\n" `shouldBe` ";)"

        it "removes only the first newline" $
          stripNewline "<>\n\n" `shouldBe` "<>\n"

        itsProp' "adding and stripping a newline is a noop" \bs ->
          stripNewline (Lazy.append bs "\n") `shouldBe` bs

        itsProp' "only one newline is removed" \bs ->
          stripNewline (Lazy.append bs "\n\n") `shouldBe` bs <> "\n"

      describe "textShow" do
        it "is equivalent to Show, but as Text" $
          textShow (1 :: Int) `shouldBe` "1"

      describe "stripNBS" do
        it "drops from the front and back" $
          stripN 3 "aaabccc" `shouldBe` "b"

        itsProp' "is a noop when stripping 0" \txt ->
          stripN 0 txt `shouldBe` txt

        itsProp' "drops from both front and back" \n ->
          stripN n (Text.center (3 + fromIntegral n * 2) '_' "o.O") `shouldBe` "o.O"

      describe "wrapIn" do
        itsProp' "starts with the wrapping text" \s ->
          Text.take 1 (wrapIn "|" s) `shouldBe` "|"

        itsProp' "ends with the wrapping text" \s ->
          Text.takeEnd 1 (wrapIn "|" s) `shouldBe` "|"

        itsProp' "increases in length by double the wrapper" \(base, wrapper) ->
          Text.length (wrapIn wrapper base)
            `shouldBe` (Text.length base) + (2 * Text.length wrapper)
