-- | Random values
module Fission.Random
  ( alphaNum
  , alphaNumSymbol
  , bsRandomLength
  ) where

import qualified Data.ByteString.Random as BS
import qualified RIO.ByteString         as BS

import           Fission.Internal.URL
import           Data.Text              as Text
import           Data.Word8

import Fission.Prelude

-- | Generate random alphanumeric @Text@ with a given length
alphaNum :: MonadIO m => Natural -> m Text
alphaNum len = randomTextBy isAsciiAlphaNum len

-- | Generate random alphanumeric symbol @Text@ with a given length
alphaNumSymbol :: MonadIO m => Natural -> m Text
alphaNumSymbol len = randomTextBy isURLCharacter len

-- | Generate random 'ByteString' with a given length
bsRandomLength :: MonadIO m => Natural -> m ByteString
bsRandomLength len =
  len
    |> (* 100)
    |> BS.random
    |> liftIO
    |> fmap (BS.take toTake)
  where
    toTake = fromIntegral len

-- | Check if a given character is alphanumeric
isAsciiAlphaNum :: Word8 -> Bool
isAsciiAlphaNum w = isAsciiUpper w
                 || isAsciiLower w
                 || isDigit w

randomTextBy :: MonadIO m => (Word8 -> Bool) -> Natural -> m Text
randomTextBy predicate len = go ""
  where
    go :: MonadIO m => Text -> m Text
    go memo = do
      byteString <- bsRandomLength (len * 2)

      let
        txt :: Text
        txt =
          byteString
            |> BS.filter predicate
            |> decodeUtf8Lenient
            |> Text.append memo

      if Text.length txt < fromIntegral len
        then
          go txt
        else
          txt
            |> Text.take (fromIntegral len)
            |> return
