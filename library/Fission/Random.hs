-- | Random values
module Fission.Random
  ( alphaNum
  , alphaNumSymbol
  , bsRandomLength
  ) where

import qualified Data.ByteString.Random as BS
import qualified RIO.ByteString         as BS
import qualified Fission.Internal.URL   as URL
import           Data.Text              as T
import           Data.Word8

import Fission.Prelude

-- | Generate random alphanumeric @Text@ with a given length
alphaNum :: MonadIO m => Natural -> m Text
alphaNum maxLen = do
  byteString <- bsRandomLength (maxLen * 2)

  byteString
    |> BS.filter isAsciiAlphaNum
    |> decodeUtf8Lenient
    |> T.take (fromIntegral maxLen)
    |> return

-- | Generate random alphanumeric symbol @Text@ with a given length
alphaNumSymbol :: MonadIO m => Natural -> m Text
alphaNumSymbol maxLen = do
  byteString <- bsRandomLength (maxLen * 2)

  byteString
    |> BS.filter URL.isValidURLCharacter
    |> decodeUtf8Lenient
    |> T.take (fromIntegral maxLen)
    |> return

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
