-- | Random values
module Fission.Random
  ( alphaNum
  , alphaNumSymbol
  ) where

import qualified Data.ByteString.Random as BS
import qualified RIO.ByteString         as BS
import qualified Fission.Internal.URL   as URL
import           Data.Text              as T
import           Data.Word8

import Fission.Prelude

-- | Generate random AlphaNumeric 'Text' with a given length
alphaNum :: MonadIO m => Natural -> m Text
alphaNum len = decodeUtf8Lenient . BS.filter isAsciiAlphaNum <$> bsRandomLength len

-- | Generate random AlphaNumericSymbol 'Text' with a given length
alphaNumSymbol :: MonadIO m => Natural -> m Text
alphaNumSymbol len = decodeUtf8Lenient . BS.filter URL.isValidURLCharacter <$> bsRandomLength len

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

-- | Check if a given character is AlphaNumeric
isAsciiAlphaNum :: Word8 -> Bool
isAsciiAlphaNum w = isAsciiUpper w
                 || isAsciiLower w
                 || isDigit w
