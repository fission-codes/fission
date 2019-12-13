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
import           GHC.Natural

import Fission.Prelude

-- | Generate random AlphaNumeric 'Text' with a given length
alphaNum :: Natural -> IO Text
alphaNum len = decodeUtf8Lenient . BS.filter isAsciiAlphaNum <$> bsRandomLength len

-- | Generate random AlphaNumericSymbol 'Text' with a given length
alphaNumSymbol :: Natural -> IO Text
alphaNumSymbol len = decodeUtf8Lenient . BS.filter URL.isValidURLCharacter <$> bsRandomLength len

-- | Generate random 'ByteString' with a given length
bsRandomLength :: Natural -> IO ByteString
bsRandomLength len = BS.take (naturalToInt len) <$> BS.random (len * 100)

-- | Check if a given character is AlphaNumeric
isAsciiAlphaNum :: Word8 -> Bool
isAsciiAlphaNum w = isAsciiUpper w
                 || isAsciiLower w
                 || isDigit w
