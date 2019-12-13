-- | Random values
module Fission.Random
  ( alphaNum
  , alphaNumSymbol
  ) where

import qualified Data.ByteString.Random as BS
import qualified RIO.ByteString         as BS
import qualified Fission.Internal.URL   as URL
import qualified Data.Text            as T
import           Data.Word8

import Fission.Prelude

-- | Generate random AlphaNumeric 'Text'
alphaNum :: Natural -> IO Text
alphaNum amount = decodeUtf8Lenient . BS.filter isAsciiAlphaNum <$> BS.random amount

-- | Generate random AlphaNumericSymbol 'Text'
alphaNumSymbol :: Natural -> IO Text
alphaNumSymbol amount = decodeUtf8Lenient . BS.filter URL.isValidURLCharacter <$> BS.random amount

isAsciiAlphaNum w = isAsciiUpper w
                  || isAsciiLower w
                  || isDigit w