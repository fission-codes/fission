-- | Random values
module Fission.Random
  ( byteString
  , text
  ) where

import           RIO
import qualified RIO.ByteString as BS

import qualified Data.ByteString.Random as BS
import           Data.Word8

import Fission.Internal.Bool (anyX)

-- | Generate random 'Text'
text :: Natural -> IO Text
text amount = decodeUtf8Lenient <$> byteString amount

-- | Generate a random 'ByteString'
byteString :: Natural -> IO ByteString
byteString amount = BS.filter isURL <$> BS.random amount

-- | Check that a byte represents a valid URL character
isURL :: Word8 -> Bool
isURL w = isAsciiUpper w
        || isAsciiLower w
        || isDigit w
        || anyX urlSpecials w

-- | List of URL special character checks
urlSpecials :: [Word8 -> Bool]
urlSpecials =
  fmap (==)
    [ _asterisk
    , _comma
    , _dollar
    , _exclam
    , _hyphen
    , _parenleft
    , _parenright
    , _period
    , _plus
    , _underscore
    ]
