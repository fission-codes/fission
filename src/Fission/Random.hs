module Fission.Random
  ( byteString
  , text
  ) where

import           RIO
import qualified RIO.ByteString as BS

import qualified Data.ByteString.Random as BS
import           Data.Word8

text :: Natural -> IO Text
text amount = decodeUtf8Lenient <$> byteString amount

byteString :: Natural -> IO ByteString
byteString amount = BS.filter isURL <$> BS.random amount

isURL :: Word8 -> Bool
isURL w = isAsciiUpper w
        || isAsciiLower w
        || isDigit w
        || any' urlSpecials w

any' :: [a -> Bool] -> a -> Bool
any' preds value = elem True (preds <*> [value])

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
