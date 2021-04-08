module Fission.Text.Encoding.Types (Encoding (..)) where

import RIO

-- | Encodings, intended for use at the type level
data Encoding
  = Binary
  | Octal -- ^ i.e. the default for 'String' and 'ByteString'
  | Hexadecimal
  | Base58_BTC
  | Base64
  | Base64_URL
  | UTF8 -- ^ i.e. the default for 'Text'
  deriving (Eq, Show)
