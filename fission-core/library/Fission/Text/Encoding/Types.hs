module Fission.Text.Encoding.Types (Encoding (..)) where

import           RIO

-- | Encodings, intended for use at the type level
data Encoding
  = ASCII         -- ^ i.e. the default for 'ByteString'
  | Base58_BTC
  | Base64        -- ^ RFC 4648
  | Base64_URL    -- ^ RFC 4648
  | UTF8
  | UTF16         -- ^ i.e. the default for 'Text'
  | UnicodePoints -- ^ i.e. the default for 'String'
  deriving (Eq, Show)
