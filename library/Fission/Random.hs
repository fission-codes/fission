-- | Random values
module Fission.Random
  ( byteString
  , text
  ) where

import qualified Data.ByteString.Random as BS
import qualified RIO.ByteString         as BS
import qualified Fission.Internal.URL   as URL

import Fission.Prelude

-- | Generate random 'Text'
text :: Natural -> IO Text
text amount = decodeUtf8Lenient <$> byteString amount

-- | Generate a random 'ByteString'
byteString :: Natural -> IO ByteString
byteString amount = BS.filter URL.isURL <$> BS.random amount
