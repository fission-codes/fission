module Fission.Internal.Base64
  ( toB64ByteString
  , toByteString
  ) where

import qualified RIO.ByteString         as BS
import qualified Data.ByteString.Base64 as BS64
import qualified Data.ByteArray         as BA

import           Fission.Prelude

-- was pack ... shit... actually it was toBase64
toB64ByteString :: BA.ByteArrayAccess a => a -> ByteString
toB64ByteString = BS64.encode . toByteString

-- was unpack
toByteString :: BA.ByteArrayAccess a => a -> ByteString
toByteString = BS.pack . BA.unpack
