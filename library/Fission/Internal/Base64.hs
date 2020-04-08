module Fission.Internal.Base64
  ( toB64ByteString
  , toByteString
  ) where

import qualified Data.ByteString.Base64 as BS64
import qualified Data.ByteArray         as BA

import qualified RIO.ByteString as BS

import           Fission.Prelude

toB64ByteString :: BA.ByteArrayAccess a => a -> ByteString
toB64ByteString = BS64.encode . toByteString

toByteString :: BA.ByteArrayAccess a => a -> ByteString
toByteString = BS.pack . BA.unpack
