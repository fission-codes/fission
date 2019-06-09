module Fission.IPFS.Address
  ( Address ()
  , mkAddress
  ) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import qualified Data.ByteString.Builder as Builder
import           Servant

import qualified Fission.Internal.UTF8 as UTF8

newtype Address = Address { unaddress :: Lazy.ByteString }

instance Display Address where
  display = Utf8Builder . Builder.lazyByteString . unaddress

instance MimeRender PlainText Address where
  mimeRender _proxy = unaddress

instance MimeRender OctetStream Address where
  mimeRender _proxy = unaddress

mkAddress :: Lazy.ByteString -> Address
mkAddress = Address . UTF8.stripNewline
