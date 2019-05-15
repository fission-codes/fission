{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Fission.IPFS.Address
  ( Address ()
  , mkAddress
  ) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import qualified Data.ByteString.Builder as Builder
import           Servant

newtype Address = Address { unaddress :: Lazy.ByteString }

instance Display Address where
  display = Utf8Builder . Builder.lazyByteString . unaddress

instance MimeRender PlainText Address where
  mimeRender _proxy = unaddress

instance MimeRender OctetStream Address where
  mimeRender _proxy = unaddress

mkAddress :: Lazy.ByteString -> Address
mkAddress = Address . stripNewline

stripNewline :: Lazy.ByteString -> Lazy.ByteString
stripNewline bs = maybe bs id $ Lazy.stripSuffix "\n" bs
