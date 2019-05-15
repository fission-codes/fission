{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Fission.File (Serialized (..)) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import qualified Data.ByteString.Builder as Builder
import           Servant

newtype Serialized = Serialized { unserialize :: Lazy.ByteString }

instance Display Serialized where
  display = Utf8Builder . Builder.lazyByteString . unserialize

instance MimeUnrender PlainText Serialized where
  mimeUnrender _proxy = Right . Serialized

instance MimeUnrender OctetStream Serialized where
  mimeUnrender _proxy = Right . Serialized
