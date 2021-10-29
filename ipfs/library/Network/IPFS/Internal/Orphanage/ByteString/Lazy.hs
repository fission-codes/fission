{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.IPFS.Internal.Orphanage.ByteString.Lazy () where

import qualified RIO.ByteString.Lazy  as Lazy
import           Servant.API

import           Network.IPFS.Prelude

instance MimeRender PlainText Lazy.ByteString where
  mimeRender _proxy = identity

instance FromJSON Lazy.ByteString where
  parseJSON = withText "ByteString" (pure . Lazy.fromStrict . encodeUtf8)
