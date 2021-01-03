{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.ByteString.Lazy () where

import qualified RIO.ByteString.Lazy as Lazy
import           Servant.API

import           Fission.Prelude

instance MimeRender PlainText Lazy.ByteString where
  mimeRender _proxy = identity

instance FromJSON Lazy.ByteString where
  parseJSON = withText "Lazy.ByteString" (pure . Lazy.fromStrict . encodeUtf8)
