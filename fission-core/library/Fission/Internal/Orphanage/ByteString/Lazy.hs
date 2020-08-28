{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.ByteString.Lazy () where

import qualified RIO.ByteString.Lazy as Lazy
import           Servant

import           Fission.Prelude

instance MimeRender PlainText Lazy.ByteString where
  mimeRender _proxy = identity

instance FromJSON ByteString where
  parseJSON = withText "ByteString" (pure . encodeUtf8)
