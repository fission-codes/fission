{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.ByteString () where

-- import           Servant.API

import           Fission.Prelude

-- instance MimeRender PlainText ByteString where
  -- mimeRender _proxy = identity

instance FromJSON ByteString where
  parseJSON = withText "ByteString" (pure . encodeUtf8)
