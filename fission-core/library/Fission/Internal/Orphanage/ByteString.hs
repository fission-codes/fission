{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.ByteString () where

import           Fission.Prelude

instance FromJSON ByteString where
  parseJSON = withText "ByteString" (pure . encodeUtf8)
