{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.ByteString.Strict () where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import Data.Aeson
import Servant

instance MimeRender PlainText Lazy.ByteString where
  mimeRender _proxy = id

instance FromJSON ByteString where
  parseJSON = withText "ByteString.Strict" (pure . encodeUtf8)
