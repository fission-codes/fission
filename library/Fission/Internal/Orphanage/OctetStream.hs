{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.OctetStream () where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import Servant

instance MimeRender OctetStream a => MimeRender OctetStream [a] where
  mimeRender proxy values = "["<> meat <>"]"
    where
      meat :: Lazy.ByteString
      meat =  Lazy.intercalate "," $ mimeRender proxy <$> values
