{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.OctetStream () where

import qualified RIO.ByteString.Lazy as Lazy
import           Servant

import Fission.Prelude

instance MimeRender OctetStream a => MimeRender OctetStream [a] where
  mimeRender proxy values = "["<> meat <>"]"
    where
      meat :: Lazy.ByteString
      meat = values
          |> fmap (mimeRender proxy)
          |> Lazy.intercalate ","
