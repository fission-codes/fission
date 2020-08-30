{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.OctetStream () where

import           Data.List.NonEmpty  as NonEmpty
import qualified RIO.ByteString.Lazy as Lazy
import           Servant

import Fission.Prelude

instance MimeRender OctetStream a => MimeRender OctetStream [a] where
  mimeRender proxy values = "["<> meat <>"]"
    where
      meat :: Lazy.ByteString
      meat =
        values
          |> fmap (mimeRender proxy)
          |> Lazy.intercalate ","

instance MimeRender OctetStream a => MimeRender OctetStream (NonEmpty a) where
  mimeRender proxy values =
    values
      |> NonEmpty.toList
      |> mimeRender proxy
