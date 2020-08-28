{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.PlainText () where

import           Data.List.NonEmpty  as NonEmpty
import qualified RIO.ByteString.Lazy as Lazy
import           Servant

import           Fission.Prelude

instance MimeRender PlainText a => MimeRender PlainText [a] where
  mimeRender proxy values = "["<> meat <>"]"
    where
      meat :: Lazy.ByteString
      meat =
        values
          |> fmap (mimeRender proxy)
          |> Lazy.intercalate ","

instance MimeRender PlainText a => MimeRender PlainText (NonEmpty a) where
  mimeRender proxy values =
    values
      |> NonEmpty.toList
      |> mimeRender proxy
