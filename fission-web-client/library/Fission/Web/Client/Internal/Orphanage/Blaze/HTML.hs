{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Web.Client.Internal.Orphanage.Blaze.HTML () where

import           RIO
import qualified RIO.ByteString.Lazy     as Lazy

import           Data.Aeson

import           Servant.API
import qualified Servant.HTML.Blaze      as Blaze
import           Servant.Swagger.UI.Core

instance MimeUnrender Blaze.HTML (SwaggerUiHtml "docs" ("docs.json" :> Get '[JSON] Value)) where
  mimeUnrender _ lbs = Right . SwaggerUiHtml . decodeUtf8Lenient $ Lazy.toStrict lbs
