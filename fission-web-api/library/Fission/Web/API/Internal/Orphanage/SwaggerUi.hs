{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Web.API.Internal.Orphanage.SwaggerUi () where

import           Data.Swagger
import           Servant.Swagger.UI.Core

import           Fission.Prelude

instance ToSchema (SwaggerUiHtml "docs" api) where
  declareNamedSchema _ =
    mempty
      |> type_ ?~ SwaggerInteger
      |> NamedSchema (Just "HTML Docs")
      |> pure
