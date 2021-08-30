{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Web.API.Internal.Orphanage.Aeson () where

import qualified Data.Aeson      as JSON
import           Data.Swagger

import           Fission.Prelude

instance ToSchema JSON.Value where
  declareNamedSchema _ =
    mempty
      |> type_ ?~ SwaggerInteger
      |> NamedSchema (Just "JSON")
      |> pure
