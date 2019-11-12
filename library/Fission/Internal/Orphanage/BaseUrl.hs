{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.BaseUrl () where

import Data.Swagger
import Servant.Client as Client

import Fission.Prelude

instance ToSchema BaseUrl where
  declareNamedSchema _ =
    mempty
      |> type_   ?~ SwaggerString
      |> example ?~ toJSON (BaseUrl Client.Https "runfission.com" 443 "")
      |> NamedSchema (Just "BaseUrl")
      |> pure
