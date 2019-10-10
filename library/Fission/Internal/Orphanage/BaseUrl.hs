{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.BaseUrl () where

import RIO

import Control.Lens
import Data.Aeson
import Data.Swagger
import Servant.Client as Client

instance ToSchema BaseUrl where
  declareNamedSchema _ =
     return $ NamedSchema (Just "BaseUrl") $ mempty
            & type_   ?~ SwaggerString
            & example ?~ toJSON (BaseUrl Client.Https "runfission.com" 443 "")
