{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.BaseUrl () where

import RIO

import Control.Lens
import Data.Swagger
import Servant.Client

instance ToSchema BaseUrl where
  declareNamedSchema _ =
     return $ NamedSchema (Just "BaseUrl") $ mempty
            & type_   ?~ SwaggerString
            & example ?~ "QmW2WQi7j6c7UgJTarActp7tDNikE4B2qXtFCfLPdsgaTQ"
