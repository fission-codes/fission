{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.BasicAuth () where

import Data.Swagger
import Servant
import Servant.Swagger

import Fission.Prelude

instance HasSwagger api => HasSwagger (BasicAuth x r :> api) where
  toSwagger _ = Proxy @api
             |> toSwagger
             |> securityDefinitions .~ [("basic", SecurityScheme SecuritySchemeBasic Nothing)]
