{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Web.Server.Internal.Orphanage.BasicAuth () where

import           Data.Swagger
import           Servant.API
import           Servant.Swagger

import           Fission.Prelude

instance HasSwagger api => HasSwagger (BasicAuth x r :> api) where
  toSwagger _ = Proxy @api
             |> toSwagger
             |> securityDefinitions .~ SecurityDefinitions [("basic", SecurityScheme SecuritySchemeBasic Nothing)]
