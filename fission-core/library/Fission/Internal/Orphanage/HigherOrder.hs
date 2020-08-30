{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.HigherOrder () where

import Fission.Prelude

import Servant
import Servant.Swagger
import Data.Swagger

import Fission.Web.Auth.Types
import Fission.Web.Swagger.Auth

instance HasSwagger api => HasSwagger (HigherOrder :> api) where
  toSwagger _ = Proxy @api
             |> toSwagger
             |> securityDefinitions .~ [("Fission Auth", fissionSecurity)]
