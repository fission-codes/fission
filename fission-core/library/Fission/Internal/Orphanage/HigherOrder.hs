{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.HigherOrder () where

import           Fission.Prelude

import           Data.Swagger
import           Servant
import           Servant.Swagger

import           Fission.Web.Auth.Types
import           Fission.Web.Swagger.Auth

instance HasSwagger api => HasSwagger (HigherOrder :> api) where
  toSwagger _ = Proxy @api
             |> toSwagger
             |> securityDefinitions .~ SecurityDefinitions [("Fission Auth", fissionSecurity)]
