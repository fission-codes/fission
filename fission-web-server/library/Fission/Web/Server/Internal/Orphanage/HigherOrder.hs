{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Web.Server.Internal.Orphanage.HigherOrder () where

import           Data.Swagger

import           Servant
import           Servant.Ekg
import           Servant.Swagger

import           Fission.Prelude

import           Fission.Web.API.Auth.Types
import           Fission.Web.Server.Swagger.Auth

instance HasSwagger api => HasSwagger (HigherOrder :> api) where
  toSwagger _ = Proxy @api
             |> toSwagger
             |> securityDefinitions .~ SecurityDefinitions [("Fission Auth", fissionSecurity)]

instance HasEndpoint sub => HasEndpoint (AuthProtect "higher-order" :> sub) where
  getEndpoint        _ = getEndpoint        $ Proxy @sub
  enumerateEndpoints _ = enumerateEndpoints $ Proxy @sub
