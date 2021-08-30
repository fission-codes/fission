{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Web.API.Auth.Types
  ( HerokuAddOnAPI
  , RegisterDID
  , HigherOrder
  ) where

import           Data.Swagger

import           Servant.API
import           Servant.Client.Core
import           Servant.Ekg                       (HasEndpoint (..))
import           Servant.Swagger

import           Fission.Web.Auth.Token.Types

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Heroku.Auth.Types as Heroku

-- | Authorization check for the Heroku Addon API
type HerokuAddOnAPI = BasicAuth "heroku add-on api" Heroku.Auth

-- | Authorization check to return encoded DID for registering new users
type RegisterDID = AuthProtect "register-did"
type instance AuthClientData (AuthProtect "register-did") = Token

type HigherOrder = AuthProtect "higher-order"
type instance AuthClientData (AuthProtect "higher-order") = Token

instance HasSwagger api => HasSwagger (RegisterDID :> api) where
  toSwagger _ =
    Proxy @api
      |> toSwagger
      |> securityDefinitions .~ SecurityDefinitions [("User-originated DID Registration", security')]

instance HasEndpoint sub => HasEndpoint (RegisterDID :> sub) where
  getEndpoint        _ = getEndpoint        $ Proxy @sub
  enumerateEndpoints _ = enumerateEndpoints $ Proxy @sub

instance HasSwagger api => HasSwagger (HerokuAddOnAPI :> api) where
  toSwagger _ =
    Proxy @api
      |> toSwagger
      |> securityDefinitions .~ SecurityDefinitions [("HerokuAddOn Auth", security')]

instance HasSwagger api => HasSwagger (HigherOrder :> api) where
  toSwagger _ =
    Proxy @api
      |> toSwagger
      |> securityDefinitions .~ SecurityDefinitions [("Fission Auth", security')]

instance HasEndpoint sub => HasEndpoint (AuthProtect "higher-order" :> sub) where
  getEndpoint        _ = getEndpoint        $ Proxy @sub
  enumerateEndpoints _ = enumerateEndpoints $ Proxy @sub

security' :: SecurityScheme
security' = SecurityScheme (SecuritySchemeApiKey keyParams) describe
  where
    keyParams :: ApiKeyParams
    keyParams = ApiKeyParams "Authorization" ApiKeyHeader

    describe :: Maybe Text
    describe = Just "A higher-order auth type that accepts both Basic Auth and Fission-formatted JWTs as described here: https://whitepaper.fission.codes/identity/authentication"
