module Fission.Web.Server.Swagger.Auth (fissionSecurity) where

import           Data.Swagger

import           Fission.Prelude

fissionSecurity :: SecurityScheme
fissionSecurity = SecurityScheme (SecuritySchemeApiKey keyParams) describe
  where
    keyParams :: ApiKeyParams
    keyParams = ApiKeyParams "Authorization" ApiKeyHeader

    describe :: Maybe Text
    describe = Just "A higher-order auth type that accepts both Basic Auth and Fission-formatted JWTs as described here: https://whitepaper.fission.codes/identity/authentication"
