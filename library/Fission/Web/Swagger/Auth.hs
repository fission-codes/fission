module Fission.Web.Swagger.Auth (fissionSecurity) where

import Fission.Prelude
import Data.Swagger

fissionSecurity :: SecurityScheme
fissionSecurity = SecurityScheme (SecuritySchemeApiKey keyParams) describe
  where
    keyParams :: ApiKeyParams
    keyParams = ApiKeyParams "Authorization" ApiKeyHeader

    describe :: Maybe Text
    describe = Just "A higher-order auth type that accepts both basic auth and fission-formatted jwts as described here: https://whitepaper.fission.codes/identity/authentication"
