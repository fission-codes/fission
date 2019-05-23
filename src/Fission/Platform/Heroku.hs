{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Fission.Platform.Heroku where

import RIO

-- import RIO.Char (toUpper)

-- import Control.Lens (makeLenses)

-- import Data.Aeson
-- import Data.Aeson.Casing
-- import Data.Aeson.TH

-- import Data.Time.Clock
-- import Data.UUID

-- type API = "heroku" :> "resources" :>

{-
Example:

POST /heroku/resources HTTP/1.1
Host: addon-slug.herokuapp.com:443
Authorization: Basic YWRkb24tc2x1ZzpzdXBlci1zZWNyZXQ=
Content-Type: application/json
Accept: application/vnd.heroku-addons+json; version=3
{
  "callback_url": "https://api.heroku.com/addons/01234567-89ab-cdef-0123-456789abcdef",
  "name": "acme-inc-primary-database",
  "oauth_grant": {
    "code": "01234567-89ab-cdef-0123-456789abcdef",
    "expires_at": "2016-03-03T18:01:31-0800",
    "type": "authorization_code"
  },
  "options": { "foo" : "bar", "baz" : "true" },
  "plan": "basic",
  "region": "amazon-web-services::us-east-1",
  "uuid": "01234567-89ab-cdef-0123-456789abcdef",
  "log_input_url": "https://token:t.01234567-89ab-cdef-0123-456789abcdef@1.us.logplex.io/logs",
  "log_drain_token": "d.01234567-89ab-cdef-0123-456789abcdef"
}

-}

-- data OAuthGrant = OAuthGrant
--   { _code      :: UUID -- ^ may be exchanged for an access_token that is scoped to the resource being provisioned for use in the Platform API
--   , _expiresAt :: UTCTime -- ^ the time at which the grant expires (ensure you have exchanged the code before this time) defaults to 5 minutes from now
--   , _type      :: Text -- ^ the oauth grant type
--   } deriving (Show, Eq)

-- makeLenses ''OAuthGrant
--  $(deriveJSON (defaultOptions snake_case ''OAuthGrant)
