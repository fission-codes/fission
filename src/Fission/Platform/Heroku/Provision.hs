{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Fission.Platform.Heroku.Provision
  ( Request (..)
  , callbackUrl
  , name
  -- , oauthGrant
  , plan
  , region
  , uuid
  , Provision (..)
  , id
  , config
  , message
  ) where

import RIO hiding (id)

import Control.Lens (makeLenses)

import Data.Aeson.TH
import Data.UUID

import Database.Selda

import           Fission.Internal.JSON
import qualified Fission.Plan          as Plan

import Fission.Platform.Heroku.Region     (Region)
import Fission.Platform.Heroku.UserConfig (UserConfig)
import Fission.User                       (User)

data Request = Request
  { _callbackUrl :: Text       -- ^ The URL which should be used to retrieve updated information about the add-on and the app which owns it.
  , _name        :: Text       -- ^ Logical name of the resource being provisioned.
  -- , _oauthGrant :: Maybe Text -- OAuthGrant -- ^ OAuth object details (nullable).
  , _plan        :: Plan.Tier  -- ^ Name of the plan to provision (e.g. `basic`).
  , _region      :: Region     -- ^ Physical hosting region of the requesting client.
  , _uuid        :: UUID       -- ^ The unique identifier Heroku uses for the installed add-on. It corresponds with the id field in the Heroku Platform API.
  } deriving (Show, Eq)

makeLenses ''Request
$(deriveJSON lens_snake_case ''Request)

{-
Response Parameters

In addition to the id parameter as documented under asynchronous provisioning,
you should include a config parameter along with any other optional parameters
of your choosing.

Name: config
Type: object

Description:
Configuration variables to be set as environment variables on any applications
that use this add-on resource. All environment variables that you send should
have the same prefix: your add-on’s name, capitalized. However,
within the context of a Heroku app, the prefix may be different for a variety
of reasons. For example, if your add-on is named fast-db and you are setting
FAST_DB_URL, the variable name on the application will default to FAST_DB_URL
but would be PRIMARY_DB_URL if the user added the add-on with
the prefix PRIMARY_DB.

Example:
HTTP/1.1 200 OK
{ "MYADDON_URL": "http://myaddon.com/52e82f5d73" }
-}

data Provision = Provision
  { _id      :: ID User
  , _config  :: UserConfig
  , _message :: Text
  } deriving (Show, Eq)

makeLenses ''Provision
$(deriveJSON lens_snake_case ''Provision)

-- data Error = ...
