{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web.Heroku where

import RIO
import RIO.Char (toUpper)

import Control.Lens (makeLenses)

import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.TH

import Data.Proxy
import Data.UUID

import Data.Time.Clock
import GHC.Generics
import Time.Types

import           Network.HTTP.Client      (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client
import qualified Servant.Client.Streaming as Client.Stream
import           Servant.Types.SourceT    (foreach)

import Fission.Web.Server

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

data OAuthGrant = OAuthGrant
  { _code      :: UUID -- ^ may be exchanged for an access_token that is scoped to the resource being provisioned for use in the Platform API
  , _expiresAt :: UTCTime -- ^ the time at which the grant expires (ensure you have exchanged the code before this time) defaults to 5 minutes from now
  , _type      :: Text -- ^ the oauth grant type
  } deriving (Show, Eq)

$(deriveJSON (defaultOptions { fieldLabelModifier = snakeCase }) ''OAuthGrant)

data Test = Test
  { _fooThere :: Text
  , _bar      :: Int
  } deriving (Show, Eq)

makeLenses ''Test
$(deriveJSON (defaultOptions { fieldLabelModifier = drop 1 . snakeCase }) ''Test)

data Region
  = California
  | Dublin
  | Frankfurt
  | Oregon
  | Singapore
  | Sydney
  | Tokyo
  | Virginia
  | Other Text -- ^ Being very lenient for now
  deriving (Show, Eq)

instance ToJSON Region where
  toJSON = \case
    California -> String "amazon-web-services::us-west-1"
    Dublin     -> String "amazon-web-services::eu-west-1"
    Frankfurt  -> String "amazon-web-services::eu-central-1"
    Oregon     -> String "amazon-web-services::us-west-2"
    Singapore  -> String "amazon-web-services::ap-southeast-1"
    Sydney     -> String "amazon-web-services::ap-southeast-2"
    Tokyo      -> String "amazon-web-services::ap-northeast-1"
    Virginia   -> String "amazon-web-services::us-east-1"
    Other txt  -> String txt

instance FromJSON Region where
  parseJSON = withText "Region" $ \case
    "amazon-web-services::us-west-1"      -> return California
    "amazon-web-services::eu-west-1"      -> return Dublin
    "amazon-web-services::eu-central-1"   -> return Frankfurt
    "amazon-web-services::us-west-2"      -> return Oregon
    "amazon-web-services::ap-southeast-1" -> return Singapore
    "amazon-web-services::ap-southeast-2" -> return Sydney
    "amazon-web-services::ap-northeast-1" -> return Tokyo
    "amazon-web-services::us-east-1"      -> return Virginia
    other                                 -> return $ Other other

data Plan = Free | Paid
  deriving (Show, Eq)

$(deriveJSON defaultOptions ''Plan)

data ProvisionReq = ProvisionReq
  { _callbackUrl :: Text             -- ^ The URL which should be used to retrieve updated information about the add-on and the app which owns it.
  , _name        :: Text             -- ^ Logical name of the resource being provisioned.
  , _oauthGrant  :: Maybe OAuthGrant -- ^ OAuth object details (nullable).
  , _plan        :: Plan             -- ^ Name of the plan to provision (e.g. `basic`).
  , _region      :: Region           -- ^ Physical hosting region of the requesting client.
  , _uuid        :: UUID             -- ^ The unique identifier Heroku uses for the installed add-on. It corresponds with the id field in the Heroku Platform API.
  } deriving (Show, Eq)

$(deriveJSON (defaultOptions { fieldLabelModifier = snakeCase }) ''ProvisionReq)

data HerokuConfig = HerokuConfig { _fissionApiUrl :: Text }
  deriving (Show, Eq)

$(deriveJSON
    (defaultOptions { fieldLabelModifier = (fmap toUpper) . snakeCase })
    ''HerokuConfig)

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

data ProvisionResp = ProvisionResp
  { _id      :: UUID
  , _message :: Text
  , _config  :: HerokuConfig
  } deriving (Show, Eq)

$(deriveJSON defaultOptions ''ProvisionResp)

type ProvisionAPI = ReqBody '[JSON] ProvisionReq
      :> Post    '[JSON] ProvisionResp

-----------------------

type APIA = "heroku" :> Capture "x" Int :> "resources" :> Get '[JSON] Text
type APIB = "foo" :> Get '[JSON] Text

type API' = APIA :<|> APIB

api :: Proxy API'
api = Proxy

server :: RIOServer cfg API'
server = heroku :<|> foo

heroku :: RIOServer cfg APIA
heroku _x = return "hi"

foo :: RIOServer cfg APIB
foo = return "hi"
