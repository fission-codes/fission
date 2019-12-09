module Fission.Platform.Heroku.Provision
  ( Request (..)
  , Provision (..)
  ) where

import           Data.UUID as UUID
import           Data.Swagger hiding (name)
import           Database.Selda
import qualified Servant.Client as Client

import           Fission.Prelude
import           Fission.Internal.Orphanage.ID ()

import qualified Fission.Plan.Types                 as Plan
import qualified Fission.Platform.Heroku.Types      as Heroku
import qualified Fission.User.Provision.Types       as User
import           Fission.Security.Types
import           Fission.User                       (User)
import           Fission.IPFS.Types                 as IPFS
import           Fission.IPFS.Peer (fission)

data Request = Request
  { callbackUrl :: Text          -- ^ The URL which should be used to retrieve updated information about the add-on and the app which owns it.
  , name        :: Text          -- ^ Logical name of the resource being provisioned.
  -- , oauthGrant :: Maybe Text -- OAuthGrant -- ^ OAuth object details (nullable).
  , plan        :: Plan.Tier     -- ^ Name of the plan to provision (e.g. `basic`).
  , region      :: Heroku.Region -- ^ Physical hosting region of the requesting client.
  , uuid        :: UUID          -- ^ The unique identifier Heroku uses for the installed add-on. It corresponds with the id field in the Heroku Platform API.
  } deriving ( Eq
             , Show
             )

instance ToJSON Request where
  toJSON Request {..} = object
    [ "callback_url" .= callbackUrl
    , "name"         .= name
    , "plan"         .= plan
    , "region"       .= region
    , "uuid"         .= uuid
    ]

instance FromJSON Request where
  parseJSON = withObject "Heroku.Request" \obj -> do
    callbackUrl <- obj .: "callback_url"
    name        <- obj .: "name"
    plan        <- obj .: "plan"
    region      <- obj .: "region"
    uuid        <- obj .: "uuid"
    return Request {..}

instance ToSchema Request where
  declareNamedSchema _ = do
    planSchema   <- declareSchemaRef <| Proxy @Plan.Tier
    regionSchema <- declareSchemaRef <| Proxy @Heroku.Region
    stringSchema <- declareSchemaRef <| Proxy @String
    uuidSchema   <- declareSchemaRef <| Proxy @UUID

    mempty
      |> example ?~ toJSON Request
                      { callbackUrl = "callback.herokuapp.com/foo"
                      , name        = "my-awesome-app"
                      , plan        = Plan.Free
                      , region      = Heroku.Tokyo
                      , uuid        = fromJust <| UUID.fromString "0cebfcfe-93c9-11e9-bc42-526af7764f64"
                      }
      |> required .~ [ "callbackUrl"
                     , "name"
                     , "plan"
                     , "region"
                     , "uuid"
                     ]
      |> properties .~ [ ("callbackUrl", stringSchema)
                             , ("name",        stringSchema)
                             , ("plan",        planSchema)
                             , ("region",      regionSchema)
                             , ("uuid",        uuidSchema)
                             ]
      |> description ?~ "Request from Heroku to provision a new user"
      |> title       ?~ "Heroku Provisioning Request"
      |> type_       ?~ SwaggerObject
      |> NamedSchema (Just "ProvisionRequest")
      |> pure

{-| Response Parameters

From Heroku

> In addition to the id parameter as documented under asynchronous provisioning,
> you should include a config parameter along with any other optional parameters
> of your choosing.
>
> Name: config
> Type: object
>
> Description:
> Configuration variables to be set as environment variables on any applications
> that use this add-on resource. All environment variables that you send should
> have the same prefix: your add-on’s name, capitalized. However,
> within the context of a Heroku app, the prefix may be different for a variety
> of reasons. For example, if your add-on is named fast-db and you are setting
> FAST_DB_URL, the variable name on the application will default to FAST_DB_URL
> but would be PRIMARY_DB_URL if the user added the add-on with
> the prefix PRIMARY_DB.
>
> Example:
> HTTP/1.1 200 OK
> { "MYADDON_URL": "http://myaddon.com/52e82f5d73" }
-}

data Provision = Provision
  { id      :: UserId        -- ^ User ID
  , config  :: User.Provision -- ^ Heroku env var payload
  , peers   :: [IPFS.Peer]    -- ^ IPFS peer list
  , message :: Text           -- ^ A helpful human-readable message
  } deriving ( Eq
             , Show
             )

instance ToJSON Provision where
  toJSON Provision {..} = object
    [ "id"      .= id
    , "config"  .= config
    , "message" .= message
    ]

instance ToSchema Provision where
  declareNamedSchema _ = do
    uId       <- declareSchemaRef <| Proxy @(UserId)
    usrCfg    <- declareSchemaRef <| Proxy @User.Provision
    ipfsPeers <- declareSchemaRef <| Proxy @[IPFS.Peer]
    txt       <- declareSchemaRef <| Proxy @Text

    mempty
      |> description ?~ "Provisioned user login information"
      |> example     ?~ toJSON provisionEx
      |> type_       ?~ SwaggerObject
      |> required    .~ ["id" , "config"]
      |> properties  .~
          [ ("id",      uId)
          , ("config",  usrCfg)
          , ("message", txt)
          , ("peers",   ipfsPeers)
          ]
      |> NamedSchema (Just "User Provision Response")
      |> pure
    where
      provisionEx = Provision
        { id      = toId 4213
        , config  = cfgEx
        , peers  = [fission]
        , message = "Provisioned successfully"
        }

      cfgEx = User.Provision
        { url      = Client.BaseUrl Client.Https "runfission.com" 443 ""
        , username = "c74bd95b8555275277d4"
        , password = Secret "GW0SHByPmY0.y+lg)x7De.PNmJvh1"
        }
