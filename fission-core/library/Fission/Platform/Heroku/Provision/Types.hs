module Fission.Platform.Heroku.Provision.Types (Provision (..)) where

import           Data.Swagger hiding (name)
import           Database.Persist.Sql

import           Network.IPFS.Types as IPFS
import qualified Servant.Client as Client

import          Test.QuickCheck
import          Test.QuickCheck.Instances ()

import           Fission.Models
import           Fission.Prelude

import qualified Fission.User.Provision.Types  as User
import           Fission.Security.Types

import Fission.Internal.Orphanage.Peer ()

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
  { id      :: UserId         -- ^ User ID
  , config  :: User.Provision -- ^ Heroku env var payload
  , peers   :: [IPFS.Peer]    -- ^ IPFS peer list
  , message :: Text           -- ^ A helpful human-readable message
  } deriving ( Eq
             , Show
             )

instance Arbitrary Provision where
  arbitrary = do
    id      <- arbitrary
    config  <- arbitrary
    peers   <- arbitrary
    message <- arbitrary
    return Provision {..}

instance FromJSON Provision where
  parseJSON = withObject "Provision" \obj -> do
    id       <- obj .: "id"
    config   <- obj .: "config"
    message  <- obj .: "message"

    let peers = []

    return Provision {..}

instance ToJSON Provision where
  toJSON Provision {..} = object
    [ "id"      .= id
    , "config"  .= config
    , "message" .= message
    ]

instance ToSchema Provision where
  declareNamedSchema _ = do
    uId       <- declareSchemaRef <| Proxy @UserId
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
      |> NamedSchema (Just "UserProvisionResponse")
      |> pure
    where
      provisionEx = Provision
        { id      = toSqlKey 4213
        , config  = cfgEx
        , peers  = [IPFS.Peer "/ip4/3.215.160.238/tcp/4001/ipfs/QmVLEz2SxoNiFnuyLpbXsH6SvjPTrHNMU88vCQZyhgBzgw"]
        , message = "Provisioned successfully"
        }

      cfgEx = User.Provision
        { url      = Client.BaseUrl Client.Https "runfission.com" 443 ""
        , username = "c74bd95b8555275277d4"
        , password = Secret "GW0SHByPmY0.y+lg)x7De.PNmJvh1"
        }
