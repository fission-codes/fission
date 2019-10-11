module Fission.User.Provision.Types
  ( Provision (..)
  , url
  , password
  , username
  , peers
  ) where

import RIO

import           Control.Lens hiding ((.=))
import           Data.Aeson
import           Data.Swagger as Swagger hiding (url)
import qualified Servant.Client as Client

import Fission.Security
import Fission.Security.Types
import Fission.Internal.Orphanage.BaseUrl ()
import Fission.IPFS.Types as IPFS

data Provision = Provision
  { _url      :: Client.BaseUrl
  , _username :: Text
  , _password :: Secret
  , _peers   :: [IPFS.Peer]
  } deriving ( Eq
             , Show
             , Generic
             )

makeLenses ''Provision

instance FromJSON Provision where
  parseJSON = withObject "User.Provision" \obj -> do
    _url      <- obj .: "INTERPLANETARY_FISSION_URL"
    _username <- obj .: "INTERPLANETARY_FISSION_USERNAME"
    _password <- obj .: "INTERPLANETARY_FISSION_PASSWORD"
    _peers <- obj .: "INTERPLANETARY_FISSION_PEERS"
    return Provision {..}

instance ToJSON Provision where
  toJSON Provision {..} = object
    [ "INTERPLANETARY_FISSION_URL"      .= _url
    , "INTERPLANETARY_FISSION_USERNAME" .= _username
    , "INTERPLANETARY_FISSION_PASSWORD" .= _password
    , "INTERPLANETARY_FISSION_PEERS" .= _peers
    ]

instance ToSchema Provision where
  declareNamedSchema _ = do
    url'      <- declareSchemaRef (Proxy :: Proxy Client.BaseUrl)
    username' <- declareSchemaRef (Proxy :: Proxy Text)
    password' <- declareSchemaRef (Proxy :: Proxy Secret)
    ipfsPeers <- declareSchemaRef (Proxy :: Proxy [IPFS.Peer])

    return $ NamedSchema (Just "UserConfig") $ mempty
      & type_      ?~ SwaggerObject
      & properties .~
          [ ("INTERPLANETARY_FISSION_URL",      url')
          , ("INTERPLANETARY_FISSION_USERNAME", username')
          , ("INTERPLANETARY_FISSION_PASSWORD", password')
          , ("INTERPLANETARY_FISSION_PEERS", ipfsPeers)
          ]
      & required .~
          [ "INTERPLANETARY_FISSION_URL"
          , "INTERPLANETARY_FISSION_USERNAME"
          , "INTERPLANETARY_FISSION_PASSWORD"
          ]
      & description ?~
          "The information that a user needs to know to access this service. Typically sent on provision"
      & example ?~ toJSON Provision
          { _url      = Client.BaseUrl Client.Https "runfission.com" 443 ""
          , _username = "c74bd95b8555275277d4"
          , _password = Secret "GW0SHByPmY0.y+lg)x7De.PNmJvh1"
          , _peers = [IPFS.Peer "/ip4/3.215.160.238/tcp/4001/ipfs/QmVLEz2SxoNiFnuyLpbXsH6SvjPTrHNMU88vCQZyhgBzgw"]
          }
