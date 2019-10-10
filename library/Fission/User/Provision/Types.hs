module Fission.User.Provision.Types
  ( Provision (..)
  , url
  , password
  , username
  ) where

import RIO

import           Control.Lens hiding ((.=))
import           Data.Aeson
import           Data.Swagger as Swagger hiding (url)
import qualified Servant.Client as Client

import Fission.Security
import Fission.Security.Types
import Fission.Internal.Orphanage.BaseUrl ()

data Provision = Provision
  { _url      :: Client.BaseUrl
  , _username :: Text
  , _password :: Secret
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
    return Provision {..}

instance ToJSON Provision where
  toJSON Provision {..} = object
    [ "INTERPLANETARY_FISSION_URL"      .= _url
    , "INTERPLANETARY_FISSION_USERNAME" .= _username
    , "INTERPLANETARY_FISSION_PASSWORD" .= _password
    ]

instance ToSchema Provision where
  declareNamedSchema _ = do
    url'      <- declareSchemaRef (Proxy :: Proxy Client.BaseUrl)
    username' <- declareSchemaRef (Proxy :: Proxy Text)
    password' <- declareSchemaRef (Proxy :: Proxy Secret)

    return $ NamedSchema (Just "UserConfig") $ mempty
      & type_      ?~ SwaggerObject
      & properties .~
          [ ("INTERPLANETARY_FISSION_URL",      url')
          , ("INTERPLANETARY_FISSION_USERNAME", username')
          , ("INTERPLANETARY_FISSION_PASSWORD", password')
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
          }
