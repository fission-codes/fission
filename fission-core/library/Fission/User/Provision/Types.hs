module Fission.User.Provision.Types (Provision (..)) where

import           Data.Swagger as Swagger hiding (url)
import qualified Servant.Client as Client

import           Fission.Prelude
import           Fission.Security
import           Fission.User.Username.Types
import           Fission.Internal.Orphanage.BaseUrl ()

data Provision = Provision
  { url      :: Client.BaseUrl
  , username :: Username
  , password :: Secret
  } deriving ( Eq
             , Show
             , Generic
             )

instance Arbitrary Provision where
  arbitrary = do
    url      <- arbitrary
    username <- arbitrary
    password <- arbitrary
    return Provision {..}

instance FromJSON Provision where
  parseJSON = withObject "User.Provision" \obj -> do
    url      <- obj .: "INTERPLANETARY_FISSION_URL"
    username <- obj .: "INTERPLANETARY_FISSION_USERNAME"
    password <- obj .: "INTERPLANETARY_FISSION_PASSWORD"
    return Provision {..}

instance ToJSON Provision where
  toJSON Provision {..} = object
    [ "INTERPLANETARY_FISSION_URL"      .= url
    , "INTERPLANETARY_FISSION_USERNAME" .= username
    , "INTERPLANETARY_FISSION_PASSWORD" .= password
    ]

instance ToSchema Provision where
  declareNamedSchema _ = do
    url'      <- declareSchemaRef <| Proxy @Client.BaseUrl
    username' <- declareSchemaRef <| Proxy @Text
    password' <- declareSchemaRef <| Proxy @Secret

    mempty
      |> type_      ?~ SwaggerObject
      |> properties .~
          [ ("INTERPLANETARY_FISSION_URL",      url')
          , ("INTERPLANETARY_FISSION_USERNAME", username')
          , ("INTERPLANETARY_FISSION_PASSWORD", password')
          ]
      |> description ?~
          "The information that a user needs to know to access this service. Typically sent on provision"
      |> example ?~ toJSON Provision
          { url      = Client.BaseUrl Client.Https "runfission.com" 443 ""
          , username = "c74bd95b8555275277d4"
          , password = Secret "GW0SHByPmY0.y+lg)x7De.PNmJvh1"
          }
      |> NamedSchema (Just "UserConfig")
      |> pure
