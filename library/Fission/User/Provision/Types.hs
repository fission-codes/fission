module Fission.User.Provision.Types
  ( Provision (..)
  , interplanetaryFissionUrl
  , interplanetaryFissionPassword
  , interplanetaryFissionUsername
  ) where

import RIO

import Control.Lens
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Swagger as Swagger

import Fission.Internal.JSON
import Fission.Security

data Provision = Provision
  { _interplanetaryFissionUrl      :: Text
  , _interplanetaryFissionUsername :: Text
  , _interplanetaryFissionPassword :: Secret
  } deriving ( Eq
             , Show
             , Generic
             )

makeLenses ''Provision
$(deriveJSON lens_SCREAMING_SNAKE_CASE ''Provision)

instance ToSchema Provision where
  declareNamedSchema = genericDeclareNamedSchema
    $ (fromAesonOptions lens_SCREAMING_SNAKE_CASE)
      { Swagger.constructorTagModifier = camelCase }
