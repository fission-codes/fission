module Fission.Platform.User
  ( Config (..)
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

data Config = Config
  { _interplanetaryFissionUrl      :: Text
  , _interplanetaryFissionUsername :: Text
  , _interplanetaryFissionPassword :: Secret
  } deriving ( Eq
             , Show
             , Generic
             )

makeLenses ''Config
$(deriveJSON lens_SCREAMING_SNAKE_CASE ''Config)

instance ToSchema Config where
  declareNamedSchema = genericDeclareNamedSchema
    $ (fromAesonOptions lens_SCREAMING_SNAKE_CASE)
      { Swagger.constructorTagModifier = camelCase }
