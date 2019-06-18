module Fission.Platform.Heroku.UserConfig
  ( UserConfig (..)
  , interplanetaryFissionUrl
  , interplanetaryFissionPassword
  , interplanetaryFissionUsername
  ) where

import RIO

import Control.Lens  (makeLenses)
import Data.Aeson.TH
import Data.Swagger (ToSchema)

import Fission.Internal.JSON
import Fission.Security

data UserConfig = UserConfig
  { _interplanetaryFissionUrl      :: Text
  , _interplanetaryFissionUsername :: Text
  , _interplanetaryFissionPassword :: Secret
  } deriving ( Eq
             , Show
             , Generic
             , ToSchema
             )

makeLenses ''UserConfig
$(deriveJSON lens_SCREAMING_SNAKE_CASE ''UserConfig)
