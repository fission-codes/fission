module Fission.Platform.Heroku.AddOn.Manifest
  ( Manifest (..)
  , id
  , name
  , api
  , API (..)
  , password
  , ssoSalt
  ) where

import RIO hiding (id)

import Control.Lens (makeLenses)
import Data.Aeson

import Fission.Platform.Heroku.Types as Heroku

data API = API
  { _password :: Heroku.Password
  , _ssoSalt  :: Text
  }
  deriving ( Show
           , Eq
           )

makeLenses ''API

instance FromJSON API where
  parseJSON = withObject "Heroku.Manifest.API" \obj -> do
    _password <- obj .: "password"
    _ssoSalt  <- obj .: "sso_salt"

    return API {..}

data Manifest = Manifest
  { _id   :: Heroku.ID
  , _name :: Text
  , _api  :: API
  }
  deriving ( Show
           , Eq
           )

makeLenses ''Manifest

instance FromJSON Manifest where
  parseJSON = withObject "Heroku.Manifest" \obj -> do
    _id   <- obj .: "id"
    _name <- obj .: "name"
    _api  <- obj .: "api" >>= parseJSON . Object

    return Manifest {..}
