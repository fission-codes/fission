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

data API = API
  { _password :: Text
  , _ssoSalt  :: Text
  } deriving ( Show
             , Eq
             )

makeLenses ''API

instance FromJSON API where
  parseJSON = withObject "Heroku.API" \obj -> do
    _password <- obj .: "password"
    _ssoSalt  <- obj .: "sso_salt"
    return API {..}

data Manifest = Manifest
  { _id   :: Text
  , _name :: Text
  , _api  :: API
  } deriving ( Show
             , Eq
             )

makeLenses ''Manifest

instance FromJSON Manifest where
  parseJSON = withObject "Heroku.Manifest" \obj -> do
    _id   <- obj .: "id"
    _name <- obj .: "name"
    _api  <- obj .: "api" -- NOTE may need a recursive parse?
    return Manifest {..}