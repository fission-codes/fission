module Fission.Platform.Heroku.AddOn.Manifest.Types
  ( Manifest (..)
  , password
  , ssoSalt
  ) where

import Fission.Prelude

data API = API
  { password :: Text
  , ssoSalt  :: Text
  } deriving ( Show
             , Eq
             )

instance FromJSON API where
  parseJSON = withObject "Heroku.API" \obj -> do
    password <- obj .: "password"
    ssoSalt  <- obj .: "sso_salt"
    return API {..}

data Manifest = Manifest
  { id   :: Text
  , name :: Text
  , api  :: API
  } deriving ( Show
             , Eq
             )

instance FromJSON Manifest where
  parseJSON = withObject "Heroku.Manifest" \obj -> do
    id   <- obj .: "id"
    name <- obj .: "name"
    api  <- obj .: "api"
    return Manifest {..}
