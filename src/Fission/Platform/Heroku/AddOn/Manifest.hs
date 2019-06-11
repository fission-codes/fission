module Fission.Platform.Heroku.AddOn.Manifest
  ( Manifest (..)
  , id
  , name
  , api
  , API (..)
  , password
  , sso_salt
  ) where

import RIO hiding (id)

import Control.Lens (makeLenses)
import Data.Aeson.TH

import Fission.Internal.JSON

data API = API
  { _password :: Text
  , _sso_salt :: Text
  } deriving ( Show
             , Eq
             )

makeLenses ''API
$(deriveJSON lens_snake_case ''API)

data Manifest = Manifest
  { _id   :: Text
  , _name :: Text
  , _api  :: API
  } deriving ( Show
             , Eq
             )

makeLenses ''Manifest
$(deriveJSON lens_snake_case ''Manifest)
