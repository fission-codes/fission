module Fission.User.Provision.Types
  ( UserConfig (..)
  , url
  , password
  , username
  ) where

import RIO

import Control.Lens (makeLenses)
import Data.Aeson.Casing
import Data.Aeson
import Data.Swagger as Swagger hiding (url)

import Fission.Internal.JSON
import Fission.Security

data Provision = Provision
  { _url      :: Text
  , _username :: Text
  , _password :: Secret
  } deriving ( Eq
             , Show
             , Generic
             )

makeLenses ''UserConfig

instance ToSchema Provision where
  toJSON UserConfig {..} = object
    [ "INTERPLANETARY_FISSION_URL"      .= _url
    , "INTERPLANETARY_FISSION_USERNAME" .= _username
    , "INTERPLANETARY_FISSION_PASSWORD" .= _password
    ]
