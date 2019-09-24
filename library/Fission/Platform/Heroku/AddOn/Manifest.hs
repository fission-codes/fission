module Fission.Platform.Heroku.AddOn.Manifest
  ( Manifest -- (..)
  , Fields
  -- , id
  -- , name
  -- , api
  -- , API -- (..)
  -- , password
  -- , ssoSalt
  ) where

import RIO hiding (id)

-- import Control.Lens (makeLenses)
-- import Data.Aeson.TH
import SuperRecord as SR

import qualified Fission.Platform.Heroku.Types as Heroku

-- import Fission.Internal.JSON

-- data API = API
--   { _password :: Text
--   , _ssoSalt  :: Text
--   } deriving ( Show
--              , Eq
--              )

-- makeLenses ''API
-- $(deriveJSON lens_snake_case ''API)

-- type API = Rec '[ "password" := Text
--                 , "sso_salt" := Text
--                 ]

-- data Manifest = Manifest
--   { _id   :: Text
--   , _name :: Text
--   , _api  :: API
--   } deriving ( Show
--              , Eq
--              )

-- makeLenses ''Manifest
-- $(deriveJSON lens_snake_case ''Manifest)

type APIFields = '["password" := Heroku.Password]

type Fields = '[ "id"   := Heroku.ID
               , "name" := Text
               , "api"  := Rec APIFields
               ]

type Manifest = Rec Fields
