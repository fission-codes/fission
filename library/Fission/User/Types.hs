module Fission.User.Types (User (..))where

import Flow
import RIO

import Control.Lens   ((?~))
import Data.Swagger
import Database.Selda

import qualified Fission.Platform.Heroku.AddOn as Heroku

import Fission.Security       (Digestable (..))
import Fission.Security.Types (SecretDigest)
import Fission.User.Role

import qualified Fission.Internal.UTF8 as UTF8

-- | A user account, most likely a developer
data User = User
  { _userID        :: ID User
  , _role          :: Role
  , _active        :: Bool
  , _herokuAddOnId :: Maybe (ID Heroku.AddOn)
  , _secretDigest  :: SecretDigest
  , _insertedAt    :: UTCTime
  , _modifiedAt    :: UTCTime
  } deriving ( Show
             , Eq
             , Generic
             , SqlRow
             )

instance Digestable (ID User) where
  digest = digest <. UTF8.textShow

instance ToSchema (ID User) where
  declareNamedSchema _ =
    pure <| NamedSchema (Just "UserID")
         <| type_ ?~ SwaggerInteger
         <| mempty

    -- mempty
    --   |> type_ ?~ SwaggerInteger
    --   |> NamedSchema (Just "UserID")
    --   |> pure
