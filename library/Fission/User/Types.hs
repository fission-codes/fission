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
  { userID        :: ID User
  , username      :: Text
  , email         :: Maybe Text
  , role          :: Role
  , active        :: Bool
  , herokuAddOnId :: Maybe (ID Heroku.AddOn)
  , secretDigest  :: SecretDigest
  , insertedAt    :: UTCTime
  , modifiedAt    :: UTCTime
  } deriving ( Show
             , Eq
             , Generic
             , SqlRow
             )

instance Digestable (ID User) where
  digest = digest . UTF8.textShow

instance ToSchema (ID User) where
  declareNamedSchema _ =
     return <| NamedSchema (Just "UserID")
            <| type_ ?~ SwaggerInteger <| mempty
