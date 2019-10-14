-- | Table creation and migration sequences
module Fission.Storage.PostgreSQL.UpgradeUser
  ( ) where

import RIO

import Database.Selda.PostgreSQL
import Database.Selda.Migrations

import Database.Selda

import qualified Fission.Storage.Table  as Table
import qualified Fission.User.Table  as User.Table
import           Fission.User.Types

import Control.Lens   ((?~))
import Data.Swagger
import Database.Selda

import qualified Fission.Platform.Heroku.AddOn as Heroku

import Fission.Security       (Digestable (..))
import Fission.Security.Types (SecretDigest)
import Fission.User.Role
import qualified Fission.Internal.UTF8 as UTF8

upgrade :: MonadSelda m => m()


-- | The 'User' table
oldUsers :: Table User
oldUsers = Table.lensPrefixed (Table.name User.Table.name)
  [ #_userID        :- autoPrimary
  , #_username      :- index
  , #_username      :- unique
  , #_active        :- index
  , #_secretDigest  :- index
  , #_secretDigest  :- unique
  , #_herokuAddOnId :- foreignKey Heroku.addOns Heroku.addOnID'
  ]

-- | A user account, most likely a developer
data OldUser = OldUser
  { _userID        :: ID OldUser
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

instance Digestable (ID OldUser) where
  digest = digest . UTF8.textShow

instance ToSchema (ID OldUser) where
  declareNamedSchema _ =
     return $ NamedSchema (Just "UserID")
            $ mempty & type_ ?~ SwaggerInteger