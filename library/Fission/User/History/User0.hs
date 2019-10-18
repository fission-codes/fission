module Fission.User.History.User0
  ( User0
  , user0Table
  ) where

import RIO

import qualified Fission.Storage.Table  as Table
import qualified Fission.User.Table  as User.Table

import Control.Lens   ((?~))
import Data.Swagger
import Database.Selda

import qualified Fission.Platform.Heroku.AddOn as Heroku

import Fission.Security       (Digestable (..))
import Fission.Security.Types (SecretDigest)
import Fission.User.Role
import qualified Fission.Internal.UTF8 as UTF8


-- | The 'User' table
user0Table :: Table User0
user0Table = Table.lensPrefixed (Table.name User.Table.name)
  [ #_userID        :- autoPrimary
  , #_active        :- index
  , #_secretDigest  :- index
  , #_secretDigest  :- unique
  , #_herokuAddOnId :- foreignKey Heroku.addOns Heroku.addOnID'
  ]

-- | A user account, most likely a developer
data User0 = User0
  { _userID        :: ID User0
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

instance Digestable (ID User0) where
  digest = digest . UTF8.textShow

instance ToSchema (ID User0) where
  declareNamedSchema _ =
     return $ NamedSchema (Just "UserID")
            $ mempty & type_ ?~ SwaggerInteger