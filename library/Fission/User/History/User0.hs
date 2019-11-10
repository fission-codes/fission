module Fission.User.History.User0
  ( User0
  , user0Table
  ) where


import Data.Swagger
import Database.Selda

import qualified Fission.Platform.Heroku.AddOn as Heroku

import           Fission.Prelude
import qualified Fission.Storage.Table  as Table
import qualified Fission.User.Table     as User.Table
import           Fission.Security          (Digestable (..))
import           Fission.Security.Types    (SecretDigest)
import           Fission.User.Role
import qualified Fission.Internal.UTF8  as UTF8

-- | The 'User' table
user0Table :: Table User0
user0Table = table (Table.name User.Table.name)
  [ #userID        :- autoPrimary
  , #active        :- index
  , #secretDigest  :- index
  , #secretDigest  :- unique
  , #herokuAddOnId :- foreignKey Heroku.addOns #addOnID
  ]

-- | A user account, most likely a developer
data User0 = User0
  { userID        :: ID User0
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

instance Digestable (ID User0) where
  digest = digest . UTF8.textShow

instance ToSchema (ID User0) where
  declareNamedSchema _ =
    mempty
      |> type_ ?~ SwaggerInteger
      |> NamedSchema (Just "UserID")
      |> pure
