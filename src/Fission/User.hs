{-# LANGUAGE DeriveAnyClass #-}

module Fission.User
  ( User (..)
  , Role (..)
  -- Selectors
  , userID'
  , role'
  , active'
  , herokuAddOnId'
  , secretDigest'
  , insertedAt'
  , modifiedAt'
  -- Lenses
  , userID
  , role
  , active
  , herokuAddOnId
  , secretDigest
  , insertedAt
  , modifiedAt
  -- Table
  , users
  , tableName
  -- Helpers
  , createFresh
  , bySecret
  , hashID
  ) where

import RIO
import qualified RIO.Text as Text

import Control.Lens (makeLenses)
import Database.Selda
import Data.Time (getCurrentTime)
import Data.UUID (UUID)

import qualified Fission.Platform.Heroku.AddOn as Heroku
  ( AddOn (..)
  , addOns
  , addOnID'
  )

import qualified Fission.Platform.Heroku.Region as Heroku (Region)

import           Fission.User.Role
import           Fission.Security (SecretDigest, Digestable (..))
import qualified Fission.Internal.UTF8  as UTF8

import           Fission.Storage.Query
import           Fission.Storage.Mutate
import qualified Fission.Storage.Table  as Table


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
             , SqlRow
             , Generic
             )

makeLenses ''User

instance DBInsertable User where
  insertX t partRs = insertWithPK users $ fmap (insertStamp t) partRs

instance Digestable (ID User) where
  digest = digest . UTF8.textShow

userID'        :: Selector User (ID User)
role'          :: Selector User Role
active'        :: Selector User Bool
herokuAddOnId' :: Selector User (Maybe (ID Heroku.AddOn))
secretDigest'  :: Selector User SecretDigest
insertedAt'    :: Selector User UTCTime
modifiedAt'    :: Selector User UTCTime

userID' :*: role'
        :*: active'
        :*: herokuAddOnId'
        :*: secretDigest'
        :*: insertedAt'
        :*: modifiedAt' = selectors users

tableName :: Table.Name User
tableName = "users"

users :: Table User
users = Table.lensPrefixed (Table.name tableName)
  [ #_userID        :- autoPrimary
  , #_active        :- index
  , #_secretDigest  :- index
  , #_secretDigest  :- unique
  , #_herokuAddOnId :- foreignKey Heroku.addOns Heroku.addOnID'
  ]

createFresh :: MonadIO m
            => MonadSelda m
            => UUID
            -> Heroku.Region
            -> SecretDigest
            -> m (ID User)
createFresh herokuUUID herokuRegion sekret = transaction do
  now     <- liftIO getCurrentTime
  hConfId <- insert1 now $ Heroku.AddOn def herokuUUID (Just herokuRegion)
  insert1 now $ User def Regular True (Just hConfId) sekret

-- TODO `limit 0 1`
bySecret :: MonadSelda m => Text -> m [User]
bySecret secret = query do
  user <- select users
  restrict $ user `is'` #_active
         .&& user ! #_secretDigest .== text secret

  return user

hashID :: ID User -> SecretDigest
hashID uID = Text.take 20 $ digest uID
