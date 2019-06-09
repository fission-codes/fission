{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Fission.User
  ( User (..)
  , Role (..)
  -- Selectors
  , id'
  , role'
  , active'
  , herokuAddOnId'
  , secretDigest'
  , insertedAt'
  , modifiedAt'
  -- Lenses
  , id
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
  ) where

import RIO hiding (id)

import Control.Lens (makeLenses)
import Database.Selda
import Data.Time (getCurrentTime)
import Data.UUID (UUID)

import qualified Fission.Platform.Heroku.AddOn  as Heroku       (AddOn (..), addOns)
import qualified Fission.Platform.Heroku.AddOn  as Heroku.AddOn (id')
import qualified Fission.Platform.Heroku.Region as Heroku       (Region)

import           Fission.Storage.SQLite
import           Fission.User.Role
import           Fission.Security (SecretDigest, Digestable (..))
import qualified Fission.Internal.UTF8  as UTF8

data User = User
  { _id            :: ID User
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

id'            :: Selector User (ID User)
role'          :: Selector User Role
active'        :: Selector User Bool
herokuAddOnId' :: Selector User (Maybe (ID Heroku.AddOn))
secretDigest'  :: Selector User SecretDigest
insertedAt'    :: Selector User UTCTime
modifiedAt'    :: Selector User UTCTime

id' :*: role'
    :*: active'
    :*: herokuAddOnId'
    :*: secretDigest'
    :*: insertedAt'
    :*: modifiedAt' = selectors users

tableName :: TableName' User
tableName = TableName' "users"

users :: Table User
users = lensTable (unTable tableName)
  [ #_id            :- autoPrimary
  , #_active        :- index
  , #_secretDigest  :- index
  , #_secretDigest  :- unique
  , #_herokuAddOnId :- foreignKey Heroku.addOns Heroku.AddOn.id'
  ]

createFresh :: (MonadIO m, MonadSelda m)
            => UUID -> Heroku.Region -> SecretDigest -> m (ID User)
createFresh herokuUUID herokuRegion sekret = transaction $ do
  now     <- liftIO getCurrentTime
  hConfId <- insert1 now . Heroku.AddOn def herokuUUID $ Just herokuRegion
  insert1 now $ User def Regular True (Just hConfId) sekret

-- TODO `limit 0 1`
bySecret :: MonadSelda m => Text -> m [User]
bySecret secret = query $ do
  user <- select users
  restrict $ user ! #_secretDigest .== text secret
         .&& user ! #_active       .== true
  return user
