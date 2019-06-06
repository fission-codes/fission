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
  ) where

import RIO hiding (id)
import RIO.List (headMaybe)

import Control.Lens (makeLenses)
import Database.Selda
import Data.Time (getCurrentTime)
import Data.UUID (UUID)

import qualified Fission.Platform.Heroku.AddOn  as Heroku       (AddOn (..), addOns)
import qualified Fission.Platform.Heroku.AddOn  as Heroku.AddOn (id')
import qualified Fission.Platform.Heroku.Region as Heroku       (Region)

import           Fission.Storage.SQLite
import           Fission.User.Role
import           Fission.Security (SecretDigest)

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

tableName :: TableName
tableName = "users"

users :: Table User
users = lensTable tableName
  [ #_id            :- autoPrimary
  , #_secretDigest  :- unique
  -- , TODO SELDA INDEX ACTIVE
  -- , TODO SELDA INDEX SECRET DIGEST & see if possible to make immutible in SQLite
  , #_herokuAddOnId :- foreignKey Heroku.addOns Heroku.AddOn.id'
  ]

createFresh :: (MonadIO m, MonadSelda m) => UUID -> Heroku.Region -> SecretDigest -> m (ID User)
createFresh herokuUUID herokuRegion sekret = transaction $ do
  now     <- liftIO getCurrentTime
  hConfId <- insert1 now . Heroku.AddOn def herokuUUID $ Just herokuRegion
  insert1 now $ User def Regular True (Just hConfId) sekret

getUserByPassword :: MonadSelda m => Col s Text -> m (Maybe User)
getUserByPassword secret = fmap headMaybe . query $ do
  user <- select users
  u' <- Inner user
  restrict (user ! #_secretDigest .== secret) -- Has a uniqueness constraint
  return user

-- getUserByPassword secret = do
--   u <- query $ do
--         user <- select users
--         restrict (user ! #_secretDigest .== secret) -- Has a uniqueness constraint
--         return user

--   return $ headMaybe u

-- authUser pwd = do
--   u <- getUserByPassword pwd
--   case u of
--     [] -> error "no results"
--     -- []
