{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Fission.User
  ( User (..)
  , Role (..)
  , users
  , tableName
  , createFresh
  , id
  , role
  , herokuAddOnId
  , insertedAt
  , modifiedAt
  ) where

import RIO hiding (id)

import Control.Lens (makeLenses)
import Database.Selda
import Data.Time (getCurrentTime)
import Data.UUID (UUID)

import qualified Fission.Platform.Heroku                as Heroku
import qualified Fission.Platform.Heroku.AddOn.Selector as Heroku.AddOn.Selector
import           Fission.Storage.SQLite
import           Fission.User.Role

data User = User
  { _id            :: ID User
  , _role          :: Role
  , _herokuAddOnId :: Maybe (ID Heroku.AddOn)
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

tableName :: TableName
tableName = "users"

users :: Table User
users = lensTable tableName
  [ #_id            :- autoPrimary
  , #_herokuAddOnId :- foreignKey Heroku.addOns Heroku.AddOn.Selector.id
  ]

createFresh :: MonadIO m
            => MonadSelda m
            => UUID
            -> Heroku.Region
            -> m (ID User)
createFresh herokuUUID herokuRegion = transaction $ do
  now     <- liftIO getCurrentTime
  hConfId <- insert1 now . Heroku.AddOn def herokuUUID $ Just herokuRegion
  insert1 now . User def Regular $ Just hConfId
