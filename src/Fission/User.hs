{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Fission.User
  ( User (..)
  , Role (..)
  , UserId
  , users
  , tableName
  , createFresh
  -- Selectors
  , selId
  , selRole
  , selHerokuAddOnId
  , selInsertedAt
  , selModifiedAt
  ) where

import RIO hiding (id)

import Database.Selda
import Database.Selda.SQLite
import Data.Time
import Data.UUID

import Fission.Platform.Heroku.Region as Heroku
import qualified Fission.Platform.Heroku.AddOn as Heroku
import           Fission.Storage.SQLite

type UserId = ID User

data Role
  = Regular
  | Admin
  deriving ( Show
           , Read
           , Eq
           , Enum
           , Bounded
           , SqlType
           )

data User = User
  { id            :: ID User
  , role          :: Role
  , herokuAddOnId :: Maybe (ID Heroku.AddOn)
  , createdAt     :: UTCTime
  , updatedAt     :: UTCTime
  } deriving ( Show
             , Eq
             , SqlRow
             , Generic
             )

tableName :: TableName
tableName = "users"

users :: Table User
users = table tableName
  [ #id            :- autoPrimary
  , #herokuAddOnId :- foreignKey Heroku.addOns Heroku.selId
  ]

selId            :: Selector User (ID User)
selRole          :: Selector User Role
selHerokuAddOnId :: Selector User (Maybe (ID Heroku.AddOn))
selInsertedAt    :: Selector User UTCTime
selModifiedAt    :: Selector User UTCTime

selId
  :*: selRole
  :*: selHerokuAddOnId
  :*: selInsertedAt
  :*: selModifiedAt = selectors users

instance DBInsertable User where
  insertX t partRs = insertWithPK users $ fmap (insertStamp t) partRs

createFresh :: MonadIO m
            => MonadSelda m
            => UUID
            -> Heroku.Region
            -> m UserId
createFresh herokuUUID herokuRegion = transaction $ do
  now     <- liftIO getCurrentTime
  hConfId <- insert1 now . Heroku.AddOn def herokuUUID $ Just herokuRegion
  insert1 now . User def Regular $ Just hConfId
