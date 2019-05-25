{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Fission.Platform.Heroku.AddOn
  ( AddOn(..)
  , tableName
  , addOns
  , setup
  , selUpdatedAt
  , selCreatedAt
  , selRegion
  , selUUID
  , selId
  ) where

import RIO hiding (id)

import Data.Has
import Database.Selda
import Data.UUID

import Fission.Platform.Heroku.Region as Heroku
import Fission.Config
import Fission.Internal.Constraint
import Fission.Internal.Orphanage ()
import Fission.Storage.SQLite

data AddOn = AddOn
  { id           :: ID AddOn
  , uuid         :: UUID
  , region       :: Maybe Heroku.Region
  -- , refreshToken :: Text
  , createdAt     :: UTCTime
  , updatedAt     :: UTCTime
  } deriving ( Show
             , Eq
             , SqlRow
             , Generic
             )

instance DBInsertable AddOn where
  insertX t partRs = insertWithPK addOns $ fmap (insertStamp t) partRs

tableName :: TableName
tableName = "heroku_add_ons"

addOns :: Table AddOn
addOns = table tableName [#id :- autoPrimary]

selId        :: Selector AddOn (ID AddOn)
selUUID      :: Selector AddOn UUID
selRegion    :: Selector AddOn (Maybe Region)
selCreatedAt :: Selector AddOn UTCTime
selUpdatedAt :: Selector AddOn UTCTime

selId
  :*: selUUID
  :*: selRegion
  :*: selCreatedAt
  :*: selUpdatedAt = selectors addOns

setup :: MonadRIO cfg m
      => HasLogFunc cfg
      => Has DBPath cfg
      => m ()
setup = setupTable addOns tableName
