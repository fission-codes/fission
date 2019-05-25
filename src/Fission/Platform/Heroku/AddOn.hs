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
  , selInsertedAt
  , selModifiedAt
  , selRegion
  , selUUID
  , selId
  ) where

import RIO hiding (id)

import Data.UUID
import Database.Selda

import Fission.Internal.Orphanage ()
import Fission.Platform.Heroku.Region as Heroku
import Fission.Storage.SQLite

data AddOn = AddOn
  { id         :: ID AddOn
  , uuid       :: UUID
  , region     :: Maybe Heroku.Region
  -- , refreshToken :: Text
  , insertedAt :: UTCTime
  , modifiedAt :: UTCTime
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
selInsertedAt :: Selector AddOn UTCTime
selModifiedAt :: Selector AddOn UTCTime

selId
  :*: selUUID
  :*: selRegion
  :*: selInsertedAt
  :*: selModifiedAt = selectors addOns
