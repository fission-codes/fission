{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Fission.Platform.Heroku.AddOn
  ( AddOn(..)
  -- Selectors
  , id'
  , uuid'
  , region'
  , insertedAt'
  , modifiedAt'
  -- Lenses
  , id
  , uuid
  , region
  , insertedAt
  , modifiedAt
  -- Table
  , tableName
  , addOns
  ) where

import RIO hiding (id)

import Control.Lens (makeLenses)
import Data.UUID
import Database.Selda

import Fission.Internal.Orphanage ()
import Fission.Platform.Heroku.Region
import Fission.Storage.SQLite

data AddOn = AddOn
  { _id         :: ID AddOn
  , _uuid       :: UUID
  , _region     :: Maybe Region
  -- , refreshToken :: Text
  , _insertedAt :: UTCTime
  , _modifiedAt :: UTCTime
  } deriving ( Show
             , Eq
             , SqlRow
             , Generic
             )

makeLenses ''AddOn

instance DBInsertable AddOn where
  insertX t partRs = insertWithPK addOns $ fmap (insertStamp t) partRs

id'         :: Selector AddOn (ID AddOn)
uuid'       :: Selector AddOn UUID
region'     :: Selector AddOn (Maybe Region)
insertedAt' :: Selector AddOn UTCTime
modifiedAt' :: Selector AddOn UTCTime

id' :*: uuid'
    :*: region'
    :*: insertedAt'
    :*: modifiedAt' = selectors addOns

tableName :: TableName' AddOn
tableName = "heroku_add_ons"

addOns :: Table AddOn
addOns = lensTable (unTable tableName) [#_id :- autoPrimary]
