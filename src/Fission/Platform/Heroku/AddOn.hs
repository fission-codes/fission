{-# LANGUAGE TemplateHaskell   #-}
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
  , id
  , uuid
  , region
  , insertedAt
  , modifiedAt
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

tableName :: TableName
tableName = "heroku_add_ons"

addOns :: Table AddOn
addOns = lensTable tableName [#_id :- autoPrimary]
