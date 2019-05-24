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
  , setup
  , tableName
  ) where

import RIO hiding (id)

import Data.Has
import Database.Selda

import qualified Fission.Platform.Heroku.AddOn as Heroku
import           Fission.Storage.SQLite
import           Fission.Internal.Constraint
import           Fission.Config

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
users = table tableName [#id :- autoPrimary]

setup :: MonadRIO cfg m
      => HasLogFunc cfg
      => Has DBPath cfg
      => m ()
setup = setupTable users tableName

instance DBInsertable User where
  insertX t partRs = insertWithPK users $ fmap (insertStamp t) partRs
