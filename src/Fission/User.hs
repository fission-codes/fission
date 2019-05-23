{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Fission.User
  ( User (..)
  , mkTable
  , setup
  , tableName
  ) where

import RIO hiding (id)

import Data.Has
import Database.Selda

import Fission.Platform.Heroku.Region
import Fission.Storage.SQLite
import Fission.Internal.Constraint
import Fission.Config

data Platform
  = Direct
  | Heroku
  deriving ( Show
           , Read
           , Eq
           , Bounded
           , Enum
           , SqlType
           )

data User = User
  { id       :: ID User
  , platform :: Platform
  , region   :: Maybe Region
  , token    :: Text
  , password :: Text
  } deriving ( Show
             , Eq
             , SqlRow
             , Generic
             )

tableName :: TableName
tableName = "users"

mkTable :: Table User
mkTable = table tableName [#id :- autoPrimary]

setup :: (MonadRIO cfg m, HasLogFunc cfg, Has DBPath cfg) => m ()
setup = setupTable mkTable tableName
