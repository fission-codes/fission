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

import Fission.Storage.SQLite
import Fission.Internal.Constraint
import Fission.Config

data User = User
  { id       :: ID User
  , email    :: Text
  , password :: Text
  } deriving Generic

instance SqlRow User

tableName :: TableName
tableName = "users"

mkTable :: Table User
mkTable = table tableName [#id :- autoPrimary]

setup :: (MonadRIO cfg m, HasLogFunc cfg, Has DBPath cfg) => m ()
setup = setupTable mkTable tableName
