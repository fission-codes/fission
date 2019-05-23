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

import Fission.Platform
import Fission.Platform.Heroku.Region as Heroku

import Fission.Storage.SQLite
import Fission.Internal.Constraint
import Fission.Config

data HerokuAddOn = HerokuAddOn
  { idH          :: ID HerokuAddOn
  , region       :: Maybe Heroku.Region
  , refreshToken :: Text
  } deriving ( Show
             , Eq
             , SqlRow
             , Generic
             )

tableName' :: TableName
tableName' = "heroku_add_ons"

mkTable' :: Table HerokuAddOn
mkTable' = table tableName' [#idH :- autoPrimary]

data User = User
  { id            :: ID User
  , platform      :: Platform
  , herokuAddOnId :: Maybe (ID HerokuAddOn)
  } deriving ( Show
             , Eq
             , SqlRow
             , Generic
             )

tableName :: TableName
tableName = "users"

mkTable :: Table User
mkTable = table tableName [#id :- autoPrimary]

setup :: MonadRIO cfg m
      => HasLogFunc cfg
      => Has DBPath cfg
      => m ()
setup = setupTable mkTable tableName
