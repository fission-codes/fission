{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Fission.Platform.Heroku.AddOn
  ( AddOn(..)
  , tableName
  , addons
  , setup
  ) where

import RIO hiding (id)

import Data.Has
import Database.Selda
import Data.UUID

import Fission.Platform.Heroku.Region as Heroku
import Fission.Config
import Fission.Internal.Constraint
import Fission.Internal.Orphanage
import Fission.Storage.SQLite

data AddOn = AddOn
  { id           :: ID AddOn
  , uuid         :: UUID
  , region       :: Maybe Heroku.Region
  , refreshToken :: Text
  } deriving ( Show
             , Eq
             , SqlRow
             , Generic
             )

tableName :: TableName
tableName = "heroku_add_ons"

addons :: Table AddOn
addons = table tableName [#id :- autoPrimary]

setup :: MonadRIO cfg m
      => HasLogFunc cfg
      => Has DBPath cfg
      => m ()
setup = setupTable addons tableName
