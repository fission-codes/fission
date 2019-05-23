{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Fission.Platform.Heroku.AddOn where

import RIO hiding (id)

import Data.Has
import Database.Selda

import Fission.Platform.Heroku.Region as Heroku

import Fission.Config
import Fission.Internal.Constraint
import Fission.Storage.SQLite

data AddOn = AddOn
  { id           :: ID AddOn
  , region       :: Maybe Heroku.Region
  , refreshToken :: Text
  } deriving ( Show
             , Eq
             , SqlRow
             , Generic
             )

tableName :: TableName
tableName = "heroku_add_ons"

mkTable :: Table AddOn
mkTable = table tableName [#id :- autoPrimary]

setup :: MonadRIO cfg m
      => HasLogFunc cfg
      => Has DBPath cfg
      => m ()
setup = setupTable mkTable tableName
