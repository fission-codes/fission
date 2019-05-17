{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Fission.Storage.SQLite (setupTable) where

import RIO hiding (id)

import Data.Has
import Database.Selda
import Database.Selda.SQLite

import Fission.Internal.Constraint
import Fission.Config

setupTable :: MonadRIO cfg m
           => HasLogFunc cfg
           => Has DBPath cfg
           => Table b
           -> TableName
           -> m ()
setupTable tbl tblName = do
  DBPath db <- view hasLens
  logInfo $ "Creating table `" <> displayShow tblName <> "` in DB " <> displayShow db
  liftIO . withSQLite db $ createTable tbl
