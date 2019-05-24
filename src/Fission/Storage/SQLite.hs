{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Fission.Storage.SQLite
  ( setupTable
  , connPool
  , Insertable (..)
  , insert1
  , build
  ) where

import RIO hiding (id)

import Data.Has
import Data.Pool
import Data.Aeson
import Data.Scientific
import Data.Aeson.Types

import Database.Selda
import Database.Selda.SQLite
import Database.Selda.Backend

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

-- TODO make configurable
connPool :: HasLogFunc cfg => DBPath -> RIO cfg (Pool SeldaConnection)
connPool (DBPath {unDBPath = path}) = do
  logInfo $ "Establishing database connection for " <> displayShow path

  pool <- liftIO $ createPool (sqliteOpen path) seldaClose 4 2 10
  logInfo $ "DB pool stats: " <> displayShow pool

  return pool

build :: UTCTime -> (UTCTime -> UTCTime -> r) -> r
build time record = record time time

class Insertable r where
  insertX :: MonadSelda m
          => UTCTime
          -> [(UTCTime -> UTCTime -> r)]
          -> m (ID r)

insert1 :: Insertable r
        => MonadSelda m
        => UTCTime
        -> (UTCTime -> UTCTime -> r)
        -> m (ID r)
insert1 t partR = insertX t [partR]

instance ToJSON (ID a) where
  toJSON = Number . fromIntegral . fromId

instance FromJSON (ID a) where
  parseJSON = \case
    num@(Number n) ->
      case toBoundedInteger n of
        Nothing  -> errMsg num
        Just int -> return $ toId int

    invalid ->
      errMsg invalid

    where
      errMsg = modifyFailure ("parsing ID failed, " ++) . typeMismatch "Number"
