{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}

module Fission.Storage.SQLite
  ( setupTable
  , connPool
  , DBInsertable (..)
  , insertX'
  , insert1
  , insert1'
  , insertStamp
  , lensTable
  , getOne
  , TableName' (..)
  ) where

import           RIO         hiding     (id)
import qualified RIO.Partial as Partial
import           RIO.Text               (stripPrefix)
import RIO.List (headMaybe)
import Data.Time (getCurrentTime)

import Data.Has
import Data.Pool

import Database.Selda
import Database.Selda.SQLite
import Database.Selda.Backend

import Fission.Internal.Constraint
import Fission.Config

newtype TableName' a = TableName' { unTable :: TableName }
  deriving ( Show
           , Eq
           , IsString
           )

class DBInsertable r where
  insertX :: MonadSelda m
          => UTCTime
          -> [(UTCTime -> UTCTime -> r)]
          -> m (ID r)

insertStamp :: UTCTime -> (UTCTime -> UTCTime -> r) -> r
insertStamp time record = record time time

insertX' :: (DBInsertable r, MonadSelda m) => [UTCTime -> UTCTime -> r] -> m (ID r)
insertX' partRs = do
  now <- liftIO getCurrentTime
  insertX now partRs

insert1 :: (DBInsertable r, MonadSelda m)
        => UTCTime -> (UTCTime -> UTCTime -> r) -> m (ID r)
insert1 t partR = insertX t [partR]

insert1' :: (DBInsertable r, MonadSelda m) => (UTCTime -> UTCTime -> r) -> m (ID r)
insert1' partR = insertX' [partR]

setupTable :: MonadRIO cfg m
           => HasLogFunc cfg
           => Has DBPath cfg
           => Table b
           -> TableName
           -> m ()
setupTable tbl tblName = do
  DBPath db <- fromCfg
  logInfo $ "Creating table `" <> displayShow tblName <> "` in DB " <> displayShow db
  liftIO . withSQLite db $ createTable tbl

-- TODO make configurable
connPool :: HasLogFunc cfg => DBPath -> RIO cfg (Pool SeldaConnection)
connPool (DBPath {unDBPath = path}) = do
  logInfo $ "Establishing database connection for " <> displayShow path

  pool <- liftIO $ createPool (sqliteOpen path) seldaClose 4 2 10 -- config these
  logInfo $ "DB pool stats: " <> displayShow pool

  return pool

lensTable :: Relational r => TableName -> [Attr r] -> Table r
lensTable tableName conf =
  tableFieldMod tableName conf (Partial.fromJust . stripPrefix "_")

getOne :: Functor f => f [a] -> f (Maybe a)
getOne = fmap headMaybe
