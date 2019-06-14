module Fission.Storage.Mutate
  ( DBInsertable (..)
  , insertX'
  , insert1
  , insert1'
  , insertStamp
  ) where

import RIO

import Data.Time (getCurrentTime)

import Database.Selda

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

insert1 :: DBInsertable r
        => MonadSelda m
        => UTCTime
        -> (UTCTime -> UTCTime -> r)
        -> m (ID r)
insert1 t partR = insertX t [partR]

insert1' :: DBInsertable r
         => MonadSelda m
         => (UTCTime -> UTCTime -> r)
         -> m (ID r)
insert1' partR = insertX' [partR]
