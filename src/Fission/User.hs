{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Fission.User
  (User (..)
  , users
  , setup
  ) where

import RIO hiding (id)

import Data.Has
import Database.Selda
import Database.Selda.SQLite

import Fission.Internal.Constraint
import Fission.Config

data User = User
  { id       :: ID User
  , email    :: Text
  , password :: Text
  } deriving Generic

instance SqlRow User

users :: Table User
users = table "users" [#id :- autoPrimary]

setup :: (MonadRIO cfg m, HasLogFunc cfg, Has DBPath cfg) => m ()
setup = do
  DBPath db <- view hasLens
  logInfo $ "Creating table `users` in DB " <> displayShow db
  liftIO . withSQLite db $ createTable users
    -- insert_ users
    --   [ User def "Link"  "pw"
    --   , User def "Zelda" "pw"
    --   ]
