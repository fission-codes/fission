{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Fission.Database
  () where

import RIO

import Control.Lens (makeLenses)

import Database.Beam
import Database.Beam.Sqlite ()
import Database.SQLite.Simple

import Fission.User

data FissionDB f = FissionDB { _fissionUsers :: f (TableEntity UserT) }
  deriving (Generic, Database be)

settings :: DatabaseSettings be FissionDB
settings = defaultDbSettings

setup :: IO ()
setup = do
  conn <- open "fission.db"
  runBeamSqliteDebug putStrLn {- for debug output -} conn $ runInsert $
    insert (_fissionUsers settings) $
    insertValues [ User "james@example.com" "b4cc344d25a2efe540adbf2678e2304c"
                 , User "betty@example.com" "82b054bd83ffad9b6cf8bdb98ce3cc2f"
                 , User "sam@example.com"   "332532dcfaa1cbf61e2a266bd723612c"
                 ]
