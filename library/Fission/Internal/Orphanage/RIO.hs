{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Fission.Internal.Orphanage.RIO () where

import RIO
import RIO.Orphans ()

import SuperRecord
import Data.Pool

import Database.Selda.Backend.Internal
import Database.Selda.PostgreSQL

import qualified Fission.Storage.Types as DB

instance Has "dbPool" cfg DB.Pool => MonadSelda (RIO (Rec cfg)) where
  type Backend (RIO (Rec cfg)) = PG

  withConnection action = do
    pool <- asksR #dbPool
    withResource pool action
