{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Fission.Internal.Orphanage.RIO () where

import RIO
import RIO.Orphans ()

import Data.Has
import Data.Pool

import Database.Selda.Backend.Internal
import Database.Selda.PostgreSQL

import qualified Fission.Config as Config
import qualified Fission.Storage.Types as DB

instance Has (DB.Pool PG) cfg => MonadSelda (RIO cfg) where
  type Backend (RIO cfg) = PG

  withConnection action = do
    DB.Pool pool <- Config.get
    withResource pool action
