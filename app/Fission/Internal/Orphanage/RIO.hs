{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Fission.Internal.Orphanage.RIO () where

import Data.Pool
import RIO.Orphans ()

import Database.Selda.Backend.Internal
import Database.Selda.PostgreSQL

import Fission.Prelude

import qualified Fission.Config as Config
import qualified Fission.Storage.Types as DB

instance Has (DB.Pool PG) cfg => MonadSelda (RIO cfg) where
  type Backend (RIO cfg) = PG

  withConnection action = do
    DB.Pool pool <- Config.get
    withResource pool action
