{-# OPTIONS_GHC -fno-warn-orphans        #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module Fission.Storage.Internal.Orphans where

import RIO
import RIO.Orphans

import Data.Has
import Data.Pool
import Database.Selda.Backend

import Fission.Config

instance Has DBPool cfg => MonadSelda (RIO cfg) where
  seldaConnection = do
    DBPool pool <- view hasLens
    liftIO $ withResource pool pure
