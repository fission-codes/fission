{-# OPTIONS_GHC -fno-warn-orphans        #-}
{-# OPTIONS_GHC -fno-warn-missing-methods        #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module Fission.Internal.Orphanage () where

import RIO
import RIO.Orphans

-- import Data.Aeson
import Data.Aeson.Types
import Data.Scientific
import Data.Has
import Data.Pool
import Data.UUID

import Database.Selda
import Database.Selda.Backend

import           Fission.Config

instance Enum    UUID
instance Bounded UUID
instance SqlType UUID

instance ToJSON (ID a) where
  toJSON = Number . fromIntegral . fromId

instance FromJSON (ID a) where
  parseJSON = \case
    num@(Number n) ->
      case toBoundedInteger n of
        Nothing -> errMsg num
        Just i  -> return $ toId i

    invalid ->
      errMsg invalid

    where
      errMsg = modifyFailure ("parsing ID failed, " ++) . typeMismatch "Number"

instance Has DBPool cfg => MonadSelda (RIO cfg) where
  seldaConnection = do
    DBPool pool <- view hasLens
    liftIO $ withResource pool pure
