{-# OPTIONS_GHC -fno-warn-orphans         #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fission.Internal.Orphanage.ID () where

import RIO

import Data.Aeson
import Data.Aeson.Types
import Data.Scientific

import Database.Selda

instance Display (ID a) where
  display = display . fromId

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
      errMsg = modifyFailure ("parsing ID failed, " <>) . typeMismatch "Number"
