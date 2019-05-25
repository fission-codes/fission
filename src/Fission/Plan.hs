{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Fission.Plan (Tier (..)) where

import RIO

import Control.Lens (makeLenses)
import Data.Aeson

data Tier = Free --  | Paid
  deriving (Show, Eq)

makeLenses ''Tier

instance ToJSON Tier where
  toJSON = String . textDisplay . displayShow

instance FromJSON Tier where
  parseJSON (String "Free") = return Free
  parseJSON (String "free") = return Free
  parseJSON str             = fail $ "Unable to parse " <> show str
