{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Fission.Plan (Tier (..)) where

import RIO

import Control.Lens (makeLenses)

import Data.Aeson
import Data.Aeson.TH

data Tier = Free --  | Paid
  deriving (Show, Eq)

makeLenses ''Tier
$(deriveJSON defaultOptions ''Tier)
