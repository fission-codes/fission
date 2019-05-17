{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Fission.Pricing (Plan (..)) where

import RIO

import Control.Lens (makeLenses)

import Data.Aeson
import Data.Aeson.TH

data Plan = Free --  | Paid
  deriving (Show, Eq)

makeLenses ''Plan
$(deriveJSON defaultOptions ''Plan)
