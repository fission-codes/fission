-- | Fission-specific AWS types
module Fission.AWS.Types
  ( ZoneID (..)
  , Route53MockEnabled (..)
  ) where

import Fission.AWS.Zone.Types
import Fission.Prelude

newtype Route53MockEnabled = Route53MockEnabled Bool
  deriving (Show, Eq)

instance FromJSON Route53MockEnabled where
  parseJSON = withBool "AWS.Route53MockEnabled" \val ->
    Route53MockEnabled <$> parseJSON (Bool val)
