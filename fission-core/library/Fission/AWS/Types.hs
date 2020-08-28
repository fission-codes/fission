-- | Fission-specific AWS types
module Fission.AWS.Types
  ( ZoneID (..)
  , MockRoute53 (..)
  ) where

import Fission.AWS.Zone.Types
import Fission.Prelude

newtype MockRoute53
  = MockRoute53 Bool
  deriving (Show, Eq)

instance FromJSON MockRoute53 where
  parseJSON = withBool "AWS.MockRoute53" \val ->
    MockRoute53 <$> parseJSON (Bool val)
