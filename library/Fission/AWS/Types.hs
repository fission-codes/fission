-- | Fission-speciric AWS types
module Fission.AWS.Types
  ( DomainName (..)
  , ZoneID (..)
  , MockEnabled (..)
  ) where

import Fission.AWS.DomainName.Types
import Fission.AWS.Zone.Types
import Fission.Prelude

newtype MockEnabled = MockEnabled Bool deriving (Show)-- newtype MockEnabled = Bool()
instance FromJSON MockEnabled where
  parseJSON = withBool "AWS.MockEnabled" \val ->
    MockEnabled <$> parseJSON (Bool val)