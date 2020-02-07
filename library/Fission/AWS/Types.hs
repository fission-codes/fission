-- | Fission-specific AWS types
module Fission.AWS.Types
  ( ZoneID (..)
  , CertARN (..)
  , Route53MockEnabled (..)
  , CertManagerMockEnabled (..)
  ) where

import Fission.Prelude

import Fission.AWS.Zone.Types
import Fission.AWS.CertManager.Types

newtype Route53MockEnabled = Route53MockEnabled Bool deriving (Show)

instance FromJSON Route53MockEnabled where
  parseJSON = withBool "AWS.Route53MockEnabled" \val ->
    Route53MockEnabled <$> parseJSON (Bool val)

newtype CertManagerMockEnabled = CertManagerMockEnabled Bool deriving (Show)

instance FromJSON CertManagerMockEnabled where
  parseJSON = withBool "AWS.CertManagerMockEnabled" \val ->
    CertManagerMockEnabled <$> parseJSON (Bool val)
