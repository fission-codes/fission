-- | Fission-speciric AWS types
module Fission.AWS.Zone.Types (ZoneID (..)) where

import RIO

import Data.Aeson
import Data.Swagger (ToSchema (..))

newtype ZoneID = ZoneID { getZoneID :: Text }
  deriving          ( Eq
                    , Generic
                    , Show
                    )
  deriving anyclass ( ToSchema )
  deriving newtype  ( IsString )

instance FromJSON ZoneID where
  parseJSON = withText "AWS.ZoneID" \txt ->
    ZoneID <$> parseJSON (String txt)
