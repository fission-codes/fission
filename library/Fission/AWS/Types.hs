module Fission.AWS.Types
  ( Domain (..)
  , ZoneId (..)
  ) where

import RIO

import Data.Aeson
import Data.Swagger (ToSchema (..))
import Servant

newtype Domain = Domain { getDomain :: Text }
  deriving ( Eq
           , Generic
           , Show
           , Ord
           )
  deriving anyclass ( ToSchema )
  deriving newtype  ( IsString )

instance FromJSON Domain where
  parseJSON = withText "AWS.Domain" \txt ->
    Domain <$> parseJSON (String txt)


newtype ZoneId = ZoneId { getZoneId :: Text }
  deriving ( Eq
           , Generic
           , Show
           , Ord
           )
  deriving anyclass ( ToSchema )
  deriving newtype  ( IsString )

instance FromJSON ZoneId where
  parseJSON = withText "AWS.ZoneId" \txt ->
    ZoneId <$> parseJSON (String txt)
