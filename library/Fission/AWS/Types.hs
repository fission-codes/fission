module Fission.AWS.Types
  ( DomainName (..)
  , ZoneId (..)
  ) where

import RIO

import Data.Aeson
import Data.Swagger (ToSchema (..))
import Servant

import qualified Fission.Internal.UTF8 as UTF8

newtype DomainName = DomainName { getDomainName :: Text }
  deriving ( Eq
           , Generic
           , Show
           , Ord
           )
  deriving anyclass ( ToSchema )
  deriving newtype  ( IsString )

instance FromJSON DomainName where
  parseJSON = withText "AWS.DomainName" \txt ->
    DomainName <$> parseJSON (String txt)

instance MimeRender PlainText DomainName where
  mimeRender _ = UTF8.textToLazyBS . getDomainName

instance MimeRender OctetStream DomainName where
  mimeRender _ = UTF8.textToLazyBS . getDomainName

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
