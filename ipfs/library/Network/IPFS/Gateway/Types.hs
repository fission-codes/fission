module Network.IPFS.Gateway.Types (Gateway (..)) where

import           Network.IPFS.Prelude
import           Data.Swagger (ToSchema (..))

-- | Type safety wrapper for IPFS Gateway
--   Used as cname value for DNS updates
newtype Gateway = Gateway { getGateway :: Text }
  deriving          ( Eq
                    , Generic
                    , Show
                    )
  deriving anyclass ( ToSchema )
  deriving newtype  ( IsString )

instance FromJSON Gateway where
  parseJSON = withText "AWS.Gateway" \txt ->
    Gateway <$> parseJSON (String txt)
