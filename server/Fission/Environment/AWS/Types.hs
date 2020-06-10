-- | App configuration for AWS
module Fission.Environment.AWS.Types (Environment (..)) where

import qualified Network.AWS.Auth  as AWS

import qualified Fission.AWS.Types as AWS
import           Fission.Prelude

data Environment
  = Environment
      { accessKey   :: !AWS.AccessKey -- ^ Access Key
      , secretKey   :: !AWS.SecretKey -- ^ Secret Key
      , baseZoneID  :: !AWS.ZoneID    -- ^ Hosted Zone of
      , mockRoute53 :: !AWS.MockRoute53
      }
  deriving Eq

instance Show Environment where
  show Environment {..} = intercalate "\n"
    [ "Environment {"
    , "  accessKey   = HIDDEN"
    , "  secretKey   = HIDDEN"
    , "  baseZoneId  = " <> show baseZoneID
    , "  mockRoute53 = " <> show mockRoute53
    , "}"
    ]

instance FromJSON Environment where
  parseJSON = withObject "AWS.Environment" \obj -> do
    accessKey   <- obj .:  "access_key"
    secretKey   <- obj .:  "secret_key"
    baseZoneID  <- obj .:  "zone_id"
    mockRoute53 <- obj .:? "mock_route53" .!= AWS.MockRoute53 False

    return Environment {..}
