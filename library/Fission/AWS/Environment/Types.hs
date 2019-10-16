-- | App configuration for AWS
module Fission.AWS.Environment.Types
  ( Environment (..)
  , accessKey
  , secretKey
  , zoneID
  , domainName
  ) where

import           RIO
import           RIO.List (intercalate)

import Control.Lens (makeLenses)
import Data.Aeson

import qualified Fission.AWS.Types as AWS
import qualified Network.AWS.Auth as AWS
import           Fission.Internal.Orphanage.PGConnectInfo ()

data Environment = Environment
  { _accessKey  :: !AWS.AccessKey  -- ^ Access Key
  , _secretKey  :: !AWS.SecretKey  -- ^ Secret Key
  , _zoneID     :: !AWS.ZoneID     -- ^ Hosted Zone
  , _domainName :: !AWS.DomainName -- ^ Domain Name
  }

makeLenses ''Environment

instance Show Environment where
  show Environment {..} = intercalate "\n"
    [ "Environment {"
    , "  _accessKey  = " <> show _accessKey
    , "  _secretKey  = HIDDEN"
    , "  _zoneId     = " <> show _zoneID
    , "  _domainName = " <> show _domainName
    , "}"
    ]

instance FromJSON Environment where
  parseJSON = withObject "AWS.Environment" \obj -> do
    _accessKey  <- obj .: "accessKey"
    _secretKey  <- obj .: "secretKey"
    _zoneID     <- obj .: "zoneId"
    _domainName <- obj .: "domainName"

    return $ Environment {..}
