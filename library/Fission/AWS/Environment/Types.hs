-- | App configuration for AWS
module Fission.AWS.Environment.Types
  ( Environment (..)
  , accessKey
  , secretKey
  , zoneId
  , domain
  ) where

import           RIO
import           RIO.List (intercalate)

import Control.Lens (makeLenses)
import Data.Aeson

import qualified Fission.AWS.Types as AWS
import qualified Network.AWS.Auth as AWS
import Network.AWS.Route53 as AWS
import Fission.AWS.Types as AWS
import           Fission.Internal.Orphanage.PGConnectInfo ()

data Environment = Environment
  { _accessKey :: !AWS.AccessKey -- ^ AWS Access Key
  , _secretKey :: !AWS.SecretKey -- ^ AWS Secret Key
  , _zoneId :: !AWS.ZoneId -- ^ AWS Hosted Zone
  , _domain :: !AWS.Domain -- ^ AWS Domain
  }

makeLenses ''Environment

instance Show Environment where
  show Environment {..} = intercalate "\n"
    [ "Environment {"
    , "  _accessKey = " <> show _accessKey
    , "  _secretKey = HIDDEN"
    , "  _zoneId = " <> show _zoneId
    , "  _domain = " <> show _domain
    , "}"
    ]

instance FromJSON Environment where
  parseJSON = withObject "AWS.Environment" \obj -> do
    _accessKey  <- obj .:? "accessKey"  .!= ""
    _secretKey  <- obj .:? "secretKey"  .!= ""
    _zoneId <- obj .:? "zoneId" .!= ""
    _domain     <- obj .:? "domain"     .!= ""

    return $ Environment {..}
