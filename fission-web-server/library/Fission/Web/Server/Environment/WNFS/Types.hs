-- | App configuration for Fission apps
module Fission.Web.Server.Environment.WNFS.Types (Environment (..)) where

import           Network.IPFS.CID.Types

import           Fission.Prelude

import qualified Fission.URL.Types            as URL

import qualified Fission.Web.Server.AWS.Types as AWS

data Environment = Environment
  { baseUserDataRootDomain :: URL.DomainName -- ^ Domain name for user data
  , baseUserDataZoneID     :: AWS.ZoneID     -- ^ AWS Zone ID for the same domain name
  , defaultDataCID         :: CID            -- ^ Initial user data CID
  } deriving (Show, Eq)

instance FromJSON Environment where
  parseJSON = withObject "FFS.Environment" \obj -> do
    baseUserDataRootDomain <- obj .: "base_user_data_root_domain"
    baseUserDataZoneID     <- obj .: "base_user_data_zone_id"
    defaultDataCID         <- obj .: "default_data_cid"

    return Environment {..}
