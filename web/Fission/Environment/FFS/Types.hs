-- | App configuration for Fission apps
module Fission.Environment.FFS.Types (Environment (..)) where

import           Network.IPFS.CID.Types

import           Fission.Prelude
 
import qualified Fission.URL.Types as URL

data Environment = Environment
  { baseUserDataRootDomain :: !URL.DomainName -- ^ Domain name for user data
  , defaultDataCID         :: !CID            -- ^ Initial user data CID
  } deriving (Show, Eq)

instance FromJSON Environment where
  parseJSON = withObject "FFS.Environment" \obj -> do
    baseUserDataRootDomain <- obj .: "base_user_data_root_domain"
    defaultDataCID         <- obj .: "default_data_cid"

    return Environment {..}
