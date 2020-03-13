-- | App configuration for Fission apps
module Fission.Environment.FFS.Types (Environment (..)) where

import           Network.IPFS.CID.Types

import           Fission.Prelude
import qualified Fission.URL.Types as URL

data Environment = Environment
  { baseUserDataRootDomain :: !URL.DomainName -- ^ Domain Name
  , defaultDataCID         :: !CID
  }

instance Show Environment where
  show Environment {..} = intercalate "\n"
    [ "Environment {"
    , "  baseUserDataRootDomain = " <> show baseUserDataRootDomain
    , "  defaultDataCID         = " <> show defaultDataCID
    , "}"
    ]

instance FromJSON Environment where
  parseJSON = withObject "FFS.Environment" \obj -> do
    baseUserDataRootDomain <- obj .: "base_user_data_root_domain"
    defaultDataCID         <- obj .: "default_data_cid"

    return Environment {..}
