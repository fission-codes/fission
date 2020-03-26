-- | App configuration for Fission apps
module Fission.Environment.WebApp.Types (Environment (..)) where

import           Network.IPFS.CID.Types

import           Fission.Prelude
import qualified Fission.URL.Types as URL

data Environment = Environment
  { baseAppDomainName :: !URL.DomainName -- ^ Default domain name
  , appPlaceholder    :: !CID            -- ^ Initial CID
  }

instance Show Environment where
  show Environment {..} = intercalate "\n"
    [ "Environment {"
    , "  baseAppDomainName = " <> show baseAppDomainName
    , "  appPlaceholder    = " <> show appPlaceholder
    , "}"
    ]

instance FromJSON Environment where
  parseJSON = withObject "WebApp.Environment" \obj -> do
    baseAppDomainName <- obj .: "base_app_domain_name"
    appPlaceholder    <- obj .: "app_placeholder_cid"

    return Environment {..}
